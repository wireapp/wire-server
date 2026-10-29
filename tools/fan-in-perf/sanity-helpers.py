#!/usr/bin/env python3
"""Checks the results of run-2000x1.py against the database and Prometheus
(through Grafana). Read-only. Prints facts; judging them is up to the reader.

  sanity-helpers.py db LOG       current DB state vs. the run that produced it
                                 (each run starts with reset, so only the last
                                 run can be checked, normally D-plain.log)
  sanity-helpers.py prom [DIR]   per run: Prometheus totals vs. tool summary
                                 (default DIR: run-2000x1.py's LOGDIR)
  sanity-helpers.py gc LOG       partition drops (oldest partition age resets)
                                 and metrics near them vs. otherwise (D-partman.log)

Run windows come from the log files: end = mtime, length = last status tick.
"""

import glob
import importlib.util
import json
import os
import re
import statistics
import subprocess
import sys
import urllib.parse
import urllib.request

_spec = importlib.util.spec_from_file_location("run2000x1", os.path.join(os.path.dirname(os.path.abspath(__file__)), "run-2000x1.py"))
run = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(run)

# Partition count and oldest partition of user_notifications (pg_partman names
# partitions by start time, so the smallest name is the oldest; '' if unpartitioned).
CHILDREN = "select count(*), coalesce(min(c.relname), '') from pg_inherits i join pg_class c on c.oid = i.inhrelid where i.inhparent = 'user_notifications'::regclass"


def psql(sql):
    r = subprocess.run(["psql", run.DB, "-X", "-At", "-F", "|", "-q", "-v", "ON_ERROR_STOP=1", "-c", sql], capture_output=True, text=True)
    if r.returncode != 0:
        raise RuntimeError("psql failed: " + r.stderr.strip()[-200:])
    return r.stdout.strip()


PROM = "http://localhost:3000/api/datasources/proxy/uid/prometheus/api/v1/"


def prom(path, **params):
    with urllib.request.urlopen(PROM + path + "?" + urllib.parse.urlencode(params), timeout=10) as r:
        return json.load(r)["data"]["result"]


def window(logpath):
    """(start, end) in epoch seconds of the run that wrote logpath; start = end - last tick - 2."""
    last = max(int(m) for m in re.findall(r"^t=(\d+)s", open(logpath).read(), re.M))
    end = int(os.path.getmtime(logpath))
    return end - last - 2, end


def check_db(logpath):
    r = run.summary_fields(logpath)
    q = psql
    rows, distinct, users = q("select count(*), count(distinct notification_id), count(distinct user_id) from user_notifications").split("|")
    partitioned = q("select count(*) from pg_partitioned_table where partrelid = 'user_notifications'::regclass") == "1"
    print(f"user_notifications: {rows} rows, {distinct} distinct ids, {users} users; tool pushes {r['pushes']}"
          + (" (partitioned: rows < pushes once TTL expired data)" if partitioned else " (plain: expect rows = pushes)"))
    print("last_user_notifications rows not equal to the user's newest notification:", q(
        "select count(*) from last_user_notifications l join (select distinct on (user_id) user_id, notification_id m "
        "from user_notifications order by user_id, notification_id desc) u using (user_id) where l.notification_id <> u.m"))
    acks, mismatched = q("select count(*), count(*) filter (where a.last_notification_id <> l.notification_id) "
                         "from user_notification_acks a join last_user_notifications l using (user_id)").split("|")
    print(f"user_notification_acks: {acks} rows, {mismatched} not at the stream's last id (consume-ack with missing=0: expect {run.READERS} and 0)")
    print("rows in other notification tables:", q("select (select count(*) from client_notifications) + (select count(*) from team_notifications) + (select count(*) from epoch_notifications) + (select count(*) from local_connection_notifications) + (select count(*) from remote_connection_notifications)"))
    if partitioned:
        print("partitions, oldest:", q(CHILDREN).replace("|", ", "), "| rows in the parent itself:", q("select count(*) from only user_notifications"),
              "| default partition:", q("select count(*) from pg_partitioned_table where partrelid = 'user_notifications'::regclass and partdefid <> 0"))
    print("pg_stat_database deadlocks, conflicts, rollbacks:", q("select deadlocks || ', ' || conflicts || ', ' || xact_rollback from pg_stat_database where datname = current_database()"))


def check_prom(logdir):
    for logpath in sorted(glob.glob(os.path.join(logdir, "*.log"))):
        r = run.summary_fields(logpath)
        t0, t1 = window(logpath)
        d = t1 - t0

        def total(metric):
            res = prom("query", query=f"sum(max_over_time({metric}[{d}s]))", time=t1)
            return int(float(res[0]["value"][1])) if res else 0

        up = prom("query", query=f'min_over_time(up{{job="fan-in-perf"}}[{max(d - 5, 1)}s])', time=t1)
        errs = sum(total(m) for m in ("fanin_perf_errors_total", "fanin_perf_fetch_errors_total", "fanin_perf_ack_errors_total"))
        parts = [
            f"pushes prom {total('fanin_perf_pushes_total'):,} / tool {r['pushes']:,}" if r["pushes"] is not None else None,
            f"delivered {total('fanin_perf_consumed_total'):,} / {r['delivered']:,}" if r["delivered"] is not None else None,
            f"acks {total('fanin_perf_acks_total'):,} / {r['acks']:,}" if r["acks"] is not None else None,
            f"errors prom {errs} / tool {r['errors']}",
            f"scrape up min {up[0]['value'][1] if up else '-'}",
        ]
        print(f"{os.path.basename(logpath)}: " + ", ".join(x for x in parts if x))


def check_gc(logpath):
    t0, t1 = window(logpath)
    gc = run.summary_fields(logpath)["gc"]
    if not gc:
        sys.exit("no gc section in the summary (not a consume run on partitioned tables)")
    gc_window = gc["window_seconds"]
    queries = {
        "push/s": "sum(fanin_perf_push_rate_current)",
        "ack/s": "sum(fanin_perf_ack_rate_current)",
        "backlog": "sum(fanin_perf_backlog)",
        "push p99 s": "histogram_quantile(0.99, sum by (le) (rate(fanin_perf_push_duration_seconds_bucket[2s])))",
        "fetch p99 s": "histogram_quantile(0.99, sum by (le) (rate(fanin_perf_fetch_duration_seconds_bucket[2s])))",
        "ack p99 s": "histogram_quantile(0.99, sum by (le) (rate(fanin_perf_ack_duration_seconds_bucket[2s])))",
    }

    def series(q, step=1):
        res = prom("query_range", query=q, start=t0 + run.WARMUP, end=t1, step=step)
        return {int(t): float(v) for t, v in res[0]["values"]} if res else {}

    age = series('max(fan_in_partman_oldest_partition_age_seconds{parent=~".*user_notifications"})')
    if not age:
        sys.exit("no fan_in_partman_* data in the run window (plain run, or exporter down?)")
    ts = sorted(age)
    drops = [t for a, t in zip(ts, ts[1:]) if age[t] < age[a] - 30]
    print(f"drops: {len(drops)} at +{[t - t0 for t in drops]} s; oldest partition age {min(age.values()):.0f}-{max(age.values()):.0f} s; "
          f"max seconds since maintenance {max(series('max(fan_in_partman_seconds_since_maintenance)').values(), default=float('nan')):.1f}")
    near = lambda t: any(abs(t - d) <= gc_window for d in drops)
    for name, q in queries.items():
        s = {t: v for t, v in series(q).items() if v == v}
        i, o = [v for t, v in s.items() if near(t)], [v for t, v in s.items() if not near(t)]
        if i and o:
            print(f"{name}: within {gc_window} s of a drop median {statistics.median(i):.4g} max {max(i):.4g} (n={len(i)}) | "
                  f"other median {statistics.median(o):.4g} max {max(o):.4g} (n={len(o)})")


def main():
    cmd, arg = (sys.argv[1:] + [None, None])[:2]
    if cmd == "db" and arg:
        check_db(arg)
    elif cmd == "prom":
        check_prom(arg or run.LOGDIR)
    elif cmd == "gc" and arg:
        check_gc(arg)
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    main()
