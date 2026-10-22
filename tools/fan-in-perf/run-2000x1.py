#!/usr/bin/env python3
"""Runs fan-in-perf experiments A-D (WPB-26288) with --targets user:2000x1 and
2,000 clients on pg_partman (partitioned) and plain tables, seed 42, and writes
a markdown report for the Jira ticket.

  run-2000x1.py [OUT.md]   (default: fan-in-2000x1-results.md)

DESTRUCTIVE: drops and recreates the database fan_in_perf. Takes about 40
minutes. Settings were tuned on the author's machine (22 CPUs, 62 GiB) before.
The tool's output of every run goes to LOGDIR. Checks of the results against
the database and Prometheus: sanity-helpers.py.
"""

import datetime
import os
import re
import statistics
import subprocess
import sys
import threading
import time
import urllib.request

DB = "postgresql://wire-server:posty-the-gres@localhost:5432/fan_in_perf"
GRAFANA = "http://localhost:3000/d/fan-in-perf/fan-in-perf"
REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
LOGDIR = "/tmp/fan-in-2000x1"
SEED = 42
TARGETS = "user:2000x1"
CLIENTS = 2000
# Tuned beforehand on plain (45 s runs, repeated): A max push/s; B, C highest rate
# whose readers kept up (backlog at writer stop < 1 s, no loss) in every run.
W_A, W_B, W_C = 64, 32, 16
POOL_B, POOL_C = 64, 64
WARMUP, DRAIN = 5, 30


def log(msg):
    print(f"[{datetime.datetime.now():%H:%M:%S}] {msg}", file=sys.stderr, flush=True)


def psql(sql):
    r = subprocess.run(["psql", DB, "-X", "-At", "-F", "|", "-q", "-v", "ON_ERROR_STOP=1", "-c", sql], capture_output=True, text=True)
    if r.returncode != 0:
        raise RuntimeError("psql failed: " + r.stderr.strip()[-200:])
    return r.stdout.strip()


def fip(args):
    return ["cabal", "run", "exe:fan-in-perf", "--", "--db", DB] + [str(a) for a in args]


def tool(args):
    r = subprocess.run(fip(args), cwd=REPO, capture_output=True, text=True)
    if r.returncode != 0:
        raise RuntimeError(f"fan-in-perf {args[0]} failed: {(r.stdout + r.stderr).strip()[-300:]}")


def deadlocks():
    return int(psql("select deadlocks from pg_stat_database where datname = current_database()"))


# Partition count and oldest partition of user_notifications (pg_partman names
# partitions by start time, so the smallest name is the oldest; '' if unpartitioned).
CHILDREN = "select count(*), coalesce(min(c.relname), '') from pg_inherits i join pg_class c on c.oid = i.inhrelid where i.inhparent = 'user_notifications'::regclass"


class DropCounter(threading.Thread):
    """Records the times of pg_partman drops: changes of the oldest child of user_notifications, polled every second."""

    def __init__(self):
        super().__init__(daemon=True)
        self.stop = threading.Event()
        self.drops = []
        self.error = None  # a failed poll ends counting; re-raised after join

    def run(self):
        last = None
        try:
            while not self.stop.wait(1):
                oldest = psql(CHILDREN).split("|")[1]
                if last is not None and oldest != last:
                    self.drops.append(time.time())
                last = oldest
        except Exception as e:
            self.error = e


# Per-second status line of consume-ack, parsed for the drop impact of D on
# partman only. Groups: second, push/s, ack/s, ack p99, lag p99, backlog (rates
# of that second; p99 is "-" in a second without acks/deliveries), e.g.
#   t=5s push/s cur=4,358 max=- | cons/s cur=4,099 max=- | ack/s cur=4,068 max=- p99<=6.3ms | lag p50<=58.7ms p99<=117.4ms | backlog=259 | ...
TICK = re.compile(r"^t=(\d+)s push/s cur=([\d,]+) .*\| ack/s cur=([\d,]+) max=\S+ p99<?=(\S+) \| lag p50<?=\S+ p99<?=(\S+) \| backlog=([\d,]+)")
GC_WINDOW = 5  # seconds around a drop


def secs(s):
    """"6.3ms" / "1.07s" -> seconds; "-" (nothing measured in that second) -> None."""
    return None if s == "-" else float(s[:-2]) / 1000 if s.endswith("ms") else float(s[:-1])


def gc_impact(ticks, drops, duration):
    """Medians of the per-second status lines within GC_WINDOW s of a drop vs. all other seconds (after warmup)."""
    rows = {True: [], False: []}
    for t, line in ticks:
        m = TICK.match(line)
        if m and WARMUP < int(m.group(1)) <= duration:
            rows[any(abs(t - d) <= GC_WINDOW for d in drops)].append(m.groups()[1:])
    def med(rs, i, f):
        v = [x for x in (f(r[i]) for r in rs) if x is not None]
        return statistics.median(v) if v else None

    as_int = lambda x: int(x.replace(",", ""))
    return {k: {"seconds": len(rs), "push_s": med(rs, 0, as_int), "ack_s": med(rs, 1, as_int), "ack_p99": med(rs, 2, secs),
                "lag_p99": med(rs, 3, secs), "backlog": med(rs, 4, as_int), "ack_p99_max": max((x for x in (secs(r[2]) for r in rs) if x is not None), default=None)}
            for k, rs in rows.items()}


def num(pat, text, conv=lambda x: int(x.replace(",", ""))):
    m = re.search(pat, text, re.M)
    return conv(m.group(1)) if m else None


def parse(text):
    s = text[text.index("\nsummary:"):]
    r = {
        "pushes": num(r"^  pushes=([\d,]+)", s),
        "push_err": num(r"^  pushes=.* errors=([\d,]+)", s),
        "push_s": num(r"push/s avg=([\d,]+)", s),
        "push_p99": num(r"^  latency .*p99<=(\S+)", s, str),
        "cons_s": num(r"cons/s avg=([\d,]+)", s),
        "missing": num(r"missing=([\d,]+)", s),
        "dup": num(r"duplicates=([\d,]+)", s),
        "backlog": num(r"backlog at writer stop: ([\d,]+)", s),
        "lag_p99": num(r"^  lag .*p99<=(\S+)", s, str),
        "fetch_err": num(r"^  fetch .* errors=([\d,]+)", s),
        "ack_s": num(r"ack/s avg=([\d,]+)", s),
        "ack_err": num(r"^  acks=[\d,]+ errors=([\d,]+)", s),
        "ack_p99": num(r"^  ack latency .*p99<=(\S+)", s, str),
        "drain": num(r"^  drain: (.*)$", s, str),
    }
    r["errors"] = sum(r[k] or 0 for k in ("push_err", "fetch_err", "ack_err"))
    r["summary"] = s.strip()
    return r


def run(label, args, partman):
    tool(["reset"])
    psql("checkpoint")
    d0 = deadlocks()
    logpath = os.path.join(LOGDIR, label + ".log")
    dc = DropCounter() if partman and label.startswith("D") else None
    ticks = []  # (wall time, status line), to line them up with the drops
    with open(logpath, "w") as lf:
        if dc:
            dc.start()
        p = subprocess.Popen(fip(args), cwd=REPO, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        for line in p.stdout:
            lf.write(line)
            if dc and line.startswith("t="):
                ticks.append((time.time(), line))
        rc = p.wait()
        if dc:
            dc.stop.set()
            dc.join()
            if dc.error:
                raise dc.error
    text = open(logpath).read()
    if "\nsummary:" not in text:
        raise RuntimeError(f"{label}: no summary (exit {rc}), see {logpath}")
    time.sleep(2)
    r = parse(text)
    r.update(label=label, exit=rc, deadlocks=deadlocks() - d0, cmd="fan-in-perf --db \"$DB\" " + " ".join(str(a) for a in args))
    if dc:
        r.update(drops=len(dc.drops), gc=gc_impact(ticks, dc.drops, args[args.index("--duration") + 1]),
                 rows=int(psql("select count(*) from user_notifications")))  # pushes - rows = expired by TTL
    return r


def common(exp, w, pool, dur):
    a = [exp, "--writers", w, "--targets", TARGETS, "--warmup", WARMUP, "--duration", dur, "--seed", SEED]
    if exp != "produce":
        a += ["--clients", CLIENTS, "--reader-pool-size", pool, "--safety-window", 200, "--drain-timeout", DRAIN]
    if exp == "consume-ack":
        a += ["--ack-every", 1]
    return a


EXPERIMENTS = [
    ("A", common("produce", W_A, None, 60)),
    ("B", common("consume", W_B, POOL_B, 120)),
    ("C", common("consume-ack", W_C, POOL_C, 120)),
    ("D", common("consume-ack", W_C, POOL_C, 660)),
]


def settle_partman():
    """The first maintenance run drops the premade past partitions in bulk; wait until the count is stable for 75 s."""
    log("waiting for pg_partman to settle")
    last, since, deadline = None, time.monotonic(), time.monotonic() + 240
    while time.monotonic() < deadline:
        n = psql(CHILDREN).split("|")[0]
        if n != last:
            last, since = n, time.monotonic()
        elif time.monotonic() - since >= 75:
            return
        time.sleep(5)
    log("warning: partition count did not settle")


# ------------------------------------------------------------ report

MODES = ("partman", "plain")
COLS = {
    "A": [("push/s (avg)", "push_s"), ("push p99", "push_p99"), ("errors", "errors")],
    "B": [("push/s (avg)", "push_s"), ("cons/s (avg)", "cons_s"), ("lag p99", "lag_p99"), ("backlog at stop", "backlog"), ("missing", "missing"), ("errors", "errors")],
    "C": [("push/s (avg)", "push_s"), ("cons/s (avg)", "cons_s"), ("ack/s (avg)", "ack_s"), ("ack p99", "ack_p99"), ("lag p99", "lag_p99"),
          ("backlog at stop", "backlog"), ("missing", "missing"), ("errors", "errors")],
}
COLS["D"] = COLS["C"]
TITLES = {
    "A": "A: maximal rate of adding notifications",
    "B": "B: adding and consuming by 2,000 clients",
    "C": "C: B, every client acknowledges every notification",
    "D": "D: C for 11 minutes while TTL garbage collection runs",
}


def cell(v):
    return "-" if v is None else f"{v:,}" if isinstance(v, int) else str(v)


def report(res, env):
    L = ["# Notification fan-in: PostgreSQL benchmark, `user:2000x1`, 2,000 clients (WPB-26288)", "",
         "## Setup", "",
         f"- Date {env['date']}, commit `{env['commit']}`, driver `tools/fan-in-perf/run-2000x1.py`",
         f"- Host: {env['cpus']} CPUs, {env['mem']} RAM; tool and database share it. {env['pg']}; {env['settings']}",
         f"- pg_partman {env['partman']}: six `*_notifications` tables range-partitioned by UUIDv7, 1 min partitions, 5 min retention, no default partition, maintenance by the background worker every 60 s. `plain`: unpartitioned, no TTL.",
         f"- Load: `--targets {TARGETS}`: 2,000 users with one client each; every push writes 1 notification to 1 user stream (plus its `last_user_notifications` upsert) in one transaction, read by 1 client, so cons/s = push/s. B-D: `--clients {CLIENTS}`, poll interval 100 ms, fetch limit 100, safety window 200 ms; C, D: ack after every notification.",
         f"- Writers A={W_A}, B={W_B}, C/D={W_C}; reader pool B={POOL_B}, C/D={POOL_C}: highest rate in sweeps beforehand at which readers keep up. Seed {SEED}. One run each.",
         "- Rates are averages over the writer phase incl. 5 s warmup; p99 are histogram bucket upper bounds (at most 25 % above). `backlog at stop`: expected minus delivered when the writers stopped. `missing`: never delivered after the drain. `errors`: failed pushes + fetches + acks.",
         "- Only the `user` stream kind is exercised (no client/team/epoch streams, fan-out 1); not comparable to runs with a mixed target set.",
         "",
         "Per mode: `drop-db`, `migrate` (`--partitioned` for partman), before each run `reset`. Commands:", "", "```"]
    L += [r["cmd"] for r in res.values() if r["label"].endswith("plain")]
    L += ["```", "(partman: same commands)", ""]
    for exp in "ABCD":
        L += [f"## {TITLES[exp]}", "", "| Mode | " + " | ".join(h for h, _ in COLS[exp]) + " |", "|---" * (len(COLS[exp]) + 1) + "|"]
        for m in MODES:
            r = res.get(f"{exp}-{m}")
            if r:
                L.append(f"| {m} | " + " | ".join(cell(r[k]) for _, k in COLS[exp]) + " |")
        L.append("")
    pm = res.get("D-partman")
    if pm:
        expired = pm["pushes"] - pm["rows"]
        secs_expired = expired / pm["push_s"] if pm["push_s"] else 0
        i, o = pm["gc"][True], pm["gc"][False]
        f = lambda x, ms=False: "-" if x is None else (f"{x * 1000:.1f}ms" if x < 1 else f"{x:.2f}s") if ms else f"{x:,.0f}"
        L += [f"D partman: {pm['drops']} partition drops (each takes an `ACCESS EXCLUSIVE` lock on the parent); {expired:,} rows (about {secs_expired:.0f} s of pushes) were expired by TTL, i.e. the later drops held data.", "",
              f"Impact of the drops: per-second status lines within {GC_WINDOW} s of a drop vs. all other seconds of D partman (medians):", "",
              "| Window | seconds | push/s (median) | ack/s (median) | ack p99 (median) | ack p99 max | lag p99 (median) | backlog (median) |", "|---|---|---|---|---|---|---|---|",
              f"| within {GC_WINDOW} s of a drop | {i['seconds']} | {f(i['push_s'])} | {f(i['ack_s'])} | {f(i['ack_p99'], True)} | {f(i['ack_p99_max'], True)} | {f(i['lag_p99'], True)} | {f(i['backlog'])} |",
              f"| other | {o['seconds']} | {f(o['push_s'])} | {f(o['ack_s'])} | {f(o['ack_p99'], True)} | {f(o['ack_p99_max'], True)} | {f(o['lag_p99'], True)} | {f(o['backlog'])} |", ""]

    L += ["## Answers", ""]
    g = lambda e, m: res.get(f"{e}-{m}", {})
    for m in MODES:
        a = g("A", m)
        L.append(f"- a. ({m}) {cell(a.get('push_s'))} notifications/s (avg) added, push p99 {a.get('push_p99')}, {cell(a.get('errors'))} errors.")
    for m in MODES:
        b = g("B", m)
        L.append(f"- b. ({m}) {cell(b.get('push_s'))} added/s (avg) and {cell(b.get('cons_s'))} consumed/s (avg) by 2,000 clients, lag p99 {b.get('lag_p99')}, {cell(b.get('missing'))} missing.")
    for m in MODES:
        c = g("C", m)
        L.append(f"- c. ({m}) {cell(c.get('push_s'))} added/s (avg), {cell(c.get('cons_s'))} consumed/s (avg) and {cell(c.get('ack_s'))} acknowledged/s (avg), ack p99 {c.get('ack_p99')}, {cell(c.get('missing'))} missing.")
    dp = g("D", "partman")
    if dp:
        i, o = dp["gc"][True], dp["gc"][False]
        r = lambda a, b: f"{a / b:.2f}" if a is not None and b else "-"
        L.append(f"- d. During TTL GC ({dp['drops']} partition drops in 11 min): within {GC_WINDOW} s of a drop vs. otherwise, ack/s {r(i['ack_s'], o['ack_s'])} x, "
                 f"ack p99 {r(i['ack_p99'], o['ack_p99'])} x, lag p99 {r(i['lag_p99'], o['lag_p99'])} x (medians); {cell(dp['errors'])} errors, {cell(dp['missing'])} missing, {cell(dp['deadlocks'])} deadlocks.")
    notes = []
    for r in res.values():
        if r["drain"] and not r["drain"].startswith("complete"):
            notes.append(f"- {r['label']}: drain {r['drain']}")
        if r["dup"]:
            notes.append(f"- {r['label']}: {r['dup']:,} duplicates")
        if r["deadlocks"]:
            notes.append(f"- {r['label']}: {r['deadlocks']} deadlocks")
        if r["exit"]:
            notes.append(f"- {r['label']}: fan-in-perf exited {r['exit']} after the summary")
    L += ["", "## Notes", ""] + (notes or ["- No deadlocks, no duplicates, all drains complete."]) + [""]
    L += ["<details><summary>Tool summaries</summary>", ""]
    for r in res.values():
        L += [f"`{r['label']}`", "", "```", r["summary"], "```", ""]
    L += ["</details>", ""]
    return "\n".join(L)


def env():
    kb = int(re.search(r"MemTotal:\s+(\d+)", open("/proc/meminfo").read()).group(1))
    return {
        "date": datetime.date.today().isoformat(),
        "commit": subprocess.run(["git", "log", "-1", "--format=%h"], cwd=REPO, capture_output=True, text=True).stdout.strip(),
        "cpus": os.cpu_count(),
        "mem": f"{kb / 1048576:.0f} GiB",
        "pg": psql("select split_part(version(), ' on ', 1)"),
        "settings": psql("select string_agg(name || '=' || current_setting(name), ', ' order by name) from pg_settings "
                         "where name in ('shared_buffers', 'max_wal_size', 'synchronous_commit', 'max_connections')"),
    }


def main():
    out = sys.argv[1] if len(sys.argv) > 1 else "fan-in-2000x1-results.md"
    try:
        urllib.request.urlopen("http://localhost:9400/metrics", timeout=1)
        sys.exit("port 9400 is busy (another fan-in-perf run?)")
    except OSError:
        pass
    log("building fan-in-perf")
    subprocess.run(["cabal", "build", "exe:fan-in-perf"], cwd=REPO, check=True, stdout=subprocess.DEVNULL)
    os.makedirs(LOGDIR, exist_ok=True)
    res, e = {}, None
    for mode in MODES:
        log(f"database for {mode}")
        tool(["drop-db"])
        tool(["migrate"] + (["--partitioned"] if mode == "partman" else []))
        if e is None:
            e = env()
        if mode == "partman":
            e["partman"] = psql("select extversion from pg_extension where extname = 'pg_partman'")
            settle_partman()
        for exp, args in EXPERIMENTS:
            log(f"{exp} {mode}")
            res[f"{exp}-{mode}"] = run(f"{exp}-{mode}", args, mode == "partman")
    md = report(res, e)
    with open(out, "w") as f:
        f.write(md)
    print(md)
    log(f"report: {out}; logs: {LOGDIR}; Grafana: {GRAFANA}")


if __name__ == "__main__":
    main()
