#!/usr/bin/env python3
"""Runs fan-in-perf experiments A-D (WPB-26288) with --targets user:2000x1 and
2,000 clients on pg_partman (partitioned) and plain tables, seed 42, and writes
a markdown report for the Jira ticket. E and F (plain only) split B and C: first
`produce`, then `consume-only[-ack]` on the rows it left, for the raw consumption
speed.

  run-2000x1.py [OUT.md] [--only A,E,...] [--quick] [--db CONNSTR]
  (OUT.md default: fan-in-2000x1-results.md)

DESTRUCTIVE: drops and recreates the database (default fan_in_perf). Takes about
40 minutes for A-D plus about 10 for E and F (--only E,F: the latter only,
--quick: all durations divided by 10, for a smoke test). Settings were tuned on
the author's machine (22 CPUs, 62 GiB) before.
The tool's output of every run goes to LOGDIR. Checks of the results against
the database and Prometheus: sanity-helpers.py.
"""

import argparse
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
# E, F: produce (W_A writers) for SPLIT_PRODUCE s, then consume-only. Long enough
# for the consume phase to last well past SPLIT_WARMUP (max rate ignores it).
SPLIT_PRODUCE, SPLIT_WARMUP, SPLIT_TIMEOUT = 120, 2, 1200
# Reader pool of E and F. With 64, the readers sometimes idled for about 5 s
# (while all connections were idle, cause not found), which dominates the
# average; 16, 32 and 48 never did in repeated runs.
POOL_SPLIT = 32
QUICK = 1  # --quick: durations divided by this


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
    if dc:
        dc.start()
    text, rc = tool_logged(logpath, args, (lambda line: ticks.append((time.time(), line)) if dc and line.startswith("t=") else None))
    if dc:
        dc.stop.set()
        dc.join()
        if dc.error:
            raise dc.error
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
    a = [exp, "--writers", w, "--targets", TARGETS, "--warmup", WARMUP, "--duration", max(1, dur // QUICK), "--seed", SEED]
    if exp != "produce":
        a += ["--clients", CLIENTS, "--reader-pool-size", pool, "--safety-window", 200, "--drain-timeout", DRAIN]
    if exp == "consume-ack":
        a += ["--ack-every", 1]
    return a


def experiments():
    """Built at run time: --quick changes the durations."""
    return [
        ("A", common("produce", W_A, None, 60)),
        ("B", common("consume", W_B, POOL_B, 120)),
        ("C", common("consume-ack", W_C, POOL_C, 120)),
        ("D", common("consume-ack", W_C, POOL_C, 660)),
    ]


def split_experiments():
    """(label, produce args, consume-only args), run on plain tables only: the 5 min
    retention of the partitioned tables would drop partitions during the consume phase."""
    produce = common("produce", W_A, None, SPLIT_PRODUCE)
    consume = ["--targets", TARGETS, "--seed", SEED, "--clients", CLIENTS, "--warmup", SPLIT_WARMUP, "--timeout", SPLIT_TIMEOUT]
    return [
        ("E", produce, ["consume-only", "--reader-pool-size", POOL_SPLIT] + consume),
        ("F", produce, ["consume-only-ack", "--reader-pool-size", POOL_SPLIT, "--ack-every", 1] + consume),
    ]


def tool_logged(logpath, args, on_line=None):
    """Runs the tool, logs its output (and passes each line to on_line); returns (output, exit code)."""
    lines = []
    with open(logpath, "w") as lf:
        p = subprocess.Popen(fip(args), cwd=REPO, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        for line in p.stdout:
            lf.write(line)
            lines.append(line)
            if on_line:
                on_line(line)
        rc = p.wait()
    return "".join(lines), rc


def parse_consume_only(text):
    """The consume-only log: `parse` for the summary lines it shares with the others, plus its own."""
    s = text[text.index("\nsummary:"):]
    r = parse(text)
    r.update(
        rows_total=num(r"^consume-only(?:-ack)?: rows total=([\d,]+)", text),
        expected=num(r"expected=([\d,]+) missing", s),
        delivered=num(r"delivered=([\d,]+)", s),
        cons_time=num(r"^summary: duration=([\d.]+)s", s, float),
        cons_max=num(r"cons/s avg=[\d,]+ max=(\S+)", s, lambda x: x if x == "-" else int(x.replace(",", ""))),
        fetch_p99=num(r"^  fetch .*p99<=(\S+)", s, str),
        t50=num(r"^  time to delivered: 50%=(\S+)", s, str),
        t90=num(r"^  time to delivered: .* 90%=(\S+)", s, str),
        t99=num(r"^  time to delivered: .* 99%=(\S+)", s, str),
        end=num(r"^  end: (.*)$", s, str),
    )
    return r


def run_split(label, produce_args, consume_args):
    """reset, produce, VACUUM ANALYZE + CHECKPOINT (else autovacuum and hint-bit
    writes would land in the measured read phase), consume-only."""
    tool(["reset"])
    psql("checkpoint")
    d0 = deadlocks()
    ptext, prc = tool_logged(os.path.join(LOGDIR, label + "-produce.log"), produce_args)
    if "\nsummary:" not in ptext:
        raise RuntimeError(f"{label}: no produce summary (exit {prc}), see {LOGDIR}")
    p = parse(ptext)
    psql("vacuum analyze")
    psql("checkpoint")
    rows = int(psql("select count(*) from user_notifications"))
    ctext, crc = tool_logged(os.path.join(LOGDIR, label + "-consume.log"), consume_args)
    if "\nsummary:" not in ctext:
        raise RuntimeError(f"{label}: no consume summary (exit {crc}), see {LOGDIR}")
    c = parse_consume_only(ctext)
    cmd = lambda a: "fan-in-perf --db \"$DB\" " + " ".join(str(x) for x in a)
    return {
        **c,
        "label": label, "exit": prc or crc, "deadlocks": deadlocks() - d0, "rows": rows, "drain": None,
        "pushes": p["pushes"], "push_s": p["push_s"], "push_p99": p["push_p99"], "push_err": p["push_err"],
        "errors": p["errors"] + c["errors"],
        "cmd": cmd(produce_args) + "\npsql -c 'VACUUM ANALYZE' -c CHECKPOINT\n" + cmd(consume_args),
        "summary": "produce:\n" + p["summary"] + "\n\nconsume:\n" + c["summary"],
    }


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
COLS["E"] = [("push/s (produce)", "push_s"), ("rows", "rows"), ("consume time (s)", "cons_time"), ("cons/s (avg)", "cons_s"), ("cons/s (max)", "cons_max"),
             ("time to 50 %", "t50"), ("90 %", "t90"), ("99 %", "t99"), ("fetch p99", "fetch_p99"), ("missing", "missing"), ("errors", "errors")]
COLS["F"] = COLS["E"][:8] + [("ack/s (avg)", "ack_s"), ("ack p99", "ack_p99")] + COLS["E"][8:]
TITLES = {
    "A": "A: maximal rate of adding notifications",
    "B": "B: adding and consuming by 2,000 clients",
    "C": "C: B, every client acknowledges every notification",
    "D": "D: C for {D} while TTL garbage collection runs",
    "E": "E: B split: produce only, then 2,000 clients consume only",
    "F": "F: C split: produce only, then 2,000 clients consume and acknowledge only",
}


def cell(v):
    return "-" if v is None else f"{v:,}" if isinstance(v, int) else str(v)


def duration_text(secs):
    return f"{secs // 60} minutes" if secs % 60 == 0 else f"{secs} s"


def report(res, env):
    dur = {x: int(a[a.index("--duration") + 1]) for x, a in experiments()}  # as run (--quick)
    split_dur = int(split_experiments()[0][1][split_experiments()[0][1].index("--duration") + 1])
    partman = any(k.endswith("-partman") for k in res)
    L = ["# Notification fan-in: PostgreSQL benchmark, `user:2000x1`, 2,000 clients (WPB-26288)", "",
         "## Setup", "",
         f"- Date {env['date']}, commit `{env['commit']}`, driver `tools/fan-in-perf/run-2000x1.py`",
         f"- Host: {env['cpus']} CPUs, {env['mem']} RAM; tool and database share it. {env['pg']}; {env['settings']}",
         *([f"- pg_partman {env['partman']}: six `*_notifications` tables range-partitioned by UUIDv7, 1 min partitions, 5 min retention, no default partition, maintenance by the background worker every 60 s. `plain`: unpartitioned, no TTL.",] if partman else []),
         f"- Load: `--targets {TARGETS}`: 2,000 users with one client each; every push writes 1 notification to 1 user stream (plus its `last_user_notifications` upsert) in one transaction, read by 1 client, so cons/s = push/s. B-D: `--clients {CLIENTS}`, poll interval 100 ms, fetch limit 100, safety window 200 ms; C, D: ack after every notification.",
         f"- Writers A={W_A}, B={W_B}, C/D={W_C}; reader pool B={POOL_B}, C/D={POOL_C}: highest rate in sweeps beforehand at which readers keep up. Seed {SEED}. One run each.",
         f"- Rates are averages over the writer phase incl. {WARMUP} s warmup; p99 are histogram bucket upper bounds (at most 25 % above). `backlog at stop`: expected minus delivered when the writers stopped. `missing`: never delivered after the drain. `errors`: failed pushes + fetches + acks.",
         f"- E, F (plain only): `reset`, `produce` ({W_A} writers, {split_dur} s), `VACUUM ANALYZE` + `CHECKPOINT`, then `consume-only[-ack]` with the same seed and targets. The rows were just written, so caches are warm. Readers start at the oldest row (a client that was offline and acknowledged nothing). cons/s avg = delivered / consume time (until the last reader caught up), incl. warmup and that tail; `time to N %`: until N % of the notifications were delivered (50 ms resolution), to tell the tail from the steady rate; max ignores the first {SPLIT_WARMUP} s. Every reader fetches its backlog once and stops (no polling). Reader pool {POOL_SPLIT}. `rows`: notifications in the table after produce.",
         *(["- `--quick`: all durations divided by 10 (smoke test), the numbers are not comparable to a full run."] if QUICK > 1 else []),
         "- Only the `user` stream kind is exercised (no client/team/epoch streams, fan-out 1); not comparable to runs with a mixed target set.",
         "",
         "Per mode: `drop-db`, `migrate` (`--partitioned` for partman), before each run `reset`. Commands:", "", "```"]
    L += [r["cmd"] for r in res.values() if r["label"].endswith("plain")]
    L += ["```", *(["(partman: same commands for A-D; E, F run on plain tables only)"] if partman else []), ""]
    for exp in "ABCDEF":
        if not any(f"{exp}-{m}" in res for m in MODES):
            continue
        L += [f"## {TITLES[exp].format(**{k: duration_text(v) for k, v in dur.items()})}", "", "| Mode | " + " | ".join(h for h, _ in COLS[exp]) + " |", "|---" * (len(COLS[exp]) + 1) + "|"]
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
        if a:
            L.append(f"- a. ({m}) {cell(a.get('push_s'))} notifications/s (avg) added, push p99 {a.get('push_p99')}, {cell(a.get('errors'))} errors.")
    for m in MODES:
        b = g("B", m)
        if b:
            L.append(f"- b. ({m}) {cell(b.get('push_s'))} added/s (avg) and {cell(b.get('cons_s'))} consumed/s (avg) by 2,000 clients, lag p99 {b.get('lag_p99')}, {cell(b.get('missing'))} missing.")
    for m in MODES:
        c = g("C", m)
        if c:
            L.append(f"- c. ({m}) {cell(c.get('push_s'))} added/s (avg), {cell(c.get('cons_s'))} consumed/s (avg) and {cell(c.get('ack_s'))} acknowledged/s (avg), ack p99 {c.get('ack_p99')}, {cell(c.get('missing'))} missing.")
    dp = g("D", "partman")
    if dp:
        i, o = dp["gc"][True], dp["gc"][False]
        r = lambda a, b: f"{a / b:.2f}" if a is not None and b else "-"
        L.append(f"- d. During TTL GC ({dp['drops']} partition drops in 11 min): within {GC_WINDOW} s of a drop vs. otherwise, ack/s {r(i['ack_s'], o['ack_s'])} x, "
                 f"ack p99 {r(i['ack_p99'], o['ack_p99'])} x, lag p99 {r(i['lag_p99'], o['lag_p99'])} x (medians); {cell(dp['errors'])} errors, {cell(dp['missing'])} missing, {cell(dp['deadlocks'])} deadlocks.")
    for e in "EF":
        x = g(e, "plain")
        if x:
            extra = f", {cell(x['ack_s'])} acknowledged/s (avg), ack p99 {x['ack_p99']}" if e == "F" else ""
            L.append(f"- e/f. ({e}, plain) raw consumption of {cell(x['rows'])} rows: {cell(x['cons_s'])} consumed/s (avg), {cell(x['cons_max'])}/s (max), 99 % delivered after {x['t99']}{extra}, fetch p99 {x['fetch_p99']}, {cell(x['missing'])} missing.")
    notes = []
    for e in ("E", "F"):
        x = res.get(f"{e}-plain")
        # fan-out 1 and one client per user: every push is one row; writers still
        # in flight when produce ends commit rows that are not counted (at most W_A)
        if x and not 0 <= x["rows"] - x["pushes"] <= W_A:
            notes.append(f"- {e}-plain: produce reported {x['pushes']:,} pushes but the table holds {x['rows']:,} rows")
        if x and not x["end"].startswith("all readers caught up"):
            notes.append(f"- {e}-plain: consume-only ended: {x['end']}")
        if x and x["rows_total"] != x["rows"]:
            notes.append(f"- {e}-plain: consume-only counted {cell(x['rows_total'])} rows, psql {x['rows']:,}")
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
    global DB, QUICK
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("out", nargs="?", default="fan-in-2000x1-results.md")
    ap.add_argument("--only", help="comma-separated experiments to run, e.g. E,F (default: all)")
    ap.add_argument("--quick", action="store_true", help="divide all durations by 10 (smoke test)")
    ap.add_argument("--db", default=DB, help="connection string; the database is dropped and recreated")
    args = ap.parse_args()
    DB, out = args.db, args.out
    QUICK = 10 if args.quick else 1
    wanted = set(args.only.upper().split(",")) if args.only else set("ABCDEF")
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
        runs = [(x, a) for x, a in experiments() if x in wanted]
        splits = [x for x in split_experiments() if x[0] in wanted] if mode == "plain" else []
        if not runs and not splits:
            continue
        log(f"database for {mode}")
        tool(["drop-db"])
        tool(["migrate"] + (["--partitioned"] if mode == "partman" else []))
        if e is None:
            e = env()
        if mode == "partman":
            e["partman"] = psql("select extversion from pg_extension where extname = 'pg_partman'")
            settle_partman()
        for exp, a in runs:
            log(f"{exp} {mode}")
            res[f"{exp}-{mode}"] = run(f"{exp}-{mode}", a, mode == "partman")
        for exp, pa, ca in splits:
            log(f"{exp} {mode}")
            res[f"{exp}-{mode}"] = run_split(f"{exp}-{mode}", pa, ca)
    e.setdefault("partman", "not run")
    md = report(res, e)
    with open(out, "w") as f:
        f.write(md)
    print(md)
    log(f"report: {out}; logs: {LOGDIR}; Grafana: {GRAFANA}")


if __name__ == "__main__":
    main()
