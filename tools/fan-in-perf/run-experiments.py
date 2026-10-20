#!/usr/bin/env python3
"""Drives fan-in-perf experiments A-D (WPB-26288) on plain and pg_partman
(partitioned) tables and writes a markdown report for a Jira comment.

  run-experiments.py --db postgresql://USER:PASS@HOST:PORT/fan_in_perf [--bin PATH]
                     [--out results.md] [--logdir DIR] [--repeats N] [--quick]

DESTRUCTIVE: drops and recreates the database named in --db (must be a
throw-away one, the local one of deploy/dockerephemeral). The tool's /metrics
endpoint is scraped every second; console output is only used for what the
metrics lack (expected/missing/duplicates, drain). Takes about 55 minutes
(--quick: a few minutes, numbers are meaningless).

A: produce, B: consume, C: consume-ack, D: C for 11 minutes, so pg_partman drops
partitions that hold data. D's baseline is the plain mode, which has no GC.
"""

import argparse
import datetime
import gzip
import json
import math
import os
import re
import shutil
import statistics
import subprocess
import sys
import threading
import time
import urllib.parse
import urllib.request

# One mix for all experiments: 1000 users x 2 clients = the ticket's 2000
# clients. 'user' pushes go to 20 users. consume-ack needs equal user/clients
# STREAMS.
TARGETS = ["--targets", "user:1000x20,clients:1000,team:10,epoch:600", "--clients-per-user", "2"]
CLIENTS = 2000
READER_POOL = 64
GC_WINDOW = 5.0  # seconds around a partition drop

COUNTERS = {
    "push_s": "pushes_total",
    "targets_s": "targets_total",
    "cons_s": "consumed_total",
    "ack_s": "acks_total",
}
ERRORS = {"push_err": "errors_total", "fetch_err": "fetch_errors_total", "ack_err": "ack_errors_total"}
HISTS = {
    "push": "push_duration_seconds",
    "fetch": "fetch_duration_seconds",
    "gate": "gate_wait_seconds",
    "ack": "ack_duration_seconds",
    "lag": "delivery_lag_seconds",
}
GAUGES = {
    "push_max": "push_rate_max",
    "cons_max": "consume_rate_max",
    "ack_max": "ack_rate_max",
    "backlog": "backlog",
}
P = "fanin_perf_"


def log(msg):
    print(f"[{datetime.datetime.now():%H:%M:%S}] {msg}", flush=True)


# ------------------------------------------------------------ environment


def db_name(conn):
    """Database named by a URI or key=value connection string; "" if it names none."""
    if re.match(r"postgres(ql)?://", conn):
        return urllib.parse.unquote(urllib.parse.urlparse(conn).path.lstrip("/"))
    m = re.search(r"(?:^|\s)dbname\s*=\s*(\S+)", conn)
    return m.group(1).strip("'") if m else ""


class Ctx:
    def __init__(self, a):
        self.a = a
        self.db = a.db
        self.dbname = db_name(a.db)
        self.bin = a.bin
        self.quick = a.quick
        self.warmup = 1 if a.quick else 5
        self.seed = a.seed
        self.drain = 5 if a.quick else 30
        self.logdir = a.logdir
        self.commands = {}  # label -> command line (without --db)
        self.env = {}

    def dur(self, secs):
        return max(8, secs // 10) if self.quick else secs

    def psql(self, sql):
        r = subprocess.run(
            ["psql", self.db, "-X", "-At", "-F", "|", "-q", "-v", "ON_ERROR_STOP=1", "-c", sql],
            capture_output=True,
            text=True,
        )
        if r.returncode != 0:
            raise RuntimeError("psql failed: " + r.stderr.strip().splitlines()[-1][:200] if r.stderr else "psql failed")
        return r.stdout.strip()

    def deadlocks(self):
        return int(self.psql("select deadlocks from pg_stat_database where datname = current_database()"))

    def cmd(self, args):
        return [self.bin, "--db", self.db, "--metrics-port", str(self.a.metrics_port)] + [str(x) for x in args]

    def tool(self, args, check=True):
        r = subprocess.run(self.cmd(args), capture_output=True, text=True)
        if check and r.returncode != 0:
            raise RuntimeError(f"fan-in-perf {args[0]} failed: {(r.stdout + r.stderr).strip()[-300:]}")
        return r.stdout


def find_bin(a):
    if a.bin:
        return a.bin
    r = subprocess.run(["cabal", "list-bin", "fan-in-perf"], capture_output=True, text=True, cwd=os.path.dirname(os.path.abspath(__file__)))
    path = r.stdout.strip().splitlines()[-1] if r.stdout.strip() else ""
    if not (path and os.access(path, os.X_OK)):
        sys.exit("fan-in-perf binary not found (build it or pass --bin)")
    return path


# ------------------------------------------------------------ metrics

LINE = re.compile(r"^([a-zA-Z_:][\w:]*)(?:\{([^}]*)\})? (\S+)$")
LE = re.compile(r'(?:^|,)le="([^"]*)"')


def parse_metrics(text, les):
    """-> (counters/gauges summed over labels, histogram name -> cumulative counts)."""
    c, h = {}, {}
    for line in text.splitlines():
        m = LINE.match(line)
        if not m or not m.group(1).startswith(P):
            continue
        name, labels, val = m.group(1), m.group(2) or "", float(m.group(3))
        if name.endswith("_bucket"):
            le = LE.search(labels).group(1)
            if le == "+Inf":
                continue
            base = name[: -len("_bucket")][len(P) :]
            h.setdefault(base, []).append(val)
            if base not in les:
                les[base] = []
            if len(les[base]) < len(h[base]):
                les[base].append(float(le))
        elif name.endswith("_sum") or name.endswith("_count"):
            continue
        else:
            c[name[len(P) :]] = c.get(name[len(P) :], 0.0) + val
    return c, h


def quantile(les, cum, q):
    total = cum[-1] if cum else 0
    if total <= 0:
        return None
    threshold = max(1, math.ceil(q * total))
    for le, v in zip(les, cum):
        if v >= threshold:
            return le
    return None


class Scraper(threading.Thread):
    def __init__(self, port):
        super().__init__(daemon=True)
        self.url = f"http://localhost:{port}/metrics"
        self.stop = threading.Event()
        self.scrapes = []  # (t, counters, hists)
        self.les = {}

    def run(self):
        while not self.stop.is_set():
            t = time.monotonic()
            try:
                with urllib.request.urlopen(self.url, timeout=2) as r:
                    c, h = parse_metrics(r.read().decode(), self.les)
                self.scrapes.append((t, c, h))
            except Exception:
                pass
            self.stop.wait(max(0.0, 1.0 - (time.monotonic() - t)))


PARTITIONS_SQL = "select count(*) from pg_inherits where inhparent = to_regclass('user_notifications')"

SAMPLE_SQL = (
    "select count(*) filter (where wait_event_type = 'Lock'), count(*) filter (where wait_event_type = 'LWLock'), "
    "count(*) filter (where state = 'active'), "
    "(" + PARTITIONS_SQL + "), "
    "(select coalesce(min(c.relname), '') from pg_inherits i join pg_class c on c.oid = i.inhrelid where i.inhparent = to_regclass('user_notifications')) "
    ", count(*) filter (where wait_event in ('transactionid', 'tuple')), count(*) filter (where wait_event = 'relation') "
    "from pg_stat_activity where datname = current_database() and backend_type = 'client backend' and pid <> pg_backend_pid()"
)


class Sampler(threading.Thread):
    def __init__(self, ctx):
        super().__init__(daemon=True)
        self.ctx = ctx
        self.stop = threading.Event()
        self.samples = []  # (t, lock, lwlock, active, children, oldest)

    def query(self, p):
        """One query over the open connection; psql echoes EOQ after each result."""
        p.stdin.write(SAMPLE_SQL + ";\n\\echo EOQ\n")
        p.stdin.flush()
        rows = []
        for line in p.stdout:
            if line.strip() == "EOQ":
                break
            rows.append(line.strip())
        return rows[0].split("|") if rows else None

    def run(self):
        # One connection for the whole run: reconnecting every second would load the measured database.
        p = subprocess.Popen(
            ["psql", self.ctx.db, "-X", "-At", "-F", "|", "-q"],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True,
        )
        try:
            while not self.stop.is_set():
                t = time.monotonic()
                f = self.query(p)
                if f is None:
                    break
                self.samples.append((t, int(f[0]), int(f[1]), int(f[2]), int(f[3]), f[4], int(f[5]), int(f[6])))
                self.stop.wait(max(0.0, 1.0 - (time.monotonic() - t)))
        finally:
            p.kill()
            p.wait()


# ------------------------------------------------------------ one run


def aggregate(sc, mask):
    """Rates, percentiles and error counts over the scrape intervals i (i-1 -> i) with mask[i]."""
    idx = [i for i in range(1, len(sc.scrapes)) if mask[i]]
    dt = sum(sc.scrapes[i][0] - sc.scrapes[i - 1][0] for i in idx)
    out = {"seconds": dt}
    if dt <= 0:
        return out

    def delta(name):
        return sum(sc.scrapes[i][1].get(name, 0.0) - sc.scrapes[i - 1][1].get(name, 0.0) for i in idx)

    for key, name in COUNTERS.items():
        out[key] = delta(name) / dt
    for key, name in ERRORS.items():
        out[key] = delta(name)
    fetches = delta("fetches_total")
    out["empty_pct"] = 100.0 * delta("empty_fetches_total") / fetches if fetches > 0 else None
    for key, name in HISTS.items():
        n = len(sc.les.get(name, []))
        cum = [0.0] * n
        for i in idx:
            a, b = sc.scrapes[i - 1][2].get(name), sc.scrapes[i][2].get(name)
            if b is None:
                continue
            for k in range(n):
                cum[k] += b[k] - (a[k] if a else 0.0)
        out[key + "_p50"] = quantile(sc.les.get(name, []), cum, 0.5)
        out[key + "_p99"] = quantile(sc.les.get(name, []), cum, 0.99)
    return out


def run_tool(ctx, label, args, partman=False, warmup=None):
    """Runs fan-in-perf with metrics and DB sampling; returns the run's result dict."""
    warmup = ctx.warmup if warmup is None else warmup
    ctx.tool(["reset"])
    deadlocks0 = ctx.deadlocks()
    ctx.commands[label] = "fan-in-perf --db \"$DB\" " + " ".join(str(x) for x in args)
    os.makedirs(ctx.logdir, exist_ok=True)
    logpath = os.path.join(ctx.logdir, label + ".log")
    sc, sm = Scraper(ctx.a.metrics_port), Sampler(ctx)
    t_run = time.monotonic()
    with open(logpath, "w") as lf:
        proc = subprocess.Popen(ctx.cmd(args), stdout=lf, stderr=subprocess.STDOUT)
        sc.start()
        sm.start()
        limit = 3600 + 60 * 5
        try:
            proc.wait(timeout=limit)
        except BaseException:
            proc.kill()
            raise
        finally:
            sc.stop.set()
            sm.stop.set()
            sc.join()
            sm.join()
    # The tool sometimes aborts in shutdown after the summary ("double free or
    # corruption", exit -6, seen with 128 writers); the data is complete then.
    if proc.returncode != 0 and "\nsummary:" not in open(logpath).read():
        raise RuntimeError(f"{label}: fan-in-perf exited {proc.returncode} without summary, see {logpath}")
    if proc.returncode != 0:
        log(f"warning: {label}: fan-in-perf exited {proc.returncode} after the summary")
    time.sleep(2)
    deadlocks = ctx.deadlocks() - deadlocks0
    with gzip.open(os.path.join(ctx.logdir, label + ".scrapes.jsonl.gz"), "wt") as g:
        for t, c, h in sc.scrapes:
            g.write(json.dumps({"t": round(t - t_run, 2), "c": c, "h": h}) + "\n")

    scr = sc.scrapes
    if len(scr) < 4:
        raise RuntimeError(f"{label}: only {len(scr)} scrapes, see {logpath}")
    pushes = [c.get("pushes_total", 0.0) for _, c, _ in scr]
    first = next((i for i, v in enumerate(pushes) if v > 0), None)
    if first is None:
        raise RuntimeError(f"{label}: no pushes recorded, see {logpath}")
    # The last interval with growth ends somewhere after the writers stopped: leave it out.
    end = max(i for i in range(1, len(pushes)) if pushes[i] > pushes[i - 1]) - 1
    start = next((i for i in range(len(scr)) if scr[i][0] >= scr[first][0] + warmup), None)
    if start is None or end - start < 2:
        raise RuntimeError(f"{label}: steady window too short, see {logpath}")
    n = len(scr)
    window = [start < i <= end for i in range(n)]
    r = {"label": label, "exp": args[0], "exit": proc.returncode, "writers": args[args.index("--writers") + 1], "window_s": scr[end][0] - scr[start][0]}
    r.update(aggregate(sc, window))
    for key, name in GAUGES.items():
        r[key] = scr[end][1].get(name)

    text = open(logpath).read()
    m = re.search(r"^(?:consume|consume-ack): delivered=([\d,]+) expected=([\d,]+) missing=([\d,]+) duplicates=([\d,]+)", text, re.M)
    if args[0] != "produce":
        if not m:
            raise RuntimeError(f"{label}: no consume summary in {logpath}")
        r["delivered"], r["expected"], r["missing"], r["dup"] = (int(x.replace(",", "")) for x in m.groups())
        d = re.search(r"^  drain: (.*)$", text, re.M)
        r["drain"] = d.group(1).split(" (")[0] if d else None
        # The backlog gauge is only read at a scrape after the writers stopped, so take the tool's exact figure.
        b = re.search(r"^  backlog at writer stop: ([\d,]+)", text, re.M)
        if not b:
            raise RuntimeError(f"{label}: no backlog line in {logpath}")
        r["backlog"] = int(b.group(1).replace(",", ""))

    t0, t1 = scr[start][0], scr[end][0]
    ws = [s for s in sm.samples if t0 <= s[0] <= t1]
    r["deadlocks"] = deadlocks
    r["active_avg"] = statistics.fmean(s[3] for s in ws) if ws else None
    r["lock_avg"] = statistics.fmean(s[1] for s in ws) if ws else None
    r["lock_row_avg"] = statistics.fmean(s[6] for s in ws) if ws else None
    r["lock_rel_avg"] = statistics.fmean(s[7] for s in ws) if ws else None
    r["lwlock_avg"] = statistics.fmean(s[2] for s in ws) if ws else None
    r["lock_pct"] = 100.0 * sum(1 for s in ws if s[1] > 0) / len(ws) if ws else None

    if partman:
        events = [b[0] for a, b in zip(sm.samples, sm.samples[1:]) if b[5] != a[5] and t0 <= b[0] <= t1]
        r["partitions_min"] = min((s[4] for s in sm.samples), default=None)
        r["partitions_max"] = max((s[4] for s in sm.samples), default=None)
        r["drops"] = len(events)
        near = lambda t: any(abs(t - e) <= GC_WINDOW for e in events)
        mid = lambda i: (scr[i - 1][0] + scr[i][0]) / 2
        r["gc_in"] = aggregate(sc, [window[i] and i > 0 and near(mid(i)) for i in range(n)])
        r["gc_out"] = aggregate(sc, [window[i] and i > 0 and not near(mid(i)) for i in range(n)])
        for key, sel in (("gc_in", True), ("gc_out", False)):
            sub = [s for s in ws if near(s[0]) == sel]
            r[key]["lock_avg"] = statistics.fmean(s[1] for s in sub) if sub else None
    return r


# ------------------------------------------------------------ experiments


def args_a(ctx, w, dur):
    return ["produce", "--writers", w, *TARGETS, "--warmup", ctx.warmup, "--duration", ctx.dur(dur), "--seed", ctx.seed]


def args_bc(ctx, w, dur, ack, pool=READER_POOL):
    return [
        "consume-ack" if ack else "consume",
        "--writers", w,
        "--reader-pool-size", pool,
        "--clients", CLIENTS,
        *TARGETS,
        "--epochs-per-client", 3,
        "--safety-window", 200,
        "--warmup", ctx.warmup,
        "--duration", ctx.dur(dur),
        "--drain-timeout", ctx.drain,
        "--seed", ctx.seed,
        *(["--ack-every", 1] if ack else []),
    ]


def setup_mode(ctx, mode):
    log(f"database for mode {mode}")
    ctx.tool(["drop-db"])
    ctx.tool(["migrate"] + (["--partitioned"] if mode == "partman" else []))
    if mode == "partman":
        ctx.env["partman_version"] = ctx.psql("select extversion from pg_extension where extname = 'pg_partman'")
        ctx.env["partman_config"] = ctx.psql("select distinct retention || ' retention, ' || partition_interval || ' partitions' from partman.part_config")
        if not ctx.quick:
            settle_partman(ctx)


def settle_partman(ctx):
    """The first maintenance run drops the premade past partitions in bulk; wait for steady state."""
    log("waiting for pg_partman to settle (initial bulk drop)")
    q = PARTITIONS_SQL
    last, since, deadline = None, time.monotonic(), time.monotonic() + 240
    while time.monotonic() < deadline:
        n = ctx.psql(q)
        if n != last:
            last, since = n, time.monotonic()
        elif time.monotonic() - since >= 75:
            return
        time.sleep(5)
    log("warning: partition count did not settle")


def sustainable(r):
    return r["push_err"] + r["fetch_err"] + r["ack_err"] == 0 and (r.get("backlog") or 0) <= r["cons_s"]


def tune(ctx, state):
    """Writer counts with the highest rates, measured on plain; reused for partman."""
    if ctx.a.no_tune:
        state["w"] = {"A": ctx.a.writers, "B": ctx.a.writers, "C": ctx.a.writers}
        state["pool"] = {"B": READER_POOL, "C": READER_POOL}
        return
    sweeps = state["sweeps"]
    sweeps["A"] = []
    for w in (8, 16, 32, 64, 128):
        log(f"tune A: writers={w}")
        sweeps["A"].append(run_tool(ctx, f"tune-A-w{w}", args_a(ctx, w, 45)))
    state["w"] = {"A": int(max(sweeps["A"], key=lambda r: r["push_s"])["writers"])}
    state["pool"] = {}
    # With acks the readers saturate at about 30k deliveries/s (a push fans out to ~50), so C needs few writers to be sustainable.
    for exp, ack, grid in (("B", False, [(16, READER_POOL), (32, READER_POOL), (64, READER_POOL)]),
                           ("C", True, [(w, READER_POOL) for w in (1, 2, 3, 4, 6, 8)])):
        sweeps[exp] = []
        for w, pool in grid:
            log(f"tune {exp}: writers={w} reader pool={pool}")
            r = run_tool(ctx, f"tune-{exp}-w{w}-p{pool}", args_bc(ctx, w, 45, ack, pool))
            r["pool"] = pool
            sweeps[exp].append(r)
        ok = [r for r in sweeps[exp] if sustainable(r)]
        best = max(ok or sweeps[exp], key=lambda r: r["cons_s"])
        state["w"][exp] = int(best["writers"])
        state["pool"][exp] = best["pool"]
        if not ok:
            state.setdefault("flags", []).append(f"{exp}: no writer count was sustainable (backlog at writer stop above 1 s of delivery, or errors); highest cons/s taken")


def run_mode(ctx, mode, rep, state):
    setup_mode(ctx, mode)
    if "w" not in state:
        tune(ctx, state)
    w, pl = state["w"], state["pool"]
    res = state["results"]
    partman = mode == "partman"
    for exp, args, label in (
        ("A", args_a(ctx, w["A"], 60), "A"),
        ("B", args_bc(ctx, w["B"], 120, False, pl["B"]), "B"),
        ("C", args_bc(ctx, w["C"], 120, True, pl["C"]), "C"),
        ("D", args_bc(ctx, w["C"], 660, True, pl["C"]), "D"),
    ):
        log(f"{label} {mode} (repeat {rep + 1}): writers={args[args.index('--writers') + 1]}")
        r = run_tool(ctx, f"{label}-{mode}-r{rep + 1}", args, partman=partman)
        r["mode"] = mode
        res.setdefault((exp, mode), []).append(r)
        write_report(ctx, state)


# ------------------------------------------------------------ report


def med(runs, key):
    v = [r[key] for r in runs if r.get(key) is not None]
    return statistics.median(v) if v else None


def tot(runs, key):
    return sum(r.get(key) or 0 for r in runs)


def distinct(runs, key):
    return " ; ".join(dict.fromkeys(str(r.get(key)) for r in runs))


def gc_merge(pm, which):
    """Median rates and percentiles over the repeats, error counts summed."""
    runs = [x[which] for x in pm if x[which].get("seconds")]
    if not runs:
        return {}
    out = {k: med(runs, k) for k in runs[0] if k not in ERRORS}
    out.update({k: tot(runs, k) for k in ERRORS})
    return out


def f_rate(x):
    return "-" if x is None else f"{x:,.0f}"


def f_lat(x):
    if x is None:
        return "-"
    return f"{x * 1000:.1f} ms" if x < 1 else f"{x:.2f} s"


def f_num(x, d=1):
    return "-" if x is None else f"{x:,.{d}f}"


LABEL = ("Mode", None, None)
LOCK = ("lock-wait backends", "lock_avg", lambda x: f_num(x, 2))
DL = ("deadlocks", "deadlocks", f_rate)
COLS_A = [LABEL, ("writers", "writers_n", f_rate), ("push/s avg", "push_s", f_rate), ("push/s max", "push_max", f_rate), ("targets/s", "targets_s", f_rate),
          ("push p50", "push_p50", f_lat), ("push p99", "push_p99", f_lat), ("push errors", "push_err", f_rate),
          ("active backends", "active_avg", f_num), LOCK, ("of which row locks", "lock_row_avg", lambda x: f_num(x, 2)), ("table locks", "lock_rel_avg", lambda x: f_num(x, 2)), DL]
COLS_B = [LABEL, ("writers", "writers_n", f_rate), ("push/s", "push_s", f_rate), ("cons/s avg", "cons_s", f_rate),
          ("lag p99", "lag_p99", f_lat), ("fetch p99", "fetch_p99", f_lat), ("backlog at stop", "backlog", f_rate), ("expected", "expected", f_rate),
          ("missing", "missing", f_rate), ("drain", "drain", str), ("errors (push/fetch)", "err_pf", str), LOCK, DL]
COLS_C = [LABEL, ("writers", "writers_n", f_rate), ("push/s", "push_s", f_rate), ("cons/s avg", "cons_s", f_rate), ("ack/s avg", "ack_s", f_rate),
          ("ack p50", "ack_p50", f_lat), ("ack p99", "ack_p99", f_lat), ("lag p99", "lag_p99", f_lat), ("fetch p99", "fetch_p99", f_lat),
          ("backlog at stop", "backlog", f_rate), ("expected", "expected", f_rate), ("missing", "missing", f_rate), ("drain", "drain", str),
          ("errors (push/fetch/ack)", "err_pfa", str), LOCK, DL]
COLS_GC = [("Window", None, None), ("seconds", "seconds", f_rate), ("push/s", "push_s", f_rate), ("cons/s", "cons_s", f_rate), ("ack/s", "ack_s", f_rate),
           ("push p99", "push_p99", f_lat), ("fetch p99", "fetch_p99", f_lat), ("ack p99", "ack_p99", f_lat), ("lag p99", "lag_p99", f_lat),
           ("lock-wait backends", "lock_avg", lambda x: f_num(x, 2)), ("errors push", "push_err", f_rate), ("fetch", "fetch_err", f_rate), ("ack", "ack_err", f_rate)]


def prep(r):
    r["writers_n"] = float(r["writers"]) if "writers" in r else None
    r.setdefault("drain", "-")
    e = [r.get(k, 0) for k in ("push_err", "fetch_err", "ack_err")]
    r["err_pf"] = f"{e[0]:.0f}/{e[1]:.0f}"
    r["err_pfa"] = f"{e[0]:.0f}/{e[1]:.0f}/{e[2]:.0f}"
    return r


def tbl(cols, rows):
    """Markdown table; numeric cells are medians over the runs, string cells list the distinct values."""
    out = ["| " + " | ".join(h for h, _, _ in cols) + " |", "|" + "|".join("---" for _ in cols) + "|"]
    for label, runs in rows:
        runs = [prep(dict(r)) for r in runs]
        cells = [label]
        for _, key, fmt in cols[1:]:
            cells.append(" ; ".join(dict.fromkeys(str(r[key]) for r in runs)) if fmt is str else fmt(med(runs, key)))
        out.append("| " + " | ".join(cells) + " |")
    return out


def ratio(a, b):
    return None if a is None or not b else a / b


def answers(state):
    res, sw = state["results"], state["sweeps"]
    g = lambda exp, mode: res.get((exp, mode), [])
    out = []
    for m in ("plain", "partman"):
        a = g("A", m)
        if a:
            out.append(f"- a. Maximal add rate, {m}: {f_rate(med(a, 'push_s'))} pushes/s ({f_rate(med(a, 'targets_s'))} targets/s) with {state['w']['A']} writers, push p99 {f_lat(med(a, 'push_p99'))}, errors {f_rate(med(a, 'push_err'))}.")
    for exp, what in (("B", "add and consume by 2,000 clients"), ("C", "add, consume and acknowledge by 2,000 clients")):
        for m in ("plain", "partman"):
            r = g(exp, m)
            if r:
                tail = f", ack {f_rate(med(r, 'ack_s'))}/s (ack p99 {f_lat(med(r, 'ack_p99'))})" if exp == "C" else ""
                out.append(f"- {'b' if exp == 'B' else 'c'}. {what}, {m}: {f_rate(med(r, 'push_s'))} pushes/s, {f_rate(med(r, 'cons_s'))} deliveries/s{tail}, lag p99 {f_lat(med(r, 'lag_p99'))}, backlog at writer stop {f_rate(med(r, 'backlog'))}, missing {f_rate(med(r, 'missing'))} (drain {distinct(r, 'drain')}), with {state['w'][exp]} writers.")
    for fl in state.get("flags", []):
        out.append(f"- Note: {fl}.")
    for m in ("plain", "partman"):
        r = g("D", m)
        if r:
            out.append(f"- d. C for 11 minutes, {m}: {f_rate(med(r, 'cons_s'))} deliveries/s, {f_rate(med(r, 'ack_s'))} acks/s, ack p99 {f_lat(med(r, 'ack_p99'))}, errors push/fetch/ack {tot(r, 'push_err'):.0f}/{tot(r, 'fetch_err'):.0f}/{tot(r, 'ack_err'):.0f}, deadlocks {tot(r, 'deadlocks'):.0f}.")
    pm = g("D", "partman")
    i, o = gc_merge(pm, "gc_in"), gc_merge(pm, "gc_out")
    if i and o:
        out.append(f"- TTL GC (pg_partman, {tot(pm, 'drops'):.0f} drops): within {GC_WINDOW:.0f} s of a drop vs. otherwise: push p99 {f_lat(i.get('push_p99'))} vs {f_lat(o.get('push_p99'))}, fetch p99 {f_lat(i.get('fetch_p99'))} vs {f_lat(o.get('fetch_p99'))}, ack p99 {f_lat(i.get('ack_p99'))} vs {f_lat(o.get('ack_p99'))}, pushes/s {f_rate(i.get('push_s'))} vs {f_rate(o.get('push_s'))}; errors in drop windows push/fetch/ack {i['push_err']:.0f}/{i['fetch_err']:.0f}/{i['ack_err']:.0f}.")
    if sw.get("A"):
        rows = "; ".join(f"{r['writers']} writers: {f_num(r['lock_avg'], 1)} of {f_num(r['active_avg'], 1)} active backends in lock waits ({f_num(r['lock_row_avg'], 1)} row, {f_num(r['lock_rel_avg'], 1)} table), push p99 {f_lat(r['push_p99'])}" for r in sw["A"])
        out.append(f"- Lock contention between writers (A sweep, plain): {rows}; deadlocks {sum(r['deadlocks'] for r in sw['A'])}.")
    return out


def write_report(ctx, state):
    res, sw = state["results"], state["sweeps"]
    get = lambda exp, mode: res.get((exp, mode), [])
    n = lambda exp, mode: len(get(exp, mode))
    L = []
    L.append("# Notification fan-in: PostgreSQL benchmark results (WPB-26288)")
    if ctx.quick:
        L += ["", "**QUICK RUN: durations divided by 10, numbers are not results.**"]
    L += ["", "Driver: `tools/fan-in-perf/run-experiments.py`, tool: `fan-in-perf` (`libs/wire-subsystems` store, SQL in `libs/wire-subsystems/fan-in-sql/`).", ""]
    L.append("## Setup")
    L += ["", f"- Date: {ctx.env['date']}, commit: {ctx.env['commit']}",
          f"- Host: {ctx.env['cpus']} CPUs, {ctx.env['mem']} RAM; tool and PostgreSQL share it ({ctx.env['pg']})",
          f"- PostgreSQL settings: {ctx.env['pg_settings']}"]
    if ctx.env.get("partman_version"):
        L.append(f"- pg_partman {ctx.env['partman_version']}: {ctx.env['partman_config']}, no default partition, UUIDv7 range partitions, maintenance by the background worker every 60 s")
    L += [f"- Modes: `plain` = plain tables, no TTL; `partman` = the six `*_notifications` tables partitioned by UUIDv7 and managed by pg_partman, which also expires them (D)",
          f"- Repeats: {ctx.a.repeats} (tables show the median, `n` runs)",
          "",
          "### Load",
          "",
          f"- Target mix `{TARGETS[1]}`, `--clients-per-user 2`: each push picks one of the four kinds uniformly; a `user` push writes to 20 of 1,000 users; `clients` pushes write both clients of a user; `team` has 10 streams, `epoch` 600. Hot keys of the workload: every `team` push updates one of only 10 `last_team_notifications` rows; a `user` push updates 20 of 1,000 `last_user_notifications` rows.",
          f"- Readers (B, C, D): `--clients {CLIENTS}` = 1,000 users x 2 clients (the ticket's 2,000), each polls its user, client, team and (3) epoch streams; fetch limit 100, poll interval 100 ms, safety window 200 ms (re-reads the last 200 ms, so late commits are not lost). C and D: every client acknowledges every notification (`--ack-every 1`).",
          "- Writers (and reader pool): chosen by the sweeps below as the highest rate; for B, C and D the highest rate at which the readers keep up (backlog at writer stop under 1 s of delivery). Payload 512 B, read-committed.",
          "- Rates are averages over the steady window (after warmup 5 s until the writers stop), from the tool's Prometheus counters scraped every second; percentiles are upper bounds of log-linear histogram buckets (at most 25 % above the true value) over the same window. `lock-wait backends` = average number of backends waiting for a heavyweight lock, sampled every second from `pg_stat_activity`.",
          "- `missing` = expected minus delivered when the drain ended. Drain `stalled`: nothing was delivered for several poll intervals, the rest is lost. Drain `timed out`: readers were still behind (backlog), the rest may still have been in flight. D on `partman`: rows a reader had not read within the 5 min retention are expired, i.e. lost.",
          ""]
    w, pl = state["w"], state["pool"]
    L += ["Mix `user:1000x20,clients:1000,team:10,epoch:600` (`KIND:STREAMS[xK]`: STREAMS = distinct stream keys of that kind, K = keys written per push). A writer picks one entry uniformly (25 % each) per push and writes one notification to every chosen stream in one transaction:", "",
          "| Kind | Streams total | Streams per push | Rows per push | Readers per stream | Deliveries per push |",
          "|---|---|---|---|---|---|",
          "| `user` | 1,000 (one per user) | 20 users | 20 | 2 (both clients of the user) | 40 |",
          "| `clients` | 1,000 users x 2 clients | 1 user | 2 (one per client) | 1 | 2 |",
          "| `team` | 10 | 1 | 1 | 200 (2,000 clients / 10 teams) | 200 |",
          "| `epoch` | 600 | 1 | 1 | 10 (2,000 clients x 3 epochs / 600) | 10 |",
          "| mix average | | | 6 | | 63 |",
          "",
          "So a push costs 6 rows on average and is read 63 times; cons/s is about 63 x push/s (B).",
          "",
          "Producers and consumers per experiment:", "",
          "| Exp | Producers (writers) | Consumers (readers) |",
          "|---|---|---|",
          f"| A | {w['A']} writer threads in a loop: pick a mix entry, write its notifications (and update the `last_*` row of each stream) in one transaction; connection pool = writers | none |",
          f"| B | {w['B']} writer threads as in A | {CLIENTS} simulated clients, each polls its streams (`user`, `clients`, `team`, 3 `epoch`) for rows after its in-memory cursor; reader pool {pl['B']}; no acks |",
          f"| C | {w['C']} writer thread(s) as in A | as B, and every client acknowledges (upserts its ack cursor) after every notification; reader pool {pl['C']} |",
          f"| D | as C ({w['C']} writer(s)), 11 minutes | as C; on `partman` pg_partman drops expired partitions meanwhile |",
          "",
          "Writers run for the duration, then the readers drain the backlog (`drain`). `push/s` counts writer transactions, `cons/s` counts notifications delivered to clients.",
          ""]
    L += ["Commands (`DB` = connection string of the experiment database). The same `--seed` in every run fixes the stream keys and the sequence of mix choices; throughput still varies between runs. Per mode, once: `fan-in-perf --db \"$DB\" drop-db`, then `fan-in-perf --db \"$DB\" migrate` (`plain`) or `migrate --partitioned` (`partman`); before each run: `fan-in-perf --db \"$DB\" reset`.", "",
          "| Run | Seed | Command |", "|---|---|---|"]
    L += [f"| `{k}` | {ctx.seed} | `{v}` |" for k, v in ctx.commands.items()] + [""]

    modes = ("plain", "partman")
    for exp, title, cols in (
        ("A", "A: maximal rate of adding notifications", COLS_A),
        ("B", "B: adding and consuming by 2,000 clients", COLS_B),
        ("C", "C: B, every client acknowledges every notification", COLS_C),
    ):
        L += [f"## {title}", ""]
        rows = [(f"{m} (n={n(exp, m)})", get(exp, m)) for m in modes if get(exp, m)]
        L += tbl(cols, rows) if rows else ["(not run)"]
        L.append("")

    L += ["## D: C while TTL garbage collection runs (pg_partman drops expired partitions)", "",
          "`plain` has no TTL and shows the cost of C alone; `partman` expires partitions (5 min retention, 1 min partitions, so a drop every minute, with data from minute 6 on). 11 minutes per run.", ""]
    rows = [(f"{m} (n={n('D', m)})", get("D", m)) for m in modes if get("D", m)]
    L += tbl(COLS_C, rows) if rows else ["(not run)"]
    L.append("")
    pm = get("D", "partman")
    if pm:
        L += [f"pg_partman: {tot(pm, 'drops'):.0f} partition drops seen in the steady window (all repeats), partitions of `user_notifications` between {min(x['partitions_min'] for x in pm)} and {max(x['partitions_max'] for x in pm)}.", "",
              f"Impact of the drops, `partman` D: seconds within {GC_WINDOW:.0f} s of a drop vs. all other seconds of the steady window:", ""]
        rows = [("within drop window", [x["gc_in"] for x in pm if x["gc_in"].get("seconds")]), ("outside", [x["gc_out"] for x in pm if x["gc_out"].get("seconds")])]
        L += tbl(COLS_GC, [(a, b) for a, b in rows if b]) + [""]

    if sw.get("A"):
        L += ["## Tuning: writers (plain, 45 s runs)", "", "A:", ""]
        L += tbl(COLS_A, [(f"w={r['writers']}", [r]) for r in sw["A"]]) + [""]
        for exp, cols in (("B", COLS_B), ("C", COLS_C)):
            L += [f"{exp}:", ""]
            L += tbl(cols, [(f"w={r['writers']}, pool={r['pool']}", [r]) for r in sw[exp]]) + [""]
        L += [f"Chosen writers: A={state['w']['A']}, B={state['w']['B']}, C and D={state['w']['C']}; reader pool B={state['pool']['B']}, C and D={state['pool']['C']} (selected on plain, reused for partman)."]
        for fl in state.get("flags", []):
            L.append(f"- {fl}")
        L.append("")

    L += ["## Answers to the ticket (measured facts)", ""] + answers(state) + [""]
    L += ["## Observations (measured, no interpretation)", ""]
    obs = []
    for exp in ("A", "B", "C", "D"):
        a, b = get(exp, "plain"), get(exp, "partman")
        if a and b:
            for key, name in (("push_s", "push/s"), ("cons_s", "cons/s"), ("ack_s", "ack/s")):
                x = ratio(med(b, key), med(a, key))
                if x is not None and med(a, key):
                    obs.append(f"- {exp}: partman {name} = {x:.2f} x plain ({f_rate(med(b, key))} vs {f_rate(med(a, key))})")
    for (exp, mode), runs in sorted(res.items()):
        for r in runs:
            if r.get("missing"):
                what = "lost" if (r.get("drain") or "").startswith("stalled") else "not yet delivered when the drain ended (readers behind, not necessarily lost)"
                obs.append(f"- {exp} {mode}: {r['missing']:,} of {r['expected']:,} notifications {what} (drain: {r['drain']})")
            if r["push_err"] + r["fetch_err"] + r["ack_err"]:
                obs.append(f"- {exp} {mode}: errors push/fetch/ack = {r['push_err']:.0f}/{r['fetch_err']:.0f}/{r['ack_err']:.0f}")
            if r["exit"]:
                obs.append(f"- {exp} {mode}: fan-in-perf aborted after printing the summary (exit {r['exit']}, shutdown crash of the tool)")
            if r["deadlocks"]:
                obs.append(f"- {exp} {mode}: {r['deadlocks']} deadlocks")
    i, o = gc_merge(pm, "gc_in"), gc_merge(pm, "gc_out")
    if i and o:
        for key, name in (("cons_s", "cons/s"), ("ack_s", "ack/s")):
            x = ratio(i.get(key), o.get(key))
            if x is not None:
                obs.append(f"- D partman: {name} within drop windows = {x:.2f} x outside")
    L += obs or ["- none"]
    L += ["", "Caveats: tool and database share one host (CPU contention); throughput varies between runs (WAL flushes); with 1 repeat the numbers are indicative only; the tool's readers are simulated clients that poll (no websockets, no reconnects, no server flow control)."]
    with open(ctx.a.out, "w") as f:
        f.write("\n".join(L) + "\n")


# ------------------------------------------------------------ main


def collect_env(ctx):
    mem = "?"
    try:
        kb = int(re.search(r"MemTotal:\s+(\d+)", open("/proc/meminfo").read()).group(1))
        mem = f"{kb / 1048576:.0f} GiB"
    except Exception:
        pass
    sha = subprocess.run(["git", "log", "-1", "--format=%h %s"], capture_output=True, text=True, cwd=os.path.dirname(os.path.abspath(__file__))).stdout.strip()
    ctx.env.update(
        date=datetime.date.today().isoformat(),
        commit=f"`{sha}`",
        cpus=os.cpu_count(),
        mem=mem,
        pg=ctx.psql("select split_part(version(), ' on ', 1)"),
        pg_settings=ctx.psql(
            "select string_agg(name || '=' || current_setting(name), ', ' order by name) from pg_settings "
            "where name in ('shared_buffers', 'max_wal_size', 'synchronous_commit', 'fsync', 'max_connections', 'wal_level')"
        ),
    )


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--db", required=True, help="connection string of the experiment database (dropped and recreated!)")
    ap.add_argument("--bin", help="fan-in-perf executable (default: cabal list-bin)")
    ap.add_argument("--out", default="fan-in-results.md")
    ap.add_argument("--logdir", default="/tmp/fan-in-perf-runs")
    ap.add_argument("--repeats", type=int, default=1)
    ap.add_argument("--quick", action="store_true", help="durations / 10, smoke test only")
    ap.add_argument("--no-tune", action="store_true", help="skip the writer sweeps, use --writers")
    ap.add_argument("--writers", type=int, default=32, help="with --no-tune")
    ap.add_argument("--seed", type=int, default=42, help="RNG seed of every run (stream keys, mix choices)")
    ap.add_argument("--metrics-port", type=int, default=9400)
    a = ap.parse_args()
    a.bin = find_bin(a)
    for tool in ("psql",):
        if not shutil.which(tool):
            sys.exit(f"{tool} not found")
    ctx = Ctx(a)
    if ctx.dbname != "fan_in_perf":
        sys.exit("--db must name the database fan_in_perf (it is dropped and recreated; the pg_partman worker and the exporter expect that name)")
    try:
        urllib.request.urlopen(f"http://localhost:{a.metrics_port}/metrics", timeout=1)
        sys.exit(f"port {a.metrics_port} is busy")
    except OSError:
        pass
    ctx.tool(["drop-db"])
    ctx.tool(["migrate"])  # creates the database for the environment queries
    collect_env(ctx)
    state = {"results": {}, "sweeps": {}}
    try:
        for rep in range(a.repeats):
            for mode in ("plain", "partman") if rep % 2 == 0 else ("partman", "plain"):
                run_mode(ctx, mode, rep, state)
    finally:
        if state["results"]:
            write_report(ctx, state)
            log(f"report: {a.out}, raw data and logs: {a.logdir}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
