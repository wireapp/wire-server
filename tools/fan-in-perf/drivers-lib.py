"""Shared code of the experiment drivers (run-2000x1.py, run-team-1.py): runs
fan-in-perf experiments A-F (WPB-26288) on pg_partman (partitioned) and plain
tables and renders a markdown report. A driver defines its scenario as a
`Config` and calls `main(cfg)`; the drivers' docstrings describe the usage.
The tool's output of every run goes to the driver's log directory.
"""

import argparse
import dataclasses
import datetime
import json
import os
import re
import subprocess
import sys
import urllib.request

DB = "postgresql://wire-server:posty-the-gres@localhost:5432/fan_in_perf"
GRAFANA = "http://localhost:3000/d/fan-in-perf/fan-in-perf"
REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
SEED = 42
WARMUP, DRAIN = 5, 30
QUICK = 1  # --quick: durations divided by this
PAYLOAD = None  # --payload-bytes: passed to the pushing commands; None: the tool's default (512)
BIN = None  # the built fan-in-perf binary, set by main (no `cabal run` check per call)
LOGDIR = None  # set by main


@dataclasses.dataclass
class Config:
    """What differs between the drivers."""
    script: str  # file name, shown in the report
    targets: str  # --targets
    reader_args: list  # simulation arguments of the commands with readers (--online-users, --clients-per-user); `produce` has no readers
    title: str  # report title, after "Notification fan-in: PostgreSQL benchmark, "
    clients: str  # "2,000 clients": the readers in titles and answers
    load_text: str  # what one push is and who reads it, for the Load bullet
    scope_text: str  # last Setup bullet: what is (not) exercised
    default_out: str
    logdir: str
    # Tuned beforehand on plain (45 s runs, repeated): A max push/s; B, C highest rate
    # whose readers kept up (backlog at writer stop < 1 s, no loss) in every run.
    w_a: int
    w_b: int
    w_c: int
    pool_b: int
    pool_c: int
    # E, F: produce (w_split writers) for split_produce s, then consume-only.
    w_split: int
    split_produce: int
    split_warmup: int
    split_timeout: int
    pool_split: int  # reader pool of E and F
    tuning_text: str  # how w_*/pool_* were chosen, for the Setup bullet


def log(msg):
    print(f"[{datetime.datetime.now():%H:%M:%S}] {msg}", file=sys.stderr, flush=True)


def fan_in_perf_cmd(args, summary_json=None):
    # the flag belongs to the load commands: after the subcommand
    return [BIN, "--db", DB] + [str(a) for a in args] + (["--summary-json", summary_json] if summary_json else [])


def tool_cmd(args):
    """A tool invocation as shown in the report (the database as $DB)."""
    return "fan-in-perf --db \"$DB\" " + " ".join(str(a) for a in args)


def reset_args(partman):
    return ["reset-db"] + (["--partitioned"] if partman else [])


def run_tool(args):
    """Runs a short command of the tool (reset-db, vacuum, db-info); returns its output."""
    r = subprocess.run(fan_in_perf_cmd(args), cwd=REPO, capture_output=True, text=True)
    if r.returncode != 0:
        raise RuntimeError(f"fan-in-perf {args[0]} failed: {(r.stdout + r.stderr).strip()[-300:]}")
    return r.stdout


def summary_json(logpath):
    """The tool's --summary-json file of a run, next to its log."""
    return os.path.splitext(logpath)[0] + ".json"


def load(logpath):
    """-> (the run's summary as parsed JSON, the text summary of its log). The tool creates the file empty and fills it after the run."""
    path = summary_json(logpath)
    if not os.path.exists(path) or os.path.getsize(path) == 0:
        raise RuntimeError(f"no summary, see {logpath}")
    with open(path) as f:
        j = json.load(f)
    text = open(logpath).read()
    return j, text[text.index("\nsummary:"):].strip()


def format_latency(seconds):
    """A latency from the summary (seconds) as the tool's text summary prints it: milliseconds below one second, else seconds
    (0.0084 -> "8.4ms", 1.07 -> "1.07s"). None (no observations) stays None; the report shows it as "-"."""
    return None if seconds is None else f"{seconds * 1000:.1f}ms" if seconds < 1 else f"{seconds:.2f}s"


def round_rate(per_second):
    """A rate from the summary as the whole number the tool's text summary prints; None stays None."""
    return None if per_second is None else round(per_second)


def drain_text(d):
    if d is None:
        return None
    word = {"complete": "complete after", "stalled": "stalled after", "timed_out": "timed out after"}.get(d["state"], "interrupted")
    return word + (f" {d['seconds']:.1f}s" if d["seconds"] is not None else "")


def end_text(e, duration):
    return {"finished": "all readers caught up after", "timed_out": "timed out after", "interrupted": "interrupted after"}[e] + f" {duration:.1f}s"


def summary_fields(logpath):
    """The fields the report uses, from the tool's summary JSON of a run (`json`: all of it)."""
    j, text = load(logpath)
    p, d, f, a, db = (j[k] or {} for k in ("pushes", "deliveries", "fetch", "acks", "database"))
    r = {
        "pushes": p.get("pushes"),
        "push_err": p.get("errors"),
        "push_s": round_rate(p.get("rate_avg")),
        "push_p99": format_latency(p.get("latency", {}).get("p99")),
        "cons_s": round_rate(d.get("rate_avg")),
        "delivered": d.get("delivered"),
        "missing": d.get("missing"),
        "dup": d.get("duplicates"),
        "backlog": d.get("backlog_at_writer_stop"),
        "lag_p99": format_latency((d.get("lag") or {}).get("p99")),
        "fetch_err": f.get("errors"),
        "ack_s": round_rate(a.get("rate_avg")),
        "acks": a.get("acks"),
        "ack_err": a.get("errors"),
        "ack_p99": format_latency(a.get("latency", {}).get("p99")),
        "drain": drain_text(j["drain"]),
        "deadlocks": db.get("deadlocks"),
        "rows": db.get("rows"),  # pushes - rows = expired by TTL
    }
    r["gc"] = j["gc"]  # consume runs on partitioned tables: the tool sampled the partition drops
    r["errors"] = sum(r[k] or 0 for k in ("push_err", "fetch_err", "ack_err"))
    r["summary"] = text
    return r


def logged_run(label, logpath, args, parser=summary_fields):
    """Runs the tool with its output logged; -> (parsed summary, exit code)."""
    _, rc = tool_logged(logpath, args)
    try:
        return parser(logpath), rc
    except RuntimeError as e:
        raise RuntimeError(f"{label}: {e} (exit {rc})")


def run(label, args, partman):
    run_tool(reset_args(partman))  # waits for pg_partman to settle
    r, rc = logged_run(label, os.path.join(LOGDIR, label + ".log"), args)
    r.update(label=label, exit=rc, cmd=tool_cmd(reset_args(partman)) + "\n" + tool_cmd(args))
    return r


def tool_args(cfg, exp, w, pool, dur):
    a = [exp, "--writers", w, "--targets", cfg.targets, *([] if exp == "produce" else cfg.reader_args), "--warmup", WARMUP, "--duration", max(1, dur // QUICK), "--seed", SEED]
    if exp != "produce":
        a += ["--reader-pool-size", pool, "--safety-window", 200, "--drain-timeout", DRAIN]
    if exp == "consume-ack":
        a += ["--ack-every", 1]
    if PAYLOAD:
        a += ["--payload-bytes", PAYLOAD]
    return a


def single_command_experiments(cfg):
    """Built at run time: --quick changes the durations."""
    return [
        ("A", tool_args(cfg, "produce", cfg.w_a, None, 60)),
        ("B", tool_args(cfg, "consume", cfg.w_b, cfg.pool_b, 120)),
        ("C", tool_args(cfg, "consume-ack", cfg.w_c, cfg.pool_c, 120)),
        ("D", tool_args(cfg, "consume-ack", cfg.w_c, cfg.pool_c, 660)),
    ]


def split_experiments(cfg):
    """(label, produce args, consume-only args), run on plain tables only: the 5 min
    retention of the partitioned tables would drop partitions during the consume phase."""
    produce = tool_args(cfg, "produce", cfg.w_split, None, cfg.split_produce)
    consume = ["--targets", cfg.targets, *cfg.reader_args, "--seed", SEED, "--warmup", cfg.split_warmup, "--timeout", cfg.split_timeout]
    return [
        ("E", produce, ["consume-only", "--reader-pool-size", cfg.pool_split] + consume),
        ("F", produce, ["consume-only-ack", "--reader-pool-size", cfg.pool_split, "--ack-every", 1] + consume),
    ]


def tool_logged(logpath, args):
    """Runs the tool and logs its output; returns (output, exit code)."""
    lines = []
    with open(logpath, "w") as lf:
        p = subprocess.Popen(fan_in_perf_cmd(args, summary_json(logpath)), cwd=REPO, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        for line in p.stdout:
            lf.write(line)
            lines.append(line)
        rc = p.wait()
    return "".join(lines), rc


def summary_fields_consume_only(logpath):
    """The consume-only log: `summary_fields` for the fields it shares with the others, plus its own."""
    r = summary_fields(logpath)
    j, _ = load(logpath)
    d = j["deliveries"]
    milestone = lambda pct: next((f"{m['seconds']:.1f}s" if m["seconds"] is not None else "-" for m in d["milestones"] if m["percent"] == pct), None)
    r.update(
        rows_total=j["rows_total"],
        expected=d["expected"],
        cons_time=round(j["duration"], 1),
        cons_max="-" if d["rate_max"] is None else round(d["rate_max"]),
        fetch_p99=format_latency(j["fetch"]["latency"]["p99"]),
        t50=milestone(50),
        t90=milestone(90),
        t99=milestone(99),
        end=end_text(j["end"], j["duration"]),
    )
    return r


def run_split(label, produce_args, consume_args):
    """reset, produce, vacuum (else autovacuum and hint-bit writes would land in
    the measured read phase; the checkpoint after it is the start of consume-only), consume-only."""
    run_tool(["reset-db"])
    p, prc = logged_run(f"{label}: produce", os.path.join(LOGDIR, label + "-produce.log"), produce_args)
    run_tool(["vacuum"])
    c, crc = logged_run(f"{label}: consume", os.path.join(LOGDIR, label + "-consume.log"), consume_args, summary_fields_consume_only)
    return {
        **c,
        "label": label, "exit": prc or crc, "deadlocks": p["deadlocks"] + c["deadlocks"], "rows": p["rows"], "drain": None,
        "pushes": p["pushes"], "push_s": p["push_s"], "push_p99": p["push_p99"], "push_err": p["push_err"],
        "errors": p["errors"] + c["errors"],
        "cmd": "\n".join(tool_cmd(a) for a in (reset_args(False), produce_args, ["vacuum"], consume_args)),
        "summary": "produce:\n" + p["summary"] + "\n\nconsume:\n" + c["summary"],
    }


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
    "B": "B: adding and consuming by {clients}",
    "C": "C: B, every client acknowledges every notification",
    "D": "D: C for {D} while TTL garbage collection runs",
    "E": "E: B split: produce only, then {clients} consume only",
    "F": "F: C split: produce only, then {clients} consume and acknowledge only",
}


def cell(v):
    return "-" if v is None else f"{v:,}" if isinstance(v, int) else str(v)


def duration_text(secs):
    return f"{secs // 60} minutes" if secs % 60 == 0 else f"{secs} s"


def gc_section(pm):
    """The TTL GC paragraph and table of D partman (printed right after D's table)."""
    expired = pm["pushes"] - pm["rows"]
    secs_expired = expired / pm["push_s"] if pm["push_s"] else 0
    gc = pm["gc"]
    win = gc["window_seconds"]
    count = lambda x: cell(None if x is None else round(x))
    row = lambda name, w: (f"| {name} | {w['seconds']} | {count(w['push_rate'])} | {count(w['ack_rate'])} | {cell(format_latency(w['ack_p99']))} | "
                           f"{cell(format_latency(w['ack_p99_max']))} | {cell(format_latency(w['lag_p99']))} | {count(w['backlog'])} |")
    by_table = ", ".join(f"`{t}` {n}" for t, n in sorted(gc["drops_by_table"].items()))
    return [f"D partman: {gc['drops']} partition drops over the six tables ({by_table}; each takes an `ACCESS EXCLUSIVE` lock on its table); {expired:,} rows (about {secs_expired:.0f} s of pushes) were expired by TTL, i.e. the later drops held data.", "",
          f"Impact of the drops: per-second figures within {win:g} s of a drop vs. all other seconds of D partman (medians):", "",
          "| Window | seconds | push/s (median) | ack/s (median) | ack p99 (median) | ack p99 max | lag p99 (median) | backlog (median) |", "|---|---|---|---|---|---|---|---|",
          row(f"within {win:g} s of a drop", gc["within"]), row("other", gc["other"]), ""]


def render_report(cfg, res, env):
    dur = {x: int(a[a.index("--duration") + 1]) for x, a in single_command_experiments(cfg)}  # as run (--quick)
    split_dur = max(1, cfg.split_produce // QUICK)  # as run (--quick)
    partman = any(k.endswith("-partman") for k in res)
    L = [f"# Notification fan-in: PostgreSQL benchmark, {cfg.title}{f', {PAYLOAD:,} B payload' if PAYLOAD else ''} (WPB-26288)", "",
         "## Setup", "",
         f"- Date {env['date']}, commit `{env['commit']}`, driver `tools/fan-in-perf/{cfg.script}`",
         f"- Host: {env['cpus']} CPUs, {env['mem']} RAM; tool and database share it. {env['pg']}; {env['settings']}",
         *([f"- pg_partman {env['partman']}: six `*_notifications` tables range-partitioned by UUIDv7, 1 min partitions, 5 min retention, no default partition, maintenance by the background worker every 60 s. `plain`: unpartitioned, no TTL.",] if partman else []),
         f"- Load: `--targets {cfg.targets}`{''.join(f' {a}' for a in cfg.reader_args)}: {cfg.load_text} B-D: every client reads, poll interval 100 ms, fetch limit 100, safety window 200 ms; C, D: ack after every notification.",
         f"- Writers A={cfg.w_a}, B={cfg.w_b}, C/D={cfg.w_c}; reader pool B={cfg.pool_b}, C/D={cfg.pool_c}: {cfg.tuning_text} Seed {SEED}. One run each.",
         f"- Rates are averages over the writer phase incl. {WARMUP} s warmup; p99 are histogram bucket upper bounds (at most 25 % above). `backlog at stop`: expected minus delivered when the writers stopped. `missing`: never delivered after the drain. `errors`: failed pushes + fetches + acks.",
         f"- E, F (plain only): `reset-db`, `produce` ({cfg.w_split} writers, {split_dur} s), `vacuum` (VACUUM ANALYZE), then `consume-only[-ack]` with the same seed and targets. The rows were just written, so caches are warm. Readers start at the oldest row (a client that was offline and acknowledged nothing). cons/s avg = delivered / consume time (until the last reader caught up), incl. warmup and that tail; `time to N %`: until N % of the notifications were delivered (50 ms resolution), to tell the tail from the steady rate; max ignores the first {cfg.split_warmup} s. Every reader fetches its backlog once and stops (no polling). Reader pool {cfg.pool_split}. `rows`: notifications in the table after produce.",
         *([f"- Payload: about {PAYLOAD:,} bytes of JSON per notification (random letters); writers and pools as tuned for the default 512 bytes."] if PAYLOAD else []),
         *(["- `--quick`: all durations divided by 10 (smoke test), the numbers are not comparable to a full run."] if QUICK > 1 else []),
         f"- {cfg.scope_text}",
         "",
         "Each experiment section lists the commands run, starting with `reset-db` (`--partitioned` for partman, which waits for pg_partman to settle). Every run starts with a CHECKPOINT.", ""]
    for exp in "ABCDEF":
        if not any(f"{exp}-{m}" in res for m in MODES):
            continue
        L += [f"## {TITLES[exp].format(clients=cfg.clients, **{k: duration_text(v) for k, v in dur.items()})}", ""]
        for m in MODES:
            if f"{exp}-{m}" in res:
                L += [f"Commands ({m}):", "", "```", res[f"{exp}-{m}"]["cmd"], "```", ""]
        if exp in "EF":
            L += ["partman: not run (its 5 min retention would drop partitions during the consume phase).", ""]
        L += ["| Mode | " + " | ".join(h for h, _ in COLS[exp]) + " |", "|---" * (len(COLS[exp]) + 1) + "|"]
        for m in MODES:
            r = res.get(f"{exp}-{m}")
            if r:
                L.append(f"| {m} | " + " | ".join(cell(r[k]) for _, k in COLS[exp]) + " |")
        L.append("")
        if exp == "D" and "D-partman" in res:
            L += gc_section(res["D-partman"])
    L += ["## Answers", ""]
    g = lambda e, m: res.get(f"{e}-{m}", {})
    for m in MODES:
        a = g("A", m)
        if a:
            L.append(f"- a. ({m}) {cell(a.get('push_s'))} notifications/s (avg) added, push p99 {a.get('push_p99')}, {cell(a.get('errors'))} errors.")
    for m in MODES:
        b = g("B", m)
        if b:
            L.append(f"- b. ({m}) {cell(b.get('push_s'))} added/s (avg) and {cell(b.get('cons_s'))} consumed/s (avg) by {cfg.clients}, lag p99 {b.get('lag_p99')}, {cell(b.get('missing'))} missing.")
    for m in MODES:
        c = g("C", m)
        if c:
            L.append(f"- c. ({m}) {cell(c.get('push_s'))} added/s (avg), {cell(c.get('cons_s'))} consumed/s (avg) and {cell(c.get('ack_s'))} acknowledged/s (avg), ack p99 {c.get('ack_p99')}, {cell(c.get('missing'))} missing.")
    dp = g("D", "partman")
    if dp:
        gc = dp["gc"]
        i, o = gc["within"], gc["other"]
        r = lambda a, b: f"{a / b:.2f}" if a is not None and b else "-"
        L.append(f"- d. During TTL GC ({gc['drops']} partition drops over the six tables in {duration_text(dur['D'])}): within {gc['window_seconds']:g} s of a drop vs. otherwise, ack/s {r(i['ack_rate'], o['ack_rate'])} x, "
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
        # in flight when produce ends commit rows that are not counted (at most w_split)
        if x and not 0 <= x["rows"] - x["pushes"] <= cfg.w_split:
            notes.append(f"- {e}-plain: produce reported {x['pushes']:,} pushes but the table holds {x['rows']:,} rows")
        if x and not x["end"].startswith("all readers caught up"):
            notes.append(f"- {e}-plain: consume-only ended: {x['end']}")
        if x and x["rows_total"] != x["rows"]:
            notes.append(f"- {e}-plain: consume-only counted {cell(x['rows_total'])} rows, the database {x['rows']:,}")
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


def db_info():
    """The tool's db-info: server version, the settings that matter for the numbers, pg_partman version (None without)."""
    return json.loads(run_tool(["db-info"]))


def env():
    kb = int(re.search(r"MemTotal:\s+(\d+)", open("/proc/meminfo").read()).group(1))
    info = db_info()
    return {
        "date": datetime.date.today().isoformat(),
        "commit": subprocess.run(["git", "log", "-1", "--format=%h"], cwd=REPO, capture_output=True, text=True).stdout.strip(),
        "cpus": os.cpu_count(),
        "mem": f"{kb / 1048576:.0f} GiB",
        "pg": info["version"],
        "settings": ", ".join(f"{k}={v}" for k, v in sorted(info["settings"].items())),
        "partman": info["partman_version"] or "not run",
    }


def main(cfg, doc):
    global DB, QUICK, BIN, PAYLOAD, LOGDIR
    ap = argparse.ArgumentParser(description=doc.split("\n\n")[0])
    ap.add_argument("out", nargs="?", default=cfg.default_out)
    ap.add_argument("--only", help="comma-separated experiments to run, e.g. E,F (default: all)")
    ap.add_argument("--quick", action="store_true", help="divide all durations by 10 (smoke test)")
    ap.add_argument("--payload-bytes", type=int, help="approximate JSON payload size of every push (default: the tool's, 512)")
    ap.add_argument("--db", default=DB, help="connection string; its fan-in tables and pg_partman are dropped and recreated")
    args = ap.parse_args()
    DB, out = args.db, args.out
    QUICK = 10 if args.quick else 1
    PAYLOAD = args.payload_bytes
    LOGDIR = cfg.logdir + (f"-p{PAYLOAD}" if PAYLOAD else "")  # keeps the logs of default runs
    wanted = set(args.only.upper().split(",")) if args.only else set("ABCDEF")
    try:
        urllib.request.urlopen("http://localhost:9400/metrics", timeout=1)
        sys.exit("port 9400 is busy (another fan-in-perf run?)")
    except OSError:
        pass
    log("building fan-in-perf")
    subprocess.run(["cabal", "build", "exe:fan-in-perf"], cwd=REPO, check=True, stdout=subprocess.DEVNULL)
    BIN = subprocess.run(["cabal", "list-bin", "exe:fan-in-perf"], cwd=REPO, check=True, capture_output=True, text=True).stdout.strip()
    os.makedirs(LOGDIR, exist_ok=True)
    res, e = {}, None
    for mode in MODES:
        runs = [(x, a) for x, a in single_command_experiments(cfg) if x in wanted]
        splits = [x for x in split_experiments(cfg) if x[0] in wanted] if mode == "plain" else []
        if not runs and not splits:
            continue
        log(f"database for {mode}")
        # every run resets again (and settles); this one only creates the database to ask
        run_tool(["reset-db"] + (["--partitioned", "--no-settle"] if mode == "partman" else []))
        if e is None:  # partman comes first, so its version is known if it runs
            e = env()
        for exp, a in runs:
            log(f"{exp} {mode}")
            res[f"{exp}-{mode}"] = run(f"{exp}-{mode}", a, mode == "partman")
        for exp, pa, ca in splits:
            log(f"{exp} {mode}")
            res[f"{exp}-{mode}"] = run_split(f"{exp}-{mode}", pa, ca)
    md = render_report(cfg, res, e)
    with open(out, "w") as f:
        f.write(md)
    print(md)
    log(f"report: {out}; logs: {LOGDIR}; Grafana: {GRAFANA}")
