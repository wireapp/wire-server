#!/usr/bin/env python3

import datetime
import requests
import json
import os
import yaml

BUCKET_BASEURL = 'https://s3.eu-west-1.amazonaws.com/public.wire.com/ci/failing-tests'
CONCOURSE_BASEURL = 'https://concourse.ops.zinfra.io/teams/main'
CONCOURSE_LOG_RETENTION_DAYS = 7

class Colors:
    GREEN = "\x1b[38;5;10m"
    YELLOW = "\x1b[38;5;11m"
    BLUE = "\x1b[38;5;6m"
    PURPLEISH = "\x1b[38;5;13m"
    ORANGE = "\x1b[38;5;3m"
    RED = "\x1b[38;5;1m"
    RESET = "\x1b[0m"

def read_flaky_tests():
    project_root = os.path.join(os.path.dirname(__file__), os.path.pardir, os.path.pardir)
    result = {}
    with open(os.path.join(project_root, 'flaky-tests.yaml'), 'r') as f:
        d = yaml.safe_load(f)
        for item in d:
            result[item['test_name']] = item['comments']
    return result

def current_week_start():
    now = datetime.date.today()
    k = now.weekday()
    return now - datetime.timedelta(days=k)

def format_date(dt):
    return dt.strftime('%Y-%m-%d')

def failing_tests_fn(week_start):
    return format_date(week_start) + '_failing_tests.json'

def fetch_week(week_start):
    url = f'{BUCKET_BASEURL}/{failing_tests_fn(week_start)}'
    r = requests.get(url)
    result = []
    if r.status_code == 200:
        for line in r.content.split(b'\n'):
            if len(line) > 0:
                item = json.loads(line.decode('utf8'))
                result.append(item)
    return result

def fetch(n_weeks=1):
    ws_start = current_week_start()
    data = []
    for i in range(n_weeks):
        ws = ws_start + datetime.timedelta(days=i)
        data += fetch_week(ws)
    return data

def tests_match(s1, s2):
    return s1 in s2 or s2 in s1

def longer(s1, s2):
    if len(s1) > len(s2):
        return s1
    else:
        return s2

def is_flake(item):
    b = item['build']
    return ((b['pipeline_name'] == 'mls' and b['job_name'] == 'test') \
            or (b['pipeline_name'] == 'staging' and b['job_name'] == 'test') \
            or (b['pipeline_name'] == 'prod' and b['job_name'] == 'test'))


def add_flake(flake_set, test_name):
    for k in flake_set:
        if tests_match(k, test_name):
            k_ = longer(k, test_name)
            if k_ != k:
                flake_set.remove(k)
                flake_set.add(k_)
            return
    flake_set.add(test_name)

def discover_flakes(data):
    flake_set = set()
    for item in data:
        if is_flake(item):
            add_flake(flake_set, item['test_name'])
    return flake_set

def search_matching_flake(test_names, test_name):
    for flake in test_names:
        if tests_match(flake, test_name):
            return flake

def associate_fails(test_names, data):
    d = {k: [] for k in test_names}
    for item in data:
        flake = search_matching_flake(test_names, item['test_name'])
        if flake:
            d[flake].append(item)
    return [{'test_name': k, 'fails': v} for k, v in  d.items()]

def associate_comments(flakes, comments):
    for flake in flakes:
        flake['comments'] = comments.get(flake['test_name'])

def sort_flakes(flakes):
    flakes.sort(key=lambda f: len(f['fails']), reverse=True)
    for flake in flakes:
        flake['fails'].sort(key=lambda f: f['build']['end_time'], reverse=True)

def humanize_days(n):
    if n < 7:
        if n == 0:
            return 'today'
        elif n == 1:
            return 'yesterday'
        else:
            return f'{n} days ago'
    else:
        weeks = n // 7
        if weeks < 4:
            return f'{weeks} week{"s" if weeks >= 2 else ""} ago'
        else:
            return f'{weeks // 4} month{"s" if weeks >= 8 else ""} ago'

def human_format_date(dt, today):
    days = (today - dt).days
    return f'· {format_date(dt)} ({humanize_days(days)})'

def create_url(build):
    return CONCOURSE_BASEURL + f"/pipelines/{build['pipeline_name']}/jobs/{build['job_name']}/builds/{build['name']}"

def pretty_flake(flake, today):
    lines = []
    lines.append(Colors.YELLOW + f"❄ \"{flake['test_name']}\"" + Colors.RESET)
    comments = flake['comments']
    if comments:
        for l in comments.splitlines():
            lines.append('    ' + l)
    else:
        lines.append('    (discovered flake, no notes in flaky-tests.yaml)')
    lines.append('')
    for fail in flake['fails']:
        b = fail['build']
        end_time = datetime.datetime.fromtimestamp(b['end_time'])
        s = human_format_date(end_time, today)
        if (today - end_time) < datetime.timedelta(days=CONCOURSE_LOG_RETENTION_DAYS):
            url = create_url(b)
            s = s + ' ' + url
        lines.append('  ' + s)

    return "\n".join(lines) + '\n'

def pager(s):
    pipe = os.popen('less -RS', 'w')
    pipe.write(s)
    pipe.close()


def main():
    today = datetime.datetime.now()
    data = fetch(n_weeks=4*4)

    test_names = discover_flakes(data)
    flaky_tests_comments = read_flaky_tests()
    test_names = test_names.union(flaky_tests_comments.keys())
    flakes = associate_fails(test_names, data)
    associate_comments(flakes, flaky_tests_comments)

    sort_flakes(flakes)
    lines = []
    for flake in flakes:
        lines.append(pretty_flake(flake, today))
    pager('\n'.join(lines))

def test():
    data = fetch(1)
    return data


if __name__ == '__main__':
    main()
