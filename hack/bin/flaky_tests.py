#!/usr/bin/env python3

import datetime
import requests
import json
import os
import yaml
import argparse

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

def associate_fails(test_names, data, default_comment=''):
    d = {k: [] for k in test_names}
    unassociated = []
    for item in data:
        flake = search_matching_flake(test_names, item['test_name'])
        if flake:
            d[flake].append(item)
        else:
            unassociated.append(item)

    return [{'test_name': k, 'fails': v, 'comments': default_comment} for k, v in  d.items()], unassociated

def associate_comments(flakes, comments, default_comments=''):
    for flake in flakes:
        flake['comments'] = comments.get(flake['test_name'], default_comments)

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
    return Colors.BLUE + f'· {format_date(dt)} ({humanize_days(days)})' + Colors.RESET

def create_url(build):
    return CONCOURSE_BASEURL + f"/pipelines/{build['pipeline_name']}/jobs/{build['job_name']}/builds/{build['name']}"

def pretty_flake(flake, today, logs=False):
    lines = []
    lines.append(Colors.YELLOW + f"❄ \"{flake['test_name']}\"" + Colors.RESET)

    if not logs:
        lines.append(f'  Run with --log "{flake["test_name"]}" to see error logs')

    comments = flake['comments']
    if comments:
        for l in comments.splitlines():
            lines.append('  ' + Colors.PURPLEISH + l + Colors.RESET)
    lines.append('')
    for fail in flake['fails']:
        b = fail['build']

        end_time = datetime.datetime.fromtimestamp(b['end_time'])
        s = human_format_date(end_time, today)
        if (today - end_time) < datetime.timedelta(days=CONCOURSE_LOG_RETENTION_DAYS):
            url = create_url(b)
            s = s + ' ' + url
        lines.append('  ' + s)
        if logs:
            lines.append('')
            for l in fail['context'].splitlines():
                lines.append('      ' + l)
            lines.append('')

    return "\n".join(lines) + '\n'

def pretty_flakes(flakes, today, logs=False):
    lines = []
    for flake in flakes:
        lines.append(pretty_flake(flake, today, logs))
    return '\n'.join(lines)

def pager(s):
    pipe = os.popen('less -RS', 'w')
    pipe.write(s)
    pipe.close()

explain = '''Tips:
    Run with --discover to manually discover new flaky tests

'''

def main():
    parser = argparse.ArgumentParser(prog='flaky_test.py', description='Shows flaky tests')
    parser.add_argument('-d', '--discover', action='store_true', help='Show failing tests that are not marked/discovered as being flaky. Use this to manually discover flaky test.')
    parser.add_argument('-l', '--logs', help='Show surrounding logs for given test')
    args = parser.parse_args()

    today = datetime.datetime.now()
    data = fetch(n_weeks=4*4)
    if args.logs:
        flakes, unassociated = associate_fails([args.logs], data)
        sort_flakes(flakes)
        pager(explain + pretty_flakes(flakes, today, logs=True))
        return

    test_names = discover_flakes(data)
    flaky_tests_comments = read_flaky_tests()
    test_names = test_names.union(flaky_tests_comments.keys())
    flakes, unassociated = associate_fails(test_names, data)


    if args.discover:

        test_names = set([i['test_name'] for i in unassociated])
        flake_candidates, _ = associate_fails(test_names, unassociated, '(if this is a flaky test, please add it to flaky-tests.yaml)')
        sort_flakes(flake_candidates)
        pager(explain + pretty_flakes(flake_candidates, today))

    else:
        associate_comments(flakes, flaky_tests_comments, '(discovered flake, please check and add it to flaky-tests.yaml, otherwise it might now show up again)')
        sort_flakes(flakes)
        pager(explain + pretty_flakes(flakes, today))


def test():
    data = fetch(1)
    return data


if __name__ == '__main__':
    main()
