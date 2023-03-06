#!/usr/bin/env python3

import sys
import argparse
import subprocess
import re
import datetime
import os

class Colors:
    GREEN = "\x1b[38;5;10m"
    YELLOW = "\x1b[38;5;11m"
    BLUE = "\x1b[38;5;6m"
    PURPLEISH = "\x1b[38;5;13m"
    ORANGE = "\x1b[38;5;3m"
    RED = "\x1b[38;5;1m"
    RESET = "\x1b[0m"

def parse_bumps(stdout, env, maxreleases=50):
    releases = 0
    result = {}
    for commit in stdout.splitlines():
        [d, h, msg] = commit.decode('utf8').split('<<>>')
        x = f'\[env:{re.escape(env)}\] Deploy wire-server version (.*)'
        m = re.match(x, msg)
        if m:
            (version, ) = m.groups()
            result[version] = {'t': int(d), 'hash': h}
            releases += 1
        if releases >= maxreleases:
            break
    return result


def get_chart_version(s):
    for x in s.split(', '):
        m = re.match('tag: chart/(.+)', x)
        if m:
            (chart_version,) = m.groups()
            return chart_version

def parse_tags(stdout, maxcommits=400):
    releases = 0
    result = []
    commits = 0
    for commit in stdout.splitlines():
        [h, cd, msg, author_name, anns] = commit.decode('utf8').split('<<>>')
        chart_version = get_chart_version(anns)
        d= {
            'hash': h,
            'msg': msg,
            'author_name': author_name,
            'chart_version': chart_version,
            'commiter_date': int(cd)
        }
        result.append(d)
        commits += 1
        if commits >= maxcommits:
            break
    return result

def humanize_difftime(d):
    secs = int(d.total_seconds())

    minutes, secs = divmod(secs, 60)
    if minutes == 0:
        return f'{secs}s'

    hours, minutes = divmod(minutes, 60)
    if hours == 0:
        return f'{minutes}m'

    days, hours = divmod(hours, 24)
    if days == 0:
        return f'{hours}h {minutes}m'

    return f'{days}d'

def humanize_time(ts):
    ts = datetime.datetime.fromtimestamp(ts)
    now = datetime.datetime.now()
    d = now - ts
    return ts.strftime('%a %b %d %H:%M %Y') + f' ({humanize_difftime(d)} ago)'


def less(data):
    process = subprocess.Popen(["less"], stdin=subprocess.PIPE)

    try:
        process.stdin.write(data)
        process.communicate()
    except IOError as e:
        pass


def main(args):
    wire_server = '.'
    cailleach = os.path.join(wire_server, '..', 'cailleach')
    p = subprocess.Popen(['git', 'log', '--format=%ct<<>>%h<<>>%s', 'origin/master'], cwd=cailleach, stdout=subprocess.PIPE)
    out, _ = p.communicate()
    env_bumps = parse_bumps(out, args.env)
    cmd = ['git', 'log', '--format=%h<<>>%ct<<>>%s<<>>%an<<>>%D', '--first-parent', args.rev]
    p = subprocess.Popen(['git', 'log', '--format=%h<<>>%ct<<>>%s<<>>%an<<>>%D', '--first-parent', args.rev], cwd=wire_server, stdout=subprocess.PIPE)
    out, _ = p.communicate()
    commits = parse_tags(out)
    output = ''
    output += ' '.join([a for a in cmd if not a.startswith('--format')]) + '\n'
    output += 'with version bumps for ' + Colors.GREEN + f'{args.env}' + Colors.RESET + '\n\n'
    for commit in commits:
        s = Colors.YELLOW + f'{commit["hash"]}' + Colors.RESET + f': {commit["msg"]}'

        s += f'''
Author: {commit["author_name"]}
'''
        if commit['chart_version']:
            s += f'Chart Version: {commit["chart_version"]}\n'

            bump = env_bumps.get(commit['chart_version'])
            if bump:
                bump_time = humanize_time(bump['t']) + f' ({bump["hash"]})'
            else:
                bump_time = Colors.RED + '<not deployed>' + Colors.RESET
            s += Colors.GREEN + f'{args.env}' + Colors.RESET + ' bump: ' + Colors.GREEN + f'{bump_time}\n' + Colors.RESET
        s += '\n'
        output += s
    return less(output.encode('utf8'))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
                        prog = 'git-log-bumps',
                        description = f'''Shows commits in wire-server together with their environment bumps in cailleach.

This script assumes that your wire-server and cailleach repos live next to each other.

Examples:
    {sys.argv[0]} --env staging --rev origin/develop          # the default
    {sys.argv[0]} --env prod --rev origin/master              # production releases
    {sys.argv[0]} --env '{{anta,bella,chala}}' --rev origin/mls # releases to anta&co

''', formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('--env', type=str, default="staging", help='Name of the environment in cailleach. default: staging')
    parser.add_argument('--rev', type=str, default="origin/develop", help="The target of git log. default: origin/develop")
    args = parser.parse_args()
    try:
        main(args)
    except BrokenPipeError:
        pass
