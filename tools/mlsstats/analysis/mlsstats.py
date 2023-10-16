#!/usr/bin/env python3

import csv

def team_conversations():
    total = {'proteus': 0, 'mixed': 0, 'mls': 0}
    teams = {}
    with open('conv-group-team-protocol.csv') as csv_file:
        csv_reader = csv.DictReader(csv_file, delimiter=',')

        for row in csv_reader:
            team = row['team']
            protocol = row['protocol']

            if protocol not in ['proteus', 'mixed', 'mls']:
                protocol = 'proteus'
            if protocol not in total:
                total[protocol] = 0
            if team not in teams:
                teams[team] = {'proteus': 0, 'mixed': 0, 'mls': 0}

            total[protocol] += 1
            teams[team][protocol] += 1

    for name, team in teams.items():
        proteus = team['proteus']
        mixed = team['mixed']
        mls = team['mls']
        print(f'In team {name}, conversations ' +
              f'in Proteus are {proteus}, ' +
              f'in Mixed are {mixed}, and ' +
              f'in MLS are {mls}.')

    proteus = total['proteus']
    mixed = total['mixed']
    mls = total['mls']
    print(f'In total, conversations in Proteus are {proteus}, '+
          f'in Mixed are {mixed}, and in MLS are {mls}.')

def conversation_clients():
    with open('conv-group-team-protocol.csv') as csv_file:
        conv = {}
        proto = {}
        csv_reader = csv.DictReader(csv_file, delimiter=',')
        for row in csv_reader:
            conversation = row['conversation']
            if row['group'] != '':
                conv[row['group']] = conversation
            proto[conversation] = row['protocol']

    with open('domain-user-client-group.csv') as csv_file:
        mls_clients = {}
        csv_reader = csv.DictReader(csv_file, delimiter=',')
        for row in csv_reader:
            group = row['group']
            if group in conv:
                conversation = conv[group]
                user = row['user']
                if conversation not in mls_clients:
                    mls_clients[conversation] = {user: 0}
                if user not in mls_clients[conversation]:
                    mls_clients[conversation][user] = 0
                mls_clients[conversation][user] += 1

    with open('user-client.csv') as csv_file:
        user_clients = {}
        csv_reader = csv.DictReader(csv_file, delimiter=',')
        for row in csv_reader:
            user = row['user']
            if user not in user_clients:
                user_clients[user] = 0
            user_clients[user] += 1

    with open('user-conv.csv') as csv_file:
        conv_clients = {}
        csv_reader = csv.DictReader(csv_file, delimiter=',')
        for row in csv_reader:
            user = row['user']
            conversation = row['conversation']
            if conversation in proto: # only team conversations
                if conversation not in conv_clients:
                    conv_clients[conversation] = {'proteus': 0, 'mls': 0}
                if conversation in mls_clients and user in mls_clients[conversation]:
                    mls = mls_clients[conversation][user]
                else:
                    mls = 0
                if user in user_clients:
                    proteus = user_clients[user] - mls
                else:
                    proteus = 0
                conv_clients[conversation]['proteus'] += proteus
                conv_clients[conversation]['mls'] += mls

    total_proteus = 0
    total_mls = 0
    for name, conversation in conv_clients.items():
        proteus = conversation['proteus']
        mls = conversation['mls']
        protocol = proto[name]
        total_proteus += proteus
        total_mls += mls
        print(f'In conversation {name} ({protocol}), there are ' +
            f'{proteus} Proteus clients and {mls} MLS clients.')

    print(f'In total, there are {total_proteus} Proteus clients ' +
          f'and {total_mls} MLS clients.')
