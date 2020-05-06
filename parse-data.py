import sys
import json
import re

filename = sys.argv[1]

data = {}
currentAggregation = None
currentRunIndex = None

runTypes = ['seq', 'par1', 'par2', 'par4', 'par8']

with open(filename) as fh:
    for lineRaw in fh:
        line = lineRaw.strip()

        if line == '':
            continue

        aggMatch = re.search('''NOW TESTING FOR nodeCount=([0-9]+), labelCount=([0-9]+), edgePercent=([0-9.]+), labelPercent=([0-9.]+)''', line)

        if aggMatch:
            nodeCount = int(aggMatch.group(1))
            labelCount = int(aggMatch.group(2))
            edgePercent = float(aggMatch.group(3))
            labelPercent = float(aggMatch.group(4))

            currentAggregation = (nodeCount, labelCount, edgePercent, labelPercent)

            if currentAggregation in data:
                raise Exception(str(currentAggregation) + ' is a duplicate aggregation.')

            data[currentAggregation] = {'seq': [], 'par1': [], 'par2': [], 'par4': [], 'par8': []}

            continue

        beginMatch = re.search('''=== BEGIN RUN [0-9]+ ===''', line)

        if beginMatch:
            currentRunIndex = 0
            continue

        realMatch = re.search('''real.+''', line)
        sysMatch = re.search('''sys.+''', line)

        if realMatch or sysMatch:
            continue

        sysCode = re.search('Exit status: ([0-9]+)', line)

        if sysCode:
            exitCode = int(sysCode.group(1))
            data[currentAggregation][runTypes[currentRunIndex]][-1]['failed'] = exitCode != 0
            currentRunIndex += 1
            continue

        userMatch = re.search('user\s.m([0-9]+),([0-9]+)s', line)

        if userMatch:
            whole = int(userMatch.group(1))
            fract = float("0." + userMatch.group(2))
            actualTime = whole + fract
            data[currentAggregation][runTypes[currentRunIndex]].append({'time': actualTime})
            continue

        endMatch = re.search('''=== END RUN [0-9]+ ===''', line)

        if endMatch:
            currentRunIndex = 0
            continue

        raise Exception('Unrecognized line: ' + line)

print('# Data parsing results')
print()

for aggregation, values in data.items():
    [nodeC, labelC, edgeP, labelP] = aggregation
    print()
    print('## Nodes = ' + str(nodeC) + ', Labels = ' + str(labelC) + ', Edge% = ' + str(edgeP) + ', Label% = ' + str(labelP) )

    print()
    print('### Seq')

    valsT1 = list(map(lambda x: x['time'], values['seq']))
    time1 = sum(valsT1)

    valsF1 = list(map(lambda x: 1 if x['failed'] else 0, values['seq']))
    failed1 = sum(valsF1)

    print('- Average time: ' + str(time1) + '.')
    print('- Failed runs: ' + str(failed1) + '.')

    print()
    print('### Par1')

    valsT2 = list(map(lambda x: x['time'], values['par1']))
    time2 = sum(valsT2)

    valsF2 = list(map(lambda x: 1 if x['failed'] else 0, values['par1']))
    failed2 = sum(valsF2)

    print('- Average time: ' + str(time2) + '.')
    print('- Failed runs: ' + str(failed2) + '.')

    print()
    print('### Par2')

    valsT3 = list(map(lambda x: x['time'], values['par2']))
    time3 = sum(valsT3)

    valsF3 = list(map(lambda x: 1 if x['failed'] else 0, values['par2']))
    failed3 = sum(valsF3)

    print('- Average time: ' + str(time3) + '.')
    print('- Failed runs: ' + str(failed3) + '.')

    print()
    print('### Par4')

    valsT4 = list(map(lambda x: x['time'], values['par4']))
    time4 = sum(valsT4)

    valsF4 = list(map(lambda x: 1 if x['failed'] else 0, values['par4']))
    failed4 = sum(valsF4)

    print('- Average time: ' + str(time4) + '.')
    print('- Failed runs: ' + str(failed4) + '.')

    print()
    print('### Par8')

    valsT8 = list(map(lambda x: x['time'], values['par8']))
    time8 = sum(valsT8)

    valsF8 = list(map(lambda x: 1 if x['failed'] else 0, values['par8']))
    failed8 = sum(valsF8)

    print('- Average time: ' + str(time8) + '.')
    print('- Failed runs: ' + str(failed8) + '.')
