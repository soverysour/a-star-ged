import sys
import itertools
import random
import json

nodeCount = None
edgePercent = None
labelCount = None
labelPercent = None

for i in range(1, len(sys.argv), 2):
    label, value = sys.argv[i], sys.argv[i + 1]

    if label == '--node':
        nodeCount = int(value)
    elif label == '--label':
        labelCount = int(value)
    elif label == '--edge-p':
        edgePercent = float(value)
    elif label == '--label-p':
        labelPercent = float(value)
    else:
        raise Exception('Bad flag: ' + label)

if nodeCount == None or edgePercent == None or labelCount == None or labelPercent == None:
    raise Exception('Forgot to give some flags.')

edgesRequired = int((nodeCount * (nodeCount - 1) // 2) * edgePercent)
labelPerEntity = int(labelCount * labelPercent)

edges = {}
labels = list(map(str, range(labelCount)))
nodes = list(range(1, nodeCount + 1))

fileContent = {'nodeProps': [], 'edgeProps': []}

# Add random labels per node.
for i in range(nodeCount):
    fileContent['nodeProps'].append({"nodeLabels": random.sample(labels, labelPerEntity)})

totalEdges = itertools.product(nodes, nodes)
withNoDuplicateEdges = list(filter(lambda x: x[0] < x[1], totalEdges))

selectedEdges = random.sample(withNoDuplicateEdges, edgesRequired)

# Add random edges + labels per edges.
for edge in selectedEdges:
    [nodeFrom, nodeTo] = edge
    associatedLabels = random.sample(labels, labelPerEntity)
    fileContent['edgeProps'].append({'from': nodeFrom, 'to': nodeTo, 'edgeLabels': associatedLabels})

print(json.dumps(fileContent))

