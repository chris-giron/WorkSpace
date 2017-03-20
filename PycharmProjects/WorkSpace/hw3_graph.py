#!/usr/bin/python3
import json
import networkx as nx
import pydot


filename='my_mdp.json'
with open(filename) as data_file:
    data = json.load(data_file)


G=nx.DiGraph()
for s in data['states']:
    for a in s['actions']:
        for t in a['transitions']:
            if not t['probability']:
                continue
            ecolor='red' if a['id'] else 'green'
            elabel='p={}, r={}'.format(t['probability'], t['reward'])
            G.add_edge(s['id'], t['to'],
                       color=ecolor,
                       label=elabel)

nx.drawing.nx_agraph.write_dot(G, filename.replace('.json', '.dot'))
(g,) = pydot.graph_from_dot_file(filename.replace('.json', '.dot'))
g.write_png(filename.replace('.json', '.png'))
open('my_mdp.png')
