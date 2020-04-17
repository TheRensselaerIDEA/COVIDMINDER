import matplotlib.pyplot as plt
import networkx as nx
import pandas as pd

Centers = pd.read_csv('CenterStates.csv', index_col='state')
Borders = pd.read_csv('data/borders/Borders.txt', index_col='ST1ST2')
Abbreviations = pd.read_csv('data/borders/USStatesAbbreviationstxt.txt')[:50]
Abbreviations = Abbreviations[Abbreviations["us state"] != 'Alaska']
Abbreviations = Abbreviations[Abbreviations["us state"] != 'Hawaii']
Abbreviations = dict(Abbreviations.to_dict('split')['data'])
States = [x for x in Abbreviations]
G = nx.Graph()
G.add_nodes_from(States)
for i in range(1, len(States)):
    state1 = States[i]
    Abb1 = Abbreviations[state1]
    for j in range(i):
        state2 = States[j]
        Abb2 = Abbreviations[state2]
        if (Borders.index == Abb1 + "-" + Abb2).any():
            weight = Borders['LENGTH'].loc[Abb1 + "-" + Abb2]
            G.add_edge(state1, state2, weight=weight)
        elif (Borders.index == Abb2 + '-' + Abb1).any():
            weight = Borders['LENGTH'].loc[Abb2 + "-" + Abb1]
            G.add_edge(state1, state2, weight=weight)
pos = dict(
    [(state, (Centers['longitude'].loc[Abbreviations[state]], Centers['latitude'].loc[Abbreviations[state]])) for state
     in G.nodes])
weights = [G[u][v]['weight'] for u, v in G.edges]
edges = nx.draw_networkx_edges(G, pos, edge_cmap=plt.cm.rainbow, edge_color=weights, width=5)
nodes = nx.draw_networkx_nodes(G, pos, node_shape='s', node_color='b', node_size=100)
labels = nx.draw_networkx_labels(G, pos, labels=Abbreviations, font_color='w', font_size=8)
plt.axis('off')
# plt.axis('equal')
plt.colorbar(edges, label='Length of border', orientation='horizontal')
plt.show()
