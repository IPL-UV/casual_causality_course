import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt

def get_adjacency_matrix(arcs, edges, nodes):
    ''' Adjacency matrix from edges and arcs'''
    # initaialize the adjacency matrix with zeros in a pandas dataframe with columns and rows as the nodes
    adjacency_matrix = pd.DataFrame(0, index=nodes, columns=nodes)
    # fill the adjacency matrix with ones for the edges
    for edge in edges:
        adjacency_matrix.loc[edge[0], edge[1]] = 1
    # fill the adjacency matrix with ones for the arcs
    for arc in arcs:
        adjacency_matrix.loc[arc[0], arc[1]] = 1
    # fill the adjacency matrix with ones for the arcs
    for arc in arcs:
        adjacency_matrix.loc[arc[1], arc[0]] = 1
    return adjacency_matrix


def draw_cpdag(arcs, edges, nodes):

    # Create a DiGraph
    G = nx.DiGraph()

    # Add nodes
    G.add_nodes_from(nodes)

    # Add directed edges
    G.add_edges_from(edges)

    # Add undirected edges (represent as bidirectional with a special attribute)
    for u, v in arcs:
        G.add_edge(u, v, undirected=True)
        G.add_edge(v, u, undirected=True)

    # Positions for consistent layout
    pos = nx.spring_layout(G)

    # Draw nodes
    nx.draw_networkx_nodes(G, pos, node_size=700)

    # Draw labels
    nx.draw_networkx_labels(G, pos)

    # Draw directed edges (excluding "undirected" ones)
    directed_draw_edges = [
        (u, v) for (u, v) in G.edges() if not G[u][v].get("undirected", False)
    ]
    nx.draw_networkx_edges(
        G, pos,
        edgelist=directed_draw_edges,
        arrows=True,
        arrowstyle='-|>',
        connectionstyle='arc3,rad=0.1',
        edge_color='black',
        arrowsize=50
    )

    # Draw undirected edges (as bidirectional lines with no arrows)
    undirected_draw_edges = [ (u, v) for (u, v) in G.edges() if G[u][v].get("undirected", False) and (u < v)]  # avoid double-drawing]
    nx.draw_networkx_edges(
        G, pos,
        edgelist=undirected_draw_edges,
        arrows=False,
        style='dashed',
        edge_color='gray'
    )

    plt.title("CPDAG with Directed and Undirected Edges")
    plt.axis('off')
    plt.show()
