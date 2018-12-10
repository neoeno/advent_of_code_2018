from typing import NamedTuple
from pathlib import Path

class Node(NamedTuple):
    children: list
    metadata: list

def parse(body):
    return list(map(int, body.split(" ")))

def take_metadata(ints, n):
    return ints[n:], ints[:n]

def take_node(ints):
    children_length, metadata_length, *rest = ints
    without_nodes, children = take_nodes(rest, children_length)
    without_metadata, metadata = take_metadata(without_nodes, metadata_length)
    return without_metadata, Node(children, metadata)

def take_nodes(ints, n):
    nodes = []
    for _ in range(0, n):
        ints, node = take_node(ints)
        nodes.append(node)
    return ints, nodes

def convert_ints_to_node(ints):
    return take_nodes(ints, 1)[1][0]

def node_value(node):
    if len(node.children) == 0: return sum(node.metadata)
    return sum(
        node_value(node.children[idx - 1])
        for idx in node.metadata
        if (idx - 1) in range(0, len(node.children))
    )

print(node_value(convert_ints_to_node(parse("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))))
print(node_value(convert_ints_to_node(parse(Path("./8-input.txt").read_text()))))
