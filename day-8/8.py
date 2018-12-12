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

def sum_metadata(node):
    return (
        sum(sum_metadata(child) for child in node.children)
        + sum(node.metadata)
    )

print(sum_metadata(convert_ints_to_node(parse("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))))
print(sum_metadata(convert_ints_to_node(parse(Path("./8-input.txt").read_text()))))
