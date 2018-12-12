from typing import NamedTuple
import re
from pathlib import Path

class Coord(NamedTuple):
    x: int
    y: int

    def distance(self, other):
        return abs(self.x - other.x) + abs(self.y - other.y)

    def add(self, vector):
        return Coord(self.x + vector.x, self.y + vector.y)

class Vector(NamedTuple):
    x: int
    y: int

class Point(NamedTuple):
    position: Coord
    velocity: Vector

    def tick(self):
        return Point(self.position.add(self.velocity), self.velocity)

class Boundary(NamedTuple):
    north: int
    east: int
    south: int
    west: int

    def coords(self):
        for y in range(self.north, self.south + 1):
            for x in range(self.west, self.east + 1):
                yield Coord(x, y)

    def size(self):
        return (self.south - self.north) * (self.east - self.west)

def parse(body):
    lines = body.split("\n")
    parser = re.compile("position=<\s*([0-9\-]+),\s*([0-9\-]+)> velocity=<\s*([0-9\-]+),\s*([0-9\-]+)>")
    for line in lines:
        match = parser.match(line)
        yield Point(
            Coord(int(match.group(1)), int(match.group(2))),
            Vector(int(match.group(3)), int(match.group(4)))
        )

def tick(points):
    return [point.tick() for point in points]

def find_boundary_around_points(points):
    return Boundary(
        min([point.position.y for point in points]),
        max([point.position.x for point in points]),
        max([point.position.y for point in points]),
        min([point.position.x for point in points])
    )

def format_screen(points):
    def fn():
        occupied_points = [point.position for point in points]
        limits = find_boundary_around_points(points)
        current_y = 0
        for coord in limits.coords():
            if coord.y != current_y:
                yield "\n"
                current_y = coord.y
            if coord in occupied_points:
                yield "#"
            else:
                yield "."
    return list(fn())

def boundary_size_timeline(points_, max):
    def fn():
        points = points_
        for n in range(0, max):
            points = tick(points)
            size = find_boundary_around_points(points).size()
            yield size
    return list(fn())

def find_minimum_boundaries_iteration(points, max_iterations):
    timeline = boundary_size_timeline(points, max_iterations)
    return timeline.index(min(timeline)) + 1

def iterate_to(points, iteration):
    for n in range(0, iteration):
        points = tick(points)
    return points

BODY = Path("./10-input.txt").read_text()

def main():
    points = list(parse(BODY))
    promising_iteration = find_minimum_boundaries_iteration(points, 10020)
    print(promising_iteration)
    picture = iterate_to(points, promising_iteration)
    print("".join(format_screen(picture)))

main()
