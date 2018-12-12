from typing import NamedTuple
from collections import defaultdict
import re

class Coord(NamedTuple):
    x: int
    y: int

    def distance(self, other):
        return abs(self.x - other.x) + abs(self.y - other.y)

class Boundary(NamedTuple):
    north: int
    east: int
    south: int
    west: int

    def dilate(self, n):
        return Boundary(self.north + n, self.east + n, self.south + 1, self.west + 1)

    def coords(self):
        for x in range(self.west, self.east):
            for y in range(self.north, self.south):
                yield Coord(x, y)

def closest_to(coord, locations):
    champion = None
    best_score = float('inf')
    for contender in locations:
        contender_score = coord.distance(contender)
        if contender_score < best_score:
            best_score = contender_score
            champion = contender
        elif contender_score == best_score:
            champion = None
    return champion

def calculate_areas(coords, locations):
    location_owns = defaultdict(int)
    for coord in coords:
        nearest_location = closest_to(coord, locations)
        if nearest_location is None: continue
        location_owns[nearest_location] += 1
    return location_owns

def find_stable_areas(areas_1, areas_2):
    stable = dict()
    for area, count in areas_1.items():
        if areas_2[area] == count:
            stable[area] = count
    return stable

def calculate_stable_areas(bounds, locations):
    first_coords = bounds.coords()
    first_areas = calculate_areas(first_coords, locations)
    second_coords = bounds.dilate(1).coords()
    second_areas = calculate_areas(second_coords, locations)
    stable_areas = find_stable_areas(first_areas, second_areas)
    return stable_areas

def find_largest_stable_area(bounds, locations):
    return max(calculate_stable_areas(bounds, locations).items(), key=lambda area: area[1])

def is_within_distance_of_all_points(coord, distance, locations):
    print(coord)
    cumulative = 0
    for location in locations:
        cumulative += coord.distance(location)
        if cumulative >= distance: return False
    return True

def find_size_of_area_within_total_distance(bounds, locations, distance):
    return sum(1 for coord in bounds.coords() if is_within_distance_of_all_points(coord, distance, locations))

def parse(body):
    lines = body.split("\n")
    parser = re.compile("(\d+), (\d+)")
    for line in lines:
        match = parser.match(line)
        yield Coord(
            int(match.group(1)),
            int(match.group(2)))

# LOCATIONS = """1, 1
# 1, 6
# 8, 3
# 3, 4
# 5, 5
# 8, 9"""

LOCATIONS = """158, 163
287, 68
76, 102
84, 244
162, 55
272, 335
345, 358
210, 211
343, 206
219, 323
260, 238
83, 94
137, 340
244, 172
335, 307
52, 135
312, 109
276, 93
288, 274
173, 211
125, 236
200, 217
339, 56
286, 134
310, 192
169, 192
313, 106
331, 186
40, 236
194, 122
244, 76
159, 282
161, 176
262, 279
184, 93
337, 284
346, 342
283, 90
279, 162
112, 244
49, 254
63, 176
268, 145
334, 336
278, 176
353, 135
282, 312
96, 85
90, 105
354, 312"""

print(find_size_of_area_within_total_distance(Boundary(-10, 500, 500, -10), list(parse(LOCATIONS)), 10000))
