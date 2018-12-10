from collections import defaultdict

class CircularList():
    def __init__(self, items):
        self.items = items

    def get(self, place):
        return self.items[place % len(self.items)]

    def insert(self, place, item):
        place = place % (len(self.items)) + 1
        items = [*self.items[:place], item, *self.items[place:]]
        return CircularList(items)

    def remove(self, place):
        place = place % len(self.items)
        items = [*self.items[:place], *self.items[(place + 1):]]
        return self.items[place], CircularList(items)

    def index(self, value):
        return self.items.index(value)

    def format(self):
        return self.items

class CircularListWithCenter():
    def __init__(self, circular_list, center):
        self.circular_list = circular_list
        self.center = center

    def get(self, place):
        return self.circular_list.get(self.center + place)

    def insert(self, place, item):
        new_list = self.circular_list.insert(self.center + place, item)
        return CircularListWithCenter(
            new_list,
            new_list.index(item)
        )

    def remove(self, place):
        new_center_item = self.circular_list.get(self.center + place + 1)
        removed, new_list = self.circular_list.remove(self.center + place)
        return removed, CircularListWithCenter(
            new_list,
            new_list.index(new_center_item)
        )

    def format(self):
        def format_item(n):
            if n == self.get(0): return f">{n: 3}"
            return f" {n: 3}"
        return " ".join(format_item(item) for item in self.circular_list.format())

def iterate(list, n):
    if n % 23 == 0:
        removed, list = list.remove(-7)
        return n + removed, list
    else:
        return 0, list.insert(1, n)

def play(list, players, rounds):
    scorecard = defaultdict(int)
    for n in range(0, rounds):
        player = (n % players) + 1
        won, list = iterate(list, n + 1)
        scorecard[player] += won
    return scorecard

def highscore(scorecard):
    winner = max(scorecard, key=lambda k: scorecard[k])
    return f"Player {winner} won, score: {scorecard[winner]}"

print(highscore(play(CircularListWithCenter(CircularList([0]), 0), 9, 25)))
print(highscore(play(CircularListWithCenter(CircularList([0]), 0), 413, 71082)))
