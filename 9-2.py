from collections import defaultdict

class CircularList():
    def __init__(self, items):
        self.items = items
        self.dead = False

    def get(self, place):
        if self.dead: raise "Operating on dead list"
        return self.items[place % len(self.items)]

    def insert(self, place, item):
        if self.dead: raise "Operating on dead list"
        self.kill()

        place = place % (len(self.items)) + 1
        self.items.insert(place, item)
        return place, CircularList(self.items)

    def remove(self, place):
        if self.dead: raise "Operating on dead list"
        self.kill()

        place = place % len(self.items)
        removed = self.items[place]
        del self.items[place]
        return place, removed, CircularList(self.items)

    def format(self):
        if self.dead: raise "Operating on dead list"
        return self.items

    def kill(self):
        self.dead = True

class CircularListWithCenter():
    def __init__(self, circular_list, center):
        self.circular_list = circular_list
        self.center = center

    def get(self, place):
        return self.circular_list.get(self.center + place)

    def insert(self, place, item):
        new_center, new_list = self.circular_list.insert(self.center + place, item)
        return CircularListWithCenter(
            new_list,
            new_center
        )

    def remove(self, place):
        index, removed, new_list = self.circular_list.remove(self.center + place)
        return removed, CircularListWithCenter(
            new_list,
            index
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
        if n % 10000 == 0: print(n / rounds)
        player = (n % players) + 1
        won, list = iterate(list, n + 1)
        scorecard[player] += won
    return scorecard

def highscore(scorecard):
    winner = max(scorecard, key=lambda k: scorecard[k])
    return f"Player {winner} won, score: {scorecard[winner]}"

print(highscore(play(CircularListWithCenter(CircularList([0]), 0), 9, 25)))
print(highscore(play(CircularListWithCenter(CircularList([0]), 0), 413, 7108200)))
## TO BEAT: 4.02s
