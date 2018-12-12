from typing import NamedTuple
from pathlib import Path

class Char(NamedTuple):
    char: str
    alive: bool

    def kill(self):
        return Char(self.char, False)

class Polymer(NamedTuple):
    chars: list

    def reduce_pairs(self, fn):
        return self.reduce_pairs_with_chars(fn, [*self.chars])

    def reduce_pairs_without(self, fn, ignoring):
        chars = [char for char in self.chars if not insensitive_eq(char.char, ignoring)]
        return self.reduce_pairs_with_chars(fn, chars)

    def reduce_pairs_with_chars(self, fn, chars):
        idx = 0
        while True:
            next_char_idx = self.char_alive_after(chars, idx)
            # print("COMPARING", idx, chars[idx], next_char_idx, chars[next_char_idx])
            should_eliminate = fn(chars[idx].char, chars[next_char_idx].char)
            if should_eliminate:
                # print("KILLING", idx, chars[idx], next_char_idx, chars[next_char_idx])
                chars[idx] = chars[idx].kill()
                chars[next_char_idx] = chars[next_char_idx].kill()
                # chars = [
                #     *chars[:idx],
                #     chars[idx].kill(),
                #     *chars[idx + 1:next_char_idx],
                #     chars[next_char_idx].kill(),
                #     *chars[next_char_idx + 1:]
                # ]
                old_idx = idx
                idx = self.char_alive_before(chars, idx)
                if idx is None: idx = self.char_alive_after(chars, old_idx)
            else:
                idx = self.char_alive_after(chars, idx)

            if idx is None or self.char_alive_after(chars, idx) is None: break
        return len(self.format(chars))

    def char_alive_after(self, chars, i):
        for idx in range(i + 1, len(chars)):
            if chars[idx].alive: return idx
        return None

    def char_alive_before(self, chars, i):
        for idx in range(i, -1, -1):
            if chars[idx].alive: return idx
        return None

    def format(self, chars):
        return "".join([char.char for char in chars if char.alive])

def does_react(a, b):
    return abs(ord(a) - ord(b)) == 32

def insensitive_eq(a, b):
    if a == b: return True
    return abs(ord(a) - ord(b)) == 32

def parse(string):
    return Polymer(
        list(map(
            lambda char: Char(char, True),
            list(string)
        ))
    )

def condense(polymer):
    return polymer.reduce_pairs(does_react)

def find_shortest_with_removals(polymer):
    letters = [chr(n + 97) for n in range(0, 26)]
    best_thus_far = float('inf')
    for letter in letters:
        contender = polymer.reduce_pairs_without(does_react, letter)
        print("without", letter, ":", contender)
        if contender < best_thus_far: best_thus_far = contender
    return best_thus_far

# print(condense(parse("dabAcCaCBAcCcaDA")))
print(find_shortest_with_removals(parse(Path("./5-input.txt").read_text())))

# 9686
# 13200
