from typing import NamedTuple
import re

class Restriction(NamedTuple):
    first: str
    then: str

class Step(NamedTuple):
    name: str
    requires: list
    enables: list

    def required_complete(self, completed):
        return all(required in completed for required in self.requires)

def convert_restrictions_to_steps(restrictions):
    steps = dict()
    for restriction in restrictions:
        if restriction.then not in steps:
            steps[restriction.then] = Step(restriction.then, list(), list())
        if restriction.first not in steps:
            steps[restriction.first] = Step(restriction.first, list(), list())
        steps[restriction.first].enables.append(steps[restriction.then])
        steps[restriction.then].requires.append(steps[restriction.first])
    return [step for step in steps.values() if len(step.requires) == 0]

def calculate_order(steps):
    available_steps = [*steps]
    completed_steps = []
    while len(available_steps) > 0:
        next_step = min(available_steps, key=lambda step: ord(step.name))
        completed_steps.append(next_step)
        available_steps = [
            *[step for step in available_steps if step is not next_step],
            *[step for step in next_step.enables if step.required_complete(completed_steps)]
        ]
    return completed_steps

def format_order(step_generator):
    return "".join(list(map(lambda i: i.name, step_generator)))

def parse(body):
    lines = body.split("\n")
    parser = re.compile("Step (\w+) must be finished before step (\w+) can begin\.")
    for line in lines:
        match = parser.match(line)
        yield Restriction(
            match.group(1),
            match.group(2))

RESTRICTIONS = """Step Z must be finished before step V can begin.
Step V must be finished before step K can begin.
Step M must be finished before step Q can begin.
Step E must be finished before step X can begin.
Step J must be finished before step W can begin.
Step L must be finished before step O can begin.
Step Q must be finished before step T can begin.
Step Y must be finished before step P can begin.
Step X must be finished before step R can begin.
Step T must be finished before step U can begin.
Step I must be finished before step O can begin.
Step P must be finished before step H can begin.
Step G must be finished before step A can begin.
Step N must be finished before step A can begin.
Step H must be finished before step B can begin.
Step F must be finished before step D can begin.
Step S must be finished before step O can begin.
Step O must be finished before step W can begin.
Step D must be finished before step U can begin.
Step W must be finished before step B can begin.
Step A must be finished before step K can begin.
Step B must be finished before step R can begin.
Step K must be finished before step C can begin.
Step R must be finished before step C can begin.
Step U must be finished before step C can begin.
Step A must be finished before step U can begin.
Step J must be finished before step I can begin.
Step D must be finished before step K can begin.
Step V must be finished before step S can begin.
Step H must be finished before step C can begin.
Step R must be finished before step U can begin.
Step I must be finished before step G can begin.
Step D must be finished before step R can begin.
Step M must be finished before step B can begin.
Step G must be finished before step R can begin.
Step M must be finished before step I can begin.
Step G must be finished before step N can begin.
Step M must be finished before step N can begin.
Step Q must be finished before step S can begin.
Step I must be finished before step S can begin.
Step J must be finished before step R can begin.
Step O must be finished before step B can begin.
Step G must be finished before step S can begin.
Step J must be finished before step C can begin.
Step M must be finished before step D can begin.
Step T must be finished before step H can begin.
Step P must be finished before step N can begin.
Step S must be finished before step K can begin.
Step T must be finished before step C can begin.
Step J must be finished before step A can begin.
Step G must be finished before step F can begin.
Step N must be finished before step R can begin.
Step N must be finished before step W can begin.
Step T must be finished before step I can begin.
Step S must be finished before step B can begin.
Step H must be finished before step F can begin.
Step B must be finished before step C can begin.
Step L must be finished before step W can begin.
Step N must be finished before step O can begin.
Step O must be finished before step A can begin.
Step H must be finished before step S can begin.
Step F must be finished before step A can begin.
Step F must be finished before step C can begin.
Step M must be finished before step A can begin.
Step Z must be finished before step H can begin.
Step Z must be finished before step L can begin.
Step E must be finished before step H can begin.
Step X must be finished before step T can begin.
Step Y must be finished before step X can begin.
Step E must be finished before step W can begin.
Step P must be finished before step R can begin.
Step Z must be finished before step E can begin.
Step W must be finished before step C can begin.
Step I must be finished before step P can begin.
Step X must be finished before step A can begin.
Step Y must be finished before step C can begin.
Step I must be finished before step F can begin.
Step L must be finished before step T can begin.
Step A must be finished before step B can begin.
Step F must be finished before step W can begin.
Step T must be finished before step R can begin.
Step X must be finished before step F can begin.
Step M must be finished before step O can begin.
Step N must be finished before step K can begin.
Step T must be finished before step S can begin.
Step J must be finished before step N can begin.
Step J must be finished before step S can begin.
Step O must be finished before step D can begin.
Step T must be finished before step P can begin.
Step Z must be finished before step D can begin.
Step L must be finished before step X can begin.
Step Q must be finished before step G can begin.
Step M must be finished before step G can begin.
Step P must be finished before step W can begin.
Step V must be finished before step P can begin.
Step D must be finished before step B can begin.
Step Y must be finished before step D can begin.
Step X must be finished before step S can begin.
Step K must be finished before step U can begin.
Step Z must be finished before step Y can begin.
Step D must be finished before step W can begin."""

print(format_order(calculate_order(convert_restrictions_to_steps(parse(RESTRICTIONS)))))
