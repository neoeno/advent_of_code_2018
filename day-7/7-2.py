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
    available_steps = []
    completed_steps = []
    working_steps = [(ord(step.name) - 64 + 60 - 1, step) for step in steps]
    i = -1
    while (len(available_steps) + len(working_steps)) > 0:
        i += 1
        working_steps = [(seconds - 1, step) for (seconds, step) in working_steps]
        newly_completed_steps = [step for (seconds, step) in working_steps if seconds == 0]
        working_steps = [(seconds, step) for (seconds, step) in working_steps if seconds > 0]

        print(format_order(newly_completed_steps))
        completed_steps.extend(newly_completed_steps)
        for newly_completed_step in newly_completed_steps:
            available_steps = [
                *[step for step in available_steps if step is not newly_completed_step],
                *[step for step in newly_completed_step.enables if step.required_complete(completed_steps)]
            ]

        while len(available_steps) > 0 and len(working_steps) < 5:
            next_step = min(available_steps, key=lambda step: ord(step.name))
            available_steps = [step for step in available_steps if step is not next_step]
            working_steps.append((ord(next_step.name) - 64 + 60, next_step))

    print(i)
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

RESTRICTIONS = """Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."""

print(format_order(calculate_order(convert_restrictions_to_steps(parse(RESTRICTIONS)))))
