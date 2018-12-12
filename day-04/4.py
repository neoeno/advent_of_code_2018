from typing import NamedTuple
from enum import Enum
import re
from datetime import datetime
from collections import defaultdict

class EventType(Enum):
    BEGIN = 0
    SLEEP = 1
    WAKE = 2

class Event(NamedTuple):
    time: datetime
    type: EventType

class BeginEvent(NamedTuple):
    time: datetime
    guard: int
    type = EventType.BEGIN

def parse_events(events):
    lines = events.split("\n")
    event_parser = re.compile("\[(.*?)\] (falls|wakes|Guard #(\d+))")
    for line in lines:
        match = event_parser.match(line)
        time = datetime.strptime(match.group(1), '%Y-%m-%d %H:%M')
        if match.group(2) == "falls":
            yield Event(time, EventType.SLEEP)
        elif match.group(2) == "wakes":
            yield Event(time, EventType.WAKE)
        else:
            yield BeginEvent(time, int(match.group(3)))

def days_for_guards(events):
    guards = defaultdict(list)
    for event in events:
        if event.type != EventType.BEGIN: continue
        guards[event.guard].append(event.time.date())
    return guards

def minutes_for_day(sorted_events):
    if len(sorted_events) == 0: return 0;
    if len(sorted_events) == 1:
        return 60 - sorted_events[0].time.minute
    sleep, wake, *rest = sorted_events
    return (wake.time.minute - sleep.time.minute) + minutes_for_day(rest)

def events_for_date(events, date):
    return filter(lambda event: event.time.date() == date, events)

def sort_events(events):
    return sorted(events, key = lambda event: event.time)

def calculate_sleep_for_guards(events):
    guards = defaultdict(int)
    for guard, dates in days_for_guards(events).items():
        for date in dates:
            sorted_events = sort_events(events_for_date(events, date))
            print(list(events_for_date(events, date)))
            guards[guard] += minutes_for_day(sorted_events)
    return guards

def sleepiest_guard(events):
    sleep_for_guards = calculate_sleep_for_guards(events)
    print(sleep_for_guards)
    return max(sleep_for_guards, key=lambda key: sleep_for_guards[key])


print(sleepiest_guard(list(parse_events(
"""[1518-11-01 00:30] falls asleep
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:55] wakes up"""
))))


# print(sleepiest_guard(list(parse_events(
# """[1518-11-01 00:30] falls asleep
# [1518-11-01 23:58] Guard #99 begins shift
# [1518-11-03 00:05] Guard #10 begins shift
# [1518-11-05 00:03] Guard #99 begins shift
# [1518-11-03 00:29] wakes up
# [1518-11-04 00:02] Guard #99 begins shift
# [1518-11-01 00:00] Guard #10 begins shift
# [1518-11-05 00:45] falls asleep
# [1518-11-02 00:50] wakes up
# [1518-11-05 00:55] wakes up
# [1518-11-03 00:24] falls asleep
# [1518-11-01 00:05] falls asleep
# [1518-11-04 00:36] falls asleep
# [1518-11-02 00:40] falls asleep
# [1518-11-04 00:46] wakes up
# [1518-11-01 00:25] wakes up
# [1518-11-01 00:55] wakes up"""
# ))))
