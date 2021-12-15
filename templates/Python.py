from sys import argv, stdin
import itertools as it
import operator as op
from collections import Counter, defaultdict, deque
from queue import PriorityQueue
from pprint import PrettyPrinter

pprint = PrettyPrinter(indent=2, compact=True, sort_dicts=False).pprint

text = stdin.read()
lines = text.splitlines()

xs = [[int(v) for v in l] for l in lines]
h = len(xs)
w = len(xs[0])
# print("\n".join("".join(map(str, r)) for r in xs))

xs = {(y, x): int(v) for y, l in enumerate(lines) for x, v in enumerate(l)}
w = max(y for y, _ in xs.keys()) + 1
h = max(x for _, x in xs.keys()) + 1
# print("\n".join("".join(str(xs[(y, x)]) for x in range(w)) for y in range(h)))
