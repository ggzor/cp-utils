from sys import argv, stdin
import itertools as it
import operator as op
from collections import Counter, defaultdict

nums = [*map(int, stdin.read().split(","))]

print(dict(list(it.dropwhile(lambda t: t[0] != "defaultdict", locals().items()))[1:]))
