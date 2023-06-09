#!/usr/bin/env python

import numpy as np
from numpy import random as random

# we have n locations, m of them are marked.
# we start with all white balls in all locations
# at each stage the balls are shifted cyclically
# after shifting the ones that left special locations change color:
# black-> white, white->black

# we denote white color with +1, black color with -1,
# this way we can simply sum the members to see trends etc.

n = 120
m = 25 # note, m needs to be < n/2
