#!/usr/bin/env python

from numpy import *
from pylab import *
from colorsys import *
import re
import sys

import sys

objs = []

varname = sys.argv[1]
rex = re.compile(varname + r" = ([^,]+)")
ret = re.compile(r"time = ([^,]+)")

xs = []
ts = []

for l in sys.stdin:
  m = rex.search(l)
  if m: break

for l in sys.stdin:
  m = ret.search(l)
  if m: 
    ts.append(float(m.group(1)))
    continue

  m = rex.search(l)
  if m: 
    xs.append(float(m.group(1)))

plot(ts,xs,'-')
show()
