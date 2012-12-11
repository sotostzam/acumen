#!/usr/bin/env python

from numpy import *
from pylab import *
from colorsys import *
import re
import sys

import sys

objs = []

while True:
  try:
    start = int(sys.stdin.readline())
    print start
    xs = map(float, sys.stdin.readline().split())
    ys = map(float, sys.stdin.readline().split())
    objs.append((start,xs,ys))
  except:
    break

def comp((s1,xs1,ys1),(s2,xs2,ys2)):
  return s1 - s2

# sort for nice colors
objs.sort(comp)

# animation mode
ion() 

# create the curves and get the bounding box
curves = []
h = 0.5
for o in objs:
  (_,xs,ys) = o
  c, = plot(xs,ys,'-')
  c.set_color(hls_to_rgb(h,0.5,1.0))
  curves.append((o,c))
  h += 0.1

def ltmax((s,xs,_)): return s + len(xs)
tmax = max(map(ltmax,objs))

for i in range(0,tmax,10):
  for (o,c) in curves:
    (start,xs,ys) = o
    if i>start:
      front = i-start
      
      #c.set_data(xs[:front],ys[:front])
      #try to replace the one live above by the two lines below
    
      back = max(0,front-100)
      c.set_data(xs[back:front],ys[back:front])
    
    else:
      c.set_data([],[])
  draw()
