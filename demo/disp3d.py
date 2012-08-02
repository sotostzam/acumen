#!/usr/bin/env python

import vtk
import math
import time
import numpy
import sys

objs = []

while True:
  try:
    start = int(sys.stdin.readline())
    xs = map(float, sys.stdin.readline().split())
    ys = map(float, sys.stdin.readline().split())
    zs = map(float, sys.stdin.readline().split())
    objs.append((start,xs,ys,zs))
  except:
    break

def comp((s1,xs1,ys1,zs1),(s2,xs2,ys2,zs2)):
  return s1 - s2

# sort for nice colors
objs.sort(comp)

offset = 0

def trajectory(obj,i):
  (_,xs,yz,zs) = obj
  return (xs[i], ys[i], zs[i])

def ball(obj):
  global offset
  sp = vtk.vtkSphereSource()
  (x,y,z) = trajectory(obj,0)
  sp.SetCenter(x,y,z)
  sp.SetRadius(0.02)
  m=vtk.vtkPolyDataMapper()
  m.SetInput(sp.GetOutput())
  a=vtk.vtkActor()
  a.SetMapper(m)
  def update():
    (x,y,z) = trajectory(obj,offset)
    sp.SetCenter(x,y,z)
  return (update,a)

def trace(obj):
  
  s = vtk.vtkProgrammableSource()
  def execute():
    global offset
    positions = [trajectory(obj,i) for i in range(offset)]
                  
    #construct a VTK polydata object: a single poly-line 'cell'
    dataset = s.GetPolyDataOutput()
    points = vtk.vtkPoints()
    cells = vtk.vtkCellArray()
    cells.InsertNextCell(len(positions))
    for p in positions:
      id = points.InsertNextPoint(*p)
      cells.InsertCellPoint(id)
    dataset.SetPoints(points)
    dataset.SetLines(cells)
    return dataset
  s.SetExecuteMethod(execute)

  m=vtk.vtkPolyDataMapper()
  m.SetInput(s.GetOutput())
  a=vtk.vtkActor()
  a.SetMapper(m)
  def update():
    s.Modified()
  return (update,a)

#boiler-plate VTK stuff....
balls = [ball(o) for o in objs]
traces = [trace(o) for o in objs]


ren = vtk.vtkRenderer()

for (_,a) in balls:
  ren.AddActor(a)
for (_,a) in traces:
  ren.AddActor(a)

renwin = vtk.vtkRenderWindow()
renwin.AddRenderer(ren)

iren = vtk.vtkRenderWindowInteractor()
iren.SetRenderWindow(renwin)

def animate(obj, evt):
  global offset
  if offset >= len(objs[0][1]): sys.exit()
  for (u,_) in balls: u()
  for (u,_) in traces: u()
  renwin.Render()
  offset += 10

iren.Initialize()
iren.AddObserver("TimerEvent", animate)
iren.CreateRepeatingTimer(1);
iren.Start()
