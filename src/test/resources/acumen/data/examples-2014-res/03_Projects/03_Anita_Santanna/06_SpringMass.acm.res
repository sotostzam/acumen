#0 {
  _3D = ("Cylinder", (-1, 0, 0), (0.010000, 1), (1, 0, 0), (1.570800, 0, 0)),
  className = Main,
  mode = "push",
  simulator = #0.0,
  system = #0.1,
  t = 0,
  t' = 1
}
#0.0 { className = Simulator, time = 0.000000 }
#0.1 {
  F = 0,
  Fs = 0,
  _3D = (("Box", (0, 0, 0), (0.300000, 0.300000, 0.300000), (0, 0, 1), (0, 0, 0)), ("Cylinder", (-0.500000, 0, 0), (0.010000, 1), (0, 1, 0), (0, 0, 1.570800))),
  c = 1,
  className = SpringMass,
  k = 10,
  m = 3,
  x = 0,
  x' = 0,
  x'' = 0,
  x0 = 0
}
------------------------------0
#0 {
  _3D = ("Cylinder", (-1, 0, 0), (0.010000, 1), (1, 0, 0), (1.570800, 0, 0)),
  className = Main,
  mode = "free_motion",
  simulator = #0.0,
  system = #0.1,
  t = 0.500000,
  t' = 1
}
#0.0 { className = Simulator, time = 0.500000 }
#0.1 {
  F = 10,
  Fs = 4.775205,
  _3D = (("Box", (-0.345857, 0, 0), (0.300000, 0.300000, 0.300000), (0, 0, 1), (0, 0, 0)), ("Cylinder", (-0.672929, 0, 0), (0.010000, -0.654143), (0, 1, 0), (0, 0, 1.570800))),
  c = 1,
  className = SpringMass,
  k = 10,
  m = 3,
  x = -0.359113,
  x' = -1.334545,
  x'' = -1.791314,
  x0 = 0
}
------------------------------51
#0 {
  _3D = ("Cylinder", (-1, 0, 0), (0.010000, 1), (1, 0, 0), (1.570800, 0, 0)),
  className = Main,
  mode = "free_motion",
  simulator = #0.0,
  system = #0.1,
  t = 10.010000,
  t' = 1
}
#0.0 { className = Simulator, time = 10.010000 }
#0.1 {
  F = 0,
  Fs = -1.744306,
  _3D = (("Box", (0.196108, 0, 0), (0.300000, 0.300000, 0.300000), (0, 0, 1), (0, 0, 0)), ("Cylinder", (-0.401946, 0, 0), (0.010000, -1.196108), (0, 1, 0), (0, 0, 1.570800))),
  c = 1,
  className = SpringMass,
  k = 10,
  m = 3,
  x = 0.193911,
  x' = -0.222680,
  x'' = -0.590560,
  x0 = 0
}
------------------------------1002
