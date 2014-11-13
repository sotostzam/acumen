#0 {
  _3D = ("Cylinder", (-1, 0, 0), (0.010000, 1), (1, 0, 0), (1.570800, 0, 0)),
  className = Main,
  mode = "push",
  simulator = #0.0,
  system1 = #0.1,
  system2 = #0.2,
  t = 0,
  t' = 1,
  x01 = 0,
  x02 = 1
}
#0.0 { className = Simulator, time = 0.000000 }
#0.1 {
  F = 0,
  Fs = 0,
  _3D = (("Box", (0, 0, 0), (0.300000, 0.300000, 0.300000), (0, 0, 1), (0, 0, 0)), ("Cylinder", (-0.500000, 0, 0), (0.010000, 1), (0, 1, 0), (0, 0, 1.570800))),
  c = 1,
  className = SpringMass,
  k = 5,
  m = 3,
  x = 0,
  x' = 0,
  x'' = 0,
  x0 = 0
}
#0.2 {
  F = 2,
  Fs = 0,
  _3D = (("Box", (1, 0, 0), (0.100000, 0.100000, 0.100000), (0, 0, 1), (0, 0, 0)), ("Cylinder", (0.500000, 0, 0), (0.010000, 1), (0, 1, 0), (0, 0, 1.570800))),
  c = 1,
  className = SpringMass,
  k = 5,
  m = 1,
  x = 1,
  x' = 0,
  x'' = 0,
  x0 = 1
}
------------------------------0
#0 {
  _3D = ("Cylinder", (-1, 0, 0), (0.010000, 1), (1, 0, 0), (1.570800, 0, 0)),
  className = Main,
  mode = "free_motion",
  simulator = #0.0,
  system1 = #0.1,
  system2 = #0.2,
  t = 1.000000,
  t' = 1,
  x01 = 0,
  x02 = 1
}
#0.0 { className = Simulator, time = 1.000000 }
#0.1 {
  F = 2.504094,
  Fs = 1.017504,
  _3D = (("Box", (-0.131283, 0, 0), (0.300000, 0.300000, 0.300000), (0, 0, 1), (0, 0, 0)), ("Cylinder", (-0.565642, 0, 0), (0.010000, -0.868717), (0, 1, 0), (0, 0, 1.570800))),
  c = 1,
  className = SpringMass,
  k = 5,
  m = 3,
  x = -0.134944,
  x' = -0.366043,
  x'' = -0.495530,
  x0 = 0
}
#0.2 {
  F = 0,
  Fs = 2.510076,
  _3D = (("Box", (0.483010, 0, 0), (0.100000, 0.100000, 0.100000), (0, 0, 1), (0, 0, 0)), ("Cylinder", (0.175863, 0, 0), (0.010000, -0.614293), (0, 1, 0), (0, 0, 1.570800))),
  c = 1,
  className = SpringMass,
  k = 5,
  m = 1,
  x = 0.477245,
  x' = -0.576439,
  x'' = 0.510076,
  x0 = 0.868717
}
------------------------------101
#0 {
  _3D = ("Cylinder", (-1, 0, 0), (0.010000, 1), (1, 0, 0), (1.570800, 0, 0)),
  className = Main,
  mode = "free_motion",
  simulator = #0.0,
  system1 = #0.1,
  system2 = #0.2,
  t = 10.010000,
  t' = 1,
  x01 = 0,
  x02 = 1
}
#0.0 { className = Simulator, time = 10.010000 }
#0.1 {
  F = -0.127484,
  Fs = -0.498536,
  _3D = (("Box", (0.083104, 0, 0), (0.300000, 0.300000, 0.300000), (0, 0, 1), (0, 0, 0)), ("Cylinder", (-0.458448, 0, 0), (0.010000, -1.083104), (0, 1, 0), (0, 0, 1.570800))),
  c = 1,
  className = SpringMass,
  k = 5,
  m = 3,
  x = 0.083922,
  x' = 0.081780,
  x'' = -0.123684,
  x0 = 0
}
#0.2 {
  F = 0,
  Fs = -0.128083,
  _3D = (("Box", (1.084622, 0, 0), (0.100000, 0.100000, 0.100000), (0, 0, 1), (0, 0, 0)), ("Cylinder", (0.583863, 0, 0), (0.010000, -1.001518), (0, 1, 0), (0, 0, 1.570800))),
  c = 1,
  className = SpringMass,
  k = 5,
  m = 1,
  x = 1.085814,
  x' = 0.119212,
  x'' = -0.128083,
  x0 = 1.083104
}
------------------------------1002