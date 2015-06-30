#0 {
  Controller_North = #0.1,
  Controller_West = #0.2,
  _3DView = (),
  className = Main,
  simulator = #0.0,
  t = 0,
  t' = 0
}
#0.0 { className = Simulator, time = 0.000000 }
#0.1 {
  Cont_West = "observe",
  Light_North = #0.1.0,
  Traffic_North = #0.1.1,
  _3D = (),
  a = false,
  arrival = 4,
  b = false,
  check_t = 0,
  className = Controller_north,
  departure = 1,
  f = false,
  g = false,
  green_t = 0,
  green_t' = 0,
  green_time = 5,
  loc = "observe",
  max = 5,
  orange_t = 0,
  orange_t' = 0,
  orange_time = 1,
  prio = 1,
  queue = 0,
  t = 0,
  t' = 1
}
#0.1.0 {
  Cont = "observe",
  a = (1, 0, 0),
  b = (1, 1, 1),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = false,
  gh = false,
  loc = "red"
}
#0.1.1 {
  Cont = "observe",
  arrival = 4,
  arrive_t = 4,
  arrive_t' = 0,
  className = Traffic,
  depart_t = 1,
  depart_t' = 0,
  departure = 1,
  loc = "Red",
  x = 0
}
#0.2 {
  Cont_North = "observe",
  Light_West = #0.2.0,
  Traffic_West = #0.2.1,
  _3D = (),
  arrival = 4,
  c = false,
  check_t = 0,
  className = Controller_west,
  d = false,
  departure = 1,
  e = false,
  green_t = 0,
  green_t' = 0,
  green_time = 5,
  h = false,
  loc = "observe",
  max = 5,
  orange_t = 0,
  orange_t' = 0,
  orange_time = 1,
  prio = 0,
  queue = 0,
  t = 0,
  t' = 0
}
#0.2.0 {
  Cont = "observe",
  a = (1, 0, 0),
  b = (1, 1, 1),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = false,
  gh = false,
  loc = "red"
}
#0.2.1 {
  Cont = "observe",
  arrival = 4,
  arrive_t = 4,
  arrive_t' = 0,
  className = Traffic,
  depart_t = 1,
  depart_t' = 0,
  departure = 1,
  loc = "Red",
  x = 0
}
------------------------------0
#0 {
  Controller_North = #0.1,
  Controller_West = #0.2,
  _3DView = ((-3, -50, 0), (-3, 0, 0)),
  className = Main,
  simulator = #0.0,
  t = 4.010000,
  t' = 1
}
#0.0 { className = Simulator, time = 4.010000 }
#0.1 {
  Cont_West = "observe",
  Light_North = #0.1.0,
  Traffic_North = #0.1.1,
  _3D = (("Cylinder", (0, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Box", (0, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0), "Global", -1), ("Text", (1.500000, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue =", "Global", -1), ("Text", (1.500000, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time =", "Global", -1), ("Text", (1.500000, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time =", "Global", -1), ("Text", (8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 0, "Global", -1), ("Text", (8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -401.000000, "Global", -1), ("Text", (8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -401.000000, "Global", -1), ("Text", (8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe", "Global", -1), ("Text", (8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red", "Global", -1), ("Text", (8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red", "Global", -1)),
  a = false,
  arrival = 4,
  b = false,
  check_t = 0,
  className = Controller_north,
  departure = 1,
  f = false,
  g = false,
  green_t = -4.010000,
  green_t' = -1,
  green_time = 5,
  loc = "observe",
  max = 5,
  orange_t = -4.010000,
  orange_t' = -1,
  orange_time = 1,
  prio = 1,
  queue = 0,
  t = 4.010000,
  t' = 1
}
#0.1.0 {
  Cont = "observe",
  a = (1, 0, 0),
  b = (1, 1, 1),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = false,
  gh = false,
  loc = "red"
}
#0.1.1 {
  Cont = "observe",
  arrival = 4,
  arrive_t = 4,
  arrive_t' = -1,
  className = Traffic,
  depart_t = -3.010000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 1
}
#0.2 {
  Cont_North = "observe",
  Light_West = #0.2.0,
  Traffic_West = #0.2.1,
  _3D = (("Cylinder", (-17, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Box", (-17, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0), "Global", -1), ("Text", (-15, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue =", "Global", -1), ("Text", (-15, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time =", "Global", -1), ("Text", (-15, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time =", "Global", -1), ("Text", (-8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 0, "Global", -1), ("Text", (-8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -401.000000, "Global", -1), ("Text", (-8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -401.000000, "Global", -1), ("Text", (-8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe", "Global", -1), ("Text", (-8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red", "Global", -1), ("Text", (-8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red", "Global", -1)),
  arrival = 4,
  c = false,
  check_t = 0,
  className = Controller_west,
  d = false,
  departure = 1,
  e = false,
  green_t = -4.010000,
  green_t' = -1,
  green_time = 5,
  h = false,
  loc = "observe",
  max = 5,
  orange_t = -4.010000,
  orange_t' = -1,
  orange_time = 1,
  prio = 0,
  queue = 0,
  t = 4.010000,
  t' = 1
}
#0.2.0 {
  Cont = "observe",
  a = (1, 0, 0),
  b = (1, 1, 1),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = false,
  gh = false,
  loc = "red"
}
#0.2.1 {
  Cont = "observe",
  arrival = 4,
  arrive_t = 4,
  arrive_t' = -1,
  className = Traffic,
  depart_t = -3.010000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 1
}
------------------------------402
#0 {
  Controller_North = #0.1,
  Controller_West = #0.2,
  _3DView = ((-3, -50, 0), (-3, 0, 0)),
  className = Main,
  simulator = #0.0,
  t = 8.020000,
  t' = 1
}
#0.0 { className = Simulator, time = 8.020000 }
#0.1 {
  Cont_West = "observe",
  Light_North = #0.1.0,
  Traffic_North = #0.1.1,
  _3D = (("Cylinder", (0, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Box", (0, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0), "Global", -1), ("Text", (1.500000, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue =", "Global", -1), ("Text", (1.500000, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time =", "Global", -1), ("Text", (1.500000, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time =", "Global", -1), ("Text", (8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 1, "Global", -1), ("Text", (8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -802.000000, "Global", -1), ("Text", (8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -802.000000, "Global", -1), ("Text", (8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe", "Global", -1), ("Text", (8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red", "Global", -1), ("Text", (8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red", "Global", -1)),
  a = false,
  arrival = 4,
  b = false,
  check_t = 0,
  className = Controller_north,
  departure = 1,
  f = false,
  g = false,
  green_t = -8.020000,
  green_t' = -1,
  green_time = 5,
  loc = "observe",
  max = 5,
  orange_t = -8.020000,
  orange_t' = -1,
  orange_time = 1,
  prio = 1,
  queue = 1,
  t = 8.020000,
  t' = 1
}
#0.1.0 {
  Cont = "observe",
  a = (1, 0, 0),
  b = (1, 1, 1),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = false,
  gh = false,
  loc = "red"
}
#0.1.1 {
  Cont = "observe",
  arrival = 4,
  arrive_t = 4,
  arrive_t' = -1,
  className = Traffic,
  depart_t = -7.020000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 2
}
#0.2 {
  Cont_North = "observe",
  Light_West = #0.2.0,
  Traffic_West = #0.2.1,
  _3D = (("Cylinder", (-17, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Box", (-17, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0), "Global", -1), ("Text", (-15, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue =", "Global", -1), ("Text", (-15, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time =", "Global", -1), ("Text", (-15, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time =", "Global", -1), ("Text", (-8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 1, "Global", -1), ("Text", (-8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -802.000000, "Global", -1), ("Text", (-8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -802.000000, "Global", -1), ("Text", (-8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe", "Global", -1), ("Text", (-8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red", "Global", -1), ("Text", (-8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red", "Global", -1)),
  arrival = 4,
  c = false,
  check_t = 0,
  className = Controller_west,
  d = false,
  departure = 1,
  e = false,
  green_t = -8.020000,
  green_t' = -1,
  green_time = 5,
  h = false,
  loc = "observe",
  max = 5,
  orange_t = -8.020000,
  orange_t' = -1,
  orange_time = 1,
  prio = 0,
  queue = 1,
  t = 8.020000,
  t' = 1
}
#0.2.0 {
  Cont = "observe",
  a = (1, 0, 0),
  b = (1, 1, 1),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = false,
  gh = false,
  loc = "red"
}
#0.2.1 {
  Cont = "observe",
  arrival = 4,
  arrive_t = 4,
  arrive_t' = -1,
  className = Traffic,
  depart_t = -7.020000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 2
}
------------------------------804
#0 {
  Controller_North = #0.1,
  Controller_West = #0.2,
  _3DView = ((-3, -50, 0), (-3, 0, 0)),
  className = Main,
  simulator = #0.0,
  t = 12.030000,
  t' = 1
}
#0.0 { className = Simulator, time = 12.030000 }
#0.1 {
  Cont_West = "observe",
  Light_North = #0.1.0,
  Traffic_North = #0.1.1,
  _3D = (("Cylinder", (0, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Box", (0, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0), "Global", -1), ("Text", (1.500000, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue =", "Global", -1), ("Text", (1.500000, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time =", "Global", -1), ("Text", (1.500000, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time =", "Global", -1), ("Text", (8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 2, "Global", -1), ("Text", (8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -1203.000000, "Global", -1), ("Text", (8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -1203.000000, "Global", -1), ("Text", (8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe", "Global", -1), ("Text", (8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red", "Global", -1), ("Text", (8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red", "Global", -1)),
  a = false,
  arrival = 4,
  b = false,
  check_t = 0,
  className = Controller_north,
  departure = 1,
  f = false,
  g = false,
  green_t = -12.030000,
  green_t' = -1,
  green_time = 5,
  loc = "observe",
  max = 5,
  orange_t = -12.030000,
  orange_t' = -1,
  orange_time = 1,
  prio = 1,
  queue = 2,
  t = 12.030000,
  t' = 1
}
#0.1.0 {
  Cont = "observe",
  a = (1, 0, 0),
  b = (1, 1, 1),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = false,
  gh = false,
  loc = "red"
}
#0.1.1 {
  Cont = "observe",
  arrival = 4,
  arrive_t = 4,
  arrive_t' = -1,
  className = Traffic,
  depart_t = -11.030000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 3
}
#0.2 {
  Cont_North = "observe",
  Light_West = #0.2.0,
  Traffic_West = #0.2.1,
  _3D = (("Cylinder", (-17, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Box", (-17, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0), "Global", -1), ("Text", (-15, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue =", "Global", -1), ("Text", (-15, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time =", "Global", -1), ("Text", (-15, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time =", "Global", -1), ("Text", (-8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 2, "Global", -1), ("Text", (-8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -1203.000000, "Global", -1), ("Text", (-8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -1203.000000, "Global", -1), ("Text", (-8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe", "Global", -1), ("Text", (-8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red", "Global", -1), ("Text", (-8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red", "Global", -1)),
  arrival = 4,
  c = false,
  check_t = 0,
  className = Controller_west,
  d = false,
  departure = 1,
  e = false,
  green_t = -12.030000,
  green_t' = -1,
  green_time = 5,
  h = false,
  loc = "observe",
  max = 5,
  orange_t = -12.030000,
  orange_t' = -1,
  orange_time = 1,
  prio = 0,
  queue = 2,
  t = 12.030000,
  t' = 1
}
#0.2.0 {
  Cont = "observe",
  a = (1, 0, 0),
  b = (1, 1, 1),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = false,
  gh = false,
  loc = "red"
}
#0.2.1 {
  Cont = "observe",
  arrival = 4,
  arrive_t = 4,
  arrive_t' = -1,
  className = Traffic,
  depart_t = -11.030000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 3
}
------------------------------1206
#0 {
  Controller_North = #0.1,
  Controller_West = #0.2,
  _3DView = ((-3, -50, 0), (-3, 0, 0)),
  className = Main,
  simulator = #0.0,
  t = 100.000000,
  t' = 1
}
#0.0 { className = Simulator, time = 100.000000 }
#0.1 {
  Cont_West = "observe",
  Light_North = #0.1.0,
  Traffic_North = #0.1.1,
  _3D = (("Cylinder", (0, 0, 1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0, 0), (0.450000, 0.100000), (1, 0.500000, 0), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (0, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Box", (0, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0), "Global", -1), ("Text", (1.500000, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue =", "Global", -1), ("Text", (1.500000, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time =", "Global", -1), ("Text", (1.500000, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time =", "Global", -1), ("Text", (8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 0, "Global", -1), ("Text", (8, 0, 0), 1, (1, 1, 1), (0, 0, 0), 37.000000, "Global", -1), ("Text", (8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -165.000000, "Global", -1), ("Text", (8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "stop", "Global", -1), ("Text", (8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red", "Global", -1), ("Text", (8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "orange", "Global", -1)),
  a = false,
  arrival = 4,
  b = false,
  check_t = 0,
  className = Controller_north,
  departure = 1,
  f = false,
  g = false,
  green_t = -1.650000,
  green_t' = -1,
  green_time = 5,
  loc = "stop",
  max = 5,
  orange_t = 0.370000,
  orange_t' = -1,
  orange_time = 1,
  prio = 1,
  queue = 0,
  t = 100.000000,
  t' = 1
}
#0.1.0 {
  Cont = "stop",
  a = (1, 1, 1),
  b = (1, 0.500000, 0),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = false,
  gh = false,
  loc = "orange"
}
#0.1.1 {
  Cont = "stop",
  arrival = 4,
  arrive_t = 3.380000,
  arrive_t' = -1,
  className = Traffic,
  depart_t = 0.360000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 0
}
#0.2 {
  Cont_North = "stop",
  Light_West = #0.2.0,
  Traffic_West = #0.2.1,
  _3D = (("Cylinder", (-17, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Cylinder", (-17, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0), "Global", -1), ("Box", (-17, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0), "Global", -1), ("Text", (-15, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue =", "Global", -1), ("Text", (-15, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time =", "Global", -1), ("Text", (-15, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time =", "Global", -1), ("Text", (-8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 3, "Global", -1), ("Text", (-8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -1369.000000, "Global", -1), ("Text", (-8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -1571.000000, "Global", -1), ("Text", (-8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe", "Global", -1), ("Text", (-8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red", "Global", -1), ("Text", (-8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red", "Global", -1)),
  arrival = 4,
  c = false,
  check_t = 0,
  className = Controller_west,
  d = false,
  departure = 1,
  e = true,
  green_t = -15.710000,
  green_t' = -1,
  green_time = 5,
  h = false,
  loc = "observe",
  max = 5,
  orange_t = -13.690000,
  orange_t' = -1,
  orange_time = 1,
  prio = 0,
  queue = 3,
  t = 100.000000,
  t' = 1
}
#0.2.0 {
  Cont = "observe",
  a = (1, 0, 0),
  b = (1, 1, 1),
  bc = false,
  c = (1, 1, 1),
  className = Light,
  fe = true,
  gh = false,
  loc = "red"
}
#0.2.1 {
  Cont = "observe",
  arrival = 4,
  arrive_t = 1.350000,
  arrive_t' = -1,
  className = Traffic,
  depart_t = -13.700000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 3
}
------------------------------10117
