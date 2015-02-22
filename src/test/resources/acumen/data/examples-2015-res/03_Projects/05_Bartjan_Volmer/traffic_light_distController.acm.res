#0 {
  Controller_North = #0.1,
  Controller_West = #0.2,
  _3DView = ((-3, -50, 0), (-3, 0, 0)),
  className = Main,
  simulator = #0.0,
  t = 0,
  t' = 1
}
#0.0 { className = Simulator, time = 0.000000 }
#0.1 {
  Cont_West = "observe",
  Light_North = #0.1.0,
  Traffic_North = #0.1.1,
  _3D = (("Cylinder", (0, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0)), ("Cylinder", (0, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Box", (0, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0)), ("Text", (1.500000, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue ="), ("Text", (1.500000, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time ="), ("Text", (1.500000, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time ="), ("Text", (8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 0), ("Text", (8, 0, 0), 1, (1, 1, 1), (0, 0, 0), 0), ("Text", (8, 0, -1), 1, (1, 1, 1), (0, 0, 0), 0), ("Text", (8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe"), ("Text", (8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red"), ("Text", (8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red")),
  a = false,
  arrival = 4,
  b = false,
  check_t = 0,
  className = Controller_north,
  departure = 1,
  f = false,
  g = false,
  green_t = 0,
  green_t' = -1,
  green_time = 5,
  loc = "observe",
  max = 5,
  orange_t = 0,
  orange_t' = -1,
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
  arrive_t' = -1,
  className = Traffic,
  depart_t = 1,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 0
}
#0.2 {
  Cont_North = "observe",
  Light_West = #0.2.0,
  Traffic_West = #0.2.1,
  _3D = (("Cylinder", (-17, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0)), ("Cylinder", (-17, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Box", (-17, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0)), ("Text", (-15, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue ="), ("Text", (-15, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time ="), ("Text", (-15, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time ="), ("Text", (-8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 0), ("Text", (-8, 0, 0), 1, (1, 1, 1), (0, 0, 0), 0), ("Text", (-8, 0, -1), 1, (1, 1, 1), (0, 0, 0), 0), ("Text", (-8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe"), ("Text", (-8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red"), ("Text", (-8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red")),
  arrival = 4,
  c = false,
  check_t = 0,
  className = Controller_west,
  d = false,
  departure = 1,
  e = false,
  green_t = 0,
  green_t' = -1,
  green_time = 5,
  h = false,
  loc = "observe",
  max = 5,
  orange_t = 0,
  orange_t' = -1,
  orange_time = 1,
  prio = 0,
  queue = 0,
  t = 0,
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
  depart_t = 1,
  depart_t' = -1,
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
  t = 4.000000,
  t' = 1
}
#0.0 { className = Simulator, time = 4.000000 }
#0.1 {
  Cont_West = "observe",
  Light_North = #0.1.0,
  Traffic_North = #0.1.1,
  _3D = (("Cylinder", (0, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0)), ("Cylinder", (0, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Box", (0, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0)), ("Text", (1.500000, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue ="), ("Text", (1.500000, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time ="), ("Text", (1.500000, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time ="), ("Text", (8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 1), ("Text", (8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -400.000000), ("Text", (8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -400.000000), ("Text", (8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe"), ("Text", (8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red"), ("Text", (8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red")),
  a = false,
  arrival = 4,
  b = false,
  check_t = 0,
  className = Controller_north,
  departure = 1,
  f = false,
  g = false,
  green_t = -4.000000,
  green_t' = -1,
  green_time = 5,
  loc = "observe",
  max = 5,
  orange_t = -4.000000,
  orange_t' = -1,
  orange_time = 1,
  prio = 1,
  queue = 1,
  t = 4.000000,
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
  depart_t = -3.000000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 1
}
#0.2 {
  Cont_North = "observe",
  Light_West = #0.2.0,
  Traffic_West = #0.2.1,
  _3D = (("Cylinder", (-17, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0)), ("Cylinder", (-17, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Box", (-17, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0)), ("Text", (-15, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue ="), ("Text", (-15, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time ="), ("Text", (-15, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time ="), ("Text", (-8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 1), ("Text", (-8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -400.000000), ("Text", (-8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -400.000000), ("Text", (-8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe"), ("Text", (-8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red"), ("Text", (-8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red")),
  arrival = 4,
  c = false,
  check_t = 0,
  className = Controller_west,
  d = false,
  departure = 1,
  e = false,
  green_t = -4.000000,
  green_t' = -1,
  green_time = 5,
  h = false,
  loc = "observe",
  max = 5,
  orange_t = -4.000000,
  orange_t' = -1,
  orange_time = 1,
  prio = 0,
  queue = 1,
  t = 4.000000,
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
  depart_t = -3.000000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 1
}
------------------------------257
#0 {
  Controller_North = #0.1,
  Controller_West = #0.2,
  _3DView = ((-3, -50, 0), (-3, 0, 0)),
  className = Main,
  simulator = #0.0,
  t = 8.000000,
  t' = 1
}
#0.0 { className = Simulator, time = 8.000000 }
#0.1 {
  Cont_West = "observe",
  Light_North = #0.1.0,
  Traffic_North = #0.1.1,
  _3D = (("Cylinder", (0, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0)), ("Cylinder", (0, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Box", (0, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0)), ("Text", (1.500000, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue ="), ("Text", (1.500000, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time ="), ("Text", (1.500000, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time ="), ("Text", (8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 2), ("Text", (8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -800.000000), ("Text", (8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -800.000000), ("Text", (8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe"), ("Text", (8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red"), ("Text", (8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red")),
  a = false,
  arrival = 4,
  b = false,
  check_t = 0,
  className = Controller_north,
  departure = 1,
  f = false,
  g = false,
  green_t = -8.000000,
  green_t' = -1,
  green_time = 5,
  loc = "observe",
  max = 5,
  orange_t = -8.000000,
  orange_t' = -1,
  orange_time = 1,
  prio = 1,
  queue = 2,
  t = 8.000000,
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
  depart_t = -7.000000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 2
}
#0.2 {
  Cont_North = "observe",
  Light_West = #0.2.0,
  Traffic_West = #0.2.1,
  _3D = (("Cylinder", (-17, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0)), ("Cylinder", (-17, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Box", (-17, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0)), ("Text", (-15, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue ="), ("Text", (-15, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time ="), ("Text", (-15, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time ="), ("Text", (-8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 2), ("Text", (-8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -800.000000), ("Text", (-8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -800.000000), ("Text", (-8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe"), ("Text", (-8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red"), ("Text", (-8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red")),
  arrival = 4,
  c = false,
  check_t = 0,
  className = Controller_west,
  d = false,
  departure = 1,
  e = false,
  green_t = -8.000000,
  green_t' = -1,
  green_time = 5,
  h = false,
  loc = "observe",
  max = 5,
  orange_t = -8.000000,
  orange_t' = -1,
  orange_time = 1,
  prio = 0,
  queue = 2,
  t = 8.000000,
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
  depart_t = -7.000000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 2
}
------------------------------514
#0 {
  Controller_North = #0.1,
  Controller_West = #0.2,
  _3DView = ((-3, -50, 0), (-3, 0, 0)),
  className = Main,
  simulator = #0.0,
  t = 12.000000,
  t' = 1
}
#0.0 { className = Simulator, time = 12.000000 }
#0.1 {
  Cont_West = "observe",
  Light_North = #0.1.0,
  Traffic_North = #0.1.1,
  _3D = (("Cylinder", (0, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0)), ("Cylinder", (0, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (0, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Box", (0, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0)), ("Text", (1.500000, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue ="), ("Text", (1.500000, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time ="), ("Text", (1.500000, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time ="), ("Text", (8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 3), ("Text", (8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -1200.000000), ("Text", (8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -1200.000000), ("Text", (8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe"), ("Text", (8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red"), ("Text", (8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red")),
  a = false,
  arrival = 4,
  b = false,
  check_t = 0,
  className = Controller_north,
  departure = 1,
  f = false,
  g = false,
  green_t = -12.000000,
  green_t' = -1,
  green_time = 5,
  loc = "observe",
  max = 5,
  orange_t = -12.000000,
  orange_t' = -1,
  orange_time = 1,
  prio = 1,
  queue = 3,
  t = 12.000000,
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
  depart_t = -11.000000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 3
}
#0.2 {
  Cont_North = "observe",
  Light_West = #0.2.0,
  Traffic_West = #0.2.1,
  _3D = (("Cylinder", (-17, 0, 1), (0.450000, 0.100000), (1, 0, 0), (0, 0, 0)), ("Cylinder", (-17, 0, 0), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0, -1), (0.450000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0.010000, -1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Cylinder", (-17, 0.010000, 1.500000), (0.650000, 0.100000), (1, 1, 1), (0, 0, 0)), ("Box", (-17, 0.010000, 0), (1.300000, 0.100000, 3), (1, 1, 1), (0, 0, 0)), ("Text", (-15, 0, 1), 1, (1, 1, 1), (0, 0, 0), "queue ="), ("Text", (-15, 0, 0), 1, (1, 1, 1), (0, 0, 0), "orange time ="), ("Text", (-15, 0, -1), 1, (1, 1, 1), (0, 0, 0), "green time ="), ("Text", (-8, 0, 1), 1, (1, 1, 1), (0, 0, 0), 3), ("Text", (-8, 0, 0), 1, (1, 1, 1), (0, 0, 0), -1200.000000), ("Text", (-8, 0, -1), 1, (1, 1, 1), (0, 0, 0), -1200.000000), ("Text", (-8, 0, 6), 1, (1, 1, 1), (0, 0, 0), "observe"), ("Text", (-8, 0, 4), 1, (1, 1, 1), (0, 0, 0), "Red"), ("Text", (-8, 0, 2), 1, (1, 1, 1), (0, 0, 0), "red")),
  arrival = 4,
  c = false,
  check_t = 0,
  className = Controller_west,
  d = false,
  departure = 1,
  e = false,
  green_t = -12.000000,
  green_t' = -1,
  green_time = 5,
  h = false,
  loc = "observe",
  max = 5,
  orange_t = -12.000000,
  orange_t' = -1,
  orange_time = 1,
  prio = 0,
  queue = 3,
  t = 12.000000,
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
  depart_t = -11.000000,
  depart_t' = -1,
  departure = 1,
  loc = "Red",
  x = 3
}
------------------------------771

------------------------------6475
