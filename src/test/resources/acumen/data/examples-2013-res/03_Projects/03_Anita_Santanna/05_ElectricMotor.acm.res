#0 {
  circuit = #0.2,
  className = Main,
  controller = #0.3,
  mode = 1,
  motor = #0.1,
  simulator = #0.0,
  t = 0,
  t' = 1
}
#0.0 { className = Simulator, time = 0.000000 }
#0.1 {
  Jeq = 0.000200,
  TL = 0,
  className = ElectricMotor,
  e = 0,
  i = 0,
  kE = 0.100000,
  kT = 0.100000,
  wm = 0,
  wm' = 0
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 0,
  i = 0,
  i' = 0,
  v = 0
}
#0.3 {
  className = PIcontroller,
  h = 0,
  i = 0,
  int_h = 0,
  int_h' = 0,
  int_i = 0,
  int_i' = 0,
  kI = 100,
  kP = 50,
  kPWM = 6,
  v = 0
}
------------------------------0
#0 {
  circuit = #0.2,
  className = Main,
  controller = #0.3,
  mode = 2,
  motor = #0.1,
  simulator = #0.0,
  t = 0.100000,
  t' = 1
}
#0.0 { className = Simulator, time = 0.100000 }
#0.1 {
  Jeq = 0.000200,
  TL = 0,
  className = ElectricMotor,
  e = 0.000000,
  i = 0.000000,
  kE = 0.100000,
  kT = 0.100000,
  wm = 0.000000,
  wm' = 0.000000
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 0.000000,
  i = 0.000000,
  i' = 0.000000,
  v = 0.000000
}
#0.3 {
  className = PIcontroller,
  h = 0,
  i = 0.000000,
  int_h = 0.000000,
  int_h' = 0,
  int_i = 0.000000,
  int_i' = 0.000000,
  kI = 100,
  kP = 50,
  kPWM = 6,
  v = 0.000000
}
------------------------------101
#0 {
  circuit = #0.2,
  className = Main,
  controller = #0.3,
  mode = 3,
  motor = #0.1,
  simulator = #0.0,
  t = 0.200000,
  t' = 1
}
#0.0 { className = Simulator, time = 0.200000 }
#0.1 {
  Jeq = 0.000200,
  TL = 0,
  className = ElectricMotor,
  e = 45.879954,
  i = 9.869510,
  kE = 0.100000,
  kT = 0.100000,
  wm = 463.734294,
  wm' = 4934.755129
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 45.386409,
  i = 9.868125,
  i' = -1.385493,
  v = 82.093464
}
#0.3 {
  className = PIcontroller,
  h = 10,
  i = 9.869510,
  int_h = 1.000000,
  int_h' = 10,
  int_i = 0.927469,
  int_i' = 9.869510,
  kI = 100,
  kP = 50,
  kPWM = 6,
  v = 82.587476
}
------------------------------202
#0 {
  circuit = #0.2,
  className = Main,
  controller = #0.3,
  mode = 3,
  motor = #0.1,
  simulator = #0.0,
  t = 1.000000,
  t' = 1
}
#0.0 { className = Simulator, time = 1.000000 }
#0.1 {
  Jeq = 0.000200,
  TL = 0,
  className = ElectricMotor,
  e = 46.782380,
  i = -0.027267,
  kE = 0.100000,
  kT = 0.100000,
  wm = 467.810164,
  wm' = -13.633565
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 46.783746,
  i = -0.027208,
  i' = 0.059145,
  v = 46.792968
}
#0.3 {
  className = PIcontroller,
  h = 0,
  i = -0.027267,
  int_h = 1.000000,
  int_h' = 0,
  int_i = 0.935620,
  int_i' = -0.027267,
  kI = 100,
  kP = 50,
  kPWM = 6,
  v = 46.791582
}
------------------------------1002
