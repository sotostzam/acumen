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
  e = 45.385548,
  i = 9.869554,
  kE = 0.100000,
  kT = 0.100000,
  wm = 458.790950,
  wm' = 4935.471167
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 44.891931,
  i = 9.868168,
  i' = -1.386993,
  v = 81.596635
}
#0.3 {
  className = PIcontroller,
  h = 10,
  i = 9.869554,
  int_h = 0.990000,
  int_h' = 10,
  int_i = 0.917582,
  int_i' = 9.870942,
  kI = 100,
  kP = 50,
  kPWM = 6,
  v = 82.090726
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
  e = 46.783779,
  i = -0.027269,
  kE = 0.100000,
  kT = 0.100000,
  wm = 467.824126,
  wm' = -13.664048
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 46.785148,
  i = -0.027210,
  i' = 0.059209,
  v = 46.794470
}
#0.3 {
  className = PIcontroller,
  h = 0,
  i = -0.027269,
  int_h = 1.000000,
  int_h' = 0,
  int_i = 0.935648,
  int_i' = -0.027328,
  kI = 100,
  kP = 50,
  kPWM = 6,
  v = 46.793081
}
------------------------------1002
