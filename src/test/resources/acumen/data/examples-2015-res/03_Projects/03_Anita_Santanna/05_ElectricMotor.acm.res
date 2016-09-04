#0 {
  circuit = #0.2,
  className = Main,
  controller = #0.3,
  mode = 1,
  motor = #0.1,
  simulator = #0.0,
  t = 0,
  t' = 1,
  type = Main
}
#0.0 { className = Simulator, time = 0.000000 }
#0.1 {
  Jeq = 0.000200,
  TL = 0,
  className = ElectricMotor,
  e = 0.000000,
  i = 0,
  kE = 0.100000,
  kT = 0.100000,
  type = ElectricMotor,
  wm = 0,
  wm' = 0.000000
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 0.000000,
  i = 0,
  i' = 0.000000,
  type = RLcircuit,
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
  type = PIcontroller,
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
  t' = 1,
  type = Main
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
  type = ElectricMotor,
  wm = 0.000000,
  wm' = 0.000000
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 0.000000,
  i = 0.000000,
  i' = 1500.000000,
  type = RLcircuit,
  v = 3000.000000
}
#0.3 {
  className = PIcontroller,
  h = 10,
  i = 0.000000,
  int_h = 0.000000,
  int_h' = 10,
  int_i = 0.000000,
  int_i' = 0.000000,
  kI = 100,
  kP = 50,
  kPWM = 6,
  type = PIcontroller,
  v = 3000.000000
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
  t' = 1,
  type = Main
}
#0.0 { className = Simulator, time = 0.200000 }
#0.1 {
  Jeq = 0.000200,
  TL = 0,
  className = ElectricMotor,
  e = 46.369039,
  i = 9.868314,
  kE = 0.100000,
  kT = 0.100000,
  type = ElectricMotor,
  wm = 463.690391,
  wm' = 4934.156877
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 46.369039,
  i = 9.868314,
  i' = -1501.382445,
  type = RLcircuit,
  v = -2916.922596
}
#0.3 {
  className = PIcontroller,
  h = 0,
  i = 9.868314,
  int_h = 1.000000,
  int_h' = 0,
  int_i = 0.927381,
  int_i' = 9.868314,
  kI = 100,
  kP = 50,
  kPWM = 6,
  type = PIcontroller,
  v = -2916.922596
}
------------------------------202

------------------------------1002
