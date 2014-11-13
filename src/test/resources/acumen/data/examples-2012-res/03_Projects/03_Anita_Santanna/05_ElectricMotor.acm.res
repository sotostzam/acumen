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
  e = 46.372871,
  i = 9.868129,
  kE = 0.100000,
  kT = 0.100000,
  wm = 463.728706,
  wm' = 4934.064394
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 45.879464,
  i = 9.866749,
  i' = -1.379461,
  v = 82.593058
}
#0.3 {
  className = PIcontroller,
  h = 10,
  i = 9.868129,
  int_h = 1.000000,
  int_h' = 10,
  int_i = 0.927457,
  int_i' = 9.868129,
  kI = 100,
  kP = 50,
  kPWM = 6,
  v = 83.086916
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
  e = 46.781901,
  i = -0.027245,
  kE = 0.100000,
  kT = 0.100000,
  wm = 467.819006,
  wm' = -13.622740
}
#0.2 {
  L = 2,
  R = 4,
  className = RLcircuit,
  e = 46.783263,
  i = -0.027187,
  i' = 0.058969,
  v = 46.792218
}
#0.3 {
  className = PIcontroller,
  h = 0,
  i = -0.027245,
  int_h = 1.000000,
  int_h' = 0,
  int_i = 0.935638,
  int_i' = -0.027245,
  kI = 100,
  kP = 50,
  kPWM = 6,
  v = 46.790836
}
------------------------------1002
