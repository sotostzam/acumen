#0 {
  className = Main,
  simulator = #0.0,
  x0 = 0,
  x0' = 1,
  x1 = 0,
  x10 = 0,
  x2 = 0,
  x3 = 0,
  x4 = 0,
  x5 = 0,
  x6 = 0,
  x7 = 0,
  x8 = 0,
  x9 = 0
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  className = Main,
  simulator = #0.0,
  x0 = 10.010000,
  x0' = 1,
  x1 = 10.000000,
  x10 = 9.910000,
  x2 = 9.990000,
  x3 = 9.980000,
  x4 = 9.970000,
  x5 = 9.960000,
  x6 = 9.950000,
  x7 = 9.940000,
  x8 = 9.930000,
  x9 = 9.920000
}
#0.0 { className = Simulator, time = 10.010000 }
------------------------------1001
SOME HYPOTHESES FALSIFIED OVER [0.0..10.009999999999831]
0 TRUE, 1 FALSE, 0 INCONCLUSIVE
- (#0:Main) 'continuous assignments do not cause delays' Tested false at 0.01, where x10 = 0, x0 = 0.01

