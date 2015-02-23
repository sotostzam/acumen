#0 {
  className = Main,
  simulator = #0.0,
  x1 = 2,
  x1' = 1,
  x2 = 2
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  className = Main,
  simulator = #0.0,
  x1 = 12.010000,
  x1' = 1,
  x2 = 12.000000
}
#0.0 { className = Simulator, time = 10.010000 }
------------------------------1001
SOME HYPOTHESES FALSIFIED OVER [0.0..10.009999999999831]
0 TRUE, 1 FALSE, 0 INCONCLUSIVE
- (#0:Main) 'continuous assignments do not cause delays' Falsified at 0.01, where x1 = 2.01, x2 = 2

