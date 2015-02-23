#0 {
  a = 1,
  b = 10,
  className = Main,
  simulator = #0.0,
  x = 1,
  x' = 1,
  y = 1,
  y' = 1
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  a = 207375.453777,
  b = 10,
  className = Main,
  simulator = #0.0,
  x = 209418.875434,
  x' = 205342.165641,
  y = 244716.189817,
  y' = 242291.123543
}
#0.0 { className = Simulator, time = 10.010000 }
------------------------------1001
SOME HYPOTHESES FALSIFIED OVER [0.0..10.009999999999831]
0 TRUE, 1 FALSE, 0 INCONCLUSIVE
- (#0:Main) 'continuous assignments do not cause delays' Falsified at 0.01, where x = 1.01, y = 1.1105518379166668

