#0 {
  a = 0,
  a' = 1,
  b = 1,
  c = 1,
  className = Main,
  simulator = #0.0
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  a = 10.010000,
  a' = 1,
  b = 10.000000,
  c = 1,
  className = Main,
  simulator = #0.0
}
#0.0 { className = Simulator, time = 10.010000 }
------------------------------1001
SOME HYPOTHESES FALSIFIED OVER [0.0..10.009999999999831]
0 TRUE, 1 FALSE, 0 INCONCLUSIVE
- (#0:Main) 'a is always = c' Falsified at 9.999999999999831, where a = 9.999999999999831, c = 1
                              Falsified momentarily at 0.0, where a = 0, c = 1

