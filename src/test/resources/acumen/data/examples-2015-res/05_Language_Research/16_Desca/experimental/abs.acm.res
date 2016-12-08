#0 {
  a = 1,
  b = 1,
  className = Main,
  simulator = #0.0,
  t = 0,
  t' = 1,
  type = Main
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  a = 0.015625,
  b = 0.015625,
  className = Main,
  simulator = #0.0,
  t = 1.015625,
  t' = 1,
  type = Main
}
#0.0 { className = Simulator, time = 1.015625 }
------------------------------66

------------------------------129
SOME HYPOTHESES INCONCLUSIVE OVER [0.0..2.0]
0 TRUE, 0 FALSE, 1 INCONCLUSIVE
~ (#0:Main) 'abs encodes a conditional' Tested almost everywhere
                                        Falsified momentarily at 1.015625, where a = 0.015625, b = -0.015625

