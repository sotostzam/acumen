#0 {
  a = 1,
  b = 1,
  className = Main,
  s = #0.0,
  t = 0,
  t' = 1
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  a = 0.100000,
  b = 0.100000,
  className = Main,
  s = #0.0,
  t = 1.100000,
  t' = 1
}
#0.0 { className = Simulator, time = 1.100000 }
------------------------------12

------------------------------21
SOME HYPOTHESES INCONCLUSIVE OVER [0.0..2.0000000000000004]
0 TRUE, 0 FALSE, 1 INCONCLUSIVE
~ (#0:Main) 'abs encodes a conditional' Tested almost everywhere
                                        Falsified momentarily at 1.0999999999999999, where a = 0.09999999999999987, b = -0.09999999999999987

