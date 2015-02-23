#0 {
  a = 2,
  a' = 2,
  b = 4,
  b' = 8,
  c = 4,
  className = Main,
  d = 4,
  d' = 8,
  e = 8,
  f = 8,
  simulator = #0.0,
  t = 0,
  t' = 1
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  a = 5.436554,
  a' = 4.797743,
  b = 22.857444,
  b' = 33.280300,
  c = 18.426232,
  className = Main,
  d = 16.000000,
  d' = 19.500000,
  e = 23.000000,
  f = 27.125000,
  simulator = #0.0,
  t = 1.000000,
  t' = 1
}
#0.0 { className = Simulator, time = 1.000000 }
------------------------------8
SOME HYPOTHESES FALSIFIED OVER [0.0..1.0]
0 TRUE, 9 FALSE, 0 INCONCLUSIVE
- (#0:Main) 'b' = d''    Falsified at 0.25, where b' = 9.065185546875, d' = 8
- (#0:Main) 'f = 2 * d'  Falsified at 0.125, where f = 8, d = 5.0
- (#0:Main) 'a' = a'     Falsified at 0.125, where a' = 2, a = 2.26629638671875
- (#0:Main) 'c = b'      Falsified at 0.125, where c = 4, b = 5.065185546875
- (#0:Main) 'b == d'     Falsified at 0.125, where b = 5.065185546875, d = 5.0
- (#0:Main) 'e = f'      Falsified at 0.25, where f = 10.0, e = 8
- (#0:Main) 'a * a == b' Falsified at 0.125, where a = 2.26629638671875, b = 5.065185546875
- (#0:Main) 'b' = b + c' Falsified at 0.125, where b' = 8, b = 5.065185546875, c = 4
- (#0:Main) 'd' = e'     Falsified at 0.375, where d' = 8, e = 10.0

