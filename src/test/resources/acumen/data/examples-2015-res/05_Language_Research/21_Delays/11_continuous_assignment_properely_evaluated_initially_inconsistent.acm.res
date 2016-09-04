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
  t' = 1,
  type = Main
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0

------------------------------8
SOME HYPOTHESES INCONCLUSIVE OVER [0.0..1.0]
9 TRUE, 0 FALSE, 7 INCONCLUSIVE
+ (#0:Main) 'b' = d''               Tested
+ (#0:Main) 'f = 2 * d'             Tested
~ (#0:Main) 'at t = 0 : b' = b + c' Tested almost everywhere
                                    Falsified initially, where t = 0, b' = -2, b = 4, c = 0.1
+ (#0:Main) 'a' = a'                Tested
~ (#0:Main) 'at t = 0 : e = f'      Tested almost everywhere
                                    Falsified initially, where t = 0, f = 0.3, e = 0.2
~ (#0:Main) 'at t = 0 : b' = d''    Tested almost everywhere
                                    Falsified initially, where t = 0, b' = -2, d' = -3
~ (#0:Main) 'at t = 0 : f = 2 * d'  Tested almost everywhere
                                    Falsified initially, where t = 0, f = 0.3, d = 4
+ (#0:Main) 'c = b'                 Tested
~ (#0:Main) 'at t = 0 : d' = e'     Tested almost everywhere
                                    Falsified initially, where t = 0, d' = -3, e = 0.2
~ (#0:Main) 'at t = 0 : a' = a'     Tested almost everywhere
                                    Falsified initially, where t = 0, a' = -1, a = 2
+ (#0:Main) 'b == d'                Tested
~ (#0:Main) 'at t = 0 : c = b'      Tested almost everywhere
                                    Falsified initially, where t = 0, c = 0.1, b = 4
+ (#0:Main) 'e = f'                 Tested
+ (#0:Main) 'a * a == b'            Tested
+ (#0:Main) 'b' = b + c'            Tested
+ (#0:Main) 'd' = e'                Tested

