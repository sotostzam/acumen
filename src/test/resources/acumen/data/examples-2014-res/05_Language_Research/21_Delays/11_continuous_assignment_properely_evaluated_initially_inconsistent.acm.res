#0 {
  a = 2,
  a' = -1,
  b = 4,
  b' = -2,
  c = 0.100000,
  className = Main,
  d = 4,
  d' = -3,
  e = 0.200000,
  f = 0.300000,
  simulator = #0.0,
  t = 0,
  t' = 1
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  a = 5.436554,
  a' = 4.797743,
  b = 20.697228,
  b' = 30.135040,
  c = 16.684802,
  className = Main,
  d = 11.638281,
  d' = 14.168750,
  e = 16.700000,
  f = 19.734375,
  simulator = #0.0,
  t = 1.000000,
  t' = 1
}
#0.0 { className = Simulator, time = 1.000000 }
------------------------------8
SOME HYPOTHESES FALSIFIED OVER [0.0..1.0]
0 TRUE, 9 FALSE, 7 INCONCLUSIVE
- (#0:Main) 'b' = d''               Falsified at 0.875, where t = 0.875, b' = 24.292967264012194, d' = 12.1375
                                    Falsified momentarily at 0.125, where t = 0.125, b' = 4.1, d' = 0.2
- (#0:Main) 'f = 2 * d'             Falsified at 0.875, where t = 0.875, f = 16.7, d = 9.8671875
                                    Falsified momentarily at 0.125, where t = 0.125, f = 8, d = 4.025
~ (#0:Main) 'at t = 0 : b' = b + c' Tested almost everywhere
                                    Falsified momentarily at 0.0, where t = 0, b' = -2, b = 4, c = 0.1
- (#0:Main) 'a' = a'                Falsified at 0.875, where t = 0.875, a' = 4.233994210859933, a = 4.797742890729985
                                    Falsified momentarily at 0.125, where t = 0.125, a' = 2, a = 2.26629638671875
~ (#0:Main) 'at t = 0 : e = f'      Tested almost everywhere
                                    Falsified momentarily at 0.0, where t = 0, f = 0.3, e = 0.2
~ (#0:Main) 'at t = 0 : b' = d''    Tested almost everywhere
                                    Falsified momentarily at 0.0, where t = 0, b' = -2, d' = -3
~ (#0:Main) 'at t = 0 : f = 2 * d'  Tested almost everywhere
                                    Falsified momentarily at 0.0, where t = 0, f = 0.3, d = 4
- (#0:Main) 'c = b'                 Falsified at 0.875, where t = 0.875, c = 13.45023747742633, b = 16.684802179967992
                                    Falsified momentarily at 0.125, where t = 0.125, c = 4, b = 4.545907592773437
~ (#0:Main) 'at t = 0 : d' = e'     Tested almost everywhere
                                    Falsified momentarily at 0.0, where t = 0, d' = -3, e = 0.2
~ (#0:Main) 'at t = 0 : a' = a'     Tested almost everywhere
                                    Falsified momentarily at 0.0, where t = 0, a' = -1, a = 2
- (#0:Main) 'b == d'                Falsified at 0.875, where b = 16.684802179967992, d = 9.8671875
                                    Falsified momentarily at 0.125, where b = 4.545907592773437, d = 4.025
~ (#0:Main) 'at t = 0 : c = b'      Tested almost everywhere
                                    Falsified momentarily at 0.0, where t = 0, c = 0.1, b = 4
- (#0:Main) 'e = f'                 Falsified at 0.875, where t = 0.875, f = 16.7, e = 14.16875
                                    Falsified momentarily at 0.125, where t = 0.125, f = 8, e = 0.3
- (#0:Main) 'a * a == b'            Falsified at 0.875, where a = 4.797742890729985, b = 16.684802179967992
                                    Falsified momentarily at 0.125, where a = 2.26629638671875, b = 4.545907592773437
- (#0:Main) 'b' = b + c'            Falsified at 0.875, where t = 0.875, b' = 24.292967264012194, b = 16.684802179967992, c = 13.45023747742633
                                    Falsified momentarily at 0.125, where t = 0.125, b' = 4.1, b = 4.545907592773437, c = 4
- (#0:Main) 'd' = e'                Falsified at 0.875, where t = 0.875, d' = 12.1375, e = 14.16875
                                    Falsified momentarily at 0.125, where t = 0.125, d' = 0.2, e = 0.3

