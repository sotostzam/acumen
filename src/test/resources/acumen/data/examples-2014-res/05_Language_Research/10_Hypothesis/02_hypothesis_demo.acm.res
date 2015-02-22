#0 {
  a = 0,
  a' = 1,
  b = 1,
  c = 0,
  className = Main,
  d = 1,
  e = 0,
  f = 0,
  firstDiscreteStep = false,
  g = 0,
  h = 0,
  simulator = #0.0,
  t = 0,
  t' = 1
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  a = 0,
  a' = 1,
  b = 0.490000,
  c = 1,
  className = Main,
  d = 1,
  e = 1.490000,
  f = 1.490000,
  firstDiscreteStep = false,
  g = 0.490000,
  h = 0.490000,
  simulator = #0.0,
  t = 0.500000,
  t' = 1
}
#0.0 { className = Simulator, time = 0.500000 }
------------------------------51
#0 {
  a = 0,
  a' = 1,
  b = 0.490000,
  c = 1,
  className = Main,
  d = 1,
  e = 1.490000,
  f = 1.490000,
  firstDiscreteStep = false,
  g = 0.490000,
  h = 0.490000,
  simulator = #0.0,
  t = 1.000000,
  t' = 1
}
#0.0 { className = Simulator, time = 1.000000 }
------------------------------102
#0 {
  a = 0.100000,
  a' = 1,
  b = 0.090000,
  c = 0.090000,
  className = Main,
  d = 0.090000,
  e = 1.090000,
  f = 1.090000,
  firstDiscreteStep = false,
  g = 1.090000,
  h = 0.090000,
  simulator = #0.0,
  t = 1.100000,
  t' = 1
}
#0.0 { className = Simulator, time = 1.100000 }
------------------------------112
SOME HYPOTHESES FALSIFIED OVER [0.0..1.1000000000000008]
1 TRUE, 8 FALSE, 0 INCONCLUSIVE
- (#0:Main) 'a == g (true until time 1.0)'                          Falsified at 1.0900000000000007, where a = 0.09, g = 1.08
                                                                    Falsified momentarily at 0.01, where a = 0.01, g = 0
- (#0:Main) 'a == d (true but initially and momentarily at 0.5)'    Falsified at 1.0900000000000007, where a = 0.09, d = 0.08
                                                                    Falsified momentarily at 0.0, where a = 0, d = 1
- (#0:Main) 'a == b (true but initially)'                           Falsified at 1.0900000000000007, where a = 0.09, b = 0.08
                                                                    Falsified momentarily at 0.0, where a = 0, b = 1
- (#0:Main) 'a == c (true but momentarily at 0.5)'                  Falsified at 1.0900000000000007, where a = 0.09, c = 0.08
                                                                    Falsified momentarily at 0.01, where a = 0.01, c = 0
- (#0:Main) 'a == h (true but at time 1.0)'                         Falsified at 1.0900000000000007, where a = 0.09, h = 0.08
                                                                    Falsified momentarily at 0.01, where a = 0.01, h = 0
+ (#0:Main) 'a == a (true)'                                         Tested
- (#0:Main) 'a == e (false but initially)'                          Falsified at 1.0900000000000007, where a = 0.09, e = 1.08
                                                                    Falsified momentarily at 0.01, where a = 0.01, e = 1
- (#0:Main) 'a == f (false but initially and at 1st discrete step)' Falsified at 1.0900000000000007, where a = 0.09, f = 1.08
                                                                    Falsified momentarily at 0.01, where a = 0.01, f = 1
- (#0:Main) 'a != a (false)'                                        Falsified at 1.0900000000000007, where a = 0.09
                                                                    Falsified momentarily at 0.0, where a = 0

