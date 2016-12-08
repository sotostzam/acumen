#0 {
  a = 0,
  a' = 1,
  b = 0,
  c = 0,
  className = Main,
  d = 0,
  e = 1,
  f = 1,
  firstDiscreteStep = false,
  g = 0,
  h = 0,
  simulator = #0.0,
  t = 0,
  t' = 1,
  type = Main
}
#0.0 { className = Simulator, time = 0.000000 }
------------------------------0
#0 {
  a = 0,
  a' = 1,
  b = 0,
  c = 0,
  className = Main,
  d = 0,
  e = 1,
  f = 1,
  firstDiscreteStep = false,
  g = 0,
  h = 0,
  simulator = #0.0,
  t = 0.500000,
  t' = 1,
  type = Main
}
#0.0 { className = Simulator, time = 0.500000 }
------------------------------33
#0 {
  a = 0,
  a' = 1,
  b = 0,
  c = 0,
  className = Main,
  d = 0,
  e = 1,
  f = 1,
  firstDiscreteStep = false,
  g = 1,
  h = 0,
  simulator = #0.0,
  t = 1.000000,
  t' = 1,
  type = Main
}
#0.0 { className = Simulator, time = 1.000000 }
------------------------------66

------------------------------73
SOME HYPOTHESES FALSIFIED OVER [0.0..1.109375]
2 TRUE, 4 FALSE, 3 INCONCLUSIVE
- (#0:Main) 'a == g (true until time 1.0)'                          Falsified at 1.0, where a = 0, g = 1
                                                                    Falsified momentarily at 1.0, where a = 0, g = 1
~ (#0:Main) 'a == d (true but initially and momentarily at 0.5)'    Tested almost everywhere
                                                                    Falsified momentarily at 0.5, where a = 0, d = 1
                                                                    Falsified initially, where a = 0, d = 1
~ (#0:Main) 'a == b (true but initially)'                           Tested almost everywhere
                                                                    Falsified initially, where a = 0, b = 1
~ (#0:Main) 'a == c (true but momentarily at 0.5)'                  Tested almost everywhere
                                                                    Falsified momentarily at 0.5, where a = 0, c = 1
+ (#0:Main) 'a == h (true but at time 1.0)'                         Tested
+ (#0:Main) 'a == a (true)'                                         Tested
- (#0:Main) 'a == e (false but initially)'                          Falsified at 0.0, where a = 0, e = 1
                                                                    Falsified momentarily at 0.0, where a = 0, e = 1
- (#0:Main) 'a == f (false but initially and at 1st discrete step)' Falsified at 0.0, where a = 0, f = 1
- (#0:Main) 'a != a (false)'                                        Falsified at 0.0, where a = 0
                                                                    Falsified momentarily at 0.0, where a = 0
                                                                    Falsified initially, where a = 0

