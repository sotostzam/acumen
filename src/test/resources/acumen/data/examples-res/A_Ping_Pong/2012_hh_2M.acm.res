#0 {
  className = Main,
  game = #0.1,
  mode = "Persist",
  score1 = 0,
  score2 = 0,
  simulator = #0.0
}
#0.0 { className = Simulator, time = 0.000000 }
#0.1 {
  ball = #0.1.0,
  ballActuator = #0.1.2,
  ballob = #0.1.1,
  bat1 = #0.1.7,
  bat2 = #0.1.8,
  batActuator1 = #0.1.3,
  batActuator2 = #0.1.4,
  className = Game,
  finish = false,
  maxEnergy = 18,
  mode = "Player2Serve",
  player1 = #0.1.5,
  player1Score = 0,
  player2 = #0.1.6,
  player2Score = 0,
  referee = #0.1.10,
  serveNumber = 2,
  t = 0,
  t' = 1,
  table = #0.1.9
}
#0.1.0 {
  _3D = ["Sphere", [0, 0, 0.500000], 0.030000, [1, 1, 1], [0, 0, 0]],
  className = Ball,
  k2 = 0.166667,
  k_z = [1, 1, -0.990000],
  mode = "Fly",
  p = [0, 0, 0.500000],
  p' = [5, 1, -3],
  p'' = [0, 0, 0]
}
#0.1.1 {
  ap = [0, 0, 0],
  className = BallObserver,
  mode = "Sample",
  p = [0, 0, 0],
  pp = [0, 0, 0],
  t = 0,
  t' = 1,
  v = [0, 0, 0]
}
#0.1.2 {
  action = 0,
  angle = [0, 0, 0],
  className = BallActuator,
  done = false,
  mode = "Initialize",
  v1 = [0, 0, 0],
  v2 = [0, 0, 0],
  v3 = [0, 0, 0]
}
#0.1.3 {
  angle = [0, 0, 0],
  className = BatActuator,
  energy = 0,
  energy' = 0,
  p = [-1.600000, 0, 0.200000],
  p' = [0, 0, 0],
  p'' = [0, 0, 0],
  p1 = [-1.600000, 0, 0.200000]
}
#0.1.4 {
  angle = [0, 0, 0],
  className = BatActuator,
  energy = 0,
  energy' = 0,
  p = [1.600000, 0, 0.200000],
  p' = [0, 0, 0],
  p'' = [0, 0, 0],
  p1 = [1.600000, 0, 0.200000]
}
#0.1.5 {
  a = [0, 0, 0],
  aTemp = [0, 0, 0],
  ballp = [0, 0, 0],
  ballv = [0, 0, 0],
  batAngle = [0, 0, 0.100000],
  batAngle' = [0, 0, 0],
  batp = [1.600000, 0, 0.200000],
  bounced = false,
  className = Player,
  count = 0,
  desiredBatP = [-1.600000, 0, 0.200000],
  desiredBatP' = [0, 0, 0],
  estimatedBatV = [0, 0, 0],
  estimatedBatV' = [0, 0, 0],
  hit = false,
  i = [0, 0, 0],
  i' = [0, 0, 0],
  mode = "Wait",
  n = 1,
  n1 = 0,
  reStart = true,
  serve = false,
  startPoint = [-1.600000, 0, 0.200000],
  t = 0,
  t' = 1,
  v = [0, 0, 0],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.6 {
  a = [0, 0, 0],
  aTemp = [0, 0, 0],
  ballp = [0, 0, 0],
  ballv = [0, 0, 0],
  batAngle = [0, 0, 0.100000],
  batAngle' = [0, 0, 0],
  batp = [1.600000, 0, 0.200000],
  bounced = false,
  className = Player,
  count = 0,
  desiredBatP = [1.600000, 0, 0.200000],
  desiredBatP' = [0, 0, 0],
  estimatedBatV = [0, 0, 0],
  estimatedBatV' = [0, 0, 0],
  hit = false,
  i = [0, 0, 0],
  i' = [0, 0, 0],
  mode = "Wait",
  n = 2,
  n1 = 0,
  reStart = true,
  serve = false,
  startPoint = [1.600000, 0, 0.200000],
  t = 0,
  t' = 1,
  v = [0, 0, 0],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.7 {
  _3D = ["Cylinder", [-1.600000, 0, 0.200000], [0.150000, 0.010000], [0.100000, 0.100000, 0.100000], [0, 0, 0.500000]],
  angle = [0, 0, 0.100000],
  className = Bat,
  displayAngle = [0, 0, 0],
  mode = "Run",
  n = 1,
  p = [-1.600000, 0, 0.200000],
  p1 = [-1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.8 {
  _3D = ["Cylinder", [1.600000, 0, 0.200000], [0.150000, 0.010000], [0.100000, 0.100000, 0.100000], [0, 0, 0.500000]],
  angle = [0, 0, 0.100000],
  className = Bat,
  displayAngle = [0, 0, 0],
  mode = "Run",
  n = 2,
  p = [1.600000, 0, 0.200000],
  p1 = [1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.9 {
  _3D = [["Box", [0, 0, -0.050000], [3, 1.500000, 0.030000], [0.100000, 0.100000, 1.000000], [0, 0, 0]], ["Box", [-1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [-1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [0, 0, 0.105000], [0.050000, 1.500000, 0.250000], [0.200000, 0.800000, 0.200000], [0, 0, 0]], ["Box", [0, 0, 0], [3, 0.020000, 0.000000], [1, 1, 1], [0, 0, 0]]],
  className = Table
}
#0.1.10 {
  acknowledged = 0,
  bounceTime = 0,
  bounced = false,
  checked = false,
  className = Referee,
  lastHit = 0,
  mode = "Initialize",
  player1Score = 0,
  player2Score = 0,
  reason = "Nothing",
  restart = 0,
  serveNumber = 2,
  status = "Normal",
  t = 0,
  t' = 1,
  x = 0,
  xv = 0,
  y = 0,
  z = 0,
  zv = 0
}
------------------------------0
#0 {
  className = Main,
  game = #0.1,
  mode = "Persist",
  score1 = 0,
  score2 = 0,
  simulator = #0.0
}
#0.0 { className = Simulator, time = 0.010000 }
#0.1 {
  ball = #0.1.0,
  ballActuator = #0.1.2,
  ballob = #0.1.1,
  bat1 = #0.1.7,
  bat2 = #0.1.8,
  batActuator1 = #0.1.3,
  batActuator2 = #0.1.4,
  className = Game,
  finish = false,
  maxEnergy = 18,
  mode = "Player2Serve",
  player1 = #0.1.5,
  player1Score = 0,
  player2 = #0.1.6,
  player2Score = 0,
  referee = #0.1.10,
  serveNumber = 2,
  t = 0.010000,
  t' = 1,
  table = #0.1.9
}
#0.1.0 {
  _3D = ["Sphere", [0.049507, 0.009901, 0.469316], 0.030000, [1, 1, 1], [0, 0, 0]],
  className = Ball,
  k2 = 0.166667,
  k_z = [1, 1, -0.990000],
  mode = "Fly",
  p = [0.049507, 0.009901, 0.469316],
  p' = [4.950699, 0.990140, -3.068420],
  p'' = [-4.930066, -0.986013, -6.841960]
}
#0.1.1 {
  ap = [0, 0, 0],
  className = BallObserver,
  mode = "Estimate0",
  p = [0, 0, 0.500000],
  pp = [0, 0, 0.500000],
  t = 0,
  t' = 1,
  v = [0, 0, 0]
}
#0.1.2 {
  action = 0,
  angle = [0, 0, 0],
  className = BallActuator,
  done = false,
  mode = "Initialize",
  v1 = [0, 0, 0],
  v2 = [0, 0, 0],
  v3 = [0, 0, 0]
}
#0.1.3 {
  angle = [0, 0, 0.100000],
  className = BatActuator,
  energy = 0.000000,
  energy' = 0.000000,
  p = [-1.600000, 0.000000, 0.200000],
  p' = [0.000000, 0.000000, 0.000000],
  p'' = [0, 0, 0],
  p1 = [-1.600000, 0, 0.200000]
}
#0.1.4 {
  angle = [0, 0, 0.100000],
  className = BatActuator,
  energy = 0.000000,
  energy' = 0.000000,
  p = [1.600000, 0.000000, 0.200000],
  p' = [0.000000, 0.000000, 0.000000],
  p'' = [0, 0, 0],
  p1 = [1.600000, 0, 0.200000]
}
#0.1.5 {
  a = [0.000000, 0.000000, 0.000000],
  aTemp = [-0.160000, 0.000000, 0.020000],
  ballp = [0, 0, 0.500000],
  ballv = [0, 0, 0],
  batAngle = [0.000000, 0.000000, 0.099000],
  batAngle' = [0.000000, 0.000000, -0.100000],
  batp = [-1.600000, 0, 0.200000],
  bounced = false,
  className = Player,
  count = 0,
  desiredBatP = [-1.600000, 0.000000, 0.200000],
  desiredBatP' = [0.000000, 0.000000, 0.000000],
  estimatedBatV = [0.000000, 0.000000, 0.000000],
  estimatedBatV' = [0.000000, 0.000000, 0.000000],
  hit = false,
  i = [-0.016000, 0.000000, 0.002000],
  i' = [-1.600000, 0.000000, 0.200000],
  mode = "Wait",
  n = 1,
  n1 = 0,
  reStart = true,
  serve = false,
  startPoint = [-1.600000, 0, 0.200000],
  t = 0.010000,
  t' = 1,
  v = [0.000000, 0.000000, 0.000000],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.6 {
  a = [0.000000, 10.125750, 0.008000],
  aTemp = [0.160000, 10.125750, 0.020000],
  ballp = [0, 0, 0.500000],
  ballv = [0, 0, 0],
  batAngle = [0.000000, 0.000000, 0.099000],
  batAngle' = [0.000000, 0.000000, -0.100000],
  batp = [1.600000, 0, 0.200000],
  bounced = false,
  className = Player,
  count = 0,
  desiredBatP = [1.600000, 0.007500, 0.200000],
  desiredBatP' = [0.000000, 0.750000, 0.000000],
  estimatedBatV = [0.000000, 0.101258, 0.000080],
  estimatedBatV' = [0.000000, 10.125750, 0.008000],
  hit = false,
  i = [0.016000, 0.000075, 0.002000],
  i' = [1.600000, 0.007500, 0.200000],
  mode = "Prepare",
  n = 2,
  n1 = 0,
  reStart = true,
  serve = true,
  startPoint = [1.600000, 0, 0.200000],
  t = 0.010000,
  t' = 1,
  v = [0.000000, 0.750000, 0.000000],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.7 {
  _3D = ["Cylinder", [-1.650000, 0.000000, 0.200000], [0.150000, 0.010000], [1, 0.100000, 0.100000], [-0.157000, -0.000000, -1.570000]],
  angle = [0, 0, 0.100000],
  className = Bat,
  displayAngle = [0.157000, 0.000000, 1.570000],
  mode = "Run",
  n = 1,
  p = [-1.600000, 0.000000, 0.200000],
  p1 = [-1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.8 {
  _3D = ["Cylinder", [1.650000, 0.000000, 0.200000], [0.150000, 0.010000], [0.100000, 0.100000, 0.100000], [0.000000, 1.570000, 1.570000]],
  angle = [0, 0, 0.100000],
  className = Bat,
  displayAngle = [0.000000, 1.570000, 1.570000],
  mode = "Run",
  n = 2,
  p = [1.600000, 0.000000, 0.200000],
  p1 = [1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.9 {
  _3D = [["Box", [0, 0, -0.050000], [3, 1.500000, 0.030000], [0.100000, 0.100000, 1.000000], [0, 0, 0]], ["Box", [-1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [-1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [0, 0, 0.105000], [0.050000, 1.500000, 0.250000], [0.200000, 0.800000, 0.200000], [0, 0, 0]], ["Box", [0, 0, 0], [3, 0.020000, 0.000000], [1, 1, 1], [0, 0, 0]]],
  className = Table
}
#0.1.10 {
  acknowledged = 0,
  bounceTime = 0,
  bounced = false,
  checked = false,
  className = Referee,
  lastHit = 0,
  mode = "Initialize",
  player1Score = 0,
  player2Score = 0,
  reason = "Nothing",
  restart = 0,
  serveNumber = 2,
  status = "Normal",
  t = 0.010000,
  t' = 1,
  x = 0.000000,
  xv = 5.000000,
  y = 0.000000,
  z = 0.500000,
  zv = -3.000000
}
------------------------------2
#0 {
  className = Main,
  game = #0.1,
  mode = "Persist",
  score1 = 0,
  score2 = 0,
  simulator = #0.0
}
#0.0 { className = Simulator, time = 0.020000 }
#0.1 {
  ball = #0.1.0,
  ballActuator = #0.1.2,
  ballob = #0.1.1,
  bat1 = #0.1.7,
  bat2 = #0.1.8,
  batActuator1 = #0.1.3,
  batActuator2 = #0.1.4,
  className = Game,
  finish = false,
  maxEnergy = 18,
  mode = "Player2Serve",
  player1 = #0.1.5,
  player1Score = 0,
  player2 = #0.1.6,
  player2Score = 0,
  referee = #0.1.10,
  serveNumber = 2,
  t = 0.020000,
  t' = 1,
  table = #0.1.9
}
#0.1.0 {
  _3D = ["Sphere", [0.098527, 0.019705, 0.437954], 0.030000, [1, 1, 1], [0, 0, 0]],
  className = Ball,
  k2 = 0.166667,
  k_z = [1, 1, -0.990000],
  mode = "Fly",
  p = [0.098527, 0.019705, 0.437954],
  p' = [4.901951, 0.980390, -3.136206],
  p'' = [-4.874826, -0.974965, -6.778606]
}
#0.1.1 {
  ap = [0.049507, 0.009901, 0.469316],
  className = BallObserver,
  mode = "Sample",
  p = [0.049507, 0.009901, 0.469316],
  pp = [0, 0, 0.500000],
  t = 0,
  t' = 1,
  v = [4.950699, 0.990140, -3.068420]
}
#0.1.2 {
  action = 0,
  angle = [0, 0, 0],
  className = BallActuator,
  done = false,
  mode = "Initialize",
  v1 = [0, 0, 0],
  v2 = [0, 0, 0],
  v3 = [0, 0, 0]
}
#0.1.3 {
  angle = [0.000000, 0.000000, 0.099000],
  className = BatActuator,
  energy = 0.000000,
  energy' = 0.000000,
  p = [-1.600000, 0.000000, 0.200000],
  p' = [0.000000, 0.000000, 0.000000],
  p'' = [0.000000, 0.000000, 0.000000],
  p1 = [-1.600000, 0, 0.200000]
}
#0.1.4 {
  angle = [0.000000, 0.000000, 0.099000],
  className = BatActuator,
  energy = 0.005063,
  energy' = 0.506288,
  p = [1.600000, 0.001013, 0.200001],
  p' = [0.000000, 0.101258, 0.000080],
  p'' = [0.000000, 10.125750, 0.008000],
  p1 = [1.600000, 0, 0.200000]
}
#0.1.5 {
  a = [0.000000, 0.000000, 0.000000],
  aTemp = [-0.320000, 0.000000, 0.040000],
  ballp = [0.049507, 0.009901, 0.469316],
  ballv = [0, 0, 0],
  batAngle = [0.000000, 0.000000, 0.098010],
  batAngle' = [0.000000, 0.000000, -0.099000],
  batp = [-1.600000, 0.000000, 0.200000],
  bounced = false,
  className = Player,
  count = 0,
  desiredBatP = [-1.600000, 0.000000, 0.200000],
  desiredBatP' = [0.000000, 0.000000, 0.000000],
  estimatedBatV = [0.000000, 0.000000, 0.000000],
  estimatedBatV' = [0.000000, 0.000000, 0.000000],
  hit = false,
  i = [-0.032000, 0.000000, 0.004000],
  i' = [-1.600000, 0.000000, 0.200000],
  mode = "Wait",
  n = 1,
  n1 = 0,
  reStart = true,
  serve = false,
  startPoint = [-1.600000, 0, 0.200000],
  t = 0.020000,
  t' = 1,
  v = [0.000000, 0.000000, 0.000000],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.6 {
  a = [0.000000, 9.185903, 0.015584],
  aTemp = [0.320000, 9.185903, 0.038960],
  ballp = [0.049507, 0.009901, 0.469316],
  ballv = [0, 0, 0],
  batAngle = [0.000000, 0.000000, 0.099000],
  batAngle' = [0.000000, 0.000000, -0.100000],
  batp = [1.600000, 0.000000, 0.200000],
  bounced = false,
  className = Player,
  count = 0,
  desiredBatP = [1.600000, 0.015000, 0.200000],
  desiredBatP' = [0.000000, 0.750000, 0.000000],
  estimatedBatV = [0.000000, 0.193117, 0.000236],
  estimatedBatV' = [0.000000, 9.185903, 0.015584],
  hit = false,
  i = [0.032000, 0.000225, 0.004000],
  i' = [1.600000, 0.015000, 0.200000],
  mode = "Prepare",
  n = 2,
  n1 = 0,
  reStart = true,
  serve = true,
  startPoint = [1.600000, 0, 0.200000],
  t = 0.020000,
  t' = 1,
  v = [0.000000, 0.750000, 0.000000],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.7 {
  _3D = ["Cylinder", [-1.650000, 0.000000, 0.200000], [0.150000, 0.010000], [1, 0.100000, 0.100000], [-0.155430, -0.000000, -1.570000]],
  angle = [0.000000, 0.000000, 0.099000],
  className = Bat,
  displayAngle = [0.155430, 0.000000, 1.570000],
  mode = "Run",
  n = 1,
  p = [-1.600000, 0.000000, 0.200000],
  p1 = [-1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.8 {
  _3D = ["Cylinder", [1.650000, 0.001013, 0.200001], [0.150000, 0.010000], [0.100000, 0.100000, 0.100000], [0.000000, 1.570000, 1.570000]],
  angle = [0.000000, 0.000000, 0.099000],
  className = Bat,
  displayAngle = [0.000000, 1.570000, 1.570000],
  mode = "Run",
  n = 2,
  p = [1.600000, 0.001013, 0.200001],
  p1 = [1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.9 {
  _3D = [["Box", [0, 0, -0.050000], [3, 1.500000, 0.030000], [0.100000, 0.100000, 1.000000], [0, 0, 0]], ["Box", [-1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [-1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [0, 0, 0.105000], [0.050000, 1.500000, 0.250000], [0.200000, 0.800000, 0.200000], [0, 0, 0]], ["Box", [0, 0, 0], [3, 0.020000, 0.000000], [1, 1, 1], [0, 0, 0]]],
  className = Table
}
#0.1.10 {
  acknowledged = 0,
  bounceTime = 0,
  bounced = false,
  checked = false,
  className = Referee,
  lastHit = 0,
  mode = "Initialize",
  player1Score = 0,
  player2Score = 0,
  reason = "Nothing",
  restart = 0,
  serveNumber = 2,
  status = "Normal",
  t = 0.020000,
  t' = 1,
  x = 0.049507,
  xv = 4.950699,
  y = 0.009901,
  z = 0.469316,
  zv = -3.068420
}
------------------------------4
#0 {
  className = Main,
  game = #0.1,
  mode = "Persist",
  score1 = 0,
  score2 = 0,
  simulator = #0.0
}
#0.0 { className = Simulator, time = 0.030000 }
#0.1 {
  ball = #0.1.0,
  ballActuator = #0.1.2,
  ballob = #0.1.1,
  bat1 = #0.1.7,
  bat2 = #0.1.8,
  batActuator1 = #0.1.3,
  batActuator2 = #0.1.4,
  className = Game,
  finish = false,
  maxEnergy = 18,
  mode = "Player2Serve",
  player1 = #0.1.5,
  player1Score = 0,
  player2 = #0.1.6,
  player2Score = 0,
  referee = #0.1.10,
  serveNumber = 2,
  t = 0.030000,
  t' = 1,
  table = #0.1.9
}
#0.1.0 {
  _3D = ["Sphere", [0.147064, 0.029413, 0.405920], 0.030000, [1, 1, 1], [0, 0, 0]],
  className = Ball,
  k2 = 0.166667,
  k_z = [1, 1, -0.990000],
  mode = "Fly",
  p = [0.147064, 0.029413, 0.405920],
  p' = [4.853737, 0.970747, -3.203359],
  p'' = [-4.821365, -0.964273, -6.715352]
}
#0.1.1 {
  ap = [0.049507, 0.009901, 0.469316],
  className = BallObserver,
  mode = "Estimate0",
  p = [0.098527, 0.019705, 0.437954],
  pp = [0.098527, 0.019705, 0.437954],
  t = 0,
  t' = 1,
  v = [4.950699, 0.990140, -3.068420]
}
#0.1.2 {
  action = 0,
  angle = [0, 0, 0],
  className = BallActuator,
  done = false,
  mode = "Initialize",
  v1 = [0, 0, 0],
  v2 = [0, 0, 0],
  v3 = [0, 0, 0]
}
#0.1.3 {
  angle = [0.000000, 0.000000, 0.098010],
  className = BatActuator,
  energy = 0.000000,
  energy' = 0.000000,
  p = [-1.600000, 0.000000, 0.200000],
  p' = [0.000000, 0.000000, 0.000000],
  p'' = [0.000000, 0.000000, 0.000000],
  p1 = [-1.600000, 0, 0.200000]
}
#0.1.4 {
  angle = [0.000000, 0.000000, 0.099000],
  className = BatActuator,
  energy = 0.011722,
  energy' = 0.665949,
  p = [1.600000, 0.002944, 0.200003],
  p' = [0.000000, 0.193117, 0.000236],
  p'' = [0.000000, 9.185903, 0.015584],
  p1 = [1.600000, 0, 0.200000]
}
#0.1.5 {
  a = [0.000000, 0.000000, 0.000000],
  aTemp = [-0.480000, 0.000000, 0.060000],
  ballp = [0.098527, 0.019705, 0.437954],
  ballv = [4.950699, 0.990140, -3.068420],
  batAngle = [0.000000, 0.000000, 0.097030],
  batAngle' = [0.000000, 0.000000, -0.098010],
  batp = [-1.600000, 0.000000, 0.200000],
  bounced = false,
  className = Player,
  count = 0,
  desiredBatP = [-1.600000, 0.000000, 0.200000],
  desiredBatP' = [0.000000, 0.000000, 0.000000],
  estimatedBatV = [0.000000, 0.000000, 0.000000],
  estimatedBatV' = [0.000000, 0.000000, 0.000000],
  hit = false,
  i = [-0.048000, 0.000000, 0.006000],
  i' = [-1.600000, 0.000000, 0.200000],
  mode = "Wait",
  n = 1,
  n1 = 0,
  reStart = true,
  serve = false,
  startPoint = [-1.600000, 0, 0.200000],
  t = 0.030000,
  t' = 1,
  v = [0.000000, 0.000000, 0.000000],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.6 {
  a = [0.000000, 8.318356, 0.022758],
  aTemp = [0.480000, 8.318356, 0.056894],
  ballp = [0.098527, 0.019705, 0.437954],
  ballv = [4.950699, 0.990140, -3.068420],
  batAngle = [0.000000, 0.000000, 0.099000],
  batAngle' = [0.000000, 0.000000, -0.100000],
  batp = [1.600000, 0.001013, 0.200001],
  bounced = false,
  className = Player,
  count = 0,
  desiredBatP = [1.600000, 0.022500, 0.200000],
  desiredBatP' = [0.000000, 0.750000, 0.000000],
  estimatedBatV = [0.000000, 0.276300, 0.000463],
  estimatedBatV' = [0.000000, 8.318356, 0.022758],
  hit = false,
  i = [0.048000, 0.000450, 0.006000],
  i' = [1.600000, 0.022500, 0.200000],
  mode = "Prepare",
  n = 2,
  n1 = 0,
  reStart = true,
  serve = true,
  startPoint = [1.600000, 0, 0.200000],
  t = 0.030000,
  t' = 1,
  v = [0.000000, 0.750000, 0.000000],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.7 {
  _3D = ["Cylinder", [-1.650000, 0.000000, 0.200000], [0.150000, 0.010000], [1, 0.100000, 0.100000], [-0.153876, -0.000000, -1.570000]],
  angle = [0.000000, 0.000000, 0.098010],
  className = Bat,
  displayAngle = [0.153876, 0.000000, 1.570000],
  mode = "Run",
  n = 1,
  p = [-1.600000, 0.000000, 0.200000],
  p1 = [-1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.8 {
  _3D = ["Cylinder", [1.650000, 0.002944, 0.200003], [0.150000, 0.010000], [0.100000, 0.100000, 0.100000], [0.000000, 1.570000, 1.570000]],
  angle = [0.000000, 0.000000, 0.099000],
  className = Bat,
  displayAngle = [0.000000, 1.570000, 1.570000],
  mode = "Run",
  n = 2,
  p = [1.600000, 0.002944, 0.200003],
  p1 = [1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.9 {
  _3D = [["Box", [0, 0, -0.050000], [3, 1.500000, 0.030000], [0.100000, 0.100000, 1.000000], [0, 0, 0]], ["Box", [-1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [-1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [0, 0, 0.105000], [0.050000, 1.500000, 0.250000], [0.200000, 0.800000, 0.200000], [0, 0, 0]], ["Box", [0, 0, 0], [3, 0.020000, 0.000000], [1, 1, 1], [0, 0, 0]]],
  className = Table
}
#0.1.10 {
  acknowledged = 0,
  bounceTime = 0,
  bounced = false,
  checked = false,
  className = Referee,
  lastHit = 0,
  mode = "Initialize",
  player1Score = 0,
  player2Score = 0,
  reason = "Nothing",
  restart = 0,
  serveNumber = 2,
  status = "Normal",
  t = 0.030000,
  t' = 1,
  x = 0.098527,
  xv = 4.901951,
  y = 0.019705,
  z = 0.437954,
  zv = -3.136206
}
------------------------------6
#0 {
  className = Main,
  game = #0.1,
  mode = "Persist",
  score1 = 12,
  score2 = 11,
  simulator = #0.0
}
#0.0 { className = Simulator, time = 40.000000 }
#0.1 {
  ball = #0.1.0,
  ballActuator = #0.1.2,
  ballob = #0.1.1,
  bat1 = #0.1.7,
  bat2 = #0.1.8,
  batActuator1 = #0.1.3,
  batActuator2 = #0.1.4,
  className = Game,
  finish = false,
  maxEnergy = 18,
  mode = "Freeze",
  player1 = #0.1.5,
  player1Score = 12,
  player2 = #0.1.6,
  player2Score = 11,
  referee = #0.1.10,
  serveNumber = 2,
  t = 0.270000,
  t' = 1,
  table = #0.1.9
}
#0.1.0 {
  _3D = ["Sphere", [3.057640, 0.611528, 0.001464], 0.030000, [1, 0, 0], [0, 0, 0]],
  className = Ball,
  k2 = 0.166667,
  k_z = [1, 1, -0.990000],
  mode = "Freeze",
  p = [3.057640, 0.611528, 0.001464],
  p' = [0, 0, 0],
  p'' = [0, 0, 0]
}
#0.1.1 {
  ap = [3.057640, 0.611528, 0.001464],
  className = BallObserver,
  mode = "Estimate0",
  p = [3.057640, 0.611528, 0.001464],
  pp = [3.057640, 0.611528, 0.001464],
  t = 0.010000,
  t' = 1,
  v = [0.000000, 0.000000, 0.000000]
}
#0.1.2 {
  action = 0,
  angle = [0, 0, 0],
  className = BallActuator,
  done = false,
  mode = "Initialize",
  v1 = [0, 0, 0],
  v2 = [0, 0, 0],
  v3 = [0, 0, 0]
}
#0.1.3 {
  angle = [0.003277, 0.000000, 0.000000],
  className = BatActuator,
  energy = 12.823559,
  energy' = 0.000000,
  p = [-1.600000, -0.000000, 0.200000],
  p' = [0.000000, 0.000000, 0.000000],
  p'' = [-0.000000, -0.000000, -0.000000],
  p1 = [-1.600000, 0, 0.200000]
}
#0.1.4 {
  angle = [0.010000, 0, 0],
  className = BatActuator,
  energy = 18.050261,
  energy' = 0.000000,
  p = [1.600000, 0.000000, 0.200000],
  p' = [0, 0, 0],
  p'' = [23.903184, 30.793748, -34.292258],
  p1 = [1.600000, 0, 0.200000]
}
#0.1.5 {
  a = [-0.000000, -0.000000, -0.000000],
  aTemp = [-684.287911, 0.055448, 93.887195],
  ballp = [3.057640, 0.611528, 0.001464],
  ballv = [0.000000, 0.000000, 0.000000],
  batAngle = [0.003244, 0.000000, 0.000000],
  batAngle' = [-0.003277, 0.000000, 0.000000],
  batp = [-1.600000, -0.000000, 0.200000],
  bounced = false,
  className = Player,
  count = 0,
  desiredBatP = [-1.600000, -0.000000, 0.200000],
  desiredBatP' = [-0.000000, 0.000000, 0.000000],
  estimatedBatV = [0.000000, -0.000000, -0.000000],
  estimatedBatV' = [-0.000000, -0.000000, -0.000000],
  hit = false,
  i = [-68.428791, 0.005545, 9.388719],
  i' = [-1.600000, -0.000000, 0.200000],
  mode = "Wait",
  n = 1,
  n1 = 0,
  reStart = false,
  serve = false,
  startPoint = [-1.600000, 0, 0.200000],
  t = 40.000000,
  t' = 1,
  v = [-0.000000, 0.000000, 0.000000],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.6 {
  a = [0.000000, 80.019268, -23.888953],
  aTemp = [5134.821302, 80.019268, -59.722383],
  ballp = [3.057640, 0.611528, 0.001464],
  ballv = [0.000000, 0.000000, 0.000000],
  batAngle = [0.010000, 0, 0],
  batAngle' = [-0.200975, 0.000032, 0.234112],
  batp = [1.600000, 0.000000, 0.200000],
  bounced = true,
  className = Player,
  count = 0,
  desiredBatP = [20.933940, 13.082788, 2.668849],
  desiredBatP' = [43.729209, 18.345842, -5.956092],
  estimatedBatV = [0.000000, 213.609668, 25.084569],
  estimatedBatV' = [0.000000, 80.019268, -23.888953],
  hit = false,
  i = [359.964459, 195.390710, 22.346935],
  i' = [20.933940, 13.082788, 2.668849],
  mode = "Prepare",
  n = 2,
  n1 = 0,
  reStart = true,
  serve = true,
  startPoint = [1.600000, 0, 0.200000],
  t = 40.000000,
  t' = 1,
  v = [43.729209, 18.345842, -5.956092],
  v2 = [0, 0, 0],
  v21 = [0, 0, 0],
  z = 0
}
#0.1.7 {
  _3D = ["Cylinder", [-1.650000, -0.000000, 0.200000], [0.150000, 0.010000], [1, 0.100000, 0.100000], [-0.000000, -0.000000, -1.570000]],
  angle = [0.003277, 0.000000, 0.000000],
  className = Bat,
  displayAngle = [0.000000, 0.000000, 1.570000],
  mode = "Run",
  n = 1,
  p = [-1.600000, -0.000000, 0.200000],
  p1 = [-1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.8 {
  _3D = ["Box", [1.550000, 0.000000, 0.200000], [0.300000, 0.300000, 0.300000], [1, 1, 0.100000], [-0.000000, 1.191259, -1.569837]],
  angle = [0.010000, 0, 0],
  className = Bat,
  displayAngle = [0.000000, -1.191259, 1.569837],
  mode = "Rest",
  n = 2,
  p = [1.600000, 0.000000, 0.200000],
  p1 = [1.600000, 0, 0.200000],
  pv = [0, 0, 0]
}
#0.1.9 {
  _3D = [["Box", [0, 0, -0.050000], [3, 1.500000, 0.030000], [0.100000, 0.100000, 1.000000], [0, 0, 0]], ["Box", [-1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [-1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, -0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [1.400000, 0.600000, -0.340000], [0.050000, 0.050000, 0.600000], [0.800000, 0.800000, 0.800000], [0, 0, 0]], ["Box", [0, 0, 0.105000], [0.050000, 1.500000, 0.250000], [0.200000, 0.800000, 0.200000], [0, 0, 0]], ["Box", [0, 0, 0], [3, 0.020000, 0.000000], [1, 1, 1], [0, 0, 0]]],
  className = Table
}
#0.1.10 {
  acknowledged = 0,
  bounceTime = 1.150000,
  bounced = false,
  checked = false,
  className = Referee,
  lastHit = 0,
  mode = "SendMessage",
  player1Score = 12,
  player2Score = 11,
  reason = "BallOutOfBoundary",
  restart = 1,
  serveNumber = 2,
  status = "Report",
  t = 0.010000,
  t' = 1,
  x = 3.057640,
  xv = 0.000000,
  y = 0.611528,
  z = 0.001464,
  zv = 0.000000
}
------------------------------7999
