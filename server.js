const WebSocket = require('./web/node_modules/ws');
const net = require('net');

/** Socket to connect with acumen's data stream */
var toAcumen = new net.Socket();
toAcumen.connect({ port: 9090 });
toAcumen.setEncoding('utf8');

console.log('Waiting for interface connection...\n');

toAcumen.on('connect', function () {
  var address = toAcumen.address();
  var address2 = acumenProgress.address();
  var port = address.port;
  var port2 = address2.port;
  var family = address.family;
  var ipaddr = address.address;
  console.log('----------------- Acumen\'s socket ----------------- ');
  console.log('Client data stream connected at port ' + port);
  console.log('Client progress stream connected at port ' + port2);
  console.log('Client ip: ' + ipaddr);
  console.log('Client is IP4/IP6: ' + family);
  console.log('--------------------------------------------------- \n');
});

var framedString = '';
var isFrame = false;
toAcumen.on('data', function (data) {
  if (webSocketClient != null) {
    if (data.substring(0, 7) === '[FRAME]') {
      framedString = data.substring(7);
      isFrame = true;
    }
    else {
      if (isFrame == true) {
        if (data.substring(data.length - 5) === '[END]') {
          framedString += data.substring(0, data.length - 5);
          webSocketClient.send(framedString);
          framedString = '';
          isFrame = false;
        }
        else {
          framedString += data;
        }
      }
      else {
        webSocketClient.send(data);
      }
    }
  }
});

toAcumen.on('error', function (err) {
  if (err.code == 'ECONNREFUSED') {
    setTimeout(function() {
      toAcumen.connect({ port: 9090 });
    }, 100);
  }   
  else {
    console.log(err);
    process.exit();
  }
})

/** Socket to connect with acumen's progress stream */
var acumenProgress = new net.Socket();
acumenProgress.connect({ port: 9080 });
acumenProgress.setEncoding('utf8');

acumenProgress.on('data', function (data) {
  if (webSocketClient != null) {
    if (data.substring(0, 10) === '[PROGRESS]') {
      var regex = /\[PROGRESS\](.*?)\[\/PROGRESS\]/g;
      var n = data.match(regex);
      var m = regex.exec(n);
      webSocketClient.send(m[1]);
    }
    else {
      console.log('Wrong progress data!');
    }
  }
});

acumenProgress.on('error', function (err) {
  if (err.code == 'ECONNREFUSED') {
    setTimeout(function() {
      acumenProgress.connect({ port: 9080 });
    }, 100);
  }   
  else {
    console.log(err);
    process.exit();
  }
})

/** WebSocket to connect with browser */
const wss = new WebSocket.Server({ port: 9091 })
var webSocketClient = null;

wss.on('connection', (ws, req) => {
  webSocketClient = ws;
  ws.on('message', function incoming(message) {
    toAcumen.write(message + "\n");
  })
})
