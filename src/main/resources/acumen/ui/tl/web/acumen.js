const connection = new WebSocket('ws://localhost:9091');

connection.onopen = () => {
  connection.send("[{\"type\": \"event\", \"event\": \"jsReady\"}]\r");
}

connection.onmessage = (event) => {
  try {
    var obj = JSON.parse(event.data);
    if (!Array.isArray(obj)) {
      switch (obj.event) {
        case "state":
          if (obj.state === "appReady") {
            editor.session.getUndoManager().reset();
            document.getElementById("undoAction").disabled = true;   // Hack to disable button cause of init code.
            var loader = document.getElementById("loader");
            setTimeout(function () {
              loader.style.display = 'none';
            }, 1000);
            loader.style.WebkitTransition = 'opacity 1s';
            loader.style.opacity = '0';
          }
          else {
            stateChanged(obj.state);
          }
          break;
        case "enableNormalize":
          document.getElementById("normalization").checked = true;
          break;
        case "viewChange":
          switch (obj.selectView) {
            case "plotView":
              changeRTab('plotTab');
              document.getElementById("plotButton").className += " active";
              break;
            case "threedView":
              changeRTab('threeDtab');
              document.getElementById("threeDButton").className += " active";
              break;
          }
          break;
        case "codeArea":
          editor.setValue(obj.text, 1);
          editedSinceLastSave = false;
          break;
        case "console":
          if (obj.data[0] === 'separator') {
            var toggler = document.getElementsByClassName("consoleItem");
            for (var i = 0; i < toggler.length; i++) {
              for (var child=toggler[i].firstChild; child!==null; child=child.nextSibling) {
                child.style.color = "grey";
              }
            }
          }
          else {
            var node = document.createElement("li");
            node.className = 'consoleItem';
            node.addEventListener("click", function () {
              let logger = document.getElementById("consoleAreaList");
              for(var child=logger.firstChild; child!==null; child=child.nextSibling) {
                child.style.backgroundColor = 'white';
              }
              this.style.backgroundColor = 'lightgrey';
            });
            if (obj.data[0] === 'status' && obj.data[3]) {
              let logger = document.getElementById("consoleAreaList");
              logger.removeChild(logger.lastChild);
            }
            if (obj.data[0] === 'error') {
              var type = document.createElement("a");
              type.style.color = 'red';
              type.style.paddingBottom = '5px';
              var bold = document.createElement('strong');
              var parText = document.createTextNode('ERROR:');
              bold.appendChild(parText);
              type.appendChild(bold);
              node.appendChild(type);
              editor.gotoLine(parseInt(obj.data[2].split(".")[0]), parseInt(obj.data[2].split(".")[1]) - 1);
              changeCTab('consoleTab');
              document.getElementById("consoleButton").className += " active";
            }
            var par = document.createElement("a");
            var parText = document.createTextNode(obj.data[1]);
            par.appendChild(parText);
            node.appendChild(par);
            document.getElementById("consoleAreaList").appendChild(node);
          }
          break;
        case "progress":
          document.getElementById("progressBar").value = obj.data;
          break;
        case "setFilename":
          document.getElementById("fileNameLabelText").innerHTML = obj.data;
          break;
        case "serverStarted":
          document.getElementById("startServer").disabled = true;
          document.getElementById("stopServer").disabled = false;
          document.getElementById("resetDevice").disabled = false;
          document.getElementById("serverLink").disabled = false;
          document.getElementById("serverLink").innerText = 'Open server link: ' + obj.link;
          document.getElementById("serverLink").onclick = function () { window.open("http://" + obj.link); };
          break;
        case "serverStopped":
          document.getElementById("startServer").disabled = false;
          document.getElementById("stopServer").disabled = true;
          document.getElementById("resetDevice").disabled = true;
          document.getElementById("serverLink").disabled = true;
          document.getElementById("serverLink").innerText = 'Server Link';
          break;
      }
    }
    else {
      if (obj[0].hasOwnProperty('action')) {
        if (obj[0].action === 'filetree') {
          showBrowser(obj[1]);
        }
        else if (obj[0].action === 'populateSemantics') {
          for (i = 1; i < obj.length; i++) {
            var group;
            if (obj[i].hasOwnProperty('traditional')) { group = document.getElementById("traditionalSemantics"); }
            else if (obj[i].hasOwnProperty('enclosure')) { group = document.getElementById("enclosureSemantics"); }
            else { group = document.getElementById("deprecatedSemantics"); }
            for (var j in obj[i]) {
              for (var k in obj[i][j]) {
                if (obj[i][j][k].id !== "separator") {
                  let listItem = document.createElement("li");
                  let label = document.createElement("label");
                  let input = document.createElement("input");
                  input.setAttribute('type', "radio");
                  input.setAttribute('name', 'semantics');
                  let tempID = obj[i][j][k].id;
                  let isEnclosure = obj[i][j][k].isEnclosure;
                  input.onclick = function () {
                    connection.send("[" + JSON.stringify(setSemantics(tempID)) + "]\r");
                    if (isEnclosure) { toggleContraction(true); }
                    else { toggleContraction(false); }
                  }
                  if (obj[i][j][k].selected) input.checked = true;
                  let textnode = document.createTextNode(obj[i][j][k].name);
                  label.appendChild(input);
                  label.appendChild(textnode);
                  listItem.appendChild(label);
                  group.appendChild(listItem);
                  enabledWhenStopped.push(input);
                  enabledWhenStopped.push(label);
                }
                else {
                  group.appendChild(document.createElement("hr"));
                }
              }
              if (obj[i].hasOwnProperty('enclosure')) {
                let contrLabel = document.createElement("label");
                let contrInput = document.createElement("input");
                contrInput.setAttribute('type', "checkbox");
                contrInput.setAttribute('id', 'contraction');
                contrInput.disabled = true;
                contrInput.onclick = function () {
                  if (this.checked == true) { contractionAction.value = 'true'; }
                  else { contractionAction.value = 'false'; }
                  connection.send("[" + JSON.stringify(contractionAction) + "]\r");
                };
                let contrSpan = document.createElement("span");
                contrSpan.innerHTML = 'Contraction';
                contrLabel.appendChild(contrInput);
                contrLabel.appendChild(contrSpan);
                group.appendChild(document.createElement("hr"));
                group.appendChild(contrLabel);
                enabledWhenStopped.push(contrLabel);
              }
            }
          }
        }
      }
      else if (obj[0].hasOwnProperty('event')) {
        if (obj[0].event === "traceTable") {
          var table = document.getElementById("traceTable");
          while (table.hasChildNodes()) {
            table.removeChild(table.firstChild);
          }
          for (i = 0; i < obj[1].length; i++) {
            var rowNode = document.createElement("TR");
            for (var j in obj[1][i]) {
              var columnNode;
              if (i == 0) columnNode = document.createElement("TH");
              else columnNode = document.createElement("TD");
              var textnode = document.createTextNode(obj[1][i][j]);
              columnNode.appendChild(textnode);
              rowNode.appendChild(columnNode);
            }
            table.appendChild(rowNode);
          }
        }
      }
    }
  }
  catch (error) {
    // FIXME Sometime when data are sent in a small period of time, sockets do not intercept different messages.
    // var split = event.data.split(/\r/g);
    console.error(error + "\nData was: " + event.data);
  }
}

connection.onerror = error => {
  console.log(`WebSocket error: ${error}`)
}

/** Action when user closes the browser window */
window.onbeforeunload = function () {
  if (editedSinceLastSave) return "You have unsaved data. Please check before closing the window.";
  connection.send("[" + JSON.stringify(exitAction) + "]\r");
}

var editor = null;

/** Prompts and Dialogs */
var confirmContinue = new function () {
  this.show = function (type) {
    var scButton = document.getElementById('saveAndContinue');
    var dcButton = document.getElementById('discardAndContinue');
    switch (type) {
      case 'save':
        scButton.setAttribute("onClick", "javascript: getResponse('save', 'newAction');");
        dcButton.setAttribute("onClick", "javascript: getResponse('discard', 'newAction');");
        break;
      case 'open':
        scButton.setAttribute("onClick", "javascript: getResponse('save', 'openAction');");
        dcButton.setAttribute("onClick", "javascript: getResponse('discard', 'openAction');");
        break;
    }
    document.getElementById('promptPanel').style.display = '';
  }
  this.close = function () {
    document.getElementById('promptPanel').style.display = 'none';
  }
}

function getResponse(res, type) {
  switch (res) {
    case 'save':
      connection.send("[" + JSON.stringify(saveFile(false)) + "]\r");
      if (type === 'newAction') { connection.send("[" + JSON.stringify(newAction) + "]\r"); }
      else if (type === 'openAction') { connection.send("[" + JSON.stringify(openAction) + "]\r"); }
      editedSinceLastSave = false;
      editor.focus();
      break;
    case 'discard':
      if (type === 'newAction') { connection.send("[" + JSON.stringify(newAction) + "]\r"); }
      else if (type === 'openAction') { connection.send("[" + JSON.stringify(openAction) + "]\r"); }
      editedSinceLastSave = false;
      editor.focus();
      break;
    case 'cancel':
      confirmContinue.close();
      return false;
  }
  confirmContinue.close();
}

/** Assign values after browser finished loading the page */
window.onload = function () {
  populateFontMenu();
  populateThemeMenu();
  document.getElementById("newAction").onclick = function () {
    if (editedSinceLastSave) { confirmContinue.show('save'); }
    else {
      connection.send("[" + JSON.stringify(newAction) + "]\r");
      editor.focus();
    }
  };
  document.getElementById("openAction").onclick = function () {
    if (editedSinceLastSave) { confirmContinue.show('open'); }
    else {
      connection.send("[" + JSON.stringify(openAction) + "]\r");
      editor.focus();
    }
  };
  document.getElementById("saveAction").onclick = function () {
    connection.send("[" + JSON.stringify(saveFile(true)) + "]\r");
  };
  document.getElementById("saveAsAction").onclick = function () {
    connection.send("[" + JSON.stringify(saveFileAs(true)) + "]\r");
  };
  document.getElementById("recoverAction").onclick = function () {
    connection.send("[" + JSON.stringify(recoverAction) + "]\r");
  };
  document.getElementById("exportAction").setAttribute("onClick", "javascript: exportTable();");
  document.getElementById("undoAction").onclick = function () {
    editor.undo();
  };
  document.getElementById("redoAction").onclick = function () {
    editor.redo();
  };
  document.getElementById("cutAction").onclick = function () {
    editor.focus();
    document.execCommand('cut');
  };
  document.getElementById("copyAction").onclick = function () {
    editor.focus();
    document.execCommand('copy');
  };
  document.getElementById("pasteAction").onclick = function () {
    editor.focus();
    document.execCommand('paste');
  };
  document.getElementById("showFind").onclick = function () {
    if (this.checked == true) {
      if (editor.search == undefined) { editor.execCommand("find"); } else { editor.searchBox.show(); }
    }
    else { editor.searchBox.hide(); }
  };
  document.getElementById("incIndentAction").onclick = function () {
    editor.indent();
  };
  document.getElementById("decIndentAction").onclick = function () {
    editor.blockOutdent();
  };
  document.getElementById("selectAllAction").onclick = function () {
    editor.selectAll();
  };
  document.getElementById("increaseFontSize").onclick = function () {
    changeFontSize(document.getElementById("editor"), 2);
  };
  document.getElementById("resetFontSize").onclick = function () {
    document.getElementById("editor").style.fontSize = '12px';
  };
  document.getElementById("reduceFontSize").onclick = function () {
    changeFontSize(document.getElementById("editor"), (-2));
  };
  document.getElementById("lineNumbers").onclick = function () {
    if (this.checked == true) { editor.renderer.setShowGutter(true); }
    else { editor.renderer.setShowGutter(false); }
  };
  document.getElementById("simulatorFields").onclick = function () {
    if (this.checked == true) { simulatorFieldsAction.value = 'true'; }
    else { simulatorFieldsAction.value = 'false'; }
    connection.send("[" + JSON.stringify(simulatorFieldsAction) + "]\r");
  };
  document.getElementById("childCount").onclick = function () {
    if (this.checked == true) { childCountAction.value = 'true'; }
    else { childCountAction.value = 'false'; }
    connection.send("[" + JSON.stringify(childCountAction) + "]\r");
  };
  document.getElementById("rngSeeds").onclick = function () {
    if (this.checked == true) { rngSeedsAction.value = 'true'; }
    else { rngSeedsAction.value = 'false'; }
    connection.send("[" + JSON.stringify(rngSeedsAction) + "]\r");
  };
  document.getElementById("normalization").onclick = function () {
    if (this.checked == true) { normalizationAction.value = 'true'; }
    else { normalizationAction.value = 'false'; }
    connection.send("[" + JSON.stringify(normalizationAction) + "]\r");
  };
  document.getElementById("startServer").onclick = function () {
    connection.send("[" + JSON.stringify(startServerAction) + "]\r");
  };
  document.getElementById("stopServer").onclick = function () {
    connection.send("[" + JSON.stringify(stopServerAction) + "]\r");
  };
  document.getElementById("resetDevice").onclick = function () {
    connection.send("[" + JSON.stringify(resetDeviceAction) + "]\r");
  };
  document.getElementById("serverLink").onclick = function () {
    connection.send("[" + JSON.stringify(resetDeviceAction) + "]\r");
  };
  document.getElementById("playButton").onclick = function () {
    connection.send("[" + JSON.stringify(playAction) + "]\r");
  };
  document.getElementById("pauseButton").onclick = function () {
    connection.send("[" + JSON.stringify(pauseAction) + "]\r");
  };
  document.getElementById("stepButton").onclick = function () {
    connection.send("[" + JSON.stringify(stepAction) + "]\r");
  };
  document.getElementById("stopButton").onclick = function () {
    connection.send("[" + JSON.stringify(stopAction) + "]\r");
  };
  document.getElementById("consoleButton").onclick = function () {
    changeCTab('consoleTab', event);
  };
  document.getElementById("browserButton").onclick = function () {
    changeCTab('browserTab', event);
  };
  document.getElementById("plotButton").onclick = function () {
    changeRTab('plotTab', event);
  };
  document.getElementById("traceButton").onclick = function () {
    connection.send("[" + JSON.stringify(traceButton) + "]\r");
    changeRTab('traceTab', event);
  };
  document.getElementById("threeDButton").onclick = function () {
    changeRTab('threeDtab', event);
  };
  document.getElementById("editor").addEventListener('input', function () {
    if (!editedSinceLastSave) {
      editedSinceLastSave = true;
      document.getElementById("fileNameLabelText").innerHTML += " (unsaved)";
    }
  });
  enabledWhenStopped = [
    document.getElementById("newAction"),
    document.getElementById("openAction"),
    document.getElementById("exportAction"),
    document.getElementById("recoverAction")
  ];
  editor = ace.edit("editor");
  editor.setTheme("ace/theme/dreamweaver");
  editor.session.setMode("ace/mode/acumen");
  editor.setOption('cursorStyle', 'smooth');
  editor.session.setOptions({ tabSize: 2, useSoftTabs: true });
  editor.on("input", updateInput);
}

/** Helper Functions */
function populateFontMenu() {
  let fonts = ["Monaco", "Menlo", "Ubuntu Mono", "Monospace"];
  menuNode = document.getElementById("fontMenu");
  editorNode = document.getElementById("editor");
  for (i in fonts) {
    let node = document.createElement("li");
    let label = document.createElement("label");
    let input = document.createElement("input");
    let text = document.createTextNode(fonts[i]);
    input.setAttribute('type', "radio");
    input.setAttribute('name', 'font');
    input.onclick = function () { editorNode.style.fontFamily = fonts[i]; }
    if (i == 0) { input.checked = true; }
    label.appendChild(input);
    label.appendChild(text);
    node.appendChild(label);
    menuNode.appendChild(node);
  }
}

function populateThemeMenu() {
  let themes = ["dreamweaver", "textMate", "ambiance", "dracula"];
  menuNode = document.getElementById("themeMenu");
  for (i in themes) {
    let node = document.createElement("li");
    let label = document.createElement("label");
    let input = document.createElement("input");
    let text = document.createTextNode(themes[i]);
    input.setAttribute('type', "radio");
    input.setAttribute('name', 'theme');
    input.onclick = function () { editor.setTheme("ace/theme/" + themes[i]); }
    if (i == 0) { input.checked = true; }
    label.appendChild(input);
    label.appendChild(text);
    node.appendChild(label);
    menuNode.appendChild(node);
  }
}

function updateInput() {
  document.getElementById("undoAction").disabled = !editor.session.getUndoManager().hasUndo();
  document.getElementById("redoAction").disabled = !editor.session.getUndoManager().hasRedo();
  connection.send("[" + JSON.stringify(getCode()) + "]\r");
}

function changeFontSize(node, value) {
  style = window.getComputedStyle(node, null).getPropertyValue('font-size');
  currentSize = parseFloat(style);
  node.style.fontSize = (currentSize + value) + 'px';
}

function toggleContraction(value) {
  if (value) {
    document.getElementById("contraction").disabled = false;
  }
  else {
    document.getElementById("contraction").disabled = true;
  }
}

function stateChanged(state) {
  if (state === "Starting") {
    var table = document.getElementById("traceTable");
    while (table.hasChildNodes()) {
      table.removeChild(table.firstChild);
    }
  }
  if (state === "Stopped") {
    document.getElementById("stopButton").disabled = true;
    document.getElementById("stopMenuButton").disabled = true;
    editor.setReadOnly(false);
    for (var i in enabledWhenStopped) {
      if (enabledWhenStopped[i].tagName === 'BUTTON' || enabledWhenStopped[i].tagName === 'INPUT') {
        enabledWhenStopped[i].disabled = false;
      }
      else {
        enabledWhenStopped[i].style.color = 'white';
        enabledWhenStopped[i].style.cursor = 'auto'
      }
    }
  }
  else {
    document.getElementById("stopButton").disabled = false;
    document.getElementById("stopMenuButton").disabled = false;
    editor.setReadOnly(true);
    for (var i in enabledWhenStopped) {
      if (enabledWhenStopped[i].tagName === 'BUTTON' || enabledWhenStopped[i].tagName === 'INPUT') {
        enabledWhenStopped[i].disabled = true;
      }
      else {
        enabledWhenStopped[i].style.color = 'grey';
        enabledWhenStopped[i].style.cursor = 'not-allowed'
      }
    }
  }
  if (state === "Stopped" || state === "Paused") {
    document.getElementById("stepButton").disabled = false;
    document.getElementById("stepMenuButton").disabled = false;
  }
  else {
    document.getElementById("stepButton").disabled = true;
    document.getElementById("stepMenuButton").disabled = true;

  }
  switch (state) {
    case "Starting": case "Resuming":                                         // Simulates class 'Playing'
      document.getElementById("playButton").style.display = 'none';
      document.getElementById("playMenuButton").style.display = 'none';
      document.getElementById("pauseButton").style.display = '';
      document.getElementById("pauseMenuButton").style.display = '';
      break;
    case "Stopped": case "Paused":                                            // Simulates class 'Ready'
      document.getElementById("playButton").style.display = '';
      document.getElementById("playMenuButton").style.display = '';
      document.getElementById("pauseButton").style.display = 'none';
      document.getElementById("pauseMenuButton").style.display = 'none';
      break;
  }
}

function exportTable() {
  var html = document.getElementById("traceTable");
  var csv = [];
  var rows = document.querySelectorAll("table tr");

  for (var i = 0; i < rows.length; i++) {
    var row = [], cols = rows[i].querySelectorAll("td, th");

    for (var j = 0; j < cols.length; j++)
      row.push(cols[j].innerText);

    csv.push(row.join(","));
  }

  // Download CSV
  download_csv(csv.join("\n"), 'table.csv');
}

function download_csv(csv, filename) {
  var csvFile;
  var downloadLink;
  csvFile = new Blob([csv], { type: "text/csv" });
  downloadLink = document.createElement("a");
  downloadLink.download = filename;
  downloadLink.href = window.URL.createObjectURL(csvFile);
  downloadLink.style.display = "none";
  document.body.appendChild(downloadLink);
  downloadLink.click();
}

function showBrowser(file) {
  if (file.hasOwnProperty('children') && file.children.length > 0) {
    for (i = 0; i < file.children.length; i++) {
      let folderNode = document.createElement("li");
      let span = document.createElement('span');
      span.innerHTML = file.children[i].name;
      span.className = 'caret';
      folderNode.appendChild(span);
      let nestedNode = document.createElement("ul");
      nestedNode.className = 'nestedNode';
      folderNode.appendChild(nestedNode);
      createChildNodes(file.children[i], nestedNode)
      document.getElementById("browserAreaList").appendChild(folderNode);
    }
  }

  // Show/Hide folders
  var toggler = document.getElementsByClassName("caret");
  var i;
  for (i = 0; i < toggler.length; i++) {
    toggler[i].addEventListener("click", function () {
      this.parentElement.querySelector(".nestedNode").classList.toggle("active");
      this.classList.toggle("caret-down");
    });
  }
}

function createChildNodes(file, parentNode) {
  if (file.hasOwnProperty('children') && file.children.length > 0) {
    for (let j = 0; j < file.children.length; j++) {
      if (file.children[j].hasOwnProperty('children')) {
        let folderNodeC = document.createElement("li");
        let spanC = document.createElement('span');
        spanC.innerHTML = file.children[j].name;
        spanC.className = 'caret';
        folderNodeC.appendChild(spanC);
        let nestedNodeC = document.createElement("ul");
        nestedNodeC.className = 'nestedNode';
        if (file.children[j].children.length > 0) {
          createChildNodes(file.children[j], nestedNodeC);
        }
        folderNodeC.appendChild(nestedNodeC);
        parentNode.appendChild(folderNodeC);
      }
      else {
        let fileNodeC = document.createElement("li");
        fileNodeC.id = '[ID]' + file.children[j].id;
        fileNodeC.addEventListener("click", function () { connection.send("[" + JSON.stringify(selectFile(fileNodeC.id.substring(4))) + "]\r") });
        let textC = document.createTextNode(file.children[j].name);
        fileNodeC.style.cursor = "pointer";
        fileNodeC.appendChild(textC);
        parentNode.appendChild(fileNodeC);
      }
    }
  }
  else {
    let fileNode = document.createElement("li");
    fileNode.id = '[ID]' + file.id;
    fileNode.addEventListener("click", function () { console.log(fileNode.id) });
    let text = document.createTextNode(file.name);
    fileNode.appendChild(text);
  }
}

/** Json and miscallenious objects */
var enabledWhenStopped = [];
var editedSinceLastSave = false;

var newAction = {
  type: 'action',
  action: 'newFile'
}

var openAction = {
  type: 'action',
  action: 'openFile'
}

function selectFile(fileID) {
  var file = {
    type: 'action',
    action: 'SelectFile',
    file: parseInt(fileID)
  }
  return file;
}

function saveFile(updateCurrentFile) {
  var saveAction = {
    type: 'action',
    action: 'saveFile',
    updateCurrent: updateCurrentFile
  }
  return saveAction;
}

function saveFileAs(updateCurrentFile) {
  var saveAction = {
    type: 'action',
    action: 'saveFileAs',
    updateCurrent: updateCurrentFile
  }
  return saveAction;
}

var recoverAction = {
  type: 'action',
  action: 'recover'
}

var simulatorFieldsAction = {
  type: 'action',
  action: 'simulatorFields',
  value: ''
}

var childCountAction = {
  type: 'action',
  action: 'childCount',
  value: ''
}

var contractionAction = {
  type: 'action',
  action: 'contraction',
  value: ''
}

var rngSeedsAction = {
  type: 'action',
  action: 'rngSeeds',
  value: ''
}

var normalizationAction = {
  type: 'action',
  action: 'normalization',
  value: ''
}

function getCode() {
  var codeUpdate = {
    type: 'codeUpdate',
    text: editor.getValue()
  }
  return codeUpdate;
}

function setSemantics(semanticsID) {
  var semanticsAction = {
    type: 'action',
    action: 'setSemantics',
    semantics: semanticsID
  }
  return semanticsAction;
}

var startServerAction = {
  type: 'action',
  action: 'startServer'
}

var stopServerAction = {
  type: 'action',
  action: 'stopServer'
}

var resetDeviceAction = {
  type: 'action',
  action: 'resetDevice'
}

var playAction = {
  type: 'action',
  action: 'Play'
}

var pauseAction = {
  type: 'action',
  action: 'Pause'
}

var stepAction = {
  type: 'action',
  action: 'Step'
}

var stopAction = {
  type: 'action',
  action: 'Stop'
}

var exitAction = {
  type: 'action',
  action: 'Exit'
}

var traceButton = {
  type: 'btnAction',
  action: 'traceTab'
}

/** Other Functions */
function changeRTab(tabName, evt) {
  var i, tabcontent, tablinks;
  tabcontent = document.getElementsByClassName("vtabcontent");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";
  }
  tablinks = document.getElementsByClassName("vtablinks");
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className.replace(" active", "");
  }
  document.getElementById(tabName).style.display = "flex";
  if (evt !== undefined) { evt.currentTarget.className += " active"; }
}

function changeCTab(tabName, evt) {
  var i, tabcontent, tablinks;
  tabcontent = document.getElementsByClassName("ctabcontent");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";
  }
  tablinks = document.getElementsByClassName("ctablinks");
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className.replace(" active", "");
  }
  document.getElementById(tabName).style.display = "block";
  if (evt !== undefined) { evt.currentTarget.className += " active"; }
}
