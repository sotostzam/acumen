package acumen.ui.tl;

import java.awt.Color;
import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

import acumen.ui.App;
import acumen.ui.SwingUtil;
import acumen.ui.SwingUtil.Flasher;

public class Console extends JList {

  private int oldEntries = 0;
  private boolean done = false;

  Flasher consoleFlasher = new Flasher();
  Color initColor = getSelectionBackground();
  private DefaultListModel model = new DefaultListModel();

  public Console() {
    setModel(model);
  }

  @Override
  public ListCellRenderer getCellRenderer() {
    return new ConsoleCellRenderer();
  }

  class ConsoleCellRenderer implements ListCellRenderer {
    DefaultListCellRenderer defaultLCR = new DefaultListCellRenderer();

    @Override
    public Component getListCellRendererComponent(
        JList list, Object value, int index,
        boolean isSelected, boolean cellHasFocus) {
      boolean messageIsOld = index >= oldEntries;
      String m = ((ConsoleMessage) value).message;
      String message = value instanceof NormalMessage ? m : "<html>"
          + (messageIsOld ? "ERROR:" : "<font color=red>ERROR:</font>")
          + "<pre>"
          + (m.replaceAll("<", "&lt;")
              .replaceAll(">", "&gt;")
              .replaceAll("\n","<br/>")) + 
            "</pre></html>";
      Component renderer = defaultLCR.getListCellRendererComponent(list,message, index, false, false);
      if (messageIsOld)
        renderer.setForeground(Color.LIGHT_GRAY);
      return renderer;
    }

  }

  public void log(String message) {
    setSelectionBackground(initColor);
    if (done) {
      logMessage(new NormalMessage(message));
      done = false;
    } else if (model.isEmpty()) {
      model.addElement(new NormalMessage(message));
    } else {
      ConsoleMessage m = (ConsoleMessage)model.get(0);
      m.message = m.message + message;
    }
  }

  public void logError(String message) {
    logMessage(new ErrorMessage(message));
    done = true;
    SwingUtil.flashFunction(
      App.ui().consolePageBackground(), Color.WHITE, Color.RED, consoleFlasher);
  }

  public void newLine() {
    done = true;
  }

  public void fadeOldMessages() {
    oldEntries = 0;
  }

  private void logMessage(ConsoleMessage m) {
    oldEntries += 1;
    model.add(0, m);
  }

}

abstract class ConsoleMessage {
  String message;
}

class NormalMessage extends ConsoleMessage {
  public NormalMessage(String message) {
    this.message = message;
  }
}

class ErrorMessage extends ConsoleMessage {
  public ErrorMessage(String message) {
    this.message = message;
  }
}