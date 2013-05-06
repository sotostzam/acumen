package acumen.ui

import java.net.URL
import javax.swing.ImageIcon
import javax.swing.Icon

object Icons {
  
  /* Simulation control*/
  private val playUrl    = this.getClass.getResource("icons/play.png")
  private val stepUrl    = this.getClass.getResource("icons/step.png")
  private val stopUrl    = this.getClass.getResource("icons/stop.png")
  private val pauseUrl   = this.getClass.getResource("icons/pause.png")
  /* Plotter */          
  private val homeUrl    = this.getClass.getResource("icons/home.png")
  private val zoomInUrl  = this.getClass.getResource("icons/zoomin.png")
  private val zoomOUtUrl = this.getClass.getResource("icons/zoomout.png")
  private val undoUrl    = this.getClass.getResource("icons/undo.png")
  private val saveUrl    = this.getClass.getResource("icons/save.png")
  private val replayUrl  = this.getClass.getResource("icons/replay.png")
  private val fasterUrl  = this.getClass.getResource("icons/faster.png")
  private val slowerUrl  = this.getClass.getResource("icons/slower.png")
  /* File browser */
  private val goUpUrl    = this.getClass.getResource("icons/goup.png")
  private val goIntoUrl  = this.getClass.getResource("icons/gointo.png")
  private val syncUrl    = this.getClass.getResource("icons/sync.png")
  
  private val default = new ImageIcon()
  private def get(u: URL) : Icon = {
    if (u == null) default
    else new ImageIcon(u)
  }

  lazy val play    = get(playUrl)
  lazy val step    = get(stepUrl)
  lazy val pause   = get(pauseUrl)
  lazy val stop    = get(stopUrl)
  lazy val home    = get(homeUrl)
  lazy val undo    = get(undoUrl)
  lazy val save    = get(saveUrl)
  lazy val zoomIn  = get(zoomInUrl)
  lazy val zoomOut = get(zoomOUtUrl)
  lazy val replay  = get(replayUrl)
  lazy val faster  = get(fasterUrl)
  lazy val slower  = get(slowerUrl)
  lazy val goUp    = get(goUpUrl)
  lazy val goInto  = get(goIntoUrl)
  lazy val sync    = get(syncUrl)
  
}
