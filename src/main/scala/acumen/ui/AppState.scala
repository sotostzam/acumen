package acumen
package ui

import AppState._

object AppState {
  sealed abstract class State
  sealed abstract class Ready extends State
  sealed abstract class Playing extends State
  case object Starting extends Playing
  case object Resuming extends Playing
  case object Stopped extends Ready
  case object Paused extends Ready

  def apply(st: State) = new AppState(st)
}

class AppState(val state : State, val canStep : Boolean = true) {
  def stopEnabled =
    state match {
      case Stopped => false
      case _ => true
    }

  def playEnabled =
    state match {
      case _:Playing => false
      case _ => true
    }

  def pauseEnabled =
    state match {
      case _:Playing if canStep => true
      case _ => false
    }

  def stepEnabled =
    state match {
      case _:Ready if canStep => true
      case _ => false
    }

  def codeEnabled =
    state match {
      case Stopped => true
      case _ => false
    }

}
