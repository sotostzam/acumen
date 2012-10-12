package acumen
package ui

import AppState._

object AppState {
  sealed abstract class State
  case object Stopped extends State
  case object Playing extends State
  case object Paused extends State

  def apply(st: State) = new AppState(st)
}

class AppState(val state : State, val canStep : Boolean = true) {
  def stopEnabled =
    state match {
      case Paused | Playing => true
      case _ => false
    }

  def playEnabled =
    state match {
      case Paused | Stopped => true
      case _ => false
    }

  def pauseEnabled =
    state match {
      case Playing if canStep => true
      case _ => false
    }

  def stepEnabled =
    state match {
      case (Stopped | Paused) if canStep => true
      case _ => false
    }

  def codeEnabled =
    state match {
      case Stopped => true
      case _ => false
    }

}
