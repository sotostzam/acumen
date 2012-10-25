package acumen
package ui

sealed abstract class AppState

object AppState {
  sealed abstract class Ready extends AppState
  sealed abstract class Playing extends AppState
  case object Starting extends Playing
  case object Resuming extends Playing
  case object Stopped extends Ready
  case object Paused extends Ready
}
