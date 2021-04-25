package main

object Player extends Enumeration {
  type Player = Value
  val Black = Value(1)
  val White = Value(2)

  implicit class PlayerValue(player: Value) {
    def other = player match {
      case Black => White
      case _     => Black
    }
  }
}
