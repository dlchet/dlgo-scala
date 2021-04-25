package main

sealed trait Move
case object Pass                  extends Move
case object Resign                extends Move
case class Play(val point: Point) extends Move
