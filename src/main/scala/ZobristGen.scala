package main

import Player._
import scala.util.Random
import java.lang.Long.parseLong

object ZobristGen extends App {
  def toScala(state: Player): String = state.toString()
  val MAX63                          = parseLong("7fffffffffffffff", 16)
  val table = (1 to 19)
    .flatMap(r =>
      (1 to 19).flatMap(c =>
        Seq(Black, White).map(s =>
          ((new Point(r, c), s), Random.between(0, MAX63))
        )
      )
    )
    .toMap
  val emptyBoard = 0

  println("package main")
  println("")
  println("import Player._")
  println("import java.lang.Long.parseLong")
  println("")
  println("object ZobristHashes {")
  println("  val hashCodeMap = Seq(")
  print(table map { case ((pt, state), hashCode) =>
    s"""    ((new Point(${pt.row}, ${pt.col}), $state), parseLong("$hashCode"))"""
  } mkString (",\n"))
  println("")
  println("  ).toMap")
  println(s"  val emptyBoard = $emptyBoard")
  println("}")
}
