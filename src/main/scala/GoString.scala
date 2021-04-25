package main

import Player._

case class GoString(
    val color: Player,
    val stones: Set[Point],
    val liberties: Set[Point]
) {
  def withoutLiberty(point: Point): GoString = {
    new GoString(color, stones, liberties - point)
  }
  def withLiberty(point: Point): GoString = {
    new GoString(color, stones, liberties + point)
  }
  def mergedWith(other: GoString): GoString = {
    assert(color == other.color)
    val combinedStones = this.stones ++ other.stones
    new GoString(
      color,
      combinedStones,
      liberties ++ other.liberties -- combinedStones
    )
  }
  def numLiberties(): Int = {
    liberties.size
  }

  override def toString = s"$color: [$stones]/[$liberties]"
}
