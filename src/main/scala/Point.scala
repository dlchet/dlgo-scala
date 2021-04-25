package main

case class Point(val row: Int, val col: Int) {
  override def toString = s"($row, $col)"
  def neighbors = {
    List(row + 1, row - 1)
      .map(new Point(_, col))
      .toList ++ List(col + 1, col - 1).map(new Point(row, _))
  }
}
