package main

import Player._
import ZobristHashes._
import scala.collection.immutable.HashMap

class Board(
    val numRows: Int,
    val numCols: Int,
    withGrid: HashMap[Point, GoString],
    withHash: Long
) {
  private var _grid = withGrid
  private var _hash = withHash

  def grid        = _grid
  def zobristHash = _hash

  override def toString(): String = {
    s"$grid"
  }
  override def equals(obj: Any): Boolean = obj match {
    case o: Board =>
      grid == o.grid && numCols == o.numCols && numRows == o.numRows
    case _ => false
  }
  def this(numRows: Int, numCols: Int) {
    this(numRows, numCols, HashMap[Point, GoString](), emptyBoard)
  }
  def copy() = {
    new Board(
      numRows,
      numCols,
      new HashMap[Point, GoString]() ++ grid.map { case ((p, s)) =>
        (p, s.copy())
      }.toMap,
      zobristHash
    )
  }

  def placeStone(player: Player, point: Point): Unit = {
    assert(isOnGrid(point))
    assert(get(point) == None)
    val neighbors = point.neighbors.filter(isOnGrid(_))
    val liberties = neighbors.filterNot(_grid.contains(_))
    val newString = new GoString(
      player,
      Set(point),
      liberties.toSet
    )
    val allGridStrings =
      neighbors.filter(_grid.contains(_)).map(_grid(_))
    val sameColorStrings =
      allGridStrings.filter(_.color == player) :+ newString
    val otherColorStrings = allGridStrings
      .filter(_.color == player.other)
    val mergedString = sameColorStrings.reduce((l, r) => l.mergedWith(r))
    _grid = _grid ++ mergedString.stones
      .map((_, mergedString))
      .toMap
    otherColorStrings foreach (s => {
      val replacement = s.withoutLiberty(point)
      if (replacement.liberties.size == 0) _removeString(s)
      else _replaceString(replacement)
    })

    _hash ^= hashCodeMap((point, player))
  }

  private def _removeString(string: GoString): Unit = {
    string.stones foreach (p => {
      p.neighbors foreach {
        case n if _grid.contains(n) =>
          if (string != _grid(n)) _replaceString(_grid(n).withLiberty(p))
        case _ =>
      }
      _grid = _grid - p
      _hash ^= hashCodeMap((p, string.color))
    })
  }

  private def _replaceString(string: GoString): Unit = {
    _grid = _grid ++ string.stones.map((_, string)).toMap
  }

  def get(point: Point): Option[Player] = {
    point match {
      case p if _grid.contains(p) => Some(_grid(p).color)
      case _                      => None
    }
  }
  def getGoString(point: Point): Option[GoString] = {
    point match {
      case p if _grid.contains(p) => Some(_grid(p))
      case _                      => None
    }
  }
  def isOnGrid(point: Point): Boolean = {
    point match {
      case p
          if p.row <= numRows && p.col <= numCols && p.row > 0 && p.col > 0 =>
        true
      case _ => false
    }
  }
}
