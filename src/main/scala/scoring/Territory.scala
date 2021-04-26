package scoring

import main.Player._
import main.Point
import main.Board

class Territory(board: Board) {

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  private def _collectRegion(
      point: Point,
      visited: Set[Point] = Set[Point]()
  ): (Set[Point], Set[Player]) = {
    if (visited.contains(point)) return (Set[Point](), Set[Player]())
    else {
      val (points, borders, _) = point.neighbors.foldLeft(
        (Set(point), Set[Player](), visited + point)
      ) { case ((ps, bs, vs), p) =>
        if (!board.isOnGrid(p)) (ps, bs, vs)
        else
          board.get(p) match {
            case None => {
              val (newPoints, newBorders) = _collectRegion(p, vs)
              (ps ++ newPoints, bs ++ newBorders, vs + p)
            }
            case Some(pl) => (ps, bs + pl, vs)
          }
      }
      return (points, borders)
    }
  }

  val (
    numBlackTerritory,
    numWhiteTerritory,
    numBlackStones,
    numWhiteStones,
    numDame,
    damePoints,
    _
  ) = (1 to board.numRows).toList
    .cross((1 to board.numCols).toList)
    .foldLeft((0, 0, 0, 0, 0, Set[Point](), Set[Point]())) {
      case ((bt, wt, bs, ws, d, dp, v), (r, c)) =>
        val point = new Point(r, c)
        point match {
          case p if v.contains(p) => (bt, wt, bs, ws, d, dp, v)
          case p =>
            board.get(p) match {
              case Some(value) =>
                if (value == Black) (bt, wt, bs + 1, ws, d, dp, v + p)
                else (bt, wt, bs, ws + 1, d, dp, v + p)
              case None => {
                val (points, borders) = _collectRegion(p)
                if (borders.size == 1) borders.head match {
                  case Black =>
                    (bt + points.size, wt, bs, ws, d, dp, v ++ points)
                  case White =>
                    (bt, wt + points.size, bs, ws, d, dp, v ++ points)
                }
                else
                  (bt, wt, bs, ws, d + points.size, dp ++ points, v ++ points)
              }
            }
        }
    }
}
