package main

import org.scalatest.wordspec._
import scala.collection.immutable.HashMap

class PointSpec extends AnyWordSpec {
  "a point on a go board" should {
    "have a col matching its constructor col" in {
      assert(new Point(4, 9).col == 9)
    }
    "have a row matching its constructor row" in {
      assert(new Point(4, 9).row == 4)
    }
    "equal same point re-constructed" in {
      assert(new Point(1, 2) == new Point(1, 2))
    }
    "be contained by a hashmap with the point added to it" in {
      val point    = new Point(1, 2)
      val pointMap = HashMap((point, true))
      assert(pointMap.keySet.head == new Point(1, 2))
      assert(pointMap.contains(new Point(1, 2)))
    }
  }
}
