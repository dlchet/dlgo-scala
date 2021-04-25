package main

import org.scalatest.wordspec._
import Player._

class GoStringSpec extends AnyWordSpec {
  "a go string" should {
    "be able to merge" in {
      val point    = new Point(1, 1)
      val oneTwo   = new Point(1, 2)
      val goString = new GoString(Black, Set(point), Set[Point]())
      assert(
        goString.mergedWith(
          new GoString(Black, Set(oneTwo), Set[Point]())
        ) == new GoString(Black, Set(point, oneTwo), Set[Point]())
      )
    }
  }
}
