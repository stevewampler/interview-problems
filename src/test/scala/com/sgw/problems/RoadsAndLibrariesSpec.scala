package com.sgw.problems

import com.sgw.utils.Timer
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class RoadsAndLibrariesSpec extends FlatSpec with Matchers with BeforeAndAfter {
  "The RoadsAndLinks object" should "correctly calculate the cost of building roads and libraries" in {
    println {
      Timer {
        RoadsAndLibraries.runFromInputStream(
          this.getClass.getResourceAsStream("/problems/RoadsAndLibrariesSpecInput00.txt")
        ) should be (
          Seq(
            4,
            12
          )
        )
      }
    }

    println {
      Timer {
        RoadsAndLibraries.runFromInputStream(
          this.getClass.getResourceAsStream("/problems/RoadsAndLibrariesSpecInput01.txt")
        ) should be (
          Seq(
            12
          )
        )
      }
    }

    println {
      Timer {
        RoadsAndLibraries.runFromInputStream(
          this.getClass.getResourceAsStream("/problems/RoadsAndLibrariesSpecInput02.txt")
        ) should be (
          Seq(
            15
          )
        )
      }
    }

    println {
      Timer {
        RoadsAndLibraries.runFromInputStream(
          this.getClass.getResourceAsStream("/problems/RoadsAndLibrariesSpecInput03.txt")
        ) should be(
          Seq(
            7850257285L,
            6785201034L,
            813348013L,
            4211840970L,
            8610471142L,
            7263742960L,
            4331105640L,
            1226092626L,
            7288635830L,
            8276704464L
          )
        )
      }
    }

    println {
      Timer {
        RoadsAndLibraries.runFromInputStream(
          this.getClass.getResourceAsStream("/problems/RoadsAndLibrariesSpecInput04.txt")
        ) should be (
          Seq(
            9234981465L,
            5854508506L,
            7754252297L,
            8085193494L,
            9504556779L,
            8011172848L,
            9123393445L,
            7326423794L,
            8259748808L,
            8049633228L
          )
        )
      }
    }
  }
}
