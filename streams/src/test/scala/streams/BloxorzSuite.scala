package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._
import scala._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level0 extends SolutionChecker {
    val level =
      """ST
        |oo
        |oo""".stripMargin
  }

  trait InifiteLevel extends GameDef with Solver with InfiniteTerrain {

  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


  test("neighborsWithHistory") {
    new Level1 {
      val res = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      val expected = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      )
     assert(res.toSet == expected)
    }
  }

  test("NewNeighborsOnly") {
    new Level1 {
      val res0 = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )
      val expected = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      assert(res0 == expected)
    }
  }

  test("avoid circles") {
    new Level1 {
      val neighbors = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )
      val result = Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream
      assert(neighbors == result)
    }
  }

  ignore("From Test 1") {
    new InifiteLevel {
      val startPos: this.type#Pos = Pos(2, 2)
      val goal: this.type#Pos = Pos(4, 4)

      val res = from(Stream(
        (Block(startPos, startPos), List[Move](Down)),
        (Block(Pos(0,2), Pos(1,2)), List[Move]())
        ), Set())
      println((res take 10).toList)
    }
  }

  ignore("optimal solution for level 2") {
    new Level0 {
      println(" PAths: From start" + pathsFromStart.toList )//.filter( _._1.b1 == goal ))
      val s = solve(solution)
      println("optimal solution for level 1 -> Moves:" + solution + " End Block " + s)
      assert(s == Block(goal, goal))
    }
  }

  ignore("optimal solution for level 1") {
    new Level1 {
//      println(" PAths: From start" + pathsFromStart.toList )//.filter( _._1.b1 == goal ))
      val s = solve(solution)
//      println("optimal solution for level 1 -> Moves:" + solution + " End Block " + s)
      assert(s == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
