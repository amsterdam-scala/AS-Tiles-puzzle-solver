package nl.amsscala
package tilessolver

import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.{FunSpec, GivenWhenThen, Matchers, Tag}

import scala.collection.parallel.immutable.ParSeq

/** @author A'dam Scala Tiles-puzzle-solver team */
class TilesSolverSuite extends FunSpec with GivenWhenThen with Matchers {

  import nl.amsscala.tilessolver.Directions.{C, Directi, E, N, S, W}
  val aa :Chain = Nil

  val cases = ParSeq(Nil,
    List(Tile(W, C)),
    List(Tile(C, W)),
    List(Tile(W, E)),
    List(Tile(W, E), Tile(W, E)),
    List(Tile(W, E), Tile(E, W)),
    List(Tile(C, E), Tile(E, C)),
    Nil, // 7
    Nil, //List(Tile(C, E), Tile(E, C), Tile(C, E)),
    List(Tile(N, C), Tile(C, S)), // 9
    Nil, //
    List(Tile(C, S), Tile(C, S), Tile(N, C)), // 11
    List(Tile(N, C), Tile(N, C), Tile(C, S), Tile(C, S)), //12
    List(Tile(C, S), Tile(N, C), Tile(N, C), Tile(C, S)),
    List(Tile(C, S), Tile(W, C), Tile(N, C), Tile(C, E)))

  describe("A solution of the Tile problem") {
    it("can create a new Tile", Tag("construction")) {
      Given("A new tile instantiation")
      When("then end and start points are the same")
      Then("reject an invalid tile definition")
      val thrown = intercept[IllegalArgumentException](Tile(C, C))
      thrown.getMessage should include(" C, C ")
    }

    it("should result in a set with one empty chain") {
      Given("an empty list")
      Solution(Nil).rawSolution should be('empty)

      Given("single tile West Center")
      Solution(cases(1)).rawSolution should be('empty)

      Given("single tile Center West")
      Solution(cases(2)).rawSolution should be('empty)

      Given("single tile West East")
      Solution(cases(3)).rawSolution should be('empty)

      Given("two same tiles")
      Solution(cases(4)).rawSolution should be('empty)

      Given("two mirrored tiles")
      Solution(cases(5)).rawSolution should be('empty)

      Given("two mirrored centered tiles")
      Solution(cases(6)).rawSolution should be('empty)

      Given("three mirrored centered tiles")
      List(Tile(C, W), Tile(C, N), Tile(C, E)).permutations.foreach(Solution(_).rawSolution should be('empty))
    }

    it("given two correct centered tiles should result in a set of chains") {
      cases(9).permutations.
        foreach(Solution(_).rawSolution should be (Set(List(Tile(C, S), Tile(N, C)))))
    }

    it("given three correct reversed centered tiles should result in a set of chains") {
      cases(11).permutations.
        foreach(Solution(_).rawSolution should be (Set(List(Tile(C, S), Tile(N, C)))))
    }

    it("given four alternated reversed centered tiles should result in a set of chains") {
      assert(Solution(cases(12)).rawSolution === Set(List(Tile(C, S), Tile(N, C))))
    }

    it("given four alternated reversed centered tiles should still result in a set of chains") {
      assert(Solution(cases(13)).rawSolution === Set(List(Tile(C, S), Tile(N, C))))
    }

    it("given four cross reversed centered tiles should still result in a set of chains") {
      assert(Solution(cases(14)).rawSolution ===
        Set(List(Tile(C, S), Tile(N, C)), List(Tile(C, E), Tile(W, C))))
    }

    it("given the modified example of the site should still result in a set of chains") {
      assert(Solution(Suggestions.modifiedExample).rawSolution ===
        Set(List(Tile(C, E), Tile(W, C)), List(Tile(C, E), Tile(W, E), Tile(W, C))))
    }

    it("given the double long linear path") {
      assert(Solution(List(Tile(C, E), Tile(W, E), Tile(W, E), Tile(W, E),
        Tile(W, C), Tile(C, S), Tile(N, S), Tile(N, S), Tile(N, S), Tile(S, C))).rawSolution ===
        Set(List(Tile(C, E), Tile(W, C)), List(Tile(C, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, E), Tile(W, E), Tile(W, C))))
    }

    it("given the example of the site should still result in a set of chains") {
      assert(Solution(Suggestions.fabioPhoto).rawSolution ===
        Set(//
          List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, S), Tile(N, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, S), Tile(N, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, E), Tile(W, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, S), Tile(N, E), Tile(W, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, C)), //
          List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, S), Tile(N, C)), //
          List(Tile(C, E), Tile(W, S), Tile(N, C)), //
          List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, C))))
    }
  }

  describe("Advanced solutions of the tile lay-out problem") {

    it("can create a new Tile ending x") {
      val displacements = Table(("end", "expect"),
        (N, (0, -1)),
        (E, (1, 0)),
        (S, (0, 1)),
        (W, (-1, 0)))

      forAll(displacements) { (n: Directi, d: Coord) => Tile(C, n).whereIsNextLayed(0, 0) should be(d)}
    }

    it("should layout the tile") {
      Given("a round walk")
      assert(Solution.virtualTilesLayouter(
        List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, W), Tile(E, N), Tile(S, N), Tile(S, C))) ==
        List(((0, 0), (Tile(C, E), 0)), ((1, 0), (Tile(W, S), 1)), ((1, 1), (Tile(N, S), 2)),
          ((1, 2), (Tile(N, W), 3)), ((0, 2), (Tile(E, N), 4)), ((0, 1), (Tile(S, N), 5)), ((0, 0), (Tile(S, C), 6))))
    }

    val crazyResult = Solution(Suggestions.crazyExample).rawSolution

    it("should give a raw crazy complex stack tiles result") {
      Given("crazy example")
      assert(crazyResult ===
        Set(List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, N), Tile(S, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, E), Tile(W, N), Tile(S, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, N), Tile(S, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, N), Tile(S, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, C)),
          List(Tile(C, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, N), Tile(S, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, N), Tile(S, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, C)),
          List(Tile(C, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, N), Tile(S, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, E), Tile(W, N), Tile(S, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, N), Tile(S, E), Tile(W, C)),
          List(Tile(C, N), Tile(S, E), Tile(W, E), Tile(W, C))))
    }

    val longestLen = crazyResult.foldLeft(0)(_ max _.size)
    val craziestResult = crazyResult.filter(_.size >= longestLen).minBy(Solution.virtualTilesLayouter(_).toMap.size)

    it("should give the craziest result with most double used tile positions") {
      Given("craziest result")
      assert(craziestResult ===
        List(Tile(C, N), Tile(S, E), Tile(W, N), Tile(S, W), Tile(E, S), Tile(N, S), Tile(N, E), Tile(W, E), Tile(W, C)))
    }

    val craziestLayout = Solution.virtualTilesLayouter(craziestResult)
    it("should layout the tile for a craziest result") {
      Given("modified example")
      assert(craziestLayout ==
        List(((0, 0), (Tile(C, N), 0)), ((0, -1), (Tile(S, E), 1)), ((1, -1), (Tile(W, N), 2)), ((1, -2), (Tile(S, W), 3)), ((0, -2),
          (Tile(E, S), 4)), ((0, -1), (Tile(N, S), 5)), ((0, 0), (Tile(N, E), 6)), ((1, 0), (Tile(W, E), 7)), ((2, 0), (Tile(W, C), 8))))
    }


    it("should find the double tile positions") {
      Given("modified example")
      assert(Solution.findOverlayedPositions(craziestLayout) === Set((0, 0), (0, -1)))
    }

  } // describe

  describe("The last tests are permutations, so are processor intensive. 4 or more minutes.") {
    it("should every time the same lists of chains, thus be stable") {
      Given("the modified example of the site all permutations (1.814.400)")
      assert(Suggestions.modifiedExample.permutations.forall(Solution(_).rawSolution ===
        Set(List(Tile(C, E), Tile(W, E), Tile(W, C)), List(Tile(C, E), Tile(W, C)))))
    }

    info("This is the heavy one. 10 minutes?")
    it("the exact example of the site all permutations (1.814.400)") {
      assert(Suggestions.fabioPhoto.permutations.forall( Solution(_).rawSolution ===
        Set(List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, C)),
          List(Tile(C, E), Tile(W, S), Tile(N, C)),
          List(Tile(C, E), Tile(W, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, S), Tile(N, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, S), Tile(N, C)),
          List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, C)),
          List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, S), Tile(N, E), Tile(W, C)))))
    }
  } // describe
}