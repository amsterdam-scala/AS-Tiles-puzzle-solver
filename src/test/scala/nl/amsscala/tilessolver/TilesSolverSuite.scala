package nl.amsscala
package tilessolver

import org.scalatest.GivenWhenThen
import org.scalatest.FunSpec

//import org.scalatest

/** @author A'dam Scala Tiles-puzzle-solver team */

class TilesSolverSuite extends FunSpec with GivenWhenThen {
  import TilesSolver._
  import Direction._

  val cases = Seq(Nil,
    List(Tile(W, C)),
    List(Tile(C, W)),
    List(Tile(W, E)),
    List(Tile(W, E), Tile(W, E)),
    List(Tile(W, E), Tile(E, W)),
    List(Tile(C, E), Tile(E, C)),
    List(Tile(C, E), Tile(C, E), Tile(E, C)),
    List(Tile(C, E), Tile(E, C), Tile(C, E)),
    List(Tile(N, C), Tile(C, S)),
    List(Tile(C, S), Tile(N, C)),
    List(Tile(C, S), Tile(C, S), Tile(N, C)),
    List(Tile(C, S), Tile(N, C), Tile(C, S)),
    List(Tile(N, C), Tile(C, S), Tile(C, S)),
    List(Tile(N, C), Tile(N, C), Tile(C, S), Tile(C, S)),
    List(Tile(C, S), Tile(N, C), Tile(N, C), Tile(C, S)),
    List(Tile(C, S), Tile(W, C), Tile(N, C), Tile(C, E)),
    List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(E, S),
      Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C)),
    List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
      Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C)),
    List(
      Tile(C, E), Tile(N, E), Tile(N, S), Tile(W, C), Tile(N, C), Tile(W, E), Tile(W, S)))

  describe("A solution of the Tile problem") {
    it("should result in an list of an empty path") {
      given("an empty list")
      expectResult(Set(Nil)) { TilesSolver.findPaths(cases(0)) }

      given("single tile West Center")
      expectResult(Set(Nil))(TilesSolver.findPaths(cases(1)))

      given("single tile Center West")
      expectResult(Set(Nil))(TilesSolver.findPaths(cases(2)))

      given("single tile West East")
      expectResult(Set(Nil))(TilesSolver.findPaths(cases(3)))

      given("two same tiles")
      expectResult(Set(Nil))(TilesSolver.findPaths(cases(4)))

      given("two mirrored tiles")
      expectResult(Set(Nil))(TilesSolver.findPaths(cases(5)))

      given("two mirrored centered tiles")
      expectResult(Set(Nil))(TilesSolver.findPaths(cases(6)))

      given("three mirrored centered tiles")
      expectResult(Set(Nil))(TilesSolver.findPaths(cases(7)))

      given("three mirrored centered tiles")
      expectResult(Set(Nil))(TilesSolver.findPaths(cases(8)))
    }
    
    it("given two correct centered tiles should result in a list of paths") {
      assert(TilesSolver.findPaths(cases(9)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given two correct reversed centered tiles should result in a list of paths") {
      assert(TilesSolver.findPaths(cases(10)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given three correct reversed centered tiles should result in a list of paths") {
      assert(TilesSolver.findPaths(cases(11)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given three correct reversed centered tiles should still result in a list of paths") {
      assert(TilesSolver.findPaths(cases(12)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given 3 correct reversed centered tiles should still result in a list of paths") {
      assert(TilesSolver.findPaths(cases(13)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }

    // 
    it("given four alternated reversed centered tiles should result in a list of paths") {
      assert(TilesSolver.findPaths(cases(14)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given four alternated reversed centered tiles should still result in a list of paths") {
      assert(TilesSolver.findPaths(cases(15)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }

    it("given four cross reversed centered tiles should still result in a list of paths") {
      assert(TilesSolver.findPaths(cases(16)) ===
        Set(List(), List(Tile(C, S), Tile(N, C)), List(Tile(C, E), Tile(W, C))))
    }

    it("given the modified example of the site should still result in a list of paths") {
      assert(TilesSolver.findPaths(cases(17)) ===
        Set( //
          List(), //
          List(Tile(C, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, E), Tile(W, C))))
    }

    it("given the example of the site should still result in a list of paths") {
      assert(TilesSolver.findPaths(cases(18)) ===
        Set( //
          List(), //
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

    info("The following tests are permutations so are processing intensive.")

    it("given the modified example of the site all permutations (1814400) the same lists of paths") {
      cases(17).permutations.foreach(casus => assert(
        TilesSolver.findPaths(casus) ===
          Set(List(),
            List(Tile(C, E), Tile(W, E), Tile(W, C)),
            List(Tile(C, E), Tile(W, C)))))
    }

    /* for (casus <- cases(18).permutations) assert(
    TilesSolver.findPaths(casus) ===
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
        List(),
        List(Tile(C, E), Tile(W, E), Tile(W, C)),
        List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, S), Tile(N, E), Tile(W, C))))*/
  }
}