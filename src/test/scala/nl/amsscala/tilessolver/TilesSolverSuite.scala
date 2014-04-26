package nl.amsscala
package tilessolver

import org.scalatest._

/** @author A'dam Scala Tiles-puzzle-solver team */

class TilesSolverSuite extends FunSpec {
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
      Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C)))

  describe("Solution") {
    it("given an empty list should result in an list of an empty path") {
      assert(TilesSolver.findPaths(cases(0)) === Set(Nil))
    }
    it("given single tile West Center should result in a list of an empty path") {
      assert(TilesSolver.findPaths(cases(1)) === Set(Nil))
    }
    it("given single tile Center West should result in a list of an empty path") {
      assert(TilesSolver.findPaths(cases(2)) === Set(Nil))
    }
    it("given single tile West East should result in a list of an empty path") {
      assert(TilesSolver.findPaths(cases(3)) === Set(Nil))
    }
    it("given two same tiles should result in a list of an empty path") {
      assert(TilesSolver.findPaths(cases(4)) === Set(Nil))
    }
    it("given two mirrored tiles should result in a list of an empty path") {
      assert(TilesSolver.findPaths(cases(5)) === Set(Nil))
    }
    it("given two mirrored centered tiles should result in a list of an empty path") {
      assert(TilesSolver.findPaths(cases(6)) === Set(Nil))
    }
    it("given three mirrored centered tiles should result in a list of an empty path") {
      assert(TilesSolver.findPaths(cases(7)) === Set(Nil))
    }
    it("given three mirrored centered tiles should still result in a list of an empty path") {
      assert(TilesSolver.findPaths(cases(8)) === Set(Nil))
    }
    it("given two correct centered tiles should result in a list of a path") {
      assert(TilesSolver.findPaths(cases(9)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given two correct reversed centered tiles should result in a list of a path") {
      assert(TilesSolver.findPaths(cases(10)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given three correct reversed centered tiles should result in a list of a path") {
      assert(TilesSolver.findPaths(cases(11)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given three correct reversed centered tiles should still result in a list of a path") {
      assert(TilesSolver.findPaths(cases(12)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given 3 correct reversed centered tiles should still result in a list of a path") {
      assert(TilesSolver.findPaths(cases(13)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }

    // 
    it("given four alternated reversed centered tiles should result in a list of a path") {
      assert(TilesSolver.findPaths(cases(14)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given four alternated reversed centered tiles should still result in a list of a path") {
      assert(TilesSolver.findPaths(cases(15)) === Set(List(), List(Tile(C, S), Tile(N, C))))
    }

    it("given four cross reversed centered tiles should still result in a list of a path") {
      assert(TilesSolver.findPaths(cases(16)) ===
        Set(List(), List(Tile(C, S), Tile(N, C)), List(Tile(C, E), Tile(W, C))))
    }

    it("given the modified example of the site should still result in a list of a path") {
      assert(TilesSolver.findPaths(cases(17)) ===
        Set( //
          List(), //
          List(Tile(C, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, E), Tile(W, C))))
    }

    it("given the example of the site should still result in a list of a path") {
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
  }

  for (casus <- cases(17).permutations) assert(
    TilesSolver.findPaths(casus) ===
      Set(List(), 
          List(Tile(C, E), Tile(W, E), Tile(W, C)), 
          List(Tile(C, E), Tile(W, C))))

  for (casus <- cases(18).permutations) assert(
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
        List(Tile(C, E), Tile(W, E), Tile(W, S), Tile(N, S), Tile(N, E), Tile(W, C))))
}
