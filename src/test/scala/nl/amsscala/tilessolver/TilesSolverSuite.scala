package nl.amsscala
package tilessolver

import org.scalatest._

/** @author A'dam Scala Tiles-puzzle-solver team */

class TilesSolverSuite extends FunSpec {
  import TilesSolver._
  import Direction._

  describe("Solution") {
    it("given an empty list should result in an list of an empty path") {
      assert(TilesSolver.findPaths(Nil) === List(Nil))
    }
    it("given single tile West Center should result in a list of an empty path") {
      assert(TilesSolver.findPaths(List(Tile(W, C))) === List(Nil))
    }
    it("given single tile Center West should result in a list of an empty path") {
      assert(TilesSolver.findPaths(List(Tile(C, W))) === List(Nil))
    }
    it("given single tile West East should result in a list of an empty path") {
      assert(TilesSolver.findPaths(List(Tile(W, E))) === List(Nil))
    }
    it("given two same tiles should result in a list of an empty path") {
      assert(TilesSolver.findPaths(List(Tile(W, E), Tile(W, E))) === List(Nil))
    }
    it("given two mirrored tiles should result in a list of an empty path") {
      assert(TilesSolver.findPaths(List(Tile(W, E), Tile(E, W))) === List(Nil))
    }
    it("given two mirrored centered tiles should result in a list of an empty path") {
      assert(TilesSolver.findPaths(List(Tile(C, E), Tile(E, C))) === List(Nil))
    }
    it("given three mirrored centered tiles should result in a list of an empty path") {
      assert(TilesSolver.findPaths(List(Tile(C, E), Tile(C, E), Tile(E, C))) === List(Nil))
    }
    it("given three mirrored centered tiles should still result in a list of an empty path") {
      assert(TilesSolver.findPaths(List(Tile(C, E), Tile(E, C), Tile(C, E))) === List(Nil))
    }
    it("given two correct centered tiles should result in a list of a path") {
      assert(TilesSolver.findPaths(List(Tile(N, C), Tile(C, S))) === List(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given two correct reversed centered tiles should result in a list of a path") {
      assert(TilesSolver.findPaths(List(Tile(C, S), Tile(N, C))) === List(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given three correct reversed centered tiles should result in a list of a path") {
      assert(TilesSolver.findPaths(List(Tile(C, S), Tile(C, S), Tile(N, C))) === List(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given three correct reversed centered tiles should still result in a list of a path") {
      assert(TilesSolver.findPaths(List(Tile(C, S), Tile(N, C), Tile(C, S))) === List(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given 3 correct reversed centered tiles should still result in a list of a path") {
      assert(TilesSolver.findPaths(List(Tile(N, C), Tile(C, S), Tile(C, S))) === List(List(), List(Tile(C, S), Tile(N, C))))
    }

    // 
    it("given four alternated reversed centered tiles should result in a list of a path") {
      assert(TilesSolver.findPaths(List(Tile(N, C), Tile(N, C), Tile(C, S), Tile(C, S))) === List(List(), List(Tile(C, S), Tile(N, C))))
    }
    it("given four alternated reversed centered tiles should still result in a list of a path") {
      assert(TilesSolver.findPaths(List(Tile(C, S), Tile(N, C), Tile(N, C), Tile(C, S))) === List(List(), List(Tile(C, S), Tile(N, C))))
    }

    it("given four cross reversed centered tiles should still result in a list of a path") {
      assert(TilesSolver.findPaths(List(Tile(C, S), Tile(W, C), Tile(N, C), Tile(C, E))) ===
        List(List(), List(Tile(C, S), Tile(N, C)), List(Tile(C, E), Tile(W, C))))
    }

    it("given the modified example of the site should still result in a list of a path") {
      assert(TilesSolver.findPaths(List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(E, S),
        Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))) ===
        List( //
          List(), //
          List(Tile(C, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, E), Tile(W, C))))
    }

    it("given the example of the site should still result in a list of a path") {
      assert(TilesSolver.findPaths(List(
        Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
        Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C))) ===
        List( //
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

    /*  val l3 = List(
*/

  }
}
