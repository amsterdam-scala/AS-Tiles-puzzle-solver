package nl.amsscala
package tilessolver

import org.scalatest
import org.scalatest.{ FunSpec, GivenWhenThen }
import scala.collection.parallel.immutable.ParSeq
import org.scalatest.junit.JUnitRunner

/** @author A'dam Scala Tiles-puzzle-solver team */

class TilesSolverSuite extends FunSpec with GivenWhenThen {

  import Directions.{ C, N, E, S, W }

  val cases = ParSeq(Nil,
    List(Tile(W, C)),
    List(Tile(C, W)),
    List(Tile(W, E)),
    List(Tile(W, E), Tile(W, E)),
    List(Tile(W, E), Tile(E, W)),
    List(Tile(C, E), Tile(E, C)),
    List(Tile(C, E), Tile(C, E), Tile(E, C)), // 7
    Nil, //List(Tile(C, E), Tile(E, C), Tile(C, E)),
    List(Tile(N, C), Tile(C, S)), // 9
    Nil, //List(Tile(C, S), Tile(N, C)),
    List(Tile(C, S), Tile(C, S), Tile(N, C)), // 11
    List(Tile(N, C), Tile(N, C), Tile(C, S), Tile(C, S)), //12
    List(Tile(C, S), Tile(N, C), Tile(N, C), Tile(C, S)),
    List(Tile(C, S), Tile(W, C), Tile(N, C), Tile(C, E)))

  describe("A solution of the Tile problem") {

    it("should reject an invalid tile definition") {
      val thrown = intercept[java.lang.IllegalArgumentException] {
        Tile(C, C)
      }
      assert(thrown.getMessage === "requirement failed: Not a proper tile definition, given C, C are the same.")
    }

    it("should result in a set with one empty chain") {
      given("an empty list")
      expectResult(Set()) { TilesSolver.findChains(cases(0)) }

      given("single tile West Center")
      expectResult(Set())(TilesSolver.findChains(cases(1)))

      given("single tile Center West")
      expectResult(Set())(TilesSolver.findChains(cases(2)))

      given("single tile West East")
      expectResult(Set())(TilesSolver.findChains(cases(3)))

      given("two same tiles")
      expectResult(Set())(TilesSolver.findChains(cases(4)))

      given("two mirrored tiles")
      expectResult(Set())(TilesSolver.findChains(cases(5)))

      given("two mirrored centered tiles")
      expectResult(Set())(TilesSolver.findChains(cases(6)))

      given("three mirrored centered tiles")
      cases(7).permutations.foreach(casus => assert(TilesSolver.findChains(casus) === Set()))
    }

    it("given two correct centered tiles should result in a set of chains") {
      cases(9).permutations.
        foreach(casus => assert(TilesSolver.findChains(casus) === Set(List(Tile(C, S), Tile(N, C)))))
    }

    it("given three correct reversed centered tiles should result in a set of chains") {
      cases(11).permutations.
        foreach(casus => assert(TilesSolver.findChains(casus) === Set(List(Tile(C, S), Tile(N, C)))))
    }

    it("given four alternated reversed centered tiles should result in a set of chains") {
      assert(TilesSolver.findChains(cases(12)) === Set(List(Tile(C, S), Tile(N, C))))
    }

    it("given four alternated reversed centered tiles should still result in a set of chains") {
      assert(TilesSolver.findChains(cases(13)) === Set(List(Tile(C, S), Tile(N, C))))
    }

    it("given four cross reversed centered tiles should still result in a set of chains") {
      assert(TilesSolver.findChains(cases(14)) ===
        Set(List(Tile(C, S), Tile(N, C)), List(Tile(C, E), Tile(W, C))))
    }

    it("given the modified example of the site should still result in a set of chains") {
      assert(TilesSolver.findChains(TilesSolver.modifiedExample) ===
        Set(List(Tile(C, E), Tile(W, C)), List(Tile(C, E), Tile(W, E), Tile(W, C))))
    }

    it("given the example of the site should still result in a set of chains") {
      assert(TilesSolver.findChains(TilesSolver.fabioPhoto) ===
        Set( //
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

  describe("A solution of the tile layout problem") {
    it("should layout the tile") {
      Given("a round walk")
      assert(TilesSolver.layoutTiles(
        List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, W), Tile(E, N), Tile(S, N), Tile(S, C))) ==
        Map((0, 2) -> (Tile(E, N), 4), (0, 0) -> (Tile(S, C), 6), (1, 1) -> (Tile(N, S), 2),
          (0, 1) -> (Tile(S, N), 5), (1, 2) -> (Tile(N, W), 3), (1, 0) -> (Tile(W, S), 1)))
    }

    val crazyResult = TilesSolver.findChains(TilesSolver.crazyExample)

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

    val craziestResult = crazyResult.maxBy(x => x.size - TilesSolver.layoutTiles(x).size)
    it("should give the craziest result with most double used tile positions") {
      Given("craziest result")
      assert(craziestResult ==
       List(Tile(C,N), Tile(S,W), Tile(E,S), Tile(N,E), Tile(W,N), Tile(S,E), Tile(W,E), Tile(W,C)))
    }

    /*    val craziestLayout = TilesSolver.layoutTiles(craziestResult)
    it("should layout the tile for a modified example") {
      Given("modified example")
      assert(tileLayout ==
        Map((0, 2) -> (Tile(E, N), 4), (0, 0) -> (Tile(S, C), 6), (1, 1) -> (Tile(N, S), 2),
          (0, 1) -> (Tile(S, N), 5), (1, 2) -> (Tile(N, W), 3), (1, 0) -> (Tile(W, S), 1)))
    }
*/
    /*       it("should give a raw crazy complex stack tiles result") {
      Given("crazy example result")
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
*/
    //    it("should layout the tile for complex stack tiles result") {
    //      Given("crazy example result")
    //      assert(TilesSolver.layoutTiles(crazyResult) ===
    //        Map((0, 0) -> (Tile(C, N), 0), (2, -1) -> (Tile(W, N), 3), (2, 0) -> (Tile(W, C), 8),
    //          (1, -2) -> (Tile(E, S), 5), (1, -1) -> (Tile(N, S), 6), (1, 0) -> (Tile(N, E), 7),
    //          (0, -1) -> (Tile(S, E), 1), (2, -2) -> (Tile(S, W), 4)))
    //    }
    /*        it("should layout the tile for complex stack tiles result") {
      Given("modified example")
      assert(TilesSolver.layoutTiles(
        List(Tile(C, E), Tile(W, S), Tile(N, S), Tile(N, W), Tile(E, N), Tile(S, N), Tile(S, C))) ==
        Map((0, 2) -> (Tile(E, N), 4), (0, 0) -> (Tile(S, C), 6), (1, 1) -> (Tile(N, S), 2),
          (0, 1) -> (Tile(S, N), 5), (1, 2) -> (Tile(N, W), 3), (1, 0) -> (Tile(W, S), 1)))
    }
*/
  } // describe

  describe("The last tests are permutations so are processor intensive. 4 or more minutes.") {
    it("should every time the same lists of chains, thus be stable") {
      Given("the modified example of the site all permutations (1.814.400)")
      TilesSolver.modifiedExample.permutations.foreach(casus => assert(TilesSolver.findChains(casus) ===
        Set(List(Tile(C, E), Tile(W, E), Tile(W, C)), List(Tile(C, E), Tile(W, C)))))
    }

    /*info("This is the heavy one. 10 minutes?")
     Given("the exact example of the site all permutations (1.814.400)")
      TilesSolver.fabioPhoto.permutations.foreach(casus => assert(TilesSolver.findChains(casus) ===
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
    }*/
  } // describe
}