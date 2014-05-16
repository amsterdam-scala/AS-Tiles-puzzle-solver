package nl.amsscala
package tilessolver

import org.scalatest
import org.scalatest.{ FunSpec, GivenWhenThen }
import scala.collection.parallel.immutable.ParSeq

/** @author A'dam Scala Tiles-puzzle-solver team */

class TilesSolverSuite extends FunSpec with GivenWhenThen {
  //  import TilesSolver._
  import Directions._

  val cases = ParSeq(Nil,
    List(Tile(W, C)),
    List(Tile(C, W)),
    List(Tile(W, E)),
    List(Tile(W, E), Tile(W, E)),
    List(Tile(W, E), Tile(E, W)),
    List(Tile(C, E), Tile(E, C)),
    List(Tile(C, E), Tile(C, E), Tile(E, C)), // 7
    List(Tile(C, E), Tile(E, C), Tile(C, E)),
    List(Tile(N, C), Tile(C, S)), // 9
    List(Tile(C, S), Tile(N, C)),
    List(Tile(C, S), Tile(C, S), Tile(N, C)), // 11
    List(Tile(C, S), Tile(N, C), Tile(C, S)),
    List(Tile(N, C), Tile(C, S), Tile(C, S)),
    List(Tile(N, C), Tile(N, C), Tile(C, S), Tile(C, S)), //14
    List(Tile(C, S), Tile(N, C), Tile(N, C), Tile(C, S)),
    List(Tile(C, S), Tile(W, C), Tile(N, C), Tile(C, E)),
    List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(E, S), // Modified example
      Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C)),
    List(Tile(S, E), Tile(W, E), Tile(N, C), Tile(C, E), Tile(W, S),
      Tile(C, E), Tile(S, W), Tile(N, E), Tile(N, S), Tile(W, C)),
    List(
      Tile(C, E), Tile(N, E), Tile(N, S), Tile(W, C), Tile(N, C), Tile(W, E), Tile(W, S)))

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

    // 
    it("given four alternated reversed centered tiles should result in a set of chains") {
      assert(TilesSolver.findChains(cases(14)) === Set(List(Tile(C, S), Tile(N, C))))
    }
    it("given four alternated reversed centered tiles should still result in a set of chains") {
      assert(TilesSolver.findChains(cases(15)) === Set(List(Tile(C, S), Tile(N, C))))
    }

    it("given four cross reversed centered tiles should still result in a set of chains") {
      assert(TilesSolver.findChains(cases(16)) ===
        Set(List(Tile(C, S), Tile(N, C)), List(Tile(C, E), Tile(W, C))))
    }

    it("given the modified example of the site should still result in a set of chains") {
      assert(TilesSolver.findChains(cases(17)) ===
        Set( //
          List(Tile(C, E), Tile(W, C)), //
          List(Tile(C, E), Tile(W, E), Tile(W, C))))
    }

    it("given the example of the site should still result in a set of chains") {
      assert(TilesSolver.findChains(cases(18)) ===
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

    info("The last tests are permutations so are processor intensive. 4 or more minutes.")

/*     it("should every time the same lists of chains, thus be stable") {
      Given("the modified example of the site all permutations (1.814.400)")
      cases(17).permutations.foreach(casus => assert(TilesSolver.findChains(casus) ===
        Set(
          List(Tile(C, E), Tile(W, E), Tile(W, C)),
          List(Tile(C, E), Tile(W, C)))))

      info("This is the heavy one. 10 minutes?")
     Given("the exact example of the site all permutations (1.814.400)")
      cases(18).permutations.foreach(casus => assert(TilesSolver.findChains(casus) ===
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