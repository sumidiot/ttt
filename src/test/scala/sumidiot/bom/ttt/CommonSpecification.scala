package sumidiot.bom.ttt

import org.scalacheck._
import Gen._
import org.scalacheck.Prop.forAll

import sumidiot.bom.ttt.Common._
import sumidiot.bom.ttt.Generators._

/**
 * Some tests for the methods of the Common utilities
 */
object CommonSpecification extends Properties("Common") {

  /**
   * Just a basic silly test, to get used to property-based testing
   */
  property("otherPlayerIsOwnInverse") = forAll { (p: Player) =>
    p == Player.other(Player.other(p))
  }

  /**
   * The genGameBoard generator is supposed to give us a board that is reachable
   * in normal play, and so we should at least have the right number of Xs and Os.
   */
  property("anyBoardIsReasonable") = forAll(genGameBoard) { (b: Board) => {
    val xPlays = b.values.filter(_ == Player.X).size
    val oPlays = b.values.filter(_ == Player.O).size
    xPlays - oPlays >= 0 && xPlays - oPlays <= 1
  }}

  /**
   * Really, genGameBoard depends on genGamePlays, and that method is more interesting.
   * We want to ensure that no initial subsequence of the list corresponds to a board
   * with a winner, because otherwise the game should have ended before we got that far.
   */
  property("randomPlayIsNonsuperfluous") = forAll(genGamePlays) { ps => {
    (0 until ps.size).forall(s => StateCheats.winner(ps.take(s).toMap).isEmpty)
  }}

  /**
   * Check that if you pass exceptions to randomPosition, you don't get back a position
   * from your list of exceptions.
   */
  property("randomPositionRespectsExceptions") = forAll { (ps: Set[Position]) => {
    (ps.size == allPositions.size) || {
      val rps = randomPosition(ps)
      !(ps.contains(rps))
    }
  }}

  /**
   * We assume genObviousWin produces boards which are reasonable and have a winner,
   * since all the winning combinations are enumerated, and so here we're checking
   * that the `winner` method at leasts gets those right.
   */
  property("obviousWinsAreWon") = forAll(genObviousWin) { (b: Board) => {
    StateCheats.winner(b) match {
      case Some(Player.X) => true
      case _              => false
    }
  }}

}
