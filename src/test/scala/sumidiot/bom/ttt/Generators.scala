package sumidiot.bom.ttt

import org.scalacheck._
import Gen._

import sumidiot.bom.ttt.Common._

/**
 * Handful of generators for use in scalacheck property-based tests
 *
 * Note that Arbitrary instances are used when you call forAll without specifying a generator
 * explicitly.
 */
object Generators {

  implicit val arbitraryPlayer: Arbitrary[Player] = Arbitrary(oneOf(Player.X, Player.O))

  implicit val arbitraryPosition: Arbitrary[Position] = Arbitrary(oneOf(allPositions))
 
  val genGamePlays: Gen[List[(Position, Player)]] =
    Gen.resultOf((u: Unit) => Common.randomPlaySequence)

  val genGameBoard: Gen[Board] =
    genGamePlays.map(_.toMap)

  /**
   * To generator a game state, we first generate a list of moves, and then
   * determine whose turn it is after that list of moves (i.e., whoever didn't have
   * the final play in that list).
   */
  val genGameState: Gen[GameState] =
    genGamePlays.map(plays => {
      if (plays.isEmpty) {
        StartingGame
      } else {
        val np = Player.other(plays.last._2)
        val b = plays.toMap
        GameState(np, b)
      }
    })

  /**
   * To generate an "obvious" win, we pick a winningCombo that we assign to X,
   * and then two random remaining positions that we assign to O.
   */
  val genObviousWin: Gen[Board] =
    for {
      xt <- oneOf(winningCombos)
      xs = List(xt._1, xt._2, xt._3)
      o1 = randomPosition(xs.toSet)
      o2 = randomPosition((o1 :: xs).toSet)
    } yield {
      (xs.map(p => p -> Player.X) ++ List(o1, o2).map(p => p -> Player.O)).toMap
    }

}
