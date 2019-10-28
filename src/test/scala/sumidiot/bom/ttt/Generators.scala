package sumidiot.bom.ttt

import org.scalacheck._
import Gen._

import sumidiot.bom.ttt.Common._

object Generators {

  implicit val arbitraryPlayer: Arbitrary[Player] = Arbitrary(oneOf(Player.X, Player.O))

  implicit val arbitraryPosition: Arbitrary[Position] = Arbitrary(oneOf(allPositions))
  
  implicit val genGamePlays: Gen[List[(Position, Player)]] =
    Gen.resultOf((u: Unit) => Common.randomPlaySequence)

  implicit val genGameBoard: Gen[Board] =
    genGamePlays.map(_.toMap)

  implicit val genGameState: Gen[GameState] = {
    genGamePlays.map(plays => {
      if (plays.isEmpty) {
        StartingGame
      } else {
        val np = Player.other(plays.last._2)
        val b = plays.toMap
        GameState(np, b)
      }
    })
  }

}
