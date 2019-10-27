package sumidiot.bom.ttt

import org.scalacheck._
import Gen._
import org.scalacheck.Prop.forAll

import sumidiot.bom.ttt.Common._

object CommonSpecification extends Properties("Common") {

  implicit val arbitraryPlayer: Arbitrary[Player] = Arbitrary(oneOf(Player.X, Player.O))

  implicit val arbitraryPosition: Arbitrary[Position] = Arbitrary(oneOf(allPositions))

  /**
   * This is supposed to generate boards which have either just finished (the last move
   * caused a win), or have not yet been won, or are a draw (filled). It assumes X plays
   * first.
   */
  implicit val genGameBoard: Gen[Board] = {
    def nonsuperfluousPlays(ps: List[(Position, Player)]): List[(Position, Player)] = {
      (ps.foldLeft((Nil, None): (List[(Position, Player)], Option[Player])) {
        case (a@(nsp, Some(_)), (_, _)) => a
        case ((nsp, None), (pos, pl)) => {
          val nl = nsp ++ List((pos, pl))
          val b = nl.toMap
          val w = StateCheats.winner(b)
          (nl, w)
        }
      })._1
    }
    for {
      numPlays <- Gen.choose(0, allPositions.size) // choose is inclusive
      numXs = math.ceil(numPlays / 2.0).toInt
      numOs = numPlays - numXs
      positions = scala.util.Random.shuffle(allPositions).take(numPlays)
      players = List.tabulate(numPlays)(i => if (i % 2 == 0) { Player.X } else { Player.O })
      allPlays = positions.zip(players)
    } yield {
      nonsuperfluousPlays(allPlays).toMap
    }
  }

  property("otherPlayerIsOwnInverse") = forAll { (p: Player) =>
    p == Player.other(Player.other(p))
  }

  property("anyBoardIsReasonable") = forAll(genGameBoard) { (b: Board) => {
    val xPlays = b.values.filter(_ == Player.X).size
    val oPlays = b.values.filter(_ == Player.O).size
    b.size <= allPositions.size && (xPlays - oPlays >= 0 && xPlays - oPlays <= 1)
  }}

}
