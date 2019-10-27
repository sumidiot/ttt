package sumidiot.bom.ttt

import org.scalacheck._
import Gen._
import org.scalacheck.Prop.forAll

import sumidiot.bom.ttt.Common._
import sumidiot.bom.ttt.Generators._

object CommonSpecification extends Properties("Common") {

  property("otherPlayerIsOwnInverse") = forAll { (p: Player) =>
    p == Player.other(Player.other(p))
  }

  property("anyBoardIsReasonable") = forAll(genGameBoard) { (b: Board) => {
    val xPlays = b.values.filter(_ == Player.X).size
    val oPlays = b.values.filter(_ == Player.O).size
    b.size <= allPositions.size && (xPlays - oPlays >= 0 && xPlays - oPlays <= 1)
  }}

  property("randomPlayIsNonsuperfluous") = forAll(genGamePlays) { ps => {
    if (ps.isEmpty) {
      true
    } else {
      val sub = ps.take(scala.util.Random.nextInt % ps.size) // take proper sequences
      StateCheats.winner(sub.toMap).isEmpty
    }
  }}

}
