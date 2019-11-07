package sumidiot.bom.ttt

import org.scalacheck._
import Gen._
import org.scalacheck.Prop.forAll

import sumidiot.bom.ttt.Common._
import sumidiot.bom.ttt.Common.{Result => TTTResult}
import sumidiot.bom.ttt.Generators._
import sumidiot.bom.ttt.OO._

object OOSpecification extends Properties("OO") {

  def infoThenGenTakeIsBehaved(gs: GameState, pos: Position): Boolean = {
    val ttt = new EmbeddedVarTicTacToe(gs)
    val maybeEnded = gameEnded(ttt)
    val originalPosPlayer = ttt.info(pos)
    val currentPlayer = ttt.turn()
    val takeRes = genTake(ttt)(pos)
    val afterTurnPosPlayer = ttt.info(pos)
    val afterTurnCurPlayer = ttt.turn()
    maybeEnded.nonEmpty ||
      (originalPosPlayer.isDefined && takeRes == TTTResult.AlreadyTaken(originalPosPlayer.get) && currentPlayer == afterTurnCurPlayer && originalPosPlayer == afterTurnPosPlayer) ||
      ((originalPosPlayer.isEmpty && afterTurnPosPlayer == Some(currentPlayer)) &&
        (
          (takeRes == TTTResult.GameEnded(None)) ||
          (takeRes == TTTResult.GameEnded(Some(currentPlayer))) ||
          (takeRes == TTTResult.NextTurn && afterTurnCurPlayer == Player.other(currentPlayer))
          ))
  }

  property("infoThenGenTakeIsBehaved") =
    forAll(genGameState, Arbitrary.arbitrary[Position])(infoThenGenTakeIsBehaved)

}
