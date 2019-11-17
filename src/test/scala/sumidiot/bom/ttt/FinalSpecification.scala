package sumidiot.bom.ttt

import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline
import cats.kernel.laws._
import cats.kernel.laws.discipline._
import cats._
import cats.syntax.all._
import org.scalacheck.Prop._


import sumidiot.bom.ttt.Common._
import sumidiot.bom.ttt.Common.{Result => TTTResult}
import sumidiot.bom.ttt.Generators._
import sumidiot.bom.ttt.Final.TicTacToe
import sumidiot.bom.ttt.Final.TicTacToeSyntax._
import sumidiot.bom.ttt.Final.{genTake,gameEnded}

/**
 * Following https://www.iteratorshq.com/blog/tagless-with-discipline-testing-scala-code-the-right-way/
 * Also note:
 *   https://www.freecodecamp.org/news/beyond-unit-tests-an-intro-to-property-and-law-testing-in-scala-dd6a15898a19/
 *   https://www.freecodecamp.org/news/an-introduction-to-law-testing-in-scala-4243d72272f9/
 *   http://chrisphelps.github.io/scala/2016/11/30/Cats-Law-Checking-With-Discipline/
 *   http://eed3si9n.com/herding-cats/checking-laws-with-discipline.html
 *   https://typelevel.org/cats/typeclasses/lawtesting.html
 *   https://typelevel.org/blog/2013/11/17/discipline.html
 */
abstract class FinalSpecification[F[_] : TicTacToe : Monad] {

  def infoIsIdempotent(pos: Position): IsEq[F[Boolean]] = true.pure[F] <->
    (for {
      op1 <- info(pos)
      op2 <- info(pos)
    } yield {
      op1 == op2
    })

  /**
   * In this test, we'd like to ensure that `genTake` causes the right things to happen.
   * One thing we have to guard against is that the game could already be done - for now, we
   * let the test just pass. Assuming the game isn't ended, we check info(pos) and turn first,
   * to get the current state of the board at the position, and the current player's turn. Then
   * we call genTake, and compare following calls to info and turn.
   * There's several cases to worry about:
   * 1. if the position is already taken, then genTake return AlreadyTaken, and both
   *    info and turn return the same thing as before calling genTake
   * 2. if the position is not already taken, then genTake is either
   *    * GameEnded(None), meaning the game ended in a draw - ensure info(pos) is the original player
   *    * GameEnded(Some) - ensure info(pos) is the original player, and the winner is that player
   *    * NextTurn - ensure info(pos) is the original player and the new current player is not the
   *      same as we started with
   */
  def infoThenGenTakeIsBehaved(pos: Position): IsEq[F[Boolean]] = true.pure[F] <-> {
    case class TestState(maybeEnded: Option[TTTResult],
                         originalPosPlayer: Option[Player],
                         currentPlayer: Player,
                         takeRes: TTTResult,
                         afterTurnPosPlayer: Option[Player],
                         afterTurnCurPlayer: Player)
    def gameIsAlreadyOver(ts: TestState) =
      ts.maybeEnded.nonEmpty
    def alreadyTakenDoneCorrectly(ts: TestState) =
      ts.originalPosPlayer.isDefined && ts.takeRes == TTTResult.AlreadyTaken(ts.originalPosPlayer.get) && ts.currentPlayer == ts.afterTurnCurPlayer && ts.originalPosPlayer == ts.afterTurnPosPlayer
    def playerGetsPosition(ts: TestState) =
      ts.originalPosPlayer.isEmpty && ts.afterTurnPosPlayer == Some(ts.currentPlayer)
    def gameEndsInDraw(ts: TestState) =
      ts.takeRes == TTTResult.GameEnded(None)
    def playerGetsWin(ts: TestState) =
      ts.takeRes == TTTResult.GameEnded(Some(ts.currentPlayer))
    def nextTurnAndStateUpdates(ts: TestState) =
      ts.takeRes == TTTResult.NextTurn && ts.afterTurnCurPlayer == Player.other(ts.currentPlayer)
    (for {
      maybeEnded <- gameEnded
      originalPosPlayer <- info(pos)
      currentPlayer <- turn
      takeRes <- genTake(pos)
      afterTurnPosPlayer <- info(pos)
      afterTurnCurPlayer <- turn
      testState = TestState(maybeEnded, originalPosPlayer, currentPlayer, takeRes, afterTurnPosPlayer, afterTurnCurPlayer)
    } yield {
      gameIsAlreadyOver(testState) ||
      alreadyTakenDoneCorrectly(testState) ||
      (playerGetsPosition(testState) &&
        (gameEndsInDraw(testState) || playerGetsWin(testState) || nextTurnAndStateUpdates(testState)))
    })
  }
}

object FinalSpecification {
  def apply[F[_] : TicTacToe : Monad]() =
    new FinalSpecification[F] {
    }
}

trait FinalTests[F[_]] extends Laws {

  def laws: FinalSpecification[F]

  def algebra(implicit eqFBool: Eq[F[Boolean]]) =
    new SimpleRuleSet(
      name = "TicTacToe",
      "info is idempotent" -> forAll (laws.infoIsIdempotent _),
      "genTake is lawful"  -> forAll (laws.infoThenGenTakeIsBehaved _)
    )

}

object FinalTests {
  def apply[F[_] : TicTacToe : Monad]() =
    new FinalTests[F] {
      override def laws = FinalSpecification()
    }
}

class FinalSpecs extends AnyFunSuite with Discipline {
  /**
   * I think you'd generally compare two State[A, B] as the same if for all a in A
   * when you .run a through the two State-s, the two .values are the same. Maybe I'm wrong
   * about that interpretation, because I'm still getting used to State. Or, if I'm right,
   * it seems like I've probably got my tests set up incorrectly - I'd imagine the property
   * being based on generating an Arbitrary GameState, and then checking the property given
   * the corresponding State. But the generic Law can't be written to assume State...
   */
  implicit val eqSQS: Eq[SGS[Boolean]] = new Eq[SGS[Boolean]] {
    def eqv(a: SGS[Boolean], b: SGS[Boolean]): Boolean =
      List.fill(200)(genGameState.sample).flatten.forall(s => {
        val ans = a.runA(s).value == b.runA(s).value
        if (!ans) {
          println(s"Failed on:\n$s")
        }
        ans
      })
  }

  /**
   * This import actually has to be in this block, not the top of the file, because otherwise
   * it's available as implicit evidence everywhere, conflicting with the implicit evidence
   * that's getting passed around.
   */
  import sumidiot.bom.ttt.Final.Instances.SGS.SGSIsTicTacToe
  checkAll("SGS is lawful TicTacToe", FinalTests[SGS]().algebra)
}
