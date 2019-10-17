package sumidiot.bom.ttt

import cats._
import cats.data.State
import cats.implicits._
import cats.free.Free
import cats.free.Free.liftF
import cats.arrow.FunctionK
import cats.~>
import Common._

/**
 * Fantastic!

 import sumidiot.bom.ttt.Common._
 import sumidiot.bom.ttt.TTTFree._
 runRandom().foldMap(freeState).run(StartingGame).value

 */
object TTTFree {

  sealed abstract class TicTacToeA[A]
  case class Info(p: Position) extends TicTacToeA[Option[Player]]
  case class Take(p: Position) extends TicTacToeA[Result]

  type TicTacToe[A] = Free[TicTacToeA, A]

  def info(p: Position): TicTacToe[Option[Player]] =
    liftF[TicTacToeA, Option[Player]](Info(p))

  def take(p: Position): TicTacToe[Result] =
    liftF[TicTacToeA, Result](Take(p))

  def takeIfNotTaken(p: Position): TicTacToe[Option[Result]] = {
    for {
      op <- info(p)
      or <- op.fold(take(p).map(_.some))(p => none[Result].pure[TicTacToe])
    } yield {
      or
    }
  }

  def runRandom(exceptions: Set[Position] = Set.empty): TicTacToe[Option[Player]] = {
    val rpos = randomPosition(exceptions)
    takeIfNotTaken(rpos).flatMap { or =>
      or match {
        case Some(Result.GameEnded(op)) => op.pure[TicTacToe]
        case Some(Result.AlreadyTaken(p)) => runRandom(exceptions + rpos)
        case Some(Result.NextTurn) => runRandom(exceptions)
        case None    => runRandom(exceptions + rpos)
      }
    }
  }

  def freeState: TicTacToeA ~> SGS =
    new (TicTacToeA ~> SGS) {
      def apply[A](fa: TicTacToeA[A]): SGS[A] =
        fa match {
          case Info(p) => {
            // A must be Option[Player]
            for {
              gs <- State.get[GameState]
            } yield {
              gs.b.get(p)
            }
          }
          case Take(pos) => {
            // need to return a State[GameState, Result]
            State.get[GameState].flatMap { gs =>
              gameEnded(gs.b) match {
                case Some(ge) => ge.asInstanceOf[Result].pure[SGS] // some hackiness here, SGS is invariant, GameState <: Result, but we need to actually make an SGS[Result]
                case None =>
                  gs.b.get(pos) match {
                    case Some(p) => Result.AlreadyTaken(p).asInstanceOf[Result].pure[SGS]
                    case None =>
                      val nb = gs.b + (pos -> gs.p)
                      val ng = GameState(Player.other(gs.p), nb)
                      for {
                        _ <- State.set(ng)
                      } yield {
                        gameEnded(nb).getOrElse(Result.NextTurn)
                      }
                  }
              }
            }
          }
        }
    }
}
