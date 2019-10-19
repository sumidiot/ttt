package sumidiot.bom.ttt

import Common._

import cats._
import cats.implicits._
import cats.data.State


/**
 * With final-style, we set up a typeclass for being a TicTacToe, where the methods
 * return things "wrapped" in the typeclass instance. When we need to chain those methods to define
 * higher-level methods (e.g., takeIfNotTaken below), we add Monad (or Applicative)
 * requirements to F.
 */
object Final extends App {

  trait TicTacToe[F[_]] {
    def info(p: Position): F[Option[Player]]
    def take(p: Position): F[Result]
  }

  /**
   * This syntax ends up letting us write exactly the same implementation of
   * takeIfNotTaken between Final and Free implementations.
   * This makes me think of these a little like the smart constructors.
   */
  object TicTacToeSyntax {
    def info[F[_]](p: Position)(implicit ev: TicTacToe[F]): F[Option[Player]] =
      ev.info(p)

    def take[F[_]](p: Position)(implicit ev: TicTacToe[F]): F[Result] =
      ev.take(p)
  }

  import TicTacToeSyntax._

  def takeIfNotTaken[F[_] : TicTacToe : Monad](p: Position): F[Option[Result]] = {
    for {
      op <- info(p)
      or <- op.fold(take(p).map(_.some))(p => none.pure[F])
    } yield {
      or
    }
  }

  def runRandom[F[_] : TicTacToe : Monad](exceptions: Set[Position] = Set.empty): F[Option[Player]] = {
    val rpos = randomPosition(exceptions)
    takeIfNotTaken(rpos).flatMap { or =>
      or match {
        case Some(Result.GameEnded(op)) => op.pure[F]
        case Some(Result.AlreadyTaken(p)) => runRandom(exceptions + rpos)
        case Some(Result.NextTurn) => runRandom(exceptions)
        case None    => runRandom(exceptions + rpos)
      }
    }
  }

  def winner[F[_]: TicTacToe : Monad]: F[Option[Player]] = {
    def comboWinner(pos1: Position, pos2: Position, pos3: Position): F[Option[Player]] = {
      for {
        pl1 <- info(pos1)
        pl2 <- info(pos2)
        pl3 <- info(pos3)
      } yield {
        for {
          pl1_ <- pl1
          pl2_ <- pl2
          pl3_ <- pl3 if pl1_ == pl2_ && pl2_ == pl3_
        } yield {
          pl3_
        }
      }   
    }
    winningCombos
      .traverse(t => comboWinner(t._1, t._2, t._3))
      .map(_.flatten.headOption)
  }


  object TicTacToe {

    /**
     * This provides an implementation of SGS[_] as a TicTacToe.
     */
    implicit case object SGSIsTicTacToe extends TicTacToe[SGS] {
      def info(p: Position): State[GameState, Option[Player]] = {
        for {
          game <- State.get[GameState]
        } yield {
          game.b.get(p)
        }
      }

      def take(pos: Position): State[GameState, Result] = {
        State(game => {
          Common.gameEnded(game.b) match {
            case Some(ge) => (game, ge)
            case None    =>
              game.b.get(pos) match {
                case Some(p) => (game, Result.AlreadyTaken(p))
                case None    =>
                  val nb = game.b + (pos -> game.p)
                  val ng = GameState(Player.other(game.p), nb)
                  Common.gameEnded(nb) match {
                    case Some(ge) => (ng, ge)
                    case None    => (ng, Result.NextTurn)
                  }
              }
          }
        })
      }

    }
  }


  /**
   * This is the 'main' of the 'App', just a quick demo
   */
  println(runRandom().run(StartingGame).value)

}
