package sumidiot.bom.ttt

import Common._

import cats._
import cats.implicits._
import cats.data.State

/**
 * Example usage:
 * import sumidiot.bom.ttt.Main._
 * val g = GameState(Player.O, Map.empty)
 * takeIfNotTaken(Position(BoardIndex.F, BoardIndex.F)).run(g).value
 */

/**
 * Another, maybe using the implicits above? Maybe unnecessary?
 * import cats.data.State
 * import sumidiot.bom.ttt.Main._
 * import TicTacToeSyntax._ // works because of previous line?
 * val s = State[GameState, Unit]((_, ()))
 * s.info(Position(BoardIndex.F, BoardIndex.F)).run(GameState(Player.X, Map.empty))
 *   // we could also do
 *   // val s2 = State.set(GameState(Player.X, Map.empty))
 *   // s2.run(GameState(Player.O, Map.empty)).value // (GameState(X, Map()), ())
 *   //  s.run(GameState(Player.O, Map.empty)).value // (GameState(O, Map()), ())
 */
object Final extends App {

  trait TicTacToe[F[_]] {
    def info(p: Position): F[Option[Player]]
    def take(p: Position): F[Result]
  }

  def takeIfNotTaken[F[_]: TicTacToe : Monad](p: Position): F[Option[Result]] = {
    val ttt = implicitly[TicTacToe[F]]
    val mf  = implicitly[Monad[F]]
    ttt.info(p).flatMap { _ match {
      case Some(_) => mf.pure(None)
      case None    => ttt.take(p).map(_.some)
    }
    }
  }

  def winner[F[_]: TicTacToe : Monad]: F[Option[Player]] = {
    val ttt = implicitly[TicTacToe[F]]
    val mf  = implicitly[Monad[F]]
    import BoardIndex._
    val combos = List(
                  (Position(F, F), Position(F, S), Position(F, T)),
                  (Position(S, F), Position(S, S), Position(S, T)),
                  (Position(T, F), Position(T, S), Position(T, T)),
                  (Position(F, F), Position(S, F), Position(T, F)),
                  (Position(F, S), Position(S, S), Position(T, S)),
                  (Position(F, T), Position(S, T), Position(T, T)),
                  (Position(F, F), Position(S, S), Position(T, T)),
                  (Position(T, F), Position(S, S), Position(F, T)))
    def comboWinner(pos1: Position, pos2: Position, pos3: Position): F[Option[Player]] = {
      for {
        pl1 <- ttt.info(pos1)
        pl2 <- ttt.info(pos2)
        pl3 <- ttt.info(pos3)
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
    def combosWinner(cs: List[(Position, Position, Position)]): F[Option[Player]] =
      cs match {
        case Nil => mf.pure(None)
        case h::t =>
          comboWinner(h._1, h._2, h._3).flatMap {
            w => w match {
              case None => combosWinner(t)
              case Some(p) => mf.pure(Some(p))
            }
          }
      }
    combosWinner(combos)
  }

  object TicTacToeSyntax {
    implicit class TicTacToeOps[F[_] : TicTacToe : Monad, X](f: F[X]) {
      def info(p: Position): F[Option[Player]] =
        implicitly[TicTacToe[F]].info(p)

      def take(p: Position): F[Result] =
        implicitly[TicTacToe[F]].take(p)
    }
  }


  object TicTacToe {

    /**
     * There's something I don't like about the below. Sort of, it's saying that
     * there's one way to use State[GameState, X] to play TicTacToe. Maybe that's right.
     * But how does that relate to being able to make State instances in a few different
     * ways, and then being able to call .info or .take on them (with the Ops above)?
     * It's sorta like that implicit ops thing is not what I should be doing, except, I
     * guess, that at the abstract level, we might not know that for a given F[_] there'd
     * only be one reasonable implementation of TicTacToe[F]?
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
          Common.winner(game.b) match {
            case Some(p) => (game, Result.GameEnded(p))
            case None    =>
              game.b.get(pos) match {
                case Some(p) => (game, Result.AlreadyTaken(p))
                case None    =>
                  val nb = game.b + (pos -> game.p)
                  val ng = GameState(Player.other(game.p), nb)
                  Common.winner(nb) match {
                    case Some(p) => (ng, Result.GameEnded(p))
                    case None    => (ng, Result.NextTurn)
                  }
              }
          }
        })
      }

    }
  }

}
