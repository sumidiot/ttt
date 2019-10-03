package sumidiot.bom.ttt

import cats._
import cats.implicits._

object Main extends App {

  sealed trait BoardIndex
  object BoardIndex {
    final case object F extends BoardIndex
    final case object S extends BoardIndex
    final case object T extends BoardIndex
  }
  final case class Position(row: BoardIndex, col: BoardIndex)

  sealed trait Player
  object Player {
    final case object X extends Player
    final case object O extends Player

    def other(p: Player): Player =
      p match {
        case X => O
        case O => X
      }
  }

  sealed trait Result
  object Result {
    final case class AlreadyTaken(by: Player) extends Result
    final case object NextTurn extends Result
    final case class GameEnded(winner: Player) extends Result
  }

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
    implicit class TicTacToeOps[F](f: F) {
      def info[F[_]: TicTacToe : Monad](p: Position): F[Option[Player]] =
        implicitly[TicTacToe[F]].info(p)
    }
  }

  type Board = Map[Position, Player]
  case class GameState(p: Player, b: Board)

  object TicTacToe {
    import cats.data.State
    type SGS[X] = State[GameState, X]
    implicit case object ThingIsTTT extends TicTacToe[SGS] {
      def info(p: Position): State[GameState, Option[Player]] = {
        State(game => {
          (game, game.b.get(p))
        })
      }

      def take(pos: Position): State[GameState, Result] = {
        State(game => {
          winner(game.b) match {
            case Some(p) => (game, Result.GameEnded(p))
            case None    =>
              game.b.get(pos) match {
                case Some(p) => (game, Result.AlreadyTaken(p))
                case None    =>
                  val nb = game.b + (pos -> game.p)
                  val ng = GameState(Player.other(game.p), nb)
                  winner(nb) match {
                    case Some(p) => (ng, Result.GameEnded(p))
                    case None    => (ng, Result.NextTurn)
                  }
              }
          }
        })
      }

      def winner(b: Board): Option[Player] = {
        /**
         * Well, this is entertaining. I need `winner` to be defined to define
         * `take`, so that I can show I'm a TicTacToe, after which point I can
         * use the more generic `winner`. Clearly I've done something wrong.
         */
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

        def comboWinner(pos1: Position, pos2: Position, pos3: Position): Option[Player] = {
          for {
            pl1 <- b.get(pos1)
            pl2 <- b.get(pos2)
            pl3 <- b.get(pos3) if pl1 == pl2 && pl2 == pl3
          } yield {
            pl3
          }   
        }
        def combosWinner(cs: List[(Position, Position, Position)]): Option[Player] =
          cs match {
            case Nil => None
            case h::t =>
              comboWinner(h._1, h._2, h._3).orElse(combosWinner(t))
          }
        combosWinner(combos)
      }
    }
  }

  /**
   * Example usage:
   * import sumidiot.bom.ttt.Main._
   * val g = GameState(Player.O, Map.empty)
   * implicit val i = TicTacToe.ThingIsTTT
   * takeIfNotTaken(Position(BoardIndex.F, BoardIndex.F)).run(g).value
   * takeIfNotTaken(Position(BoardIndex.S, BoardIndex.F)).run(res5._1).value
   */

  /**
   * Another, maybe using the implicits above? Maybe unnecessary?
   * imports*
   * val s = State[GameState, Unit](gs => (gs, ()))
   *   // how is that different from s2.set(GameState(Player.X, Map.empty)) ?
   * s.info(Position(BoardIndex.F, BoardIndex.F)).run(GameState(Player.X, Map.empty))
   */
}
