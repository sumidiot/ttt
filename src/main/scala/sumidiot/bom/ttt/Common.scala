package sumidiot.bom.ttt

import cats.data.State

object Common {
  
  sealed trait BoardIndex
  object BoardIndex {
    final case object F extends BoardIndex
    final case object S extends BoardIndex
    final case object T extends BoardIndex
  }
  final case class Position(row: BoardIndex, col: BoardIndex)

  val allPositions =
    List(
      Position(BoardIndex.F, BoardIndex.F),
      Position(BoardIndex.F, BoardIndex.S),
      Position(BoardIndex.F, BoardIndex.T),
      Position(BoardIndex.S, BoardIndex.F),
      Position(BoardIndex.S, BoardIndex.S),
      Position(BoardIndex.S, BoardIndex.T),
      Position(BoardIndex.T, BoardIndex.F),
      Position(BoardIndex.T, BoardIndex.S),
      Position(BoardIndex.T, BoardIndex.T),
    )
  def randomPosition(exceptions: Set[Position]): Position =
    scala.util.Random.shuffle((allPositions.toSet -- exceptions).toList).head

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

  /**
   * These are one choice we could make about how to represent a game.
   * Another is a string with the first character being whose turn it is,
   * and then 9 subsequent characters for the board positions, with X, O, or . in them.
   * In that case, we might still have a State[String, ] monad, and we'd have one
   * implementation of TicTacToe for it. This, of course, is mostly just relying on
   * some equivalance between two different representations of GameState. In some sense,
   * you might imagine a TicTacToeState trait, with operations like info and take,
   * and then State[TTTS, _] is TicTacToe for any TTTS : TicTacToeState. That seems
   * duplicative, or maybe I've missed the mark with my SGS bits below.
   *
   * What would a non-State-based implementation look like? Maybe a database, so IO?
   */
  type Board = Map[Position, Player]
  case class GameState(p: Player, b: Board) {
    override def toString(): String = {
      def bchar(r: BoardIndex, c: BoardIndex): String =
        b.get(Position(r, c)).map(_.toString).getOrElse(".")
      s"""
      |Player $p's turn, given:
      |  ${bchar(BoardIndex.F, BoardIndex.F)} | ${bchar(BoardIndex.F, BoardIndex.S)} | ${bchar(BoardIndex.F, BoardIndex.T)}
      |  ---------
      |  ${bchar(BoardIndex.S, BoardIndex.F)} | ${bchar(BoardIndex.S, BoardIndex.S)} | ${bchar(BoardIndex.S, BoardIndex.T)}
      |  ---------
      |  ${bchar(BoardIndex.T, BoardIndex.F)} | ${bchar(BoardIndex.T, BoardIndex.S)} | ${bchar(BoardIndex.T, BoardIndex.T)}
      """.stripMargin
    }
  }

  val StartingGame = GameState(Player.X, Map.empty)

  type SGS[X] = State[GameState, X]

  def winner(b: Board): Option[Player] = {
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
