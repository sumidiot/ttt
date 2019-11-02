package sumidiot.bom.ttt

import cats.data.State
import cats.implicits._
import scala.util.Try

object Common {
  
  sealed trait BoardIndex
  object BoardIndex {
    final case object F extends BoardIndex
    final case object S extends BoardIndex
    final case object T extends BoardIndex
  }
  final case class Position(row: BoardIndex, col: BoardIndex)

  object Position {

    def apply(s: String): Option[Position] =
      Try {
        def idx(c: Char): BoardIndex =
          c match {
            case 'F' => BoardIndex.F
            case 'S' => BoardIndex.S
            case 'T' => BoardIndex.T
            // partial function, deal with it
          }
        val r = s(0)
        val c = s(1)
        val R = idx(r)
        val C = idx(c)
        Position(R, C)
      }.toOption
  }

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

  val winningCombos = {
    import BoardIndex._
    List(
         (Position(F, F), Position(F, S), Position(F, T)),
         (Position(S, F), Position(S, S), Position(S, T)),
         (Position(T, F), Position(T, S), Position(T, T)),
         (Position(F, F), Position(S, F), Position(T, F)),
         (Position(F, S), Position(S, S), Position(T, S)),
         (Position(F, T), Position(S, T), Position(T, T)),
         (Position(F, F), Position(S, S), Position(T, T)),
         (Position(T, F), Position(S, S), Position(F, T))
    )
  }

  sealed trait Player
  object Player {
    final case object X extends Player
    final case object O extends Player

    def other(p: Player): Player =
      p match {
        case X => O
        case O => X
      }

    /**
     * Really this should return an Option or something, but that's a different exercise
     */
    def apply(char: Char): Player =
      char match {
        case 'X' => X
        case 'O' => O
        case _   => ???
      }
  }

  sealed trait Result
  object Result {
    final case class AlreadyTaken(by: Player) extends Result
    final case object NextTurn extends Result
    final case class GameEnded(winner: Option[Player]) extends Result
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
  object Board {

    def show(b: Board): String = {
      def bchar(r: BoardIndex, c: BoardIndex): String =
        b.get(Position(r, c)).map(_.toString).getOrElse(".")
      s"""
      |  ${bchar(BoardIndex.F, BoardIndex.F)} | ${bchar(BoardIndex.F, BoardIndex.S)} | ${bchar(BoardIndex.F, BoardIndex.T)}
      |  ---------
      |  ${bchar(BoardIndex.S, BoardIndex.F)} | ${bchar(BoardIndex.S, BoardIndex.S)} | ${bchar(BoardIndex.S, BoardIndex.T)}
      |  ---------
      |  ${bchar(BoardIndex.T, BoardIndex.F)} | ${bchar(BoardIndex.T, BoardIndex.S)} | ${bchar(BoardIndex.T, BoardIndex.T)}
      """.stripMargin
    }

    def apply(lpop: List[(Position, Option[Player])]): Board =
      lpop.flatMap(pop => pop._2.map(p => pop._1 -> p)).toMap

  }

  /**
   * This method returns a list of plays which has no prefix subsequence which results
   * in a winning board. The plays correspond to a normal, fair game, with X starting.
   * The final state may be a filled board with no winner, a finished game with a winner,
   * or an unfinished game with no winner.
   */
  def randomPlaySequence: List[(Position, Player)] = {
    def potentiallySuperfluousPlays: List[(Position, Player)] = {
      // Without abs, many values are negative, so .take produces empty list
      val numPlays = math.abs(scala.util.Random.nextInt()) % (allPositions.size + 1)
      val positions = scala.util.Random.shuffle(allPositions).take(numPlays)
      val players = List.tabulate(numPlays)(i => if (i % 2 == 0) { Player.X } else { Player.O })
      positions.zip(players)
    }
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
    nonsuperfluousPlays(potentiallySuperfluousPlays)
  }

  case class GameState(p: Player, b: Board) {
    override def toString(): String = {
      s"""
      |Player $p's turn, given:
      |${Board.show(b)}
      """.stripMargin
    }
  }

  val StartingGame = GameState(Player.X, Map.empty)

  type SGS[X] = State[GameState, X]

  /**
   * This object some stores some simple methods which rely entirely on the State
   * implementation for results. Note that all of them are abstracted up and out in
   * Final and Free.
   */
  object StateCheats {

    def gameEnded(b: Board): Option[Result.GameEnded] = {
      winner(b) match {
        case w@Some(p) => Some(Result.GameEnded(w))
        case None =>
          if (isDraw(b)) {
            Some(Result.GameEnded(None))
          } else {
            None
          }
      }
    }

    def isDraw(b: Board): Boolean = {
      allPositions.traverse(b.get).isDefined
    }

    def winner(b: Board): Option[Player] = {
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
      combosWinner(winningCombos)
    }

  }

}
