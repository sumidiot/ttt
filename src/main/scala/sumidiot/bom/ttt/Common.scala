package sumidiot.bom.ttt

import cats.data.State
import scala.util.Try

/**
 * Some utilities for use across implementations, including the base types, which
 * we summarize here:
 *   * Position(row, col), where row and col are represented by a BoardIndex, one of
 *     F, S, or T ("First", "Second", "Third")
 *   * Player, one of X or Y
 *   * Board, a Map[Position, Player]
 *   * GameState(player, board), the current taken spots, along with whose turn it is
 *   * Result, the result of trying to take a position. This is "already taken",
 *     "ok, next player's turn", or "game is over" (which would be the result if the
 *     game was over before you tried to take the spot, or if it wasn't, but your taking
 *     the spot was successful and it caused you to win the game)
 */
object Common {
  
  /**
   * How we refer to coordinates in the game board. Here we use the same names for
   * the coordinates in rows or columns
   */
  sealed trait BoardIndex
  object BoardIndex {
    final case object F extends BoardIndex
    final case object S extends BoardIndex
    final case object T extends BoardIndex
  }

  /**
   * A Position represents a location on the game board
   */
  final case class Position(row: BoardIndex, col: BoardIndex)

  object Position {
    def apply(p: (BoardIndex, BoardIndex)): Position =
      Position(p._1, p._2)

    // ugly hack utility for converting String to Position
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

  /**
   * List all the available positions on the board, for convenience.
   * Using cats.Semigroupal.mapN because we can, though writing out the 9 board positions
   * is easier and quicker.
   */
  val allPositions = {
    import cats.implicits._
    val indices = List(BoardIndex.F, BoardIndex.S, BoardIndex.T)
    (indices, indices).mapN(Position.apply)
  }
 
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

  /**
   * A "Result" is one of the types given by the "Book of Monads" setup for this exercise.
   * It is the "Result" of trying to "take" a board position. In most of our implementations,
   * this means the "genTake" method, where inner implementations are only required to
   * implement a "forceTake" method. See the README for more.
   */
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
   * or an unfinished game with no winner. An un-started game is possible.
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
          val w = BoardHelpers.winner(b)
          (nl, w)
        }
      })._1
    }
    nonsuperfluousPlays(potentiallySuperfluousPlays)
  }

  case class GameState(player: Player, board: Board) {
    override def toString(): String = {
      s"""
      |Player $player's turn, given:
      |${Board.show(board)}
      """.stripMargin
    }
  }

  /**
   * We allow X to be the default starting player
   */
  val StartingGame = GameState(Player.X, Map.empty)

  /**
   * This sets up a State-based implementation
   */
  type SGS[X] = State[GameState, X]

  /**
   * This object some stores some simple methods which rely entirely on the GameState
   * implementation for results. Note that all of them are abstracted up and out in
   * Final and Free.
   */
  object BoardHelpers {

    /**
     * A game has ended if there is a winner, or we're in a draw, with a full board
     */
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

    /**
     * We're in a draw if all positions are taken. Note that this doesn't check
     * if there's a winner, so you shouldn't really use this method, you should use gameEnded.
     */
    def isDraw(b: Board): Boolean =
      allPositions.forall(p => b.get(p).isDefined)
      // allPositions.traverse(b.get).isDefined // <-- equivalent formulation, with cats.implicits._

    /**
     * This doesn't check that the board is in a state that is actually reachable by
     * following the rules of the game. In particular, a board with Xs down one row,
     * and Os down the other, is a reasonable object to make, even though it is unreachable
     * in normal play (X goes first, so they would get to place their third piece, and thus
     * win, before O got to place their third piece).
     *
     * The result of this is that, for poorly formed boards, the winner may not be entirely
     * well-defined, and this method will return one of the two winners.
     */
    def winner(b: Board): Option[Player] = {
      def playerWins(op1: Option[Player], op2: Option[Player], op3: Option[Player]): Option[Player] =
        for {
          pl1 <- op1
          pl2 <- op2
          pl3 <- op3 if pl1 == pl2 && pl2 == pl3
        } yield {
          pl3
        }

      def comboWinner(pos1: Position, pos2: Position, pos3: Position): Option[Player] =
        playerWins(b.get(pos1), b.get(pos2), b.get(pos3))

      winningCombos
        .map(t => comboWinner(t._1, t._2, t._3))
        .flatten.headOption
    }

  }

}
