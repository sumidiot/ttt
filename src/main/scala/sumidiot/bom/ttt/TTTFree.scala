package sumidiot.bom.ttt

import cats._
import cats.data.State
import cats.implicits._
import cats.free.Free
import cats.free.Free.liftF
import cats.~>
import Common._


/**
 * This object is largely filled with duplicate code from the Final implementation.
 * Mostly that's because, really, we could provide evidence that the TicTacToe[_]
 * we define with Free implements the TicTacToe interface required by Final, and so
 * we could use the Final implementation of all of the Free version (since Free gives
 * you Monad also, which is the added requirement of some of the methods in Final).
 */
object TTTFree extends App {

  sealed abstract class TicTacToeA[A]
  case class Info(p: Position) extends TicTacToeA[Option[Player]]
  case class Take(p: Position) extends TicTacToeA[Unit]

  type TicTacToe[A] = Free[TicTacToeA, A]

  def info(p: Position): TicTacToe[Option[Player]] =
    liftF[TicTacToeA, Option[Player]](Info(p))

  def take(p: Position): TicTacToe[Unit] =
    liftF[TicTacToeA, Unit](Take(p))

  def takeIfNotTaken(p: Position): TicTacToe[Option[Result]] = {
    for {
      op <- info(p)
      or <- op.fold(genTake(p).map(_.some))(p => none.pure[TicTacToe])
    } yield {
      or
    }
  }

  def runRandom(exceptions: Set[Position] = Set.empty): TicTacToe[Option[Player]] = {
    val rpos = randomPosition(exceptions)
    def cont(r: Result): TicTacToe[Option[Player]] =
      r match {
        case Result.GameEnded(op) => op.pure[TicTacToe]
        case _                    => runRandom(exceptions + rpos)
      }
    for {
      r <- genTake(rpos)
      res <- cont(r)
    } yield {
      res
    }
  }

  def winner: TicTacToe[Option[Player]] = {
    
    /**
     * We implement this method with two helper methods, and the implementation
     * works bottom-up - the final evaluated expression is a .traverse at the bottom,
     * calling the method in the middle, which calls the top function. Basically,
     * the idea is to, for each winning combination, pull out which player occupies
     * each cell in that combo (this is the middle function, `comboWinner`), and
     * given those three `Option[Player]` see if they're all defined and the same player.
     */

    def playerWins(op1: Option[Player], op2: Option[Player], op3: Option[Player]): Option[Player] =
      for {
        p1 <- op1
        p2 <- op2
        p3 <- op3 if p1 == p2 && p2 == p3
      } yield {
        p3
      }
    def comboWinner(pos1: Position, pos2: Position, pos3: Position): TicTacToe[Option[Player]] =
      (info(pos1), info(pos2), info(pos3)).mapN(playerWins)

    winningCombos
      .traverse(t => comboWinner(t._1, t._2, t._3))
      .map(_.flatten.headOption)
  }

  /**
   * Given just the `info` method of a `TicTacToe` we can check if the game has ended and,
   * if it has, who has won. Like winner, this relies on the extra `Applicative` contraint.
   */
  def gameEnded: TicTacToe[Option[Result.GameEnded]] = {

    /**
     * This method is implemented somewhat bottom-up. The final for-comprehension at the bottom
     * relies on the `drawResult` method in the middle, which calls the top `isDraw` method.
     * Basically the idea is:
     *   1. See if there's a winner - if so, we're done
     *   2. Otherwise, see if we're in a draw (all positions taken) and return as appropriate
     */

    def isDraw: TicTacToe[Boolean] =
      allPositions
        .traverse(pos => info(pos))
        .map(_.traverse(x => x).isDefined)

    def drawResult: TicTacToe[Option[Result.GameEnded]] =
      isDraw.map(d =>
          if (d) {
            Some(Result.GameEnded(None))
          } else {
            None
          }
      )

    def result(op: Option[Player], or: Option[Result.GameEnded]): Option[Result.GameEnded] =
      op.map(p => Result.GameEnded(Some(p))).orElse(or)

    (winner, drawResult).mapN(result)
  }
  
  /**
   * We know most of the steps required to generically 'take' a position,
   * if somebody can "force" the move. In particular, we must first check
   * if the game is already done, sort of as an initial guard. If the game
   * isn't done, we can check if the position is already taken. If it's not
   * then we can forceTake, and then check the game state.
   */
  def genTake(pos: Position): TicTacToe[Result] = {

    /**
     * This implementation reads sort of from the bottom up. The first def
     * is used in the next def, which is used in the final `for`-comprehension.
     *
     * Note that there are 3 types of Result that can be returned:
     *   1. GameEnded, which we basically pass through from `gameEnded`
     *   2. AlreadyTaken, if the position is already taken
     *   3. NextTurn, if we successfully took the position and the game isn't over
     *       (or, if it is, we'll return that GameEnded)
     */

    def forceTakeAndCheck(pos: Position): TicTacToe[Result] =
      for {
        _ <- take(pos)
        ge <- gameEnded.map(_.getOrElse(Result.NextTurn))
      } yield {
        ge
      }

    def takeSinceNotDone(pos: Position): TicTacToe[Result] =
      for {
        op <- info(pos) // op is an Option[Player]
        res <- op.fold(forceTakeAndCheck(pos))(p => (Result.AlreadyTaken(p) : Result).pure[TicTacToe])
      } yield {
        res
      }

    for {
      ge <- gameEnded
      res <- ge.fold(takeSinceNotDone(pos))(r => (r: Result).pure[TicTacToe])
    } yield {
      res
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
           for {
              game <- State.get[GameState]
              nb = game.b + (pos -> game.p)
              ng = GameState(Player.other(game.p), nb)
              _ <- State.set(ng)
            } yield { () }

            /*
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
            */
          }
        }
    }


  /**
   * This is the 'main' of this 'App', just a quick little demo
   */
  println(runRandom().foldMap(freeState).run(StartingGame).value)
}
