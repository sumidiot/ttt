package sumidiot.bom.ttt

import Common._
import cats.implicits._

/**
 * This is sort of a variant on the typeclass pattern, where the methods in the
 * typeclass expect to be given a T. This makes it maybe closer to the OO implementation,
 * where the Final typeclass looks different.
 *
 * Again, we use the `forceTake` understanding of `take`. See Final for discussion.
 */
object OOTypeclass extends App {

  trait TicTacToe[T] {
    def info(t: T, p: Position): Option[Player]
    def take(t: T, p: Position): T // this is sort of a major difference to the OO version
    def turn(t: T): Player
  }

  object TicTacToe {

    implicit case object GameStateIsTicTacToe extends TicTacToe[GameState] {

      override def info(gs: GameState, p: Position): Option[Player] =
        gs.b.get(p)

      override def take(gs: GameState, p: Position): GameState =
        gs.copy(p = Player.other(gs.p), b = gs.b + (p -> gs.p))

      override def turn(gs: GameState): Player =
        gs.p

    }

    implicit class TicTacToeOps[T: TicTacToe](t: T) {

      def info(p: Position): Option[Player] =
        implicitly[TicTacToe[T]].info(t, p)

      def take(p: Position): T =
        implicitly[TicTacToe[T]].take(t, p)

      def turn(): Player =
        implicitly[TicTacToe[T]].turn(t)

    }

  }

  import TicTacToe._


  def takeIfNotTaken[T: TicTacToe](ttt: T)(p: Position): (T, Option[Result]) =
    ttt.info(p).fold({ val (nt, r) = genTake(ttt)(p); (nt, r.some) })(p => (ttt, none))


  def runRandom[T: TicTacToe](ttt: T)(exceptions: Set[Position] = Set.empty): (T, Option[Player]) = {
    val rpos = randomPosition(exceptions)
    def cont(t: T)(r: Result): (T, Option[Player]) =
      r match {
        case Result.GameEnded(op) => (t, op)
        case _                    => runRandom(t)(exceptions + rpos)
      }
    val (nt, res) = genTake(ttt)(rpos)
    cont(nt)(res)
  }


  /**
   * This implementation was copied over from Final, and then made to work with
   * as few changes as I was able to identify.
   */
  def winner[T: TicTacToe](ttt: T): Option[Player] = {
    
    def playerWins(op1: Option[Player], op2: Option[Player], op3: Option[Player]): Option[Player] =
      for {
        p1 <- op1
        p2 <- op2
        p3 <- op3 if p1 == p2 && p2 == p3
      } yield {
        p3
      }

    /**
     * In Final this was mapN, now just apply the function
     */
    def comboWinner(pos1: Position, pos2: Position, pos3: Position): Option[Player] =
      playerWins(ttt.info(pos1), ttt.info(pos2), ttt.info(pos3))


    /**
     * In Final there was an extra traverse and some mapping in here
     */
    winningCombos
      .map(t => comboWinner(t._1, t._2, t._3)) // List[Option[Player]]
      .flatten.headOption
  }

  /**
   * This implementation was copied over from Final, and then made to work with
   * as few changes as I was able to identify.
   */
  def gameEnded[T: TicTacToe](ttt: T): Option[Result.GameEnded] = {

    /**
     * This one is kind entertaining, compared to the Final version. The traverse here
     * actually pulls out the "all takene" notion that we're looking for.
     */
    def isDraw: Boolean =
      allPositions
        .traverse(pos => ttt.info(pos)) // List[Option[Player]] traversed to Option[List[Player]]
        .isDefined

    def drawResult: Option[Result.GameEnded] =
      if (isDraw) {
        Some(Result.GameEnded(None))
      } else {
        None
      }

    def result(op: Option[Player], or: Option[Result.GameEnded]): Option[Result.GameEnded] =
      op.map(p => Result.GameEnded(Some(p))).orElse(or)

    result(winner(ttt), drawResult)
  }
  
  /**
   * Again, copied from Final and changed to work
   */
  def genTake[T: TicTacToe](ttt: T)(pos: Position): (T, Result) = {

    def forceTakeAndCheck: (T, Result) = {
      val nt = ttt.take(pos)
      (nt, gameEnded(nt).getOrElse(Result.NextTurn))
    }

    def takeSinceNotDone: (T, Result) =
      ttt.info(pos)
         .fold(forceTakeAndCheck)(p => (ttt, Result.AlreadyTaken(p)))

    gameEnded(ttt).map((ttt, _)).getOrElse(takeSinceNotDone)
  }


  /**
   * This is the `main` of the `App` that we are
   */
  val res = runRandom(StartingGame)()
  println(res)
}
