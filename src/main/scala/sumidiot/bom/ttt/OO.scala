package sumidiot.bom.ttt

import Common._
import cats.implicits._

/**
 * This object aims to provide something non-Monad-y, that might be what more people
 * might be used to seeing, sort of object-oriented-y.
 *
 * We use, here, the "forceTake" interpretation of take. See Final for discussion.
 */
object OO extends App {

  trait TicTacToe {
    def info(p: Position): Option[Player]
    def take(p: Position): Unit
    def turn(): Player
  }

  /**
   * Relying on a var isn't very "functional"-y, but let's see it anyway.
   */
  class EmbeddedVarTicTacToe(var gs: GameState = StartingGame) extends TicTacToe {

    override def info(p: Position): Option[Player] =
      gs.b.get(p)

    override def take(p: Position): Unit =
      gs = gs.copy(p = Player.other(gs.p), b = gs.b + (p -> gs.p))

    override def turn(): Player =
      gs.p

  }


  def takeIfNotTaken(ttt: TicTacToe)(p: Position): Option[Result] =
    ttt.info(p).fold(genTake(ttt)(p).some)(p => none)


  def runRandom(ttt: TicTacToe)(exceptions: Set[Position] = Set.empty): Option[Player] = {
    val rpos = randomPosition(exceptions)
    def cont(r: Result): Option[Player] =
      r match {
        case Result.GameEnded(op) => op
        case _                    => runRandom(ttt)(exceptions + rpos)
      }
    cont(genTake(ttt)(rpos))
  }


  /**
   * This implementation was copied over from Final, and then made to work with
   * as few changes as I was able to identify.
   */
  def winner(ttt: TicTacToe): Option[Player] = {
    
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
  def gameEnded(ttt: TicTacToe): Option[Result.GameEnded] = {

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
  def genTake(ttt: TicTacToe)(pos: Position): Result = {

    def forceTakeAndCheck: Result = {
      ttt.take(pos)
      gameEnded(ttt).getOrElse(Result.NextTurn)
    }

    def takeSinceNotDone: Result =
      ttt.info(pos)
         .fold(forceTakeAndCheck)(p => Result.AlreadyTaken(p))

    gameEnded(ttt).getOrElse(takeSinceNotDone)
  }


  /**
   * This is the `main` of the `App` that we are
   */
  val ttt = new EmbeddedVarTicTacToe()
  val res = runRandom(ttt)()
  println(ttt.gs)
  println(res)
}