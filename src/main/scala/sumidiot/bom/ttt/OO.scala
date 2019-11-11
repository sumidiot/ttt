package sumidiot.bom.ttt

import Common._
import cats.implicits._

import scala.util.Try
import scala.io.StdIn.readLine

import sumidiot.bom.ttt.{Doobie => Doo}
import doobie._
import cats.effect._

/**
 * This object aims to provide something non-Monad-y, that might be what more people
 * might be used to seeing, sort of object-oriented-y.
 *
 * We use, here, the "forceTake" interpretation of take. See Final for discussion.
 * Note, also, that we don't use `switchPlayer` from Final, that's not entirely intentional,
 * we're still sorting that out over in the Final implementations anyway.
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

  /**
   * This silly implementation asks the user to keep track of things. Note that if you
   * runRandom on this, it'll ask, a lot of times, about the whole board, because it's
   * always checking for winners and the game being done.
   */
  class InteractiveLawlessTicTacToe extends TicTacToe {

    override def info(p: Position): Option[Player] =
      Try({
        Some(Player(readLine(s"Who owns $p? ")(0)))
      }).getOrElse(None)

    override def take(p: Position): Unit =
      println(s"Please let the current player take $p")

    override def turn(): Player =
      Try({
        Player(readLine("Who'se turn is it? ")(0))
      }).getOrElse(Player.X)
  }

  class DoobieTicTacToe[T <: Transactor[IO]](transactor: Resource[IO, T] = Doo.h2transactor) extends TicTacToe {

    override def info(p: Position): Option[Player] =
      Doo.run(transactor)(Doo.Queries.info(p)).unsafeRunSync

    override def take(p: Position): Unit = {
      Doo.run(transactor)(Doo.Queries.take(p)).unsafeRunSync
      Doo.run(transactor)(Doo.Queries.switchPlayer).unsafeRunSync
    }

    override def turn(): Player =
      Doo.run(transactor)(Doo.Queries.turn).unsafeRunSync

  }


  def takeIfNotTaken(ttt: TicTacToe)(p: Position): Option[Result] =
    ttt.info(p).fold(genTake(ttt)(p).some)(p => none)
 

  def board(ttt: TicTacToe): Board =
    allPositions.flatMap { p => ttt.info(p).map(op => p -> op) }.toMap


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
     * actually pulls out the "all taken" notion that we're looking for.
     */
    def isDraw: Boolean =
      allPositions
        .traverse(pos => ttt.info(pos)) // List[Option[Player]] traversed to Option[List[Player]]
        .isDefined // .traverse().isDefined is the same as the built-in .forall(_.isDefined)

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
   * This is the `main` of the `App` that we are, split up into blocks for various versions,
   * like the Final `main`
   */
  {
    {
      /**
       * This version one is the version with an embedded var
       */
      println("Starting EmbeddedVarTicTacToe")
      val ttt = new EmbeddedVarTicTacToe()
      val res = runRandom(ttt)()
      println(ttt.gs)
      println(res)
      println("Leaving EmbeddedVarTicTacToe")
    }

    {
      /**
       * This one uses the doobie version
       */
      println("Starting DoobieTicTacToe")
      val ttt = new DoobieTicTacToe(Doobie.h2transactor)
      Doo.initializeDB(Doobie.h2transactor)
      val res = runRandom(ttt)()
      println(Board.show(board(ttt)))
      println(res)
      println("Leaving DoobieTicTacToe")
    }
  }
}
