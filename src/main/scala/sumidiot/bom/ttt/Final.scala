package sumidiot.bom.ttt

import Common._

import cats._
import cats.implicits._

import cats.effect.LiftIO
import cats.effect.IO
import cats.effect.Console.io._


/**
 * With final-style, we set up a typeclass for being a TicTacToe, where the methods
 * return things "wrapped" in the typeclass instance. When we need to chain those methods to define
 * higher-level methods (e.g., takeIfNotTaken below), we add Monad (or Applicative)
 * requirements to F.
 */
object Final extends App {

  /**
   * The original version of this was supposed to have a
   *   take(p: Position): F[Result]
   *
   * However, we have since implemented `genTake`, which requires the `forceTake` version
   *
   * Now, genTake requires a monad, where the original `take` suggestion possibly
   * got around that. However, all `take` implementations would likely have the same
   * (`genTake`) pattern.
   *
   * In the real world, we'd probably set visibility restrictions on `forceTake`,
   * and require users to rely on `genTake` (and maybe rename it back to just `take`).
   *
   * Note that we have also added `turn`. We anticipate updating this to be Option[Player]
   * in the near future, to account for games which have completed.
   */
  trait TicTacToe[F[_]] {
    def info(p: Position): F[Option[Player]]
    def forceTake(p: Position): F[Unit]
    def turn(): F[Player]
    def switchPlayer(): F[Unit]
  }

  /**
   * This syntax ends up letting us write exactly the same implementation of
   * takeIfNotTaken between Final and Free implementations.
   * This makes me think of these a little like the smart constructors.
   */
  object TicTacToeSyntax {
    def info[F[_]](p: Position)(implicit ev: TicTacToe[F]): F[Option[Player]] =
      ev.info(p)

    def forceTake[F[_]](p: Position)(implicit ev: TicTacToe[F]): F[Unit] =
      ev.forceTake(p)

    def turn[F[_]]()(implicit ev: TicTacToe[F]): F[Player] =
      ev.turn()

    def switchPlayer[F[_]]()(implicit ev: TicTacToe[F]): F[Unit] =
      ev.switchPlayer()

  }

  import TicTacToeSyntax._

  /**
   * This was the method suggested as an exercise in the book. Note, here,
   * we rely on our `genTake`, instead of the original `take`, because of our interpretation
   * of `forceTake`.
   */
  def takeIfNotTaken[F[_] : TicTacToe : Monad](p: Position): F[Option[Result]] =
    for {
      op <- info(p)
      or <- op.fold(genTake(p).map(_.some))(pl => none.pure[F])
    } yield {
      or
    }


  /**
   * Extract all taken position information from the TicTacToe to obtain a Board
   * representation of the state
   */
  def board[F[_] : TicTacToe : Applicative]: F[Board] =
    allPositions.traverse { p => info(p).map(op => p -> op) }.map(Board.apply)

  /**
   * This method is roughly just recursive, taking a random step until that
   * causes the game to be over
   */
  def runRandom[F[_] : TicTacToe : Monad](exceptions: Set[Position] = Set.empty): F[Option[Player]] = {
    val rpos = randomPosition(exceptions)
    def cont(r: Result): F[Option[Player]] =
      r match {
        case Result.GameEnded(op)   => op.pure[F]
        case Result.NextTurn        =>
          for {
            //_ <- switchPlayer() // i'm still sussing out where this belongs to be called
            res <- runRandom(exceptions + rpos)
          } yield {
            res
          }
        case Result.AlreadyTaken(_) => runRandom(exceptions + rpos)
      }
    for {
      r <- genTake(rpos)
      res <- cont(r)
    } yield {
      res
    }
  }

  /**
   * The goal with this method is to present sort of an interactive program for playing.
   *
   * Generally the idea is something like:
   *   Present the current board
   *   Prompt with: "Move for Player <P>? "
   *   Ask for input, interpret as board position to take, try to take it, and iterate
   * 
   * When the game is done, prompt should be "Draw" or "Won by <P>" and not ask for input
   */
  def run1IO[F[_] : TicTacToe : Monad](implicit L: LiftIO[F]): F[Result] = {
    def readUntilValid: F[Position] =
      for {
        l <- L.liftIO(readLn)
        m <- Position(l).map(_.pure[F]).getOrElse(readUntilValid)
      } yield {
        m
      }
    for {
      b <- board
      _ <- L.liftIO(putStrLn(Board.show(b)))
      p <- turn
      _ <- L.liftIO(putStrLn(s"Move for $p? "))
      m <- readUntilValid
      r <- genTake(m)
    } yield {
      r
    }
  }

  def runIO[F[_] : TicTacToe : Monad](implicit L: LiftIO[F]): F[Option[Player]] = {
    def cont(r: Result) : F[Option[Player]] =
      r match {
        case Result.GameEnded(op)   => op.pure[F]
        case Result.NextTurn        => runIO
        case Result.AlreadyTaken(_) => runIO
      }
    // compare this 1-line with the for-comprehension in runRandom
    run1IO.flatMap(cont)
  }

  /**
   * We can generically check for a winner by running through all the
   * winning combinations. This relies only on the `info` method of the `TicTacToe`,
   * adding also the Applicative constraint.
   */
  def winner[F[_] : TicTacToe : Applicative]: F[Option[Player]] = {
    
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
    def comboWinner(pos1: Position, pos2: Position, pos3: Position): F[Option[Player]] =
      (info(pos1), info(pos2), info(pos3)).mapN(playerWins)

    winningCombos
      .traverse(t => comboWinner(t._1, t._2, t._3))
      .map(_.flatten.headOption)
  }

  /**
   * Given just the `info` method of a `TicTacToe` we can check if the game has ended and,
   * if it has, who has won. Like winner, this relies on the extra `Applicative` contraint.
   */
  def gameEnded[F[_] : TicTacToe : Applicative]: F[Option[Result.GameEnded]] = {

    /**
     * This method is implemented somewhat bottom-up. The final for-comprehension at the bottom
     * relies on the `drawResult` method in the middle, which calls the top `isDraw` method.
     * Basically the idea is:
     *   1. See if there's a winner - if so, we're done
     *   2. Otherwise, see if we're in a draw (all positions taken) and return as appropriate
     */

    def isDraw: F[Boolean] =
      allPositions
        .traverse(pos => info(pos))
        .map(_.traverse(x => x).isDefined)

    def drawResult: F[Option[Result.GameEnded]] =
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
   * then we can forceTake, and then check the game state to see if that turn
   * caused the player to win.
   */
  def genTake[F[_] : TicTacToe : Monad](pos: Position): F[Result] = {

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

    def forceTakeAndCheck(pos: Position): F[Result] =
      for {
        _ <- forceTake(pos)
        ge <- gameEnded.map(_.getOrElse(Result.NextTurn))
        _ <- switchPlayer()
      } yield {
        ge
      }

    def takeSinceNotDone(pos: Position): F[Result] =
      for {
        op <- info(pos) // op is an Option[Player]
        res <- op.fold(forceTakeAndCheck(pos))(p => (Result.AlreadyTaken(p) : Result).pure[F])
      } yield {
        res
      }

    for {
      ge <- gameEnded
      res <- ge.fold(takeSinceNotDone(pos))(r => (r: Result).pure[F])
    } yield {
      res
    }
  }


  /**
   * This object contains a bunch of instances of the TicTacToe typeclass. All of them
   * are for types which happen to be Monads, so we don't have to provide any new instances
   * of that (or Applicative).
   */
  object Instances {

    object SGS {

      import cats.data.State

      /**
       * This provides an implementation of SGS[_] as a TicTacToe.
       */
      implicit case object SGSIsTicTacToe extends TicTacToe[SGS] {
        override def info(p: Position): State[GameState, Option[Player]] =
          for {
            game <- State.get[GameState]
          } yield {
            game.board.get(p)
          }

        override def forceTake(pos: Position): State[GameState, Unit] =
          for {
            game <- State.get[GameState]
            nb = game.board + (pos -> game.player)
            _ <- State.set(game.copy(board = nb))
          } yield { () }

        override def turn(): State[GameState, Player] =
          for {
            game <- State.get[GameState]
          } yield {
            game.player
          }

        override def switchPlayer(): State[GameState, Unit] =
          for {
            game <- State.get[GameState]
            _ <- State.set(game.copy(player = Player.other(game.player)))
          } yield { () }

      }
    
      /**
       * I'd be surprised if this is the "correct" thing to do, but it does seem to work.
       * This implicit is used if you use the runIO runner with SGS.
       */
      implicit def stateLiftIO: LiftIO[SGS] =
        new LiftIO[SGS] {
          override def liftIO[A](ioa: IO[A]): SGS[A] =
            State.pure(ioa.unsafeRunSync())
        }

    }
    
    object Doobie {

      import doobie._
      import sumidiot.bom.ttt.{Doobie => Doo}

      /**
       * This provides an implementation of IO[_] as a TicTacToe, using doobie to interact
       * with a database. Realistically, we should probably make this an instance of
       * ConnectionIO, and later transact it into IO.
       */
      implicit case object DoobieIOTicTacToe extends TicTacToe[IO] {
        override def info(p: Position): IO[Option[Player]] =
          Doo.run(Doo.h2transactor)(Doo.Queries.info(p))

        override def forceTake(pos: Position): IO[Unit] =
          Doo.run(Doo.h2transactor)(Doo.Queries.take(pos))
        
        override def turn(): IO[Player] =
          Doo.run(Doo.h2transactor)(Doo.Queries.turn)

        override def switchPlayer(): IO[Unit] =
          Doo.run(Doo.h2transactor)(Doo.Queries.switchPlayer)

      }

      implicit case object DoobieConnectionIOTicTacToe extends TicTacToe[ConnectionIO] {
        override def info(p: Position): ConnectionIO[Option[Player]] =
          Doo.Queries.info(p)

        override def forceTake(pos: Position): ConnectionIO[Unit] =
          Doo.Queries.take(pos)
        
        override def turn(): ConnectionIO[Player] =
          Doo.Queries.turn

        override def switchPlayer(): ConnectionIO[Unit] =
          Doo.Queries.switchPlayer

      }
    
    }


    object IOSGS {

      import cats.data.StateT
      import cats.arrow.FunctionK.lift

      type IOSGS[X] = StateT[IO, GameState, X]

      /**
       * This provides an implementation of IOSGS[_] as a TicTacToe. Note that mostly,
       * through mapK, we re-use the SGS implementation. The only extension is, effectively,
       * to log when things change.
       */
      implicit case object IOSGSIsTicTacToe extends TicTacToe[IOSGS] {

        override def info(p: Position): StateT[IO, GameState, Option[Player]] =
          Instances.SGS.SGSIsTicTacToe.info(p).mapK(lift(IO.eval))

        override def forceTake(pos: Position): StateT[IO, GameState, Unit] =
          for {
            p  <- turn
            op <- Instances.SGS.SGSIsTicTacToe.forceTake(pos).mapK(lift(IO.eval))
            _  <- StateT.liftF(putStrLn(s"Player $p took $pos"))
          } yield { op }

        override def turn(): StateT[IO, GameState, Player] =
          Instances.SGS.SGSIsTicTacToe.turn.mapK(lift(IO.eval))

        override def switchPlayer(): StateT[IO, GameState, Unit] =
          for {
            _ <- Instances.SGS.SGSIsTicTacToe.switchPlayer.mapK(lift(IO.eval))
            p <- turn
            _ <- StateT.liftF(putStrLn(s"Switching player to $p"))
          } yield { () }

      }

    }
    

    /**
     * The Book suggests an implementation like this, but I can't figure out how you'd
     * change the player between turns, so I've commented this out for now.
     */
    /**
    object RTPSGS {

      import cats.data.ReaderT

      type RTPSGS[X] = ReaderT[State[Board, ?], Player, X]

      implicit case object RTPSGSIsTicTacToe extends TicTacToe[RTPSGS] {
        override def info(p: Position): RTPSGS[Option[Player]] =
          ReaderT[State[Board, ?], Player, Option[Player]]({
            (_: Player) => // we don't need to know whose turn it is to inspect the board
              for {
                board <- State.get[Board]
              } yield {
                board.get(p)
              }
          })

        override def forceTake(pos: Position): RTPSGS[Unit] =
          ReaderT[State[Board, ?], Player, Unit]({
            (player: Player) =>
              for {
                board <- State.get[Board]
                nb = board + (pos -> player)
                _ <- State.set(nb)
              } yield { () } // this doesn't change the player in any way, does this work?
          })

        override def turn(): RTPSGS[Player] =
          ReaderT[State[Board, ?], Player, Player]({
            (player: Player) =>
              State.pure(player)
          })

        override def switchPlayer(): RTPSGS[Unit] =
          ReaderT[State[Board, ?], Player, Unit]({
            (player: Player) =>
              State.pure(())
          })

      }
    }
    **/

  }


  /**
   * This is the 'main' of the 'App', just a quick demo. It's separated in blocks
   * to limit the imports.
   */
  {
    {
      /**
       * In this first version, we just demonstrate the use of the basic SGS implementation.
       */
      import Instances.SGS._
      println("State[GameState, _] based version ready for runRandom")
      println(runRandom[SGS]().run(StartingGame).value)
      println("Leaving SGS version")
    }

    {
      /**
       * Basically the same as the last, but using the IOSGS implementation to show
       * moves being made.
       */
      import Instances.IOSGS._
      println("IOSGS based version ready for runRandom")
      println(runRandom[IOSGS]().run(StartingGame).unsafeRunSync)
      println("Leaving IOSGS version")
    }

    {
      /**
       * In this version, we use the Doobie-backed database, relying on the default
       * h2transactor provided in the s.b.t.Doobie module.
       *
       * I've sprinkled date-logging in here because it seems like this IO-based
       * version is notably slower than the ConnectionIO-based one below, and for a
       * noob like me, a little surprising.
       */
      import Instances.Doobie.DoobieIOTicTacToe
      import sumidiot.bom.ttt.{Doobie => Doo}
      import java.util.Date
      println(s"${new Date()} -- Beginning Doobie-based IO implementation")
      Doo.initializeDB(Doo.h2transactor)
      println(s"${new Date()} -- DB initialized, current Board:")
      board.map(b => println(Board.show(b))).unsafeRunSync
      println(runRandom[IO]().unsafeRunSync)
      println(s"${new Date()} -- Board after running")
      board.map(b => println(Board.show(b))).unsafeRunSync
      println(s"${new Date()} -- Leaving Doobie version")
    }
    
    {
      /**
       * Here's another version, this time this is the block which knows the transactor,
       * vs in the example above the defaults rely on the provided h2transactor.
       *
       * Some of the implicits are kinda ugly in here. Presumably there's a nice way to
       * clean those up.
       */
      import Instances.Doobie.DoobieConnectionIOTicTacToe
      import doobie._
      import sumidiot.bom.ttt.{Doobie => Doo}
      import java.util.Date
      println(s"${new Date()} -- Beginning Doobie-based ConnectionIO implementation")
      Doo.initializeDB(Doo.h2transactor)
      println(s"${new Date()} -- DB initialized, current Board:")
      Doo.run(Doo.h2transactor)(board(DoobieConnectionIOTicTacToe, implicits.AsyncConnectionIO).map(b => println(Board.show(b)))).unsafeRunSync
      println(Doo.run(Doo.h2transactor)(runRandom[ConnectionIO]()(DoobieConnectionIOTicTacToe, implicits.AsyncConnectionIO)).unsafeRunSync)
      println(s"${new Date()} -- Board after running")
      Doo.run(Doo.h2transactor)(board(DoobieConnectionIOTicTacToe, implicits.AsyncConnectionIO).map(b => println(Board.show(b)))).unsafeRunSync
      println(s"${new Date()} -- Leaving Doobie version")
    }

    {
      /**
       * This shows that the RTPSGS implementation isn't correct yet, as anticipated,
       * because the player never "switches" to O.
       */

      /**
      import Instances.RTPSGS._
      println({
        // This is a fun line, with the both the reader and the state being run
        val (b, op) = runRandom[TicTacToe.RTPSGS]().run(Player.X).run(Map.empty).value
        for {
          p <- op
        } yield {
          GameState(p, b)
        }
      })
      */
    }

    {
      /**
       * This version is very similar to SGS, but the IOSGS version does some logging
       * as methods are called, and here we actually use the runIO variant, which
       * differs from runRandom by asking the user for moves.
       *
       * This block is commented out because it's annoying :)
       */
      import Instances.IOSGS._
      println("Starting IOSGS version")
      println(runIO[IOSGS].run(StartingGame).unsafeRunSync)
      println("Leaving IOSGS version")
    }

  }

}
