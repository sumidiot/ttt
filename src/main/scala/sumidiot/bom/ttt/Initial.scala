package sumidiot.bom.ttt

import Common._

import cats._
import cats.data._
import cats.implicits._

import scala.annotation.tailrec


/**
 * Example usage:
 *
   import sumidiot.bom.ttt.Common._
   import sumidiot.bom.ttt.Initial._
   import cats.implicits._
   val i = info(Position(BoardIndex.F, BoardIndex.F))
   val ig = runGame(i)
   ig.run(StartingGame).value
 
   val steps = for {
     op <- info(Position(BoardIndex.F, BoardIndex.F))
     _ <- println(op).pure[TicTacToe]
     r <- take(Position(BoardIndex.F, BoardIndex.F))
     _ <- println(r).pure[TicTacToe]
     op2 <- info(Position(BoardIndex.F, BoardIndex.F))
     _ <- println(op2).pure[TicTacToe]
   } yield {
     ()
   }
   val stepsG = runGame(steps)
   stepsG.run(StartingGame).value

   runGame(runRandom()).run(StartingGame).value
 */
object Initial {

  sealed abstract class TicTacToe[A]
  /**
   * The `k` fields in these constuctors is the continuation. The input is the
   * result type of the operation that the case class represents
   */
  case class Info[A](p: Position, k: Option[Player] => TicTacToe[A]) extends TicTacToe[A]
  case class Take[A](p: Position, k: Result => TicTacToe[A]) extends TicTacToe[A]
  case class Done[A](a: A) extends TicTacToe[A]

  object TicTacToe {

    implicit def ticTacToeFunctor: Functor[TicTacToe] =
      new Functor[TicTacToe] {
        def map[A, B](fa: TicTacToe[A])(f: A => B): TicTacToe[B] = {
          fa match {
            case i@Info(p, k) => {
              Info(p, op => this.map(k(op))(f)) // recursive
            }
            case t@Take(p, k) => {
              Take(p, op => this.map(k(op))(f)) // recursive
            }
            case d@Done(a) => {
              Done(f(a))
            }
          }
        }
      }

    
    /**
     * I have no idea if (and only little confidence that) this is 'lawful'
     */
    implicit def ticTacToeApplicative(implicit fttt: Functor[TicTacToe]): Applicative[TicTacToe] =
      new Applicative[TicTacToe] {
        def ap[A, B](ff: TicTacToe[A => B])(fa: TicTacToe[A]): TicTacToe[B] = {
          fa match {
            case i@Info(p, k) => {
              ff match {
                case ii@Info(pp, kk) => {
                  // so what do I have right now
                  // fa: TicTacToe[A] is i: Info[A], p: Position, k: Option[Player] => TicTacToe[A]
                  // ff: TicTacToe[A => B] is ii: Info[A => B], pp: Position, kk: Option[Player] => TicTacToe[A => B]
                  // and I'm supposed to produce a TicTacToe[B]
                  // seems like I should produce an Info, since I'd got two hanging around
                  // The player could be p or pp, so probably i need to 'continue' one of them, and
                  //   leave the other alone. Sorta gotta splice the continuations or something
                  Info(p, op => { // op is now an Option[Player]
                    this.ap(ii)(k(op))
                  })
                }
                case tt@Take(pp, kk) => {
                  Info(p, op => {
                    // following the Done and Info examples, it seems like I'm going to return
                    // this Info(p, _), so, with that in mind
                    // what do i have right now
                    // fa: TicTacToe[A] is i: Info[A], p: Position, k: Option[Player] => TicTacToe[A]
                    // ff: TicTacToe[A => B] is tt: Take[A => B], pp: Position, kk: Result => TicTacToe[A => B]
                    // op: Option[Player]
                    // k(op): TicTacToe[A]
                    this.ap(tt)(k(op)) // ah, right, so we sort of end up dealing with the tt until we've reduced the A down to a Done, roughly?
                  })
                }
                case dd@Done(aa) => {
                  // so, what do i have right now
                  // fa: TicTacToe[A] is i: Info[A], p: Position, k: Option[Player] => TicTacToe[A]
                  // ff: TicTacToe[A => B] is dd: Done[A => B], aa: A => B
                  // I'm supposed to produce a TicTacToe[B]
                  Info(p, op => {
                    fttt.map(k(op))(aa)
                  })
                }
              }
            }
            case t@Take(p, k) => {
              ff match {
                case ii@Info(pp, kk) => {
                  Take(p, op => {
                    // supposed to return a TicTacToe[B]
                    // k: Result => TicTacToe[A]
                    // kk: Option[Player] => TicTacToe[A => B]
                    // op: Result
                    // def ap[A, B](ff: TicTacToe[A => B])(fa: TicTacToe[A]): TicTacToe[B]
                    this.ap(ii)(k(op))
                  })
                }
                case tt@Take(pp, kk) => {
                  Take(p, op => {
                    // fa: TicTacToe[A], is t: Take[A], p: Position, k: Result => TicTacToe[A]
                    // ff: TicTacToe[A => B], is tt: Take[A => B], pp: Position, kk: Result => TicTacToe[A => B]
                    // op: Result
                    this.ap(tt)(k(op))
                  })
                }
                case dd@Done(aa) => {
                  Take(p, op => {
                    fttt.map(k(op))(aa)
                  })
                }
              }
            }
            case d@Done(a) => {
              ff match {
                case ii@Info(pp, kk) => {
                  // so, what do i have right now
                  // fa: TicTacToe[A], is d: Done[A], a: A
                  // ff: TicTacToe[A => B], is ii: Info[A => B], pp: Position, kk: Option[Player] => TicTacToe[A => B]
                  // I'm supposed to produce a TicTacToe[B]
                  // somehow, I sorta know ff "ends with" a Done[A=>B] which is an A => B
                  // I guess I want to return an Info, for some reason
                  Info(pp, op => {
                    // in here, i have to return a TicTacToe[B]
                    // op is a Option[Player], kk(op) is a TicTacToe[A => B]
                    fttt.map(kk(op))(_(a))
                  })
                }
                case tt@Take(pp, kk) => {
                  Take(pp, or => {
                    fttt.map(kk(or))(_(a))
                  })
                }
                case dd@Done(aa) => {
                  Done(aa(a))
                }
              }
            }
          }
        }

        def pure[A](a: A): TicTacToe[A] = {
          Done(a)
        }
      }

      /**
       * We don't require the evidence of being Applicative, I guess
       * and the compiler errors out with unused (implicit attt: Applicative[TicTacToe])
       */
    implicit case object TicTacToeMonad extends Monad[TicTacToe] {
      override def pure[A](a: A): TicTacToe[A] =
        Done(a)

      override def flatMap[A, B](fa: TicTacToe[A])(f: A => TicTacToe[B]): TicTacToe[B] = {
        fa match {
          case i@Info(p, k) => {
            Info(p, op => {
              this.flatMap(k(op))(f)
            })
          }
          case t@Take(p, k) => {
            Take(p, r => {
              this.flatMap(k(r))(f)
            })
          }
          case d@Done(a) => {
            f(a)
          }
        }
      }

      @tailrec
      override def tailRecM[A, B](init: A)(fn: A => TicTacToe[Either[A, B]]): TicTacToe[B] = {
        fn(init) match {
          case Done(Left(a)) => tailRecM(a)(fn)
          case Done(Right(b)) => this.pure(b)
          case Info(pos, k) => { // k: Option[Player] => TicTacToe[Either[A, B]]
            ???
          }
          case Take(p, k) => {
            ???
          }
        }
      }
    }
  }


  def takeIfNotTaken1(p: Position): TicTacToe[Option[Result]] = {
    Info(p, op => {
      op match {
        case Some(_) => Done(None)
        case None    => Take(p, r => Done(Some(r)))
      }
    })
  }

  def info(p: Position): TicTacToe[Option[Player]] =
    Info(p, TicTacToe.TicTacToeMonad.pure _)

  def take(p: Position): TicTacToe[Result] =
    Take(p, TicTacToe.TicTacToeMonad.pure _)

  def takeIfNotTaken2(p: Position): TicTacToe[Option[Result]] = {
    import cats.syntax.applicative._
    info(p).flatMap { _ match {
      case Some(_) => none[Result].pure[TicTacToe] // TicTacToe.TicTacToeMonad.pure(None)
      case None    => take(p).map(_.some)
    }}
  }


  def runGame[A](ttt: TicTacToe[A]): SGS[A] = {
    ttt match {
      case i@Info(p, k) => {
        // the for comprehension below is the same as this:
        // State.get[GameState].flatMap(gs => runGame(k(gs.b.get(p))))
        for {
          gs <- State.get[GameState]
          a <- runGame(k(gs.b.get(p)))
        } yield {
          a
        }
      }
      case t@Take(pos, k) => { // k: Result => TicTacToe[A]
        State.get[GameState].flatMap { gs =>
          winner(gs.b) match {
            case Some(p) => runGame(k(Result.GameEnded(p)))
            case None =>
              gs.b.get(pos) match {
                case Some(p) => runGame(k(Result.AlreadyTaken(p)))
                case None    =>
                  val nb = gs.b + (pos -> gs.p)
                  val ng = GameState(Player.other(gs.p), nb)
                  for {
                    _ <- State.set(ng)
                    a <- runGame(k(winner(nb).fold[Result](Result.NextTurn)(p => Result.GameEnded(p))))
                  } yield {
                    a
                  }
              }
        }}
      }
      case d@Done(a) => {
        State.pure(a)
      }
    }
  }

  /**
   * This just about works, but it does fail to recognize that you can end in a draw
   */
  def runRandom(exceptions: Set[Position] = Set.empty): TicTacToe[Player] = {
    val rpos = randomPosition(exceptions)
    takeIfNotTaken2(rpos).flatMap { or =>
      or match {
        case Some(Result.GameEnded(p)) => p.pure[TicTacToe]
        case Some(Result.AlreadyTaken(p)) => runRandom(exceptions + rpos)
        case Some(Result.NextTurn) => runRandom(exceptions)
        case None    => runRandom(exceptions + rpos)
      }
    }
  }

}
