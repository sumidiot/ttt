package sumidiot.bom.ttt

import cats._
import cats.data._
import cats.implicits._

import scala.annotation.tailrec

object Main extends App {

  /**
   * This top section is the same as in the final style example
   */

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

  /**
   * These are the new bits for initial style
   */

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
              Take(p, op => this.map(k(op))(f))
            }
            case d@Done(a) => {
              Done(f(a))
            }
          }
        }
      }

    
    /**
     * I have no idea if (and little confidence that) this is 'lawful'
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


  type Board = Map[Position, Player]
  case class GameState(p: Player, b: Board)
  type SGS[X] = State[GameState, X]

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
