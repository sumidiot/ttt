package sumidiot.bom.ttt

import cats._
import cats.implicits._

// import scala.annotation.tailrec

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
                    // note that pp is unused in here, which makes me antsy
                    this.ap(kk(op))(k(op))
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
              ???
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
    implicit def ticTacToeMonad: Monad[TicTacToe] =
      new Monad[TicTacToe] {
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

        // @tailrec
        override def tailRecM[A, B](init: A)(fn: A => TicTacToe[Either[A, B]]): TicTacToe[B] = {
          ???
        }
      }
  }



}
