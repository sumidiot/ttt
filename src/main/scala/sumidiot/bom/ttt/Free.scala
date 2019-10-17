package sumidiot.bom.ttt

object Free {

  sealed abstract class TicTacToe[A]
  case class Info(p: Position) extends TicTacToe[Option[Player]]
  case class Take(p: Position) extends TicTacToe[Result]
  case class Done[A](x: A) extends TicTacToe[A]
  case class FlatMap[A, B](x: TicTacToe[A], f: A => TicTacToe[B]) extends TicTacToe[B]

  def info(p: Position): TicTacToe[Option[Player]] =
    Info(p)

  def take(p: Position): TicTacToe[Result] =
    Take(p)


  object TicTacToe {

    implicit def ticTacToeFunctor: Functor[TicTacToe] =
      new Functor[TicTacToe] {
        def map[A, B](fa: TicTacToe[A])(f: A => B): TicTacToe[B] = {
          FlatMap(fa, a => Done(f(a)))
        }
      }

    implicit def ticTacToeApplicative(implicit fttt: Functor[TicTacToe]): Applicative[TicTacToe] =
      new Applicative[TicTacToe] {
        def ap[A, B](ff: TicTacToe[A => B])(fa: TicTacToe[A]): TicTacToe[B] = {
          ???
        }

        def pure[A](a: A): TicTacToe[A] = {
          Done(a)
        }
      }

  }
}
