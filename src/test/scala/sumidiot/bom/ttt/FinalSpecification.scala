package sumidiot.bom.ttt

import org.scalacheck._
import Prop.forAll

import org.scalatest.funsuite.AnyFunSuite

import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline
import cats.kernel.laws._
import cats.kernel.laws.discipline._
import cats.{Eq, Monad}
import org.scalacheck.Prop._


import sumidiot.bom.ttt.Common._
import sumidiot.bom.ttt.Common.{Result => TTTResult}
import sumidiot.bom.ttt.Generators._
import sumidiot.bom.ttt.Final.TicTacToe
import sumidiot.bom.ttt.Final.{genTake,gameEnded}

import cats._

/**
 * Following https://www.iteratorshq.com/blog/tagless-with-discipline-testing-scala-code-the-right-way/
 * Also note:
 *   https://www.freecodecamp.org/news/beyond-unit-tests-an-intro-to-property-and-law-testing-in-scala-dd6a15898a19/
 *   https://www.freecodecamp.org/news/an-introduction-to-law-testing-in-scala-4243d72272f9/
 *   http://chrisphelps.github.io/scala/2016/11/30/Cats-Law-Checking-With-Discipline/
 *   http://eed3si9n.com/herding-cats/checking-laws-with-discipline.html
 *   https://typelevel.org/cats/typeclasses/lawtesting.html
 *   https://typelevel.org/blog/2013/11/17/discipline.html
 */
trait FinalSpecification[F[_]] {

  def ttt: TicTacToe[F]
  implicit def M: Monad[F]
  
  def infoIsIdempotent(pos: Position): IsEq[F[Boolean]] = {
    M.flatMap(ttt.info(pos))(op1 => {
      M.map(ttt.info(pos))(op2 => op1 == op2)
    }) <-> M.pure(true)
  }

  def infoThenGenTakeIsBehaved(pos: Position): IsEq[F[Boolean]] = {
    implicit val ittt = ttt
    M.flatMap(gameEnded)(maybeEnded => {
      maybeEnded match {
        case Some(_) => M.pure(true) // game's over, ignore everything (or should we test you can't take it?)
        case None    =>
          M.flatMap(ttt.info(pos))(oposPlayer => {
            M.flatMap(ttt.turn)(curPlayer => {
              oposPlayer match {
                case Some(posPlayer) => {
                  // if we call genTake then AlreadyTaken, and info(pos) is oposPlayer and turn is curPlayer
                  M.flatMap(genTake(pos))(takeRes => {
                    M.flatMap(ttt.info(pos))(oposPlayer2 => {
                      M.map(ttt.turn)(curPlayer2 => {
                        (curPlayer == curPlayer2) && (oposPlayer == oposPlayer2) && takeRes == TTTResult.AlreadyTaken(posPlayer)
                      })
                    })
                  })
                }
                case None => {
                  // if we call genTake then either NextTurn or GameEnded
                  // if NextTurn then turn == Player.other(curPlayer) and info(pos) == Some(curPlayer)
                  // if GameEnded then winner is not Player.other(curPlayer) (could be draw or win)
                  //    EXCEPT the generated game could have already been won
                  M.flatMap(genTake(pos))(takeRes => {
                    takeRes match {
                      case TTTResult.NextTurn => {
                        M.flatMap(ttt.info(pos))(oposPlayer2 => {
                          M.map(ttt.turn)(curPlayer2 => {
                            /**
                             * I added these println-s while debugging a failure.
                             * Which somewhat makes me wonder how else I should set up these tests.
                             *
                             *  println("here")
                             *  println(curPlayer)
                             *  println(curPlayer2)
                             *  println(oposPlayer2)
                             */
                            (curPlayer != curPlayer2) && oposPlayer2.map(_ == curPlayer).getOrElse(false)
                          })
                        })
                      }
                      case TTTResult.GameEnded(None) => {
                        M.pure(true) // everything is awesome
                      }
                      case TTTResult.GameEnded(Some(winner)) => {
                        M.pure(winner == curPlayer)
                      }
                      case _ => {
                        // this shouldn't happen, so we want the test to fail
                        M.pure(false)
                      }
                    }
                  })
                }
              }
            })
          })
      }
    }) <-> M.pure(true)
  }
}

object FinalSpecification {
  def apply[F[_]](instance: TicTacToe[F])(implicit ev: Monad[F]) =
    new FinalSpecification[F] {
      override val ttt = instance
      override implicit val M: Monad[F] = ev
    }
}

trait FinalTests[F[_]] extends Laws {

  def laws: FinalSpecification[F]

  def algebra(implicit eqFBool: Eq[F[Boolean]]) =
    new SimpleRuleSet(
      name = "TicTacToe",
      "info is idempotent" -> forAll (laws.infoIsIdempotent _),
      "genTake is lawful"  -> forAll (laws.infoThenGenTakeIsBehaved _)
    )

}

object FinalTests {
  def apply[F[_]](instance: TicTacToe[F])(implicit ev: Monad[F]) =
    new FinalTests[F] {
      override def laws = FinalSpecification(instance)
    }
}

class FinalSpecs extends AnyFunSuite with Discipline {
  /**
   * I think you'd generally compare two State[A, B] as the same if for all a in A
   * when you .run a through the two State-s, the two .values are the same. Maybe I'm wrong
   * about that interpretation, because I'm still getting used to State. Or, if I'm right,
   * it seems like I've probably got my tests set up incorrectly - I'd imagine the property
   * being based on generating an Arbitrary GameState, and then checking the property given
   * the corresponding State. But the generic Law can't be written to assume State...
   */
  implicit val eqSQS: Eq[SGS[Boolean]] = new Eq[SGS[Boolean]] {
    def eqv(a: SGS[Boolean], b: SGS[Boolean]): Boolean =
      List.fill(200)(genGameState.sample).flatten.forall(s => {
        val ans = a.runA(s).value == b.runA(s).value
        if (!ans) {
          println(s"Failed on:\n$s")
        }
        ans
      })
  }
  checkAll("SGS is lawful TicTacToe", FinalTests(TicTacToe.SGSIsTicTacToe).algebra)
}
