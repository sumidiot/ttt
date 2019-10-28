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
import sumidiot.bom.ttt.Generators._
import sumidiot.bom.ttt.Final.TicTacToe

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
  
  def infoIsIdempotent(pos: Position): IsEq[F[Boolean]] =
    M.flatMap(ttt.info(pos))(op1 => M.map(ttt.info(pos))(op2 => op1 == op2)) <-> M.pure(true)

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
      "info is idempotent" -> forAll (laws.infoIsIdempotent _)
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
      List.fill(200)(genGameState.sample).flatten.forall(s => a.runA(s).value == b.runA(s).value)
  }
  checkAll("SGS is lawful TicTacToe", FinalTests(TicTacToe.SGSIsTicTacToe).algebra)
}
