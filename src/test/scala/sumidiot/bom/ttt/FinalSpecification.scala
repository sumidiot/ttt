package sumidiot.bom.ttt

import org.specs2._
import org.scalacheck._
import Prop.forAll

import org.typelevel.discipline.Laws
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
 */
trait FinalSpecification[F[_]] {

  def ttt: TicTacToe[F]
  implicit def M: Monad[F]
  
  def infoIsIdempotent(pos: Position) =
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

class FinalSpecs extends Specification with org.typelevel.discipline.specs2.Discipline {
  checkAll("final monad", FinalTests(TicTacToe.SGSIsTicTacToe).algebra({
    new Eq[SGS[Boolean]] {
      def eqv(a: SGS[Boolean], b: SGS[Boolean]): Boolean =
        a === b
    }}))
}
