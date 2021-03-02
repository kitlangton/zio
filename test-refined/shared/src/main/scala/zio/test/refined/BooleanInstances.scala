package zio.test.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.Or
import zio.Has
import zio.random.Random
import zio.test.magnolia.DeriveGen
import zio.test.{Gen, Sized}

object boolean extends BooleanInstances

trait BooleanInstances {
  implicit def orDeriveGen[T, A, B](implicit
    raGen: DeriveGen[Refined[T, A]],
    rbGen: DeriveGen[Refined[T, B]]
  ): DeriveGen[Refined[T, A Or B]] = {
    val genA: Gen[Has[Random] with Sized, T] = raGen.derive.map(_.value)
    val genB: Gen[Has[Random] with Sized, T] = rbGen.derive.map(_.value)
    DeriveGen.instance(
      Gen.oneOf[Has[Random] with Sized, T](genA, genB).map(Refined.unsafeApply)
    )
  }

  def orGen[R <: Has[Random], T, A, B](implicit
    genA: Gen[R, T],
    genB: Gen[R, T]
  ): Gen[R, Refined[T, A Or B]] = Gen.oneOf(genA, genB).map(Refined.unsafeApply)
}
