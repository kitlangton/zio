package zio.test

import zio.stacktracer.TracingImplicits.disableAutoTrace

sealed trait TestFunction[-In, +Out] {
  def unapply(in: In): Option[Out] =
    runTrace(in).result.toOption

  def ??(message: String): TestFunction[In, Out]

  def &&[A1 <: In](that: TestFunction[A1, Boolean])(implicit ev: Out <:< Boolean): TestFunction[A1, Boolean]

  def ||[A1 <: In](that: TestFunction[A1, Boolean])(implicit ev: Out <:< Boolean): TestFunction[A1, Boolean]

  def >>>[B](that: TestFunction[Out, B]): TestFunction[In, B]

  def unary_![A1 <: In](implicit ev: Out <:< Boolean): TestFunction[A1, Boolean]

  private[test] def runTrace(in: In): TestTrace[Out]
  private[test] def testArrow: TestArrow[In, Out]
}

object TestFunction {
  def make[A]: TestFunctionMakePartiallyApplied[A] =
    TestFunctionMakePartiallyApplied[A]()

  final case class TestFunctionMakePartiallyApplied[In]() {
    def apply[Out](f: In => Out): TestFunction[In, Out] =
      macro SmartAssertMacros.makeArrow_impl[In, Out]
  }

  def makeEither[A]: TestFunctionMakeEitherPartiallyApplied[A] =
    TestFunctionMakeEitherPartiallyApplied[A]

  final case class TestFunctionMakeEitherPartiallyApplied[In]() {
    def apply[Out](pf: In => Either[String, Out]): TestFunction[In, Out] =
      TestFunctionArrow(TestArrow.make[In, Out] { a =>
        pf(a) match {
          case Left(error)  => TestTrace.fail(error)
          case Right(value) => TestTrace.succeed(value)
        }
      })
  }

  final case class TestFunctionArrow[-In, +Out](testArrow: TestArrow[In, Out]) extends TestFunction[In, Out] {
    override def ??(message: String): TestFunction[In, Out] =
      TestFunctionArrow(testArrow ?? message)

    override def &&[A1 <: In](that: TestFunction[A1, Boolean])(implicit
      ev: Out <:< Boolean
    ): TestFunction[A1, Boolean] =
      TestFunctionArrow(testArrow && that.testArrow)

    override def ||[A1 <: In](that: TestFunction[A1, Boolean])(implicit
      ev: Out <:< Boolean
    ): TestFunction[A1, Boolean] =
      TestFunctionArrow(testArrow || that.testArrow)

    override def unary_![A1 <: In](implicit ev: Out <:< Boolean): TestFunction[A1, Boolean] =
      TestFunctionArrow(testArrow.unary_!)

    override private[test] def runTrace(in: In): TestTrace[Out] =
      TestArrow.run(testArrow, Right(in))

    override def >>>[B](that: TestFunction[Out, B]): TestFunction[In, B] =
      TestFunctionArrow(testArrow >>> that.testArrow)
  }
}
