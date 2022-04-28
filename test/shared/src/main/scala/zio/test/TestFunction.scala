package zio.test

import zio.stacktracer.TracingImplicits.disableAutoTrace

sealed trait TestFunction[-In, +Out] {
  def unapply(in: In): Option[Out] =
    runTrace(in).result.toOption

  def ??(message: String): TestFunction[In, Out]

  def &&[A1 <: In](that: TestFunction[A1, Boolean])(implicit ev: Out <:< Boolean): TestFunction[A1, Boolean]

  def ||[A1 <: In](that: TestFunction[A1, Boolean])(implicit ev: Out <:< Boolean): TestFunction[A1, Boolean]

  def unary_![A1 <: In](implicit ev: Out <:< Boolean): TestFunction[A1, Boolean]

  private[test] def runTrace(in: In): TestTrace[Out]
  private[test] def testArrow: TestArrow[In, Out]
}

object TestFunction {
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
  }
}
