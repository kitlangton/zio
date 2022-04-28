package zio.test

import zio.stacktracer.TracingImplicits.disableAutoTrace

case class TestResult(arrow: TestArrow[Any, Boolean]) { self =>

  lazy val result: TestTrace[Boolean] = TestArrow.run(arrow, Right(()))

  lazy val failures: Option[TestTrace[Boolean]] = TestTrace.prune(result, false)

  def isFailure: Boolean = failures.isDefined

  def isSuccess: Boolean = failures.isEmpty

  def &&(that: TestResult): TestResult = TestResult(arrow && that.arrow)

  def ||(that: TestResult): TestResult = TestResult(arrow || that.arrow)

  def unary_! : TestResult = TestResult(!arrow)

  def implies(that: TestResult): TestResult = !self || that

  def ==>(that: TestResult): TestResult = self.implies(that)

  def iff(that: TestResult): TestResult =
    (self ==> that) && (that ==> self)

  def <==>(that: TestResult): TestResult =
    self.iff(that)

  def ??(message: String): TestResult = self.label(message)

  def label(message: String): TestResult = TestResult(arrow.label(message))

  def setGenFailureDetails(details: GenFailureDetails): TestResult =
    TestResult(arrow.setGenFailureDetails(details))
}

object TestResult {
  def all(asserts: TestResult*): TestResult = asserts.reduce(_ && _)

  def any(asserts: TestResult*): TestResult = asserts.reduce(_ || _)
}
