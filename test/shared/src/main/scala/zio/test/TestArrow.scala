package zio.test

import zio.stacktracer.TracingImplicits.disableAutoTrace

import scala.language.implicitConversions
import zio.Trace

import scala.util.control.NonFatal

sealed trait TestArrow[-In, +Out] { self =>
  def ??(message: String): TestArrow[In, Out] = self.label(message)

  def label(message: String): TestArrow[In, Out] = self.meta(customLabel = Some(message))

  def setGenFailureDetails(details: GenFailureDetails): TestArrow[In, Out] =
    self.meta(genFailureDetails = Some(details))

  import TestArrow._

  def meta(
    span: Option[Span] = None,
    parentSpan: Option[Span] = None,
    code: Option[String] = None,
    location: Option[String] = None,
    completeCode: Option[String] = None,
    customLabel: Option[String] = None,
    genFailureDetails: Option[GenFailureDetails] = None
  ): TestArrow[In, Out] = self match {
    case meta: Meta[In, Out] =>
      meta.copy(
        span = meta.span.orElse(span),
        parentSpan = meta.parentSpan.orElse(parentSpan),
        code = code.orElse(meta.code),
        location = meta.location.orElse(location),
        completeCode = meta.completeCode.orElse(completeCode),
        customLabel = meta.customLabel.orElse(customLabel),
        genFailureDetails = meta.genFailureDetails.orElse(genFailureDetails)
      )
    case _ =>
      Meta(
        arrow = self,
        span = span,
        parentSpan = parentSpan,
        code = code,
        location = location,
        completeCode = completeCode,
        customLabel = customLabel,
        genFailureDetails = genFailureDetails
      )
  }

  def span(span: (Int, Int)): TestArrow[In, Out] =
    meta(span = Some(Span(span._1, span._2)))

  def withCode(code: String): TestArrow[In, Out] =
    meta(code = Some(code))

  def withCompleteCode(completeCode: String): TestArrow[In, Out] =
    meta(completeCode = Some(completeCode))

  def withLocation(implicit trace: Trace): TestArrow[In, Out] =
    trace match {
      case Trace(_, file, line) =>
        meta(location = Some(s"$file:$line"))
      case _ => self
    }

  def withParentSpan(span: (Int, Int)): TestArrow[In, Out] =
    meta(parentSpan = Some(Span(span._1, span._2)))

  def >>>[C](that: TestArrow[Out, C]): TestArrow[In, C] =
    AndThen[In, Out, C](self, that)

  def &&[A1 <: In](that: TestArrow[A1, Boolean])(implicit ev: Out <:< Boolean): TestArrow[A1, Boolean] =
    And(self.asInstanceOf[TestArrow[A1, Boolean]], that)

  def ||[A1 <: In](that: TestArrow[A1, Boolean])(implicit ev: Out <:< Boolean): TestArrow[A1, Boolean] =
    Or(self.asInstanceOf[TestArrow[A1, Boolean]], that)

  def unary_![A1 <: In](implicit ev: Out <:< Boolean): TestArrow[A1, Boolean] =
    Not(self.asInstanceOf[TestArrow[A1, Boolean]])
}

object TestArrow {

  def succeed[A](value: => A): TestArrow[Any, A] = TestArrowF(_ => TestTrace.succeed(value))

  def fromFunction[In, B](f: In => B): TestArrow[In, B] = make(f andThen TestTrace.succeed)

  def suspend[In, B](f: In => TestArrow[Any, B]): TestArrow[In, B] = TestArrow.Suspend(f)

  def make[In, B](f: In => TestTrace[B]): TestArrow[In, B] =
    makeEither(e => TestTrace.die(e).annotate(TestTrace.Annotation.Rethrow), f)

  def makeEither[In, B](onFail: Throwable => TestTrace[B], onSucceed: In => TestTrace[B]): TestArrow[In, B] =
    TestArrowF {
      case Left(error)  => onFail(error)
      case Right(value) => onSucceed(value)
    }

  def run[A, B](arrow: TestArrow[A, B], in: Either[Throwable, A]): TestTrace[B] = attempt {
    arrow match {
      case TestArrowF(f) =>
        f(in)

      case AndThen(f, g) =>
        val t1 = run(f, in)
        t1.result match {
          case Result.Fail           => t1.asInstanceOf[TestTrace[B]]
          case Result.Die(err)       => t1 >>> run(g, Left(err))
          case Result.Succeed(value) => t1 >>> run(g, Right(value))
        }

      case And(lhs, rhs) =>
        run(lhs, in) && run(rhs, in)

      case Or(lhs, rhs) =>
        run(lhs, in) || run(rhs, in)

      case Not(arrow) =>
        !run(arrow, in)

      case Suspend(f) =>
        in match {
          case Left(exception) =>
            TestTrace.die(exception)
          case Right(value) =>
            run(f(value), in)
        }

      case Meta(arrow, span, parentSpan, code, location, completeCode, customLabel, genFailureDetails) =>
        run(arrow, in)
          .withSpan(span)
          .withCode(code)
          .withParentSpan(parentSpan)
          .withLocation(location)
          .withCompleteCode(completeCode)
          .withCustomLabel(customLabel)
          .withGenFailureDetails(genFailureDetails)
    }

  }

  private def attempt[A](expr: => TestTrace[A]): TestTrace[A] =
    try {
      expr
    } catch {
      case NonFatal(exception) =>
        val trace = exception.getStackTrace
        var met   = false
        val newTrace = trace.filterNot { trace =>
          if (trace.toString.contains("zio.test.TestArrow")) {
            met = true
          }
          met
        }
        exception.setStackTrace(newTrace)
        TestTrace.die(exception)
    }

  private[test] final case class Span(start: Int, end: Int) {
    def substring(str: String): String = str.substring(start, end)
  }

  private final case class Meta[-In, +Out](
    arrow: TestArrow[In, Out],
    span: Option[Span],
    parentSpan: Option[Span],
    code: Option[String],
    location: Option[String],
    completeCode: Option[String],
    customLabel: Option[String],
    genFailureDetails: Option[GenFailureDetails]
  ) extends TestArrow[In, Out]

  private final case class TestArrowF[-In, +Out](f: Either[Throwable, In] => TestTrace[Out]) extends TestArrow[In, Out]
  private final case class AndThen[In, X, Out](f: TestArrow[In, X], g: TestArrow[X, Out])    extends TestArrow[In, Out]
  private final case class And[In](left: TestArrow[In, Boolean], right: TestArrow[In, Boolean])
      extends TestArrow[In, Boolean]
  private final case class Or[In](left: TestArrow[In, Boolean], right: TestArrow[In, Boolean])
      extends TestArrow[In, Boolean]
  private final case class Not[In](arrow: TestArrow[In, Boolean])         extends TestArrow[In, Boolean]
  private final case class Suspend[In, Out](f: In => TestArrow[Any, Out]) extends TestArrow[In, Out]
}
