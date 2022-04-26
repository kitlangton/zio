package zio.test

import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.test.TestArrow.Span

private[test] sealed trait TestTrace[+A] { self =>

  def values: List[Any] =
    self.asInstanceOf[TestTrace[Any]] match {
      case TestTrace.Node(Result.Succeed(value), _, _, _, _, _, _, _, _, _) =>
        List(value)
      case TestTrace.Node(_, _, _, _, _, _, _, _, _, _) =>
        List()
      case TestTrace.AndThen(left, right) =>
        left.values ++ right.values
      case TestTrace.And(left, right) =>
        left.values ++ right.values
      case TestTrace.Or(left, right) =>
        left.values ++ right.values
      case TestTrace.Not(trace) =>
        trace.values
    }

  def isFailure(implicit ev: A <:< Boolean): Boolean = !isSuccess

  def isSuccess(implicit ev: A <:< Boolean): Boolean =
    TestTrace.prune(self.asInstanceOf[TestTrace[Boolean]], false).isEmpty

  def isDie: Boolean =
    self.asInstanceOf[TestTrace[_]] match {
      case node: TestTrace.Node[_]        => node.result.isDie
      case TestTrace.AndThen(left, right) => left.isDie || right.isDie
      case TestTrace.And(left, right)     => left.isDie || right.isDie
      case TestTrace.Or(left, right)      => left.isDie || right.isDie
      case TestTrace.Not(trace)           => trace.isDie
    }

  /**
   * Apply the metadata to the rightmost node in the trace.
   */
  final def withSpan(span: Option[Span] = None): TestTrace[A] = if (span.isDefined) {
    self match {
      case node: TestTrace.Node[_]        => node.copy(span = span)
      case TestTrace.AndThen(left, right) => TestTrace.AndThen(left, right.withSpan(span))
      case zip                            => zip
    }
  } else {
    self
  }

  private def modifyNode[_](f: TestTrace.Node[_] => TestTrace.Node[_]): TestTrace[A] =
    self match {
      case node: TestTrace.Node[_] =>
        f(node).asInstanceOf[TestTrace[A]]
      case TestTrace.AndThen(left, right) =>
        TestTrace.AndThen(left.modifyNode(f), right.modifyNode(f))
      case and: TestTrace.And =>
        TestTrace.And(and.left.modifyNode(f), and.right.modifyNode(f)).asInstanceOf[TestTrace[A]]
      case or: TestTrace.Or =>
        TestTrace.Or(or.left.modifyNode(f), or.right.modifyNode(f)).asInstanceOf[TestTrace[A]]
      case not: TestTrace.Not =>
        TestTrace.Not(not.trace.modifyNode(f)).asInstanceOf[TestTrace[A]]
    }

  /**
   * Apply the parent span to every node in the tree.
   */
  def withParentSpan(span: Option[Span]): TestTrace[A] =
    span.fold(self) { parentSpan =>
      modifyNode(node => node.copy(span = node.parentSpan.orElse(Some(parentSpan))))
    }

  /**
   * Apply the location to every node in the tree.
   */
  def withLocation(location: Option[String]): TestTrace[A] =
    location.fold(self) { location =>
      modifyNode(node => node.copy(location = node.location.orElse(Some(location))))
    }

  def withCustomLabel(customLabel: Option[String]): TestTrace[A] =
    customLabel.fold(self) { customLabel =>
      modifyNode(node => node.copy(customLabel = node.customLabel.orElse(Some(customLabel))))
    }

  /**
   * Apply the code to every node in the tree.
   */
  final def withCode(fullCode: Option[String]): TestTrace[A] =
    fullCode.fold(self) { fullCode =>
      modifyNode(_.copy(fullCode = Some(fullCode)))
    }

  /**
   * Apply the code to every node in the tree.
   */
  final def withCompleteCode(completeCode: Option[String]): TestTrace[A] =
    completeCode.fold(self) { completeCode =>
      modifyNode(_.copy(completeCode = Some(completeCode)))
    }

  final def withGenFailureDetails(genFailureDetails: Option[GenFailureDetails]): TestTrace[A] =
    genFailureDetails.fold(self) { genFailureDetails =>
      modifyNode(node => node.copy(genFailureDetails = node.genFailureDetails.orElse(Some(genFailureDetails))))
    }

  def genFailureDetails: Option[GenFailureDetails] =
    self match {
      case node: TestTrace.Node[_]          => node.genFailureDetails
      case andThen: TestTrace.AndThen[_, _] => andThen.left.genFailureDetails.orElse(andThen.right.genFailureDetails)
      case and: TestTrace.And               => and.left.genFailureDetails.orElse(and.right.genFailureDetails)
      case or: TestTrace.Or                 => or.left.genFailureDetails.orElse(or.right.genFailureDetails)
      case not: TestTrace.Not               => not.trace.genFailureDetails
    }

  final def implies(that: TestTrace[Boolean])(implicit ev: A <:< Boolean): TestTrace[Boolean] =
    !self || that

  final def ==>(that: TestTrace[Boolean])(implicit ev: A <:< Boolean): TestTrace[Boolean] =
    implies(that)

  final def <==>(that: TestTrace[Boolean])(implicit ev: A <:< Boolean): TestTrace[Boolean] =
    self ==> that && that ==> self.asInstanceOf[TestTrace[Boolean]]

  final def &&(that: TestTrace[Boolean])(implicit ev: A <:< Boolean): TestTrace[Boolean] =
    TestTrace.And(self.asInstanceOf[TestTrace[Boolean]], that)

  final def ||(that: TestTrace[Boolean])(implicit ev: A <:< Boolean): TestTrace[Boolean] =
    TestTrace.Or(self.asInstanceOf[TestTrace[Boolean]], that)

  final def unary_!(implicit ev: A <:< Boolean): TestTrace[Boolean] =
    TestTrace.Not(self.asInstanceOf[TestTrace[Boolean]])

  final def >>>[B](that: TestTrace[B]): TestTrace[B] =
    TestTrace.AndThen(self, that)

  def result: Result[A]
}

private[test] object TestTrace {

  /**
   * Prune all non-failures from the trace.
   */
  def prune(trace: TestTrace[Boolean], negated: Boolean): Option[TestTrace[Boolean]] =
    trace match {
      case node @ TestTrace.Node(Result.Succeed(bool), _, _, _, _, _, _, _, _, _) =>
        if (bool == negated) {
          Some(node.copy(children = node.children.flatMap(prune(_, negated))))
        } else
          None

      case TestTrace.Node(Result.Fail, _, _, _, _, _, _, _, _, _) =>
        if (negated) None else Some(trace)

      case TestTrace.Node(Result.Die(_), _, _, _, _, _, _, _, _, _) =>
        Some(trace)

      case TestTrace.AndThen(left, right) =>
        prune(right, negated).map { next =>
          TestTrace.AndThen(left, next)
        }

      case and: TestTrace.And =>
        (prune(and.left, negated), prune(and.right, negated)) match {
          case (None, Some(right)) if !negated => Some(right)
          case (Some(left), None) if !negated  => Some(left)
          case (Some(left), Some(right))       => Some(TestTrace.And(left, right))
          case _                               => None
        }

      case or: TestTrace.Or =>
        (prune(or.left, negated), prune(or.right, negated)) match {
          case (Some(left), Some(right))                  => Some(TestTrace.Or(left, right))
          case (Some(left), _) if negated || left.isDie   => Some(left)
          case (_, Some(right)) if negated || right.isDie => Some(right)
          case (_, _)                                     => None
        }

      case not: TestTrace.Not =>
        prune(not.trace, !negated)
    }

  private[test] case class Node[+A](
    result: Result[A],
    message: ErrorMessage = ErrorMessage.choice("Result was true", "Result was false"),
    children: Option[TestTrace[Boolean]] = None,
    span: Option[Span] = None,
    parentSpan: Option[Span] = None,
    fullCode: Option[String] = None,
    location: Option[String] = None,
    completeCode: Option[String] = None,
    customLabel: Option[String] = None,
    override val genFailureDetails: Option[GenFailureDetails] = None
  ) extends TestTrace[A] {

    def renderResult: Any =
      result match {
        case Result.Fail           => "<FAIL>"
        case Result.Die(err)       => err
        case Result.Succeed(value) => value
      }

    def code: String =
      span match {
        case Some(span) => span.substring(fullCode.getOrElse(""))
        case None       => fullCode.getOrElse("")
      }
  }

  private[test] case class AndThen[A, +B](left: TestTrace[A], right: TestTrace[B]) extends TestTrace[B] {
    override def result: Result[B] = right.result
  }

  private[test] case class And(left: TestTrace[Boolean], right: TestTrace[Boolean]) extends TestTrace[Boolean] {
    override def result: Result[Boolean] = left.result.zipWith(right.result)(_ && _)
  }

  private[test] case class Or(left: TestTrace[Boolean], right: TestTrace[Boolean]) extends TestTrace[Boolean] {
    override def result: Result[Boolean] = left.result.zipWith(right.result)(_ || _)
  }

  private[test] case class Not(trace: TestTrace[Boolean]) extends TestTrace[Boolean] {
    override def result: Result[Boolean] = trace.result match {
      case Result.Succeed(value) => Result.Succeed(!value)
      case other                 => other
    }
  }

  def fail: TestTrace[Nothing]                        = Node(Result.Fail)
  def fail(message: ErrorMessage): TestTrace[Nothing] = Node(Result.Fail, message = message)
  def succeed[A](value: A): TestTrace[A]              = Node(Result.succeed(value))
  def option[A](value: Option[A])(message: ErrorMessage): TestTrace[A] = {
    val result = value.fold[Result[A]](Result.Fail)(a => Result.succeed(a))
    Node[A](result, message = message)
  }

  def boolean(value: Boolean)(message: ErrorMessage): TestTrace[Boolean] =
    Node(Result.succeed(value), message = message)

  def die(throwable: Throwable): TestTrace[Nothing] =
    Node(Result.die(throwable), message = ErrorMessage.throwable(throwable))

}
