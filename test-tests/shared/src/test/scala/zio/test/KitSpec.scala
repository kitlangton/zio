package zio.test

object KitSpec extends ZIOBaseSpec {

  val myArrow: TestFunction[Option[String], Int] =
    TestFunction.makeEither {
      case Some(string) => Right(string.length)
      case None         => Left("WHY HAVE YOU DONE THIS")
    }

  def spec = suite("SmartAssertionSpec")(
    test("assertion") {
      val optionString = Some(None)
      assertTrue(optionString.get.is(myArrow) > 5)
    }
  )

}
