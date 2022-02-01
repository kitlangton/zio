package zio

import zio.test.TestAspect.exceptScala3
import zio.test._

object ZLoggerSpec extends ZIOBaseSpec {
  trait Animal
  trait Dog     extends Animal
  object Scotty extends Dog

  def spec =
    suite("ZLoggerSpec") {
      suite("Set") {
        test("simple lookup") {
          val logger = ZLogger.simple[String, Unit](_ => ())

          val set = ZLogger.Set(logger)

          val loggers = set.getAll[String]

          assertTrue(loggers.exists(_ eq logger))
        } +
          test("supertype lookup 1") {
            val logger = ZLogger.simple[Animal, Unit](_ => ())

            val set = ZLogger.Set(logger)

            val loggers = set.getAll[Scotty.type]

            assertTrue(loggers.exists(_ eq logger))
          } +
          test("supertype lookup 2") {
            val logger = ZLogger.simple[Any, Unit](_ => ())

            val set = ZLogger.Set(logger)

            val loggers = set.getAll[Int]

            assertTrue(loggers.exists(_ eq logger))
          } @@ exceptScala3 +
          test("supertype lookup 3") {
            val logger = ZLogger.simple[Cause[Any], Unit](_ => ())

            val set = ZLogger.Set(logger)

            val loggers = set.getAll[Cause[Dog]]

            assertTrue(loggers.exists(_ eq logger))
          }
      }
    }
}
