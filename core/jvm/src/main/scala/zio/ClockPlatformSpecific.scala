package zio

import zio.duration.Duration
import zio.internal.{NamedThreadFactory, Scheduler}

import java.util.concurrent.{ScheduledExecutorService, ScheduledThreadPoolExecutor, TimeUnit}

private[zio] trait ClockPlatformSpecific {
  import Scheduler.CancelToken

  private[zio] val globalScheduler = new Scheduler {

    private[this] val service = makeService()

    private[this] val ConstFalse = () => false

    override def schedule(task: Runnable, duration: Duration): CancelToken = (duration: @unchecked) match {
      case Duration.Infinity => ConstFalse
      case Duration.Finite(_) =>
        val future = service.schedule(
          new Runnable {
            def run: Unit =
              task.run()
          },
          duration.toNanos,
          TimeUnit.NANOSECONDS
        )

        () => {
          val canceled = future.cancel(true)

          canceled
        }
    }
  }

  private[this] def makeService(): ScheduledExecutorService = {
    val service = new ScheduledThreadPoolExecutor(1, new NamedThreadFactory("zio-timer", true))
    service.setRemoveOnCancelPolicy(true)
    service
  }
}
