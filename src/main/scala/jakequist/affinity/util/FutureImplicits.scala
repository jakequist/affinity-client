package jakequist.affinity.util

import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingDeque}
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

trait FutureImplicits {


  implicit class FutureImp[T](base: Future[T]) {

    def ar(implicit dur: Duration = Duration.Inf): T = awaitResult(dur)

    def get(implicit dur: Duration = Duration.Inf): T = awaitResult(dur)

    def arOpt(implicit dur: Duration = Duration.Inf): Option[T] = Try(ar(dur)).toOption

    def awaitResult(implicit dur: Duration = Duration.Inf): T = {
      Await.result(base, dur)
    }

    def awaitReady(implicit dur: Duration = Duration.Inf): Future[T] = {
      Await.ready(base, dur)
      base
    }

    def barrier(implicit dur: Duration = Duration.Inf): Unit = base.ar(dur)


    def mapTry[A](fn: PartialFunction[Try[T], A])(implicit exc: ExecutionContext): Future[A] = {
      val p = Promise[A]()
      base.onComplete {
        case v: Success[T] if fn.isDefinedAt(v) =>
          p.success(fn(v))
        case v: Failure[T] if fn.isDefinedAt(v) =>
          p.success(fn(v))
        case v: Failure[T] =>
          p.failure(v.exception)
        case _ =>
          p.failure(new MatchError("mapTry handler not defined"))
      }
      p.future
    }

    def shiftMap[A](fn: T => Future[A])(implicit exc: ExecutionContext): Future[(T, A)] = {
      val p = Promise[(T, A)]()
      base.onComplete {
        case Success(value) =>

          fn(value).onComplete {
            case Success(v2) =>
              p.success((value, v2))
            case Failure(e2) =>
              p.failure(e2)
          }

        case Failure(exception) =>
          p.failure(exception)
      }
      p.future
    }



  }


  implicit class IterableImp[T](base: Iterable[T])(implicit exc: ExecutionContext) {



    def mapSeq[A](fn: T => Future[A]): Future[Iterable[A]] = {

      if (base.isEmpty) {
        Future.successful(Nil)
      } else {

        import scala.collection.JavaConverters._
        val queue = new ConcurrentLinkedQueue[T]()
        val ret = new ConcurrentLinkedQueue[A]()
        val promise = Promise[Iterable[A]]()
        queue.addAll(base.asJavaCollection)

        def tick(v: Try[A]): Unit = {
          v match {
            case Success(v) =>
              ret.add(v)
              if (queue.isEmpty) {
                promise.success(ret.asScala.toSeq)
              } else {
                fn(queue.remove()).onComplete(tick)
              }
            case Failure(e) =>
              promise.failure(e)
          }
        }

        fn(queue.remove()).onComplete(tick)
        promise.future
      }
    }
  }


  implicit class IterableFutureImp[T](base: Iterable[Future[T]]) {

    def awaitAllResults: Iterable[T] = {
      base
        .seq
        .map(f => {
          f.awaitResult
        })
    }

    def awaitAllResults(dur: Duration): Iterable[T] = {
      base
        .seq
        .map(f => {
          f.awaitResult(dur)
        })
    }

    def awaitAllReady(dur: Duration): Iterable[Future[T]] = {
      base
        .seq
        .map(f => {
          f.awaitReady(dur)
          f
        })
    }

    def awaitAllReady: Iterable[Future[T]] = {
      awaitAllReady(Duration.Inf)
    }

    def onComplete(fn: Iterable[Try[T]] => Unit)(implicit ex: ExecutionContext): Unit = {
      val list = mutable.MutableList[Try[T]]()
      base.foreach(f => {
        f.onComplete(t => {
          synchronized {
            list += t
            if (list.size == base.size) {
              fn(list.toSeq)
            }
          }
        })
      })
    }

    def onSuccess(fn: Iterable[T] => Unit)(implicit ex: ExecutionContext): Unit = {
      onComplete(s => {
        fn(s.map {
          case Success(v) =>
            v
          case Failure(e) =>
            throw new Exception(e)
        })
      })
    }


    def mapFut[B](fn: T => B)(implicit ex: ExecutionContext): Iterable[Future[B]] = {
      base.map(f => {
        f.map(fn)
      })
    }

    def invertFutures(implicit ex: ExecutionContext): Future[Iterable[T]] = {

      val list = mutable.MutableList[T]()
      val promise = Promise[Iterable[T]]()
      val latch = new AtomicInteger(base.size)
      var exception: Option[Throwable] = None

      if (base.isEmpty) {

        Future.successful(Nil)

      } else {

        base.foreach(f => {
          f.onComplete(t => {
            list.synchronized {
              t match {
                case Success(v) =>
                  list += v
                case Failure(e) =>
                  exception = Some(e)
              }
            }
            if (latch.decrementAndGet() == 0) {
              if (exception.isDefined) {
                promise.complete(Failure(exception.get))
              } else {
                promise.complete(Success(list.toSeq))
              }
            }
          })
        })

        promise.future
      }
    }

  }


  implicit class AnonIterableFutureImp(base: Iterable[Future[_]]) {
    def barrier(dur: Duration): Unit = {
      base
        .seq
        .foreach(f => {
          f.awaitReady(dur)
        })
    }

    def barrier: Unit = {
      barrier(Duration.Inf)
    }
  }

}

object FutureImplicits extends FutureImplicits
