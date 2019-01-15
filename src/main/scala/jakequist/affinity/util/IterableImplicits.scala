package jakequist.affinity.util

import java.io._
import java.util.concurrent.LinkedBlockingDeque

import scala.collection.{Seq, mutable}
import scala.collection.parallel.{ForkJoinTaskSupport, ParIterable, ParSeq}
import scala.reflect.ClassTag
import scala.util.{Failure, Random, Success, Try}

import scala.concurrent.{ExecutionContext, Future}

trait IterableImplicits {

  implicit def nilToEmptySet[T](n: Nil.type): Set[T] = Set.empty[T]

  implicit class TryIterableImps[T](base: Iterable[Try[T]]) {

    def hasFailures = failures.nonEmpty

    def failures: Iterable[Failure[T]] = {
      base.flatMap {
        case v: Failure[T] => Some(v)
        case _ => None
      }
    }

    def successes: Iterable[Success[T]] = {
      base.flatMap {
        case v: Success[T] => Some(v)
        case _ => None
      }
    }

  }

  implicit class NumericTraversableImps[+A: Numeric](base: TraversableOnce[A]) {

    def sumOpt: Option[A] = {
      if (base.isEmpty) None
      else Some(base.sum)
    }

    def minOpt: Option[A] = {
      if (base.isEmpty) {
        None
      } else {
        Some(base.min)
      }
    }

    def maxOpt: Option[A] = {
      if (base.isEmpty) {
        None
      } else {
        Some(base.max)
      }
    }

    def avg = {
      avgOpt.get
    }


    def avgOpt: Option[Double] = {
      if (base.isEmpty) None
      else {

        val num = implicitly[Numeric[A]]
        var total = 0.0
        var sum = 0.0

        base.foreach(a => {
          total += 1.0
          sum = sum + num.toDouble(a)
        })
        Some(sum / total)
      }
    }


  }


  //
  //  implicit class FractionalTraversableImps[+A: Fractional](base: TraversableOnce[A]) {
  //
  //  }


  //
  //  implicit class LongIterableImps(base: Iterable[Long]) {
  //
  //    def sumOpt: Option[Long] = {
  //      if (base.isEmpty) None
  //      else Some(base.sum)
  //    }
  //
  //    def minOpt: Option[Long] = {
  //      if (base.isEmpty) {
  //        None
  //      } else {
  //        Some(base.min)
  //      }
  //    }
  //
  //    def maxOpt: Option[Long] = {
  //      if (base.isEmpty) {
  //        None
  //      } else {
  //        Some(base.max)
  //      }
  //    }
  //  }
  //
  //
  //  implicit class IntIterableImps(base: Iterable[Int]) {
  //    def minOpt: Option[Int] = {
  //      if (base.isEmpty) {
  //        None
  //      } else {
  //        Some(base.min)
  //      }
  //    }
  //
  //    def maxOpt: Option[Int] = {
  //      if (base.isEmpty) {
  //        None
  //      } else {
  //        Some(base.max)
  //      }
  //    }
  //  }


  //  implicit class StringIteratorImp(base: Iterator[String]) {
  //    def toInputStream: InputStream = {
  //      val enumerator = base
  //        .map(s => {
  //          new ByteArrayInputStream(s.getBytes)
  //        })
  //
  //      new SequenceInputStream(enumerator.asJavaEnumeration)
  //    }
  //  }

  implicit class EitherIterableImp[A, B](base: Iterable[Either[A, B]]) {
    def left: Iterable[A] = {
      base.flatMap {
        case Left(v) => Some(v)
        case _ => None
      }
    }

    def right: Iterable[B] = {
      base.flatMap {
        case Right(v) => Some(v)
        case _ => None
      }
    }
  }

  implicit class ParIterableImp[T](base: ParIterable[T]) {
    def withParallelism(n: Int): ParIterable[T] = {
      base.tasksupport = new ForkJoinTaskSupport(
        new scala.concurrent.forkjoin.ForkJoinPool(n)
      )
      base
    }
  }


  implicit class IteratorImp[T](base: Iterator[T]) {


    def until(fn: T => Boolean): Unit = {
      while (base.hasNext) {
        if (fn(base.next()) == true) {
          return
        }
      }
    }

    //    def groupWhile[M](zero: => M)(conv: (M, T) => M)(test: M => Boolean): Iterator[Seq[T]] = {
    //      new GroupWhileIterator[T, M](zero _, conv, test, base)
    //    }
    //
    //    def poolEach[B](size: Int)(fn: T => Unit)(implicit exc: ExecutionContext): Unit = {
    //      poolMap(size)(fn).size
    //    }
    //
    //    def poolMap[B](size: Int)(fn: T => B)(implicit exc: ExecutionContext): Iterator[B] = {
    //      if (size == 1) {
    //        base.map(fn)
    //      } else {
    //        new IteratorPool[T, B](
    //          base,
    //          fn,
    //          size
    //        )
    //      }
    //    }


    def flatFirst[B](fn: T => Option[B]): Option[B] = {
      while (base.hasNext) {
        fn(base.next()) match {
          case Some(v) =>
            return Some(v)
          case _ => // pass
        }
      }
      None
    }

    def firstByOption[B](fn: T => B)(implicit ord: Ordering[B]): Option[T] = {
      base
        .reduceOption((a, b) => {
          if (ord.gt(fn(a), fn(b))) {
            b
          } else {
            a
          }
        })
    }

    def lastByOption[B](fn: T => B)(implicit ord: Ordering[B]): Option[T] = {
      base
        .reduceOption((a, b) => {
          if (ord.gt(fn(a), fn(b))) {
            a
          } else {
            b
          }
        })
    }

    def hashDistinct: Iterator[T] = {
      val set = mutable.HashSet[Int]()
      base.filter(v => {
        if (set.contains(v.hashCode())) {
          false
        } else {
          set += v.hashCode()
          true
        }
      })
    }

    def headOption: Option[T] = {
      base.hasNext match {
        case true => Option(base.next())
        case _ => None
      }
    }

    def head: T = {
      base.hasNext match {
        case true => base.next()
        case _ => throw new IllegalStateException("empty iterator")
      }
    }


  }


  implicit class TraversableTuple2Imp[A, B, R <: Traversable[(A, B)]](base: R) {

    def map1[T](fn: A => T) = base.map(t => t.copy(_1 = fn(t._1)))

    def map2[T](fn: B => T) = base.map(t => t.copy(_2 = fn(t._2)))

  }

  implicit class TraversableTuple3Imp[A, B, C, R <: Traversable[(A, B, C)]](base: R) {

    def map1[T](fn: A => T) = base.map(t => t.copy(_1 = fn(t._1)))

    def map2[T](fn: B => T) = base.map(t => t.copy(_2 = fn(t._2)))

    def map3[T](fn: C => T) = base.map(t => t.copy(_3 = fn(t._3)))

  }

  implicit class IterableTuple4Imp[A, B, C, D](base: Option[(A, B, C, D)]) {

    def map1[T](fn: A => T) = base.map(t => t.copy(_1 = fn(t._1)))

    def map2[T](fn: B => T) = base.map(t => t.copy(_2 = fn(t._2)))

    def map3[T](fn: C => T) = base.map(t => t.copy(_3 = fn(t._3)))

    def map4[T](fn: D => T) = base.map(t => t.copy(_4 = fn(t._4)))

  }

  implicit class IteratorTuple4Imp[A, B, C, D](base: Iterator[(A, B, C, D)]) {

    def map1[T](fn: A => T) = base.map(t => t.copy(_1 = fn(t._1)))

    def map2[T](fn: B => T) = base.map(t => t.copy(_2 = fn(t._2)))

    def map3[T](fn: C => T) = base.map(t => t.copy(_3 = fn(t._3)))

    def map4[T](fn: D => T) = base.map(t => t.copy(_4 = fn(t._4)))

  }


  implicit class TraversableImp[T](base: Traversable[T]) {


    def firstOption[B](fn: T => Option[B]): Option[B] = {
      val iter = base.toIterator
      while (iter.hasNext) {
        fn(iter.next()) match {
          case Some(b) =>
            return Some(b)
          case _ =>
        }
      }
      None
    }

    def firstByOption[B](fn: T => B)(implicit ord: Ordering[B]): Option[T] = {
      base
        .reduceOption((a, b) => {
          if (ord.gt(fn(a), fn(b))) {
            b
          } else {
            a
          }
        })
    }

    def lastByOption[B](fn: T => B)(implicit ord: Ordering[B]): Option[T] = {
      base
        .reduceOption((a, b) => {
          if (ord.gt(fn(a), fn(b))) {
            a
          } else {
            b
          }
        })
    }


    def hashDistinct: Traversable[T] = {
      val set = mutable.HashSet[Int]()
      base.filter(v => {
        if (set.contains(v.hashCode())) {
          false
        } else {
          set += v.hashCode()
          true
        }
      })
    }

    def isSingular: Boolean = {
      base.size == 1
    }

    def isPlural: Boolean = {
      base.size >= 2
    }


    def weightedMap[K, B](pf: PartialFunction[T, (K, Option[B])]): Traversable[(K, B)] = {
      val lifted = pf.lift
      base
        .flatMap(v => lifted(v))
        .filter(_._2.isDefined)
        .map(t => {
          (t._1, t._2.get)
        })
    }

    def weightedBest[B](pf: PartialFunction[T, (Int, Option[B])]): Option[B] = {
      val lifted = pf.lift
      base
        .flatMap(v => lifted(v))
        .filter(_._2.isDefined)
        .reduceOption((a, b) => {
          if (a._1 > b._1) {
            a
          } else {
            b
          }
        })
        .flatMap(_._2)
    }


  }


  implicit class ParSeqPairImp[K, V](base: ParIterable[(K, V)]) {

    def keys: ParIterable[K] = base.map(_._1)

    def values: ParIterable[V] = base.map(_._2)

    def cogroup[W](that: ParIterable[(K, W)]) = {

      val map = mutable.HashMap[K, (mutable.MutableList[V], mutable.MutableList[W])]()
      val baseSeq = base.seq
      val thatSeq = that.seq

      baseSeq.foreach(t => {
        val l = map.getOrElseUpdate(t._1, (mutable.MutableList[V](), mutable.MutableList[W]()))
        l._1 += t._2
      })
      thatSeq.foreach(t => {
        val l = map.getOrElseUpdate(t._1, (mutable.MutableList[V](), mutable.MutableList[W]()))
        l._2 += t._2
      })

      map
        .map(t => {
          (t._1, (t._2._1.toSeq, t._2._2.toSeq))
        })
        .toSeq
        .par
    }


    def leftOuterJoin[W](that: ParIterable[(K, W)]) = {
      cogroup(that)
        .flatMap(t => {
          t._2._1 match {
            case Seq(left) =>
              Some(t._1, (left, t._2._2.headOption))
            case _ => None
          }
        })
    }

    def mapValues[U](fn: V => U) = {
      base.map(v => {
        (v._1, fn(v._2))
      })
    }

    def swap: ParIterable[(V, K)] = {
      base.map(v => {
        (v._2, v._1)
      })
    }

    def groupByKey: ParIterable[(K, ParIterable[V])] = {
      base
        .groupBy(_._1)
        .map(v => {
          (v._1, v._2.map(_._2))
        })
    }


    def reduceByKey(fn: (V, V) => V): ParIterable[(K, V)] = {
      groupByKey
        .mapValues(v => v.reduce(fn))
    }

  }


  implicit class PairIterImp[K, V](base: Iterable[(K, V)]) {

    def keys: Iterable[K] = base.map(_._1)

    def values: Iterable[V] = base.map(_._2)

    def cogroup[W](that: Iterable[(K, W)]) = {
      val map = mutable.HashMap[K, (mutable.MutableList[V], mutable.MutableList[W])]()
      base.foreach(t => {
        val l = map.getOrElseUpdate(t._1, (mutable.MutableList[V](), mutable.MutableList[W]()))
        l._1 += t._2
      })
      that.foreach(t => {
        val l = map.getOrElseUpdate(t._1, (mutable.MutableList[V](), mutable.MutableList[W]()))
        l._2 += t._2
      })
      map
        .map(t => {
          (t._1, (t._2._1.toSeq, t._2._2.toSeq))
        })
        .toSeq
    }


    def leftOuterJoin[W](that: Iterable[(K, W)]) = {
      cogroup(that)
        .flatMap(t => {
          t._2._1 match {
            case Seq(left) =>
              Some(t._1, (left, t._2._2.headOption))
            case _ => None
          }
        })
    }

    def mapValues[U](fn: V => U) = {
      base.map(v => {
        (v._1, fn(v._2))
      })
    }

    def swap: Iterable[(V, K)] = {
      base.map(v => {
        (v._2, v._1)
      })
    }

    def groupByKey: Iterable[(K, Iterable[V])] = {
      base
        .groupBy(_._1)
        .map(v => {
          (v._1, v._2.map(_._2))
        })
    }


    def reduceByKey(fn: (V, V) => V): Iterable[(K, V)] = {
      groupByKey
        .mapValues(v => v.reduce(fn))
    }

  }

  implicit class PairIterRightImp[K, V](base: Iterable[(K, Iterable[V])]) {
    def flattenRight: Iterable[(K, V)] = {
      base
        .flatMap(t => {
          t._2.map(v => {
            (t._1, v)
          })
        })
    }
  }

  implicit class PairIterLeftImp[K, V](base: Iterable[(Iterable[K], V)]) {
    def flattenLeft: Iterable[(K, V)] = {
      base
        .flatMap(t => {
          t._1.map(v => {
            (v, t._2)
          })
        })
    }
  }


  implicit class IterImp[T](base: Iterable[T]) {


    def fluentPrintln(str: String): Iterable[T] = {
      base.foreach(v => System.out.println(str, v))
      base
    }

    def fluentEach(fn: T => Unit): Iterable[T] = {
      base.foreach(fn)
      base
    }

    def tailOrNil: Traversable[T] = {
      if (base.isEmpty) Nil
      else base.tail
    }

    def shiftMap[A](fn: T => A): Iterable[(T, A)] = {
      base.map(v => {
        (v, fn(v))
      })
    }

    def mostCommonElement: Option[T] = {
      if (base.isEmpty) {
        None
      } else {
        Some(base.groupBy(identity)
          .mapValues(_.size)
          .maxBy(_._2)
          ._1)
      }
    }

    def without(items: T*): Iterable[T] = {
      base.filter(v => {
        !items.contains(v)
      })
    }

    def flattenRightWith[A](value: A): Iterable[(T, A)] = {
      base.map(v => {
        (v, value)
      })
    }

    def flattenLeftWith[A](value: A): Iterable[(A, T)] = {
      base.map(v => {
        (value, v)
      })
    }

    def keyBy[A](fn: T => A): Seq[(A, T)] = {
      base.map(v => (fn(v), v)).toSeq
    }


    def valueBy[A](fn: T => A): Seq[(T, A)] = {
      base.map(v => (v, fn(v))).toSeq
    }


    def flatExpand[A](fn: T => Iterable[A]): Iterable[(T, A)] = {
      base.flatMap(t => {
        fn(t).map(tt => {
          (t, tt)
        })
      })
    }


    def isAllSame: Boolean = {
      if (base.isEmpty) {
        true
      } else {
        val head = base.head
        base.forall(v => v equals head)
      }
    }

    def isUnique: Boolean = {
      val set = mutable.HashSet[T]()
      base.foreach(v => {
        if (set.contains(v)) {
          return false
        } else {
          set += v
        }
      })
      true
    }


    def assertUnique: Iterable[T] = {
      isUnique match {
        case true => base
        case false => throw new IterableImplicits.DuplicateElementException(s"Duplicate items in: ${base}")
      }
    }


    def distinct: Iterable[T] = {
      base.toList.distinct
    }

    def containsAny(other: Iterable[T]): Boolean = {
      base.toList.intersect(other.toList).nonEmpty
    }

    def containsAll(other: Iterable[T]): Boolean = {
      other.toList.diff(base.toList).isEmpty
    }


    def crossCollectFirst[A, R](that: Iterable[A])(fn: PartialFunction[(T, A), R]): Option[R] = {
      val lifted = fn.lift
      for (i <- base) {
        for (j <- that) {
          lifted.apply((i, j)) match {
            case Some(r) => return Some(r)
            case _ => // pass
          }
        }
      }
      None
    }

    def cross[A](that: Iterable[A]): Iterable[(T, A)] = {
      val ret = mutable.MutableList[(T, A)]()
      for (i <- base) {
        for (j <- that) {
          ret += ((i, j))
        }
      }
      ret.toList
    }

    def crossSelfPairs(): Iterable[(T, T)] = {
      val ret = mutable.MutableList[(T, T)]()
      val seq = base.toIndexedSeq
      for (i <- 0 until seq.size) {
        for (j <- (i + 1) until seq.size) {
          ret += ((seq(i), seq(j)))
        }
      }
      ret.toSeq
    }

    def crossExists[A](that: Iterable[A])(fn: (T, A) => Boolean): Boolean = {
      for (i <- base) {
        for (j <- that) {
          if (fn(i, j)) {
            return true
          }
        }
      }
      false
    }

    def shuffle: Iterable[T] = {
      Random.shuffle(base)
    }

    def equalsAny(that: Iterable[T]): Boolean = {
      base.exists(a => {
        that.exists(b => {
          a == b
        })
      })
    }


  }


  implicit class ArrayImp[T](base: Array[T]) {
    def get(i: Int): Option[T] = {
      if (i >= base.length) {
        None
      } else {
        Some(base(i))
      }
    }
  }


  implicit class SeqImp[T](base: Seq[T]) {

    def mod(i: Int): T = {
      base.apply(i % base.size)
    }

    def dropRightWhile(p: T => Boolean): Seq[T] = {
      base.drop(base.lastIndexWhere(!p(_)) + 1)
    }


  }

  implicit class OptionSeqImp[T](base: Option[Seq[T]]) {
    def flattenEmpty: Option[Seq[T]] = {
      base match {
        case Some(v) if v == null => None
        case Some(v) if v.isEmpty => None
        case v => v
      }
    }

    def orNil: Seq[T] = {
      base match {
        case Some(v) if v.isEmpty => Nil
        case None => Nil
        case Some(v) => v
      }
    }
  }

  //
  //  implicit class OptionIterableStringImp[T](base: Option[Iterable[String]]) {
  ////
  ////    def toIter: Iterable[String] = {
  ////      base match {
  ////        case None => None
  ////        case Some(i) =>
  ////          i.filter(_.nonBlank)
  ////      }
  ////    }
  //
  //    def isBlank = {
  //      toIter.isEmpty
  //    }
  //
  //    def nonBlank = {
  //      toIter.nonEmpty
  //    }
  //
  //  }


  implicit class IterableStringImp(base: Seq[String]) {

    import StringImplicits._

    def nonBlanks: Iterable[String] = {
      base.filter(_.nonBlank)
    }

    def isBlank = {
      nonBlanks.isEmpty
    }

    def nonBlank = {
      nonBlanks.nonEmpty
    }

    def toCSVRow: String = {
      base
        .map(s => {
          if (s.isAlphaNumeric) {
            s
          } else {
            '"' + s.escapeDoubleQuotes + '"'
          }
        })
        .mkString(",")
    }

  }


  implicit class OptionSetImp[T](base: Option[Set[T]]) {
    def flattenEmpty: Option[Set[T]] = {
      base match {
        case Some(v) if v.isEmpty => None
        case v => v
      }
    }

    def orNil: Set[T] = {
      base match {
        case Some(v) if v.isEmpty => Nil
        case None => Nil
        case Some(v) => v
      }
    }
  }

  implicit class OptionImp[T](base: Option[T]) {

    def reduceWith(that: Option[T])(fn: (T, T) => T): Option[T] = {
      (base, that) match {
        case (Some(a), Some(b)) => Some(fn(a, b))
        case (Some(a), _) => Some(a)
        case (_, Some(b)) => Some(b)
        case _ => None
      }
    }

    def flatNull: Option[T] = {
      base match {
        case Some(null) => None
        case v => v
      }
    }
//
//    def asOpt[R: Manifest]: Option[R] = {
//      base match {
//        case Some(b) =>
//          b.asOpt[R]
//        case _ => None
//      }
//    }

    def tri[R](fn: (T) => R): Option[R] = {
      base match {
        case Some(v) if v == null => None
        case Some(v) => Option(fn(v))
        case _ => None
      }
    }

    def orNullT: T = base.getOrElse(null.asInstanceOf[T])


    def orException(str: String) = {
      base match {
        case Some(v) => v
        case None => throw new Exception(str)
      }
    }

    def orException(ex: Throwable): T = {
      base match {
        case Some(v) => v
        case None => throw ex
      }
    }

  }


}

object IterableImplicits extends IterableImplicits {

  case class DuplicateElementException(s: String) extends Exception(s)

}
