package jakequist.affinity.util

import java.net.{URI, URL}
import java.util.Date

import com.google.common.net.InternetDomainName
import org.joda.time.DateTime
import play.api.libs.json._
import DateImplicits._

object AuxJson {

  import Json._

  class OptionFormats[A: Format] extends Format[Option[A]] with Serializable {
    override def reads(json: JsValue): JsResult[Option[A]] = {
      json match {
        case JsNull => JsSuccess(None)
        case v => JsSuccess(Json.fromJson[A](v).asOpt)
      }
    }

    override def writes(o: Option[A]): JsValue = {
      o match {
        case None => JsNull
        case Some(v) => Json.toJson(v)
      }
    }
  }

  implicit def toOptionFormat[A: Format]: Format[Option[A]] = new OptionFormats[A]()


  class Tuple2Formats[A: Format, B: Format] extends Format[Tuple2[A, B]] with Serializable {
    override def writes(o: (A, B)): JsValue = JsArray(Seq(Json.toJson(o._1), Json.toJson(o._2)))

    override def reads(json: JsValue): JsResult[(A, B)] = json match {
      case JsArray(vals) if vals.size == 2 => JsSuccess(
        Tuple2(vals(0).as[A], vals(1).as[B])
      )
      case _ => JsError()
    }
  }

  implicit def toTuple2Format[A: Format, B: Format]: Format[Tuple2[A, B]] = new Tuple2Formats[A, B]()


  class Tuple3Formats[A: Format, B: Format, C: Format] extends Format[Tuple3[A, B, C]] with Serializable {
    override def writes(o: (A, B, C)): JsValue = JsArray(Seq(Json.toJson(o._1), Json.toJson(o._2), Json.toJson(o._3)))

    override def reads(json: JsValue): JsResult[(A, B, C)] = json match {
      case JsArray(vals) if vals.size == 3 => JsSuccess(
        Tuple3(vals(0).as[A], vals(1).as[B], vals(2).as[C])
      )
      case _ => JsError()
    }
  }

  implicit def toTuple3Format[A: Format, B: Format, C: Format]: Format[Tuple3[A, B, C]] = new Tuple3Formats[A, B, C]()

  //  implicit object Tuple2Formats extends Format[Tuple2[Double, Double]] with Serializable {
  //    override def writes(o: (Double, Double)): JsValue = JsArray(Seq(JsNumber(o._1), JsNumber(o._2)))
  //
  //    override def reads(json: JsValue): JsResult[(Double, Double)] = json match {
  //      case JsArray(vals) if vals.size == 2 => JsSuccess(
  //        Tuple2(vals(0).as[JsNumber].value.toDouble, vals(1).as[JsNumber].value.toDouble)
  //      )
  //      case _ => JsError()
  //    }
  //  }


  implicit object SymbolFormats extends Format[Symbol] with Serializable {
    override def writes(o: Symbol): JsValue = JsString(o.name)

    override def reads(json: JsValue): JsResult[Symbol] = {
      json match {
        case JsString(s) => JsSuccess(Symbol(s))
        case v => JsError(s"Expected a string, instead got: ${v}")
      }
    }
  }


  implicit object UrlFormats extends Format[URL] with Serializable {
    override def writes(o: URL): JsValue = JsString(o.toString)

    override def reads(json: JsValue): JsResult[URL] = json match {
      case v: JsString =>
        v.value match {
          case s if s.trim.nonEmpty => JsSuccess(new URL(v.value))
          case _ => JsError()
        }
      case _ => JsError()
    }

  }


  implicit object UriFormats extends Format[URI] with Serializable {
    override def writes(o: URI): JsValue = JsString(o.toString)

    override def reads(json: JsValue): JsResult[URI] = json match {
      case v: JsString =>
        v.value match {
          case s if s.trim.nonEmpty => JsSuccess(new URI(v.value))
          case _ => JsError()
        }
      case _ => JsError()
    }

  }


  implicit object InternetDomainNameFormats extends Format[InternetDomainName] with Serializable {
    override def writes(o: InternetDomainName): JsValue = JsString(o.toString)

    override def reads(json: JsValue): JsResult[InternetDomainName] = json match {
      case v: JsString =>
        v.value match {
          case s if s.trim.nonEmpty => JsSuccess(InternetDomainName.from(v.value))
          case _ => JsError()
        }
      case _ => JsError()
    }

  }


  implicit object utilDateFormats extends Format[java.util.Date] with Serializable {
    override def writes(o: java.util.Date): JsValue = JsNumber(o.getTime)

    override def reads(json: JsValue): JsResult[java.util.Date] = json match {
      case v: JsNumber => JsSuccess(new DateTime(v.value.longValue()).toJava)
      case v: JsString => JsSuccess(DateTime.parse(v.value).toJava)
      case _ => JsError()
    }
  }


  implicit object sqlDateFormats extends Format[java.sql.Date] with Serializable {
    override def writes(o: java.sql.Date): JsValue = JsNumber(o.getTime)

    override def reads(json: JsValue): JsResult[java.sql.Date] = json match {
      case v: JsNumber => JsSuccess(new DateTime(v.value.longValue()).toSql)
      case v: JsString => JsSuccess(DateTime.parse(v.value).toSql)
      case _ => JsError()
    }
  }


  implicit object stringFormats extends Format[String] with Serializable {
    override def writes(o: String): JsValue = JsString(o)

    override def reads(json: JsValue): JsResult[String] = json match {
      case v: JsString => JsSuccess(v.value)
      case _ => JsError()
    }
  }

  implicit object intFormats extends Format[Int] with Serializable {
    override def writes(o: Int): JsValue = JsNumber(o)

    override def reads(json: JsValue): JsResult[Int] = json match {
      case v: JsNumber => JsSuccess(v.value.toInt)
      case _ => JsError()
    }
  }

  implicit object longFormats extends Format[Long] with Serializable {
    override def writes(o: Long): JsValue = JsNumber(o)

    override def reads(json: JsValue): JsResult[Long] = json match {
      case v: JsNumber => JsSuccess(v.value.toLong)
      case _ => JsError()
    }
  }

  implicit object doubleFormats extends Format[Double] with Serializable {
    override def writes(o: Double): JsValue = JsNumber(o)

    override def reads(json: JsValue): JsResult[Double] = json match {
      case v: JsNumber => JsSuccess(v.value.toDouble)
      case _ => JsError()
    }
  }

  implicit object booleanFormats extends Format[Boolean] with Serializable {
    override def writes(o: Boolean): JsValue = JsBoolean(o)

    override def reads(json: JsValue): JsResult[Boolean] = json match {
      case v: JsBoolean => JsSuccess(v.value)
      case _ => JsError()
    }
  }


  implicit object jsonFormats extends Format[JsValue] with Serializable {
    override def writes(o: JsValue): JsValue = o

    override def reads(json: JsValue): JsResult[JsValue] = json match {
      case v: JsValue => JsSuccess(v)
      case _ => JsError()
    }
  }


  class MapFormats[T: Format] extends Format[scala.collection.Map[String, T]] with Serializable {
    override def writes(o: scala.collection.Map[String, T]): JsValue = JsObject(o.map(t => (t._1, Json.toJson(t._2))).toSeq)

    override def reads(json: JsValue): JsResult[scala.collection.Map[String, T]] = json match {
      case v: JsObject => JsSuccess(v.value.map(t => {
        (t._1, Json.fromJson[T](t._2).get)
      }).toMap)
      case _ => JsError()
    }
  }

  implicit def toMapFormats[T](implicit f: Format[T]): Format[scala.collection.Map[String, T]] = new MapFormats[T]()


  class ArrayFormats[T: Format] extends Format[Seq[T]] with Serializable {
    override def writes(o: Seq[T]): JsValue = JsArray(o.map(t => Json.toJson(t)).toSeq)

    override def reads(json: JsValue): JsResult[Seq[T]] = json match {
      case JsNull => JsSuccess(Seq.empty)
      case v: JsArray => JsSuccess(v.value.map(t => {
        val j = Json.fromJson[T](t)
        try {
          j.get
        } catch {
          case ex: Exception => throw new Exception(s"unable to parse json: ${j} ${t}", ex)
        }
      }))
      case _ => JsError()
    }
  }

  implicit def toSeqFormats[T](implicit f: Format[T]): Format[Seq[T]] = new ArrayFormats[T]()


}