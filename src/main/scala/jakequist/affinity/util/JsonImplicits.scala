package jakequist.affinity.util

import play.api.libs.json._

import scala.collection.Map
import scala.reflect.ClassTag


trait JsonImplicits {


  implicit class AnyImp[T](base: T) {

    // Convenience: execute a block in a fluent way
    def fluently(block : T => Unit) : T = {
      block(base)
      base
    }



    def toJson(implicit f:Format[T]) : JsValue = {
      Json.toJson(base)
    }

    def toJsonPretty(implicit f:Format[T]) : String = {
      Json.prettyPrint(Json.toJson(base))
    }

  }



  implicit class JsValuesImp(base: JsValue) {

    def asObj: JsObject = {
      base.asInstanceOf[JsObject]
    }

    def asAry: JsArray = {
      base.asInstanceOf[JsArray]
    }

    def formatPretty: String = {
      Json.prettyPrint(base)
    }

    def toStringPretty = formatPretty

    def trim: JsValue = {
      JsonImplicits.trimImp(base).getOrElse(JsNull)
    }

    def hasKey(s: String) = {
      base match {
        case j: JsObject =>
          j.keys.contains(s)
        case _ =>
          false
      }
    }

    def without(keys: String*): JsValue = {
      val keySet = Set(keys: _*)
      base match {
        case JsObject(map) =>
          Json.toJsObject( map.filterKeys(v => !keySet.contains(v)) )
        case _ => base
      }
    }

  }


}


object JsonImplicits extends JsonImplicits {

  def trimImp(js: JsValue): Option[JsValue] = {
    js match {
      case null => None
      case JsNull => None
      case JsString(s) if s == null => None
      case JsString(s) if s.isEmpty => None
      case JsArray(v) if v.isEmpty => None
      case JsArray(v) =>
        v.flatMap(vv => trimImp(vv)) match {
          case vvv if vvv.isEmpty => None
          case vvv => Some(JsArray(vvv))
        }
      case JsObject(v) if v == null => None
      case JsObject(v) if v.isEmpty => None
      case JsObject(v) =>
        val r = v.flatMap(t => {
          trimImp(t._2) match {
            case Some(vv) => Some(t._1, vv)
            case _ => None
          }
        })
        r match {
          case vv if vv.isEmpty => None
          case vv => Some(JsObject(vv))
        }
      case v => Option(v)
    }
  }


}


