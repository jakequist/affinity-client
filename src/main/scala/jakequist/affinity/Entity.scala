package jakequist.affinity

import java.sql.Date

import play.api.libs.json._


trait Entity {

}

object Entity {

  implicit def formats : Format[Entity] = new Format[Entity] {
    override def writes(o: Entity): JsValue = ???
    override def reads(json: JsValue): JsResult[Entity] = ???
  }

}

