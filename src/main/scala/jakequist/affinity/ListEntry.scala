package jakequist.affinity

import java.sql.{Date, Timestamp}

import play.api.libs.json.{Format, Json}
import jakequist.affinity.util.AuxJson._


case class ListEntry(
                      id: Int,
                      list_id: Int,
                      creator_id: Int,
                      entity_id: Int,
                      entity: Entity,
                      created_at: Date
                    )


object ListEntry {
  implicit def formats : Format[ListEntry] = Json.format[ListEntry]
}