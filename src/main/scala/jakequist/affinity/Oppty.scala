package jakequist.affinity

import com.kifi.macros.json
import play.api.libs.json.Json

case class Oppty(
                  id: Int, //integer	The unique identifier of the opportunity object.
                  name: String, //integer	The name of the opportunity (see below).
                  person_ids: Seq[Int], //number[]	An array of unique identifiers for persons that are associated with the opportunity
                  organization_ids: Seq[Int], //number[]	An array of unique identifiers for organizations that are associated with the opportunity
                  list_entries: Seq[ListEntry] //ListEntry[]	An array of list entry resources associated with the opportunity (at most 1 list entry). If the
                )

object Oppty {
  implicit def format = Json.using[Json.WithDefaultValues].format[Oppty]
}

@json
case class OpptySearchResponse(
                                opportunities : Seq[Oppty],
                                next_page_token: Option[String]
                              )