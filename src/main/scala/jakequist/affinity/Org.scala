package jakequist.affinity

import com.kifi.macros.json
import play.api.libs.json.Json


case class Org(

                id: Int, // The unique identifier of the organization object.
                name: String, // The name of the organization (see below).
                domain: Option[String], // The website name of the organization. This is used by Affinity to automatically associate person objects with an organization.
                crunchbase_uuid: Option[String] = None, // The Crunchbase UUID of the organization
                person_ids: Seq[String] = Nil, // []	An array of unique identifiers of person that are associated with the organization
                global: Boolean = false, // Returns whether this organization is a part of Affinity’s global dataset of organizations. This is always false if the organization was created by you.
                list_entries: Seq[ListEntry] = Nil, // []	An array of list entry resources associated with the organization, only returned as part of the Get a specific organization endpoint.
                interaction_dates: Option[InteractionDates] = None
              )


object Org {
  implicit def format = Json.using[Json.WithDefaultValues].format[Org]
}


@json
case class OrgSearchResponse(
                              organizations: Seq[Org],
                              next_page_token: Option[String]
                            )