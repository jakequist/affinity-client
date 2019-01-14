package jakequist.affinity

import java.sql.Date

import com.kifi.macros.json
import jakequist.affinity.util.AuxJson._
import play.api.libs.json.Json


@json
case class InteractionDates(
                             first_email_date: Option[Date] = None,
                             last_email_date: Option[Date] = None,
                             last_event_date: Option[Date] = None,
                             last_interaction_date: Option[Date] = None,
                             next_event_date: Option[Date] = None
                           )

case class Person(
                   id: Int,
                   `type`: Int, // 0 external, 1 internal
                   first_name: Option[String] = None,
                   last_name: Option[String] = None,
                   emails: Seq[String] = Nil,
                   phone_numbers: Seq[String] = Nil,
                   primary_email: Option[String] = None,
                   organization_ids: Seq[Int] = Nil,
                   list_entries: Seq[ListEntry] = Nil,
                   interaction_dates: Option[InteractionDates] = None
                 ) extends Entity

object Person {
  implicit def format = Json.using[Json.WithDefaultValues].format[Person]
}


@json
case class PersonSearchResponse(
                                 persons: Seq[Person],
                                 next_page_token: Option[String]
                               )