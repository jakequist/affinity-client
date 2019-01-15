package jakequist.affinity

import com.kifi.macros.json
import jakequist.affinity.util.AuxJson._
import play.api.libs.json.{Format, Json}


case class Field(
                id: Int,
                name: String,
                value_type: Int,
                allows_multiple: Boolean,
                dropdown_options: Seq[String] = Nil
                )


object Field {

  implicit def formats : Format[Field] = Json.using[Json.WithDefaultValues].format[Field]

}