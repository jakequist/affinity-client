package jakequist.affinity

import com.kifi.macros.json
import play.api.libs.json.Json._
import jakequist.affinity.util.AuxJson._


@json
case class List(
                 id: Int,
                 `type`: Int,  // The type of the entities contained within the list
                 name: String,  // The title of the list that is displayed in Affinity
                 public: Boolean,  // If the list is publicly accessible to all users in your team, this is true. Otherwise, this is false
                 owner_id: Int,  // The unique id of the internal person who created this list
                 list_size: Int  // The number of list entries contained within the list.
               )

