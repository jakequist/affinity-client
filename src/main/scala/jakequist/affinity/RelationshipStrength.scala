package jakequist.affinity

import com.kifi.macros.json


@json
case class RelationshipStrength(
                                 internal_id: Int,
                                 external_id: Int,
                                 strength: Double
                               )
