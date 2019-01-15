package jakequist.affinity

import com.kifi.macros.json
import play.api.libs.json.JsValue

@json
case class FieldValue(
                       id: Int, // integer	The unique identifier of the field value object.
                       field_id: Int, // integer	The unique identifier of the field the value is associated with.
                       entity_id: Int, // integer	The unique identifier of the person, organization, or opportunity object the field value is associated with.
                       list_entry_id: Int, // integer	The unique identifier of the list entry object the field value is associated with. This only exists if the field the value is associated with is list-specific.
                       value: JsValue // One of many	The value attribute can take on many different types, depending on the field value_type. See below for reference.
                     )