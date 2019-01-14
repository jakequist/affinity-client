package jakequist.affinity

case class Field(
                id: Int,
                name: String,
                value_type: Int,
                allows_multiples: Boolean,
                dropdown_options: Seq[String] = Nil
                )


object Field {


}