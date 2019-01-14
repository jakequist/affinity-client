package jakequist.affinity

import scala.concurrent.Await
import scala.concurrent.duration._

object TestMain {

  val API_KEY = "oHNuqLN0qLnbkGSiRPdLS0h9usrxN6TAjZk-ThBlY_8"

  def main(args: Array[String]): Unit = {

    val client = AffinityClient(API_KEY)

    println(Await.result(
      client.getPersonFields(),
      5.seconds
    ))

    client.close
    AffinityClient.shutdown
  }

}
