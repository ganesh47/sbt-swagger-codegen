import eu.unicredit.swagger.generators.DefaultJsonGenerator
import treehugger.forest

/**
  * Created by Ganesh on 5/1/2017.
  */
object ConstructingTreeDefTests {
  /**
    *
    *
    * implicit def tuple3Reads[A, B, C](implicit aReads: Reads[A], bReads: Reads[B], cReads: Reads[C]):    Reads[Tuple3[A, B, C]] = new Reads[Tuple3[A, B, C]] {
    * case JsArray(arr) if arr.size == 3 => for {
    * a <- aReads.reads(arr(0))
    * b <- bReads.reads(arr(1))
    * c <- cReads.reads(arr(2))
    * } yield (a, b, c)
    * case _ => JsError(Seq(JsPath() -> Seq(ValidationError("Expected array of three elements"))))
    * }
    *
    * implicit def tuple2Writes[A, B, C](implicit aWrites: Writes[A], bWrites: Writes[B], cWrites: Writes[C]): Writes[Tuple3[A, B, C]] = new Writes[Tuple3[A, B, C]] {
   def writes(tuple: Tuple3[A, B, C]) = JsArray(Seq(aWrites.writes(tuple._1), bWrites.writes(tuple._2), cWrites.writes(tuple._3)))
    */

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def main(args: Array[String]): Unit = {

    val offset = 1
    val limit = 3

    val reads = new DefaultJsonGenerator() prepareJsonTree(offset, limit)
    reads.map(x=>treeToString(x)).foreach(println)

  }


}
