import org.scalatest.Matchers

class One extends Matchers {
  val frequencies1 = List(+1, +1, +1)
  val result1 = 3

  "getResultingFrequency" should {

    "return 3" {
      val one = new OneA(frequencies1)

      one should equal (result1)
    }
  }
}
