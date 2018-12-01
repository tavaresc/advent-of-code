import scala.io.Source

object OneB extends App {
  //println(System.getProperty("user.dir"))

  val filename = "inputs/input_one.txt"

  val frequencies = Source.fromFile(filename).getLines.toList.map(_.toInt)
  //println(s"Frequencies length = ${frequencies.length}")


  def calIntermediateFrequencies
  (freqs: List[Int],
  frequenciesSums: Set[Int],
  currentSum: Int,
  counter: Int): Int = {
    freqs match {
      case Nil =>
        println(s"------------ loop $counter ---------")
        calIntermediateFrequencies(frequencies, frequenciesSums, currentSum, counter + 1)
      case h :: tail =>
        val sum = h + currentSum
        if (!frequenciesSums.contains(sum)) {
          //println(s"head = $h")
          calIntermediateFrequencies(tail, frequenciesSums + sum, sum, counter)
        } else {
          //println(s"head is = $h \nfrequenciesSums = $frequenciesSums")
          sum
        }
    }
  }

  val resultingFrequency = calIntermediateFrequencies(frequencies, Set.empty, 0, 1)
  println(resultingFrequency)
}
