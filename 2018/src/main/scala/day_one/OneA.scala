import scala.io.Source

object OneA extends App {
  val filename = "inputs/input_one.txt"

  val frequencies = Source.fromFile(filename).getLines.toList.map(_.toInt)
  //println(fileContent)

  val resultingFrequency = frequencies.foldLeft(0){ (acc, i) => acc + i }
  println(resultingFrequency)
}
