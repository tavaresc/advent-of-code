import scala.io.Source

/*
More info in https://adventofcode.com/2018/day/2#part2

Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.

The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given
the following box IDs:

abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and
 fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.

What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing
character from either ID, producing fgij.)
 */

case class TwoB(filename: String) {
  //val filename = "inputs/input_two.txt"

  val idsInput = Source.fromFile(filename).getLines.toList.map(_.toList)

  val length = idsInput.head.length

  def compareIds(boxesIds: List[List[Char]]): List[Char] = {
    boxesIds match {
      case Nil => Nil
      case id :: Nil => Nil
      case id :: idListHead :: idList =>
        val tuples = id.zip(idListHead).filter { case (i, j) => i == j }

        if (tuples.length == length - 1)
          tuples.map(_._1)
        else
          compareIds(id :: idList)
    }
  }

  def applyComparison(boxesIds: List[List[Char]]): List[Char] = {
    boxesIds match {
      case _ :: list =>
        val result = compareIds(boxesIds)
        if (result.nonEmpty)
          result
        else
          applyComparison(list)
      case _ => Nil
    }
  }

  println(applyComparison(idsInput).mkString)
}

object TwoB {
  def start() = TwoB("inputs/input_two.txt")
}