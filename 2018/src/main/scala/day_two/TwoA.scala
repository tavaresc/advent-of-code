import scala.io.Source

/*
More info in https://adventofcode.com/2018/day/2

To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number that have an ID
containing exactly two of any letter and then separately counting those with exactly three of any letter. You can
multiply those two counts together to get a rudimentary checksum and compare it to what your device predicts.

For example, if you see the following box IDs:

abcdef contains no letters that appear exactly two or three times.
bababc contains two a and three b, so it counts for both.
abbcde contains two b, but no letter appears exactly three times.
abcccd contains three c, but no letter appears exactly two times.
aabcdd contains two a and two d, but it only counts once.
abcdee contains two e.
ababab contains three a and three b, but it only counts once.
Of these box IDs, four of them contain a letter which appears exactly twice, and three of them contain a letter which
appears exactly three times. Multiplying these together produces a checksum of 4 * 3 = 12.

What is the checksum for your list of box IDs?
 */
case class TwoA(filename: String) {
  val idsInput = Source.fromFile(filename).getLines.toList.map(_.toList)

  def calcChecksum(): String = {
    val charCounts = idsInput
      .map(_
        .groupBy(identity)
        .values
        .map(_.length)
        .toSet
      )
      .foldLeft((0, 0)) {
        case (counts, charCount) =>
          (counts._1 + charCount.count(_ == 2), counts._2 + charCount.count(_ == 3))
      }

    (charCounts._1 * charCounts._2).toString
  }

  def calcChecksum(boxId: List[Char], twiceCounter: Int, threeTimesCounter: Int): (Int, Int) = {
    boxId match {
      case Nil => (twiceCounter, threeTimesCounter)
      case h :: tail =>
        val counter = tail.count(_ == h) + 1 // 1 for the occurrence in head

        val cTwice =
          if (counter == 2 && twiceCounter == 0) twiceCounter + 1
          else twiceCounter
        val cThree =
          if (counter == 3 && threeTimesCounter == 0) threeTimesCounter + 1
          else threeTimesCounter
        calcChecksum((boxId.filterNot(_ == h)), cTwice, cThree)
    }
  }

  val counterPair =
    idsInput
      .map(calcChecksum(_, 0, 0))
      .foldLeft(0, 0) { case ((c2, c3), (i0, i1)) => ((c2 + i0), (c3 + i1)) }
  //println(counterPair)
  println(counterPair._1 * counterPair._2)
  //println(calcChecksum())
}

object TwoA {
  def start() = TwoA("inputs/input_two.txt")
}