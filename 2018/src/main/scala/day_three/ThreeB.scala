import scala.io.Source
import scala.util.matching.Regex

/*
More info in https://adventofcode.com/2018/day/3#part2

Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single square inch of fabric with any
other claim. If you can somehow draw attention to it, maybe the Elves will be able to make Santa's suit after all!

For example, in the claims above, only claim 3 is intact after all claims are made.

What is the ID of the only claim that doesn't overlap?
*/

case class ThreeB(claims: Seq[String]) {
  // fabric size = 2000 x 2000
  val fabric: IndexedSeq[IndexedSeq[Int]] = IndexedSeq.fill(2000){IndexedSeq.fill(2000){0}}

  def getId(s: String): String = {
    val regex: Regex = """#(\d+)""".r
    val id = regex.findAllIn(s).mkString
    id.substring(1)
  }

  def getXY(s: String): List[Int] = {
    val regex: Regex = """@\s(\d+),(\d+)""".r
    val coordinates = regex.findAllIn(s).mkString
    coordinates.substring(2).trim.split(",").toList.map(_.toInt)
  }

  def getWidthHeight(s: String): List[Int] = {
    val regex: Regex = """:\s(\d+)x(\d+)""".r
    val mesures = regex.findAllIn(s).mkString
    mesures.substring(2).trim.split("x").toList.map(_.toInt)
  }

  def parseClaims(coordinates: List[Int], mesures: List[Int]): Seq[(Int, Int)] = {
    val x = coordinates(0)
    val y = coordinates(1)
    val w = mesures(0)
    val h = mesures(1)

    for {
      row <- y to (y + h - 1)
      col <- x to (x + w - 1)
    } yield (row, col)
  }
  val parsedClaims: Seq[(Int, Int)]  = claims.flatMap(c => parseClaims(getXY(c), getWidthHeight(c)))

  val newFabric = parsedClaims.foldLeft(fabric){ case (fab, (i, j)) =>
    val seq1 = fab(i)
    val value = seq1(j) + 1
    fab.updated(i, seq1.updated(j, value))
  }

  val parsedClaimsWithIds: Seq[(String,Seq[(Int, Int)])] =
    claims.map(c => (getId(c), parseClaims(getXY(c), getWidthHeight(c))))

  val duplicatedParsedClaims = parsedClaims.groupBy(x => x).mapValues(_.length).filter(_._2 > 1).map(_._1).toSeq
/*
  val nonOverlapdClaims = parsedClaimsWithIds.find {
    case (id, pair) => pair.forall{ case (x, y) => !duplicatedParsedClaims.contains((x, y))
    }
  }.get._1
*/
  val result = parsedClaimsWithIds.find{ case (id, pair) => pair.forall { case (x, y) => newFabric(x)(y) == 1 } }.get._1

  println(s"---- idDoesntOverlap = $result")
  //println(s"---- nonOverlapdClaims = $nonOverlapdClaims")
  //newFabric.map(println(_))
}

object ThreeB {
  val claims = Source.fromFile("inputs/input_three.txt").getLines.toSeq

  def start() = ThreeB(claims)
}
