import scala.io.Source
import scala.util.matching.Regex

/*
More info in https://adventofcode.com/2018/day/3

The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.

Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims have an ID and consist
 of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:

The number of inches between the left edge of the fabric and the left edge of the rectangle.
The number of inches between the top edge of the fabric and the top edge of the rectangle.
The width of the rectangle in inches.
The height of the rectangle in inches.
A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from
 the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of fabric represented by # (and
  ignores the square inches of fabric represented by .) in the diagram below:

...........
...........
...#####...
...#####...
...#####...
...#####...
...........
...........
...........
The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas. For example,
 consider the following claims:

#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
Visually, these claim the following areas:

........
...2222.
...2222.
.11XX22.
.11XX22.
.111133.
.111133.
........
The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the others, does not
overlap either of them.)

If the Elves all proceed with their own plans, none of them will have enough fabric. How many square inches of fabric
are within two or more claims?
*/

case class ThreeA(claims: Seq[String]) {
  // fabric size = 1000 x 1000
  val fabric: IndexedSeq[IndexedSeq[Int]] = IndexedSeq.fill(2000){IndexedSeq.fill(2000){0}}

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

  val duplicatedParsedclaims = parsedClaims.groupBy(x => x).mapValues(_.length).filter(_._2 > 1).size


  val newFabric = parsedClaims.foldLeft(fabric){ case (fab, (i, j)) =>
    val seq1 = fab(i)
    val value = seq1(j) + 1
    fab.updated(i, seq1.updated(j, value))
  }

  val result = newFabric.map(a => a.filter(_ > 1).length).sum

  println(s"---- numberOfOverlapedCells = $result")

  println(s"---- duplicatedParsedclaims = $duplicatedParsedclaims")
  //newFabric.map(println(_))
}

object ThreeA {
  val claims = Source.fromFile("inputs/input_three.txt").getLines.toSeq

  def start() = ThreeA(claims)
}
