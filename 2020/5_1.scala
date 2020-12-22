/*

Day 5
Part 1
cat 5.input | scala 5_1.scala
*/
object Solution {
  case class BoardingPass(row: Int, column: Int)

  def decode(code: String, map: Map[String, String]): Int = {
    val re = s"([${map.keySet.mkString}])".r
    val matched = (m: scala.util.matching.Regex.Match) => m.matched
    def parseBinary(s: String) = java.lang.Integer.parseInt(s, 2)
    parseBinary(re.replaceAllIn(code, matched.andThen(map)))
  }

  def parse(line: String) = {
    val RowBinMap = Map("F" -> "0", "B" -> "1")
    val ColBinMap = Map("L" -> "0", "R" -> "1")
    BoardingPass(
      decode(line.slice(0, 7),  RowBinMap),
      decode(line.slice(7, 10), ColBinMap)
    )
  }

  def seatId(bp: BoardingPass): Int = bp.row * 8 + bp.column

  def main(args: Array[String]): Unit = {
    val scanner = new java.util.Scanner(System.in)
    def it: LazyList[String] =
      if (scanner.hasNext)
        scanner.nextLine #:: it
      else
        LazyList.empty

    System.out.println(it.map(parse).map(seatId).max)
  }
}
