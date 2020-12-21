/*

Day 4
Part 1
cat 4.input | scala 4_1.scala
*/
object Solution {
  import scala.util.Try

  val PairRE = raw"(\w+):([^\s]+)".r

  def parse(line: String) = (
    for {
      fragment <- line.split(raw"\s")
      m <- PairRE.findAllMatchIn(fragment)
    } yield m.group(1) -> m.group(2)
  ).toMap

  val Required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def valid(m: Map[String, String]) =
    Required.diff(m.keySet).isEmpty

  def main(args: Array[String]): Unit = {
    val scanner = new java.util.Scanner(System.in).useDelimiter(raw"\n\n")
    def it: LazyList[String] =
      if (scanner.hasNext)
        scanner.next #:: it
      else
        LazyList.empty

    System.out.println(it.map(parse).count(valid))
  }
}
