/*

Day 4
Part 1
cat 4.input | scala 4_1.scala
*/
object Solution {
  import scala.util.Try

  val PairRE = raw"(\w+):([^\s]+)".r
  val PassportRE = raw"(\d{9})".r
  val HairColorRE = raw"#([\da-f]{6})".r
  val EyeColorRE = raw"(amb|blu|brn|gry|grn|hzl|oth)".r
  val InchesRE = raw"(\d+)in".r
  val CentimetersRE = raw"(\d+)cm".r

  object AsInt {
    import scala.util.Try
    def unapply(s: String): Option[Int] = Try(s.toInt).toOption
  }

  def parse(line: String) = (
    for {
      fragment <- line.split(raw"\s")
      m <- PairRE.findAllMatchIn(fragment)
    } yield m.group(1) -> m.group(2)
  ).toMap

  type Validator = PartialFunction[String, Boolean]

  val Required = {
    val byr: Validator = { case AsInt(y) => y >= 1920 && y <= 2002 }
    val iyr: Validator = { case AsInt(y) => y >= 2010 && y <= 2020 }
    val eyr: Validator = { case AsInt(y) => y >= 2020 && y <= 2030 }
    val hgt: Validator = {
      case InchesRE(AsInt(height)) => height >= 59 && height <= 76
      case CentimetersRE(AsInt(height)) => height >= 150 && height <= 193
    }
    val hcl: Validator = { case HairColorRE(_) => true }
    val ecl: Validator = { case EyeColorRE(_) => true }
    val pid: Validator = { case PassportRE(_) => true }

    Map(
      "byr" -> byr,
      "iyr" -> iyr,
      "eyr" -> eyr,
      "hgt" -> hgt,
      "hcl" -> hcl,
      "ecl" -> ecl,
      "pid" -> pid
    )
  }

  def valid(m: Map[String, String]) = Required.forall { case (key, pf) =>
    m.get(key).collect(pf).getOrElse(false)
  }

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
