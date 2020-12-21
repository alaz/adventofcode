/*

Day 2
Part 1
cat 2.input | scala 2_1.scala
*/
object Solution {
  case class Input(min: Int, max: Int, letter: Char, password: String)

  object AsInt {
    import scala.util.Try
    def unapply(s: String): Option[Int] = Try(s.toInt).toOption
  }

  private val InputRE = """(\d+)-(\d+) (.): (.+)""".r

  def unmarshal(s: String): Input = s match {
    case InputRE(AsInt(min), AsInt(max), letter, password) =>
      Input(min, max, letter(0), password)
  }

  def valid(in: Input) = {
    val Input(min, max, letter, password) = in
    val n = password.count { _ == letter }
    n >= min && n <= max
  }

  def main(args: Array[String]): Unit = {
    val scanner = new java.util.Scanner(System.in)
    def it: LazyList[String] =
      if (scanner.hasNext)
        scanner.nextLine #:: it
      else
        LazyList.empty

    val count = it.map(unmarshal).count(valid)
    System.out.println(count)
  }
}
