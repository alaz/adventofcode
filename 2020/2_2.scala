/*

Day 2
Part 2
cat 2.input | scala 2_2.scala
*/
object Solution {
  case class Input(pos1: Int, pos2: Int, letter: Char, password: String)

  object AsInt {
    import scala.util.Try
    def unapply(s: String): Option[Int] = Try(s.toInt).toOption
  }

  private val InputRE = """(\d+)-(\d+) (.): (.+)""".r

  def unmarshal(s: String): Input = s match {
    case InputRE(AsInt(pos1), AsInt(pos2), letter, password) =>
      Input(pos1, pos2, letter(0), password)
  }

  def valid(in: Input) = {
    val Input(pos1, pos2, letter, password) = in
    val at1 = password(pos1 - 1) == letter
    val at2 = password(pos2 - 1) == letter
    at1 && !at2 || !at1 && at2
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
