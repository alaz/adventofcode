/*

Day 3
Part 1
cat 3.input | scala 3_1.scala
*/
object Solution {
  def step(state: (Int, Int), line: String): (Int, Int) = {
    val (step, trees) = state
    val i = step % line.length
    if (line(i) == '#')
      (step + 3, trees + 1)
    else
      (step + 3, trees)
  }

  def solve(it: LazyList[String]) =
    it.foldLeft(0 -> 0)(step)._2

  def main(args: Array[String]): Unit = {
    val scanner = new java.util.Scanner(System.in)
    def it: LazyList[String] =
      if (scanner.hasNext)
        scanner.nextLine #:: it
      else
        LazyList.empty

    System.out.println(solve(it))
  }
}
