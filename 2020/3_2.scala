/*

Day 3
Part 2
cat 3.input | scala 3_2.scala
*/
object Solution {
  def step(x: Int, y: Int) =
    (state: (Int, Int), input: (String, Int)) => {
      val (step, trees) = state
      val (line, index) = input
      lazy val i = step % line.length

      if (index % y != 0)
        (step, trees)
      else if (line(i) == '#')
        (step + x, trees + 1)
      else
        (step + x, trees)
    }

  def solve(it: LazyList[String]) = {
    val fns = Seq(
      step(1, 1),
      step(3, 1),
      step(5, 1),
      step(7, 1),
      step(1, 2)
    )
    val zeros = fns.map(_ => 0 -> 0)

    def fn(states: Seq[(Int, Int)], input: (String, Int)) =
      fns.zip(states).map { case (fn, state) => fn(state, input) }

    it.zipWithIndex
      .foldLeft(zeros)(fn)
      .map(_._2.toLong)
      .product
  }

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
