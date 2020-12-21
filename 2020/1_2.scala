/*

Day 1
Part 2
cat 1.txt | scala 1_2.scala
*/
object Solution {
  import scala.collection.immutable.SortedSet

  def f3(s: SortedSet[Int]): Int = {
    def f2(n: Int, s: SortedSet[Int]) = {
      val r = s.range(0, n)
      r.find { x => r.contains(n - x) }
    }

    (
      for {
        x <- s
        y <- f2(2020 - x, s)
        z = 2020 - x - y
      } yield x * y * z
    ).head
  }

  def main(args: Array[String]): Unit = {
    val scanner = new java.util.Scanner(System.in)
    def it: LazyList[Int] =
      if (scanner.hasNext)
        scanner.nextInt #:: it
      else
        LazyList.empty

    System.out.println(f3(SortedSet.from(it)))
  }
}
