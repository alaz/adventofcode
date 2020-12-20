/*

Day 1
Part 1
cat 1.txt | scala 1_1.scala
*/
object Solution {
  import scala.collection.immutable.SortedSet

  def find(it: LazyList[Int]): Int = {
    it.foldLeft(SortedSet.empty[Int]) {
      case (set, x) if set.contains(x) =>
        return x * (2020 - x)
      case (set, x) =>
        set + (2020 - x)
    }
    0
  }

  def main(args: Array[String]): Unit = {
    val scanner = new java.util.Scanner(System.in)
    def it: LazyList[Int] =
      if (scanner.hasNext)
        scanner.nextInt #:: it
      else
        LazyList.empty

    System.out.println(find(it))
  }
}
