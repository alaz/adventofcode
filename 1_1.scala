/*
https://adventofcode.com

Day 1
Part 1

cat 1.txt | scala 1_1.scala
*/
object Solution {
  def fuel(i: Int) =
    if (i < 6) 0
    else i / 3 - 2

  def total(it: LazyList[Int]) = it.map(fuel).sum

  def main(args: Array[String]): Unit = {
    val scanner = new java.util.Scanner(System.in)
    def it: LazyList[Int] =
      if (scanner.hasNext)
        scanner.nextInt #:: it
      else
        LazyList.empty

    System.out.println(total(it))
  }
}
