/*
https://adventofcode.com

Day 1
Part 2

cat 1.txt | scala 1_2.scala
*/
object Solution {
  def fuel(i: Int) =
    if (i < 6) 0
    else i / 3 - 2

  def module(mass: Int) =
    Iterator.iterate(fuel(mass))(fuel).takeWhile { _ > 0 }.sum

  def total(it: LazyList[Int]) = it.map(module).sum

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
