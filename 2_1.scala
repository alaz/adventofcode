/*
https://adventofcode.com

Day 2
Part 1

cat 2.txt | scala 2_1.scala
*/
object Solution {
  import scala.annotation.tailrec

  def execute(a: Array[Int], result: Int) = {
    @tailrec def step(i: Int): Int = a(i) match {
      case 1 =>
        a(a(i+3)) = a(a(i+1)) + a(a(i+2))
        step(i + 4)
      case 2 =>
        a(a(i+3)) = a(a(i+1)) * a(a(i+2))
        step(i + 4)
      case 99 =>
        a(result)
    }

    step(0)
  }

  def main(args: Array[String]): Unit = {
    val it = new Iterator[Int] {
      private val scanner = new java.util.Scanner(System.in).useDelimiter("[,\\s]")
      override def hasNext = scanner.hasNext
      override def next = scanner.nextInt
    }

    System.out.println(execute(it.toArray, 0))
  }
}
