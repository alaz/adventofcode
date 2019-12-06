/*
https://adventofcode.com

Day 3
Part 2

cat 3.txt | scala 3_2.scala
*/
import scala.math._
import scala.util._

case class Point(x: Int, y: Int) {
  def distance = abs(x) + abs(y)
  def distance(p2: Point) = abs(x - p2.x) + abs(y - p2.y)
}

object Point {
  val Zero = Point(0, 0)
}

trait Vec {
  def length: Int
}
case class H(h: Int) extends Vec {
  override def length = abs(h)
}
case class V(v: Int) extends Vec {
  override def length = abs(v)
}

object Vec {
  val U = """U(\d+)""".r
  val R = """R(\d+)""".r
  val D = """D(\d+)""".r
  val L = """L(\d+)""".r

  private object AsInt {
    def unapply(s: String): Option[Int] =
      Try { java.lang.Integer.valueOf(s).intValue }.toOption
  }

  def parse(s: String): Vec = s match {
    case U(AsInt(v)) => V(v)
    case R(AsInt(h)) => H(h)
    case D(AsInt(v)) => V(-v)
    case L(AsInt(h)) => H(-h)
  }
}

case class Segment(startPoint: Point, vec: Vec) {
  lazy val endPoint: Point = vec match {
    case H(h) => startPoint.copy(x = startPoint.x + h)
    case V(v) => startPoint.copy(y = startPoint.y + v)
  }
}

object Segment {
  def normalize(s: Segment) = s match {
    case Segment(_, H(h)) if h < 0 => Segment(s.endPoint, H(-h))
    case Segment(_, V(v)) if v < 0 => Segment(s.endPoint, V(-v))
    case _ => s
  }

  def intersect(s1: Segment)(s2: Segment) = PartialFunction.condOpt((s1, s2)) {
    case (s1, s2) if s1.startPoint == s2.startPoint || s1.startPoint == s2.endPoint =>
      s1.startPoint
    case (s1, s2) if s1.endPoint == s2.startPoint || s1.endPoint == s2.endPoint =>
      s1.endPoint

    case (s1 @ Segment(_, V(_)), s2 @ Segment(_, H(_))) if
        (s1.startPoint.y to s1.endPoint.y contains s2.startPoint.y) &&
        (s2.startPoint.x to s2.endPoint.x contains s1.startPoint.x) =>
      Point(s1.startPoint.x, s2.startPoint.y)

    case (s1 @ Segment(_, H(_)), s2 @ Segment(_, V(_))) if
        (s2.startPoint.y to s2.endPoint.y contains s1.startPoint.y) &&
        (s1.startPoint.x to s1.endPoint.x contains s2.startPoint.x) =>
      Point(s2.startPoint.x, s1.startPoint.y)
  }
}

object Solution {
  def parseSegments(line: String) = {
    import java.util.Scanner
    import scala.jdk.CollectionConverters._

    val scanner = new Scanner(line).useDelimiter(",")
    val it = scanner.asScala.map(Vec.parse)

    Iterator.unfold(Point.Zero) { point =>
      it.nextOption.map { vec =>
        val segment = Segment(point, vec)
        (segment, segment.endPoint)
      }
    }
  }

  def intersections(wires: Seq[Seq[Segment]]) = {
    import Segment._

    def withLength(wire: Seq[Segment]) =
      wire.zip(wire.scanLeft(0) {
        case (l, segment) => l + abs(segment.vec.length)
      })

    def rest(intersection: Point, segment: Segment) =
      segment.startPoint.distance(intersection)

    for {
      (wire1, index1) <- wires.zipWithIndex
      (wire2, index2) <- wires.zipWithIndex if index1 < index2
      (segment1, length1) <- withLength(wire1)
      (segment2, length2) <- withLength(wire2)
      intersection <- intersect(normalize(segment1))(normalize(segment2))
    } yield {
      val l1 = length1 + rest(intersection, segment1)
      val l2 = length2 + rest(intersection, segment2)
      l1 + l2
    }
  }

  def main(args: Array[String]): Unit = {
    import java.io.BufferedReader
    import java.io.InputStreamReader

    val wires = Using.resource(new BufferedReader(new InputStreamReader(System.in))) { in =>
      Iterator
        .continually { Option(in.readLine) }
        .takeWhile { _.isDefined }
        .flatten
        .map(parseSegments)
        .map(_.toSeq)
        .toSeq
    }

    System.out.println(intersections(wires).drop(1).min)
  }
}
