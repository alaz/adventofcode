/*
https://adventofcode.com

Day 3
Part 1

cat 3.txt | scala 3_1.scala
*/
import scala.math._
import scala.util._

case class Point(x: Int, y: Int) {
  def distance = abs(x) + abs(y)
}

object Point {
  val Zero = Point(0, 0)
  implicit val ordering = Ordering.by[Point, Int](_.distance)
}

trait Vec
case class H(h: Int) extends Vec
case class V(v: Int) extends Vec

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
  implicit def ordering(implicit pointOrdering: Ordering[Point]) = pointOrdering.on[Segment] { segment =>
    if (pointOrdering.compare(segment.startPoint, segment.endPoint) < 0)
      segment.startPoint
    else
      segment.endPoint
  }

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

  def intersections(wires: Seq[Seq[Segment]]) =
    for {
      (wire, index) <- wires.zipWithIndex
      (otherWire, otherIndex) <- wires.zipWithIndex if index < otherIndex
      segment <- wire
      intersection <- otherWire.flatMap(Segment.intersect(segment))
    } yield {
      intersection
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
        .map(_.drop(1).map(Segment.normalize).toSeq.sorted)
        .toSeq
    }

    val intersection = intersections(wires).headOption
    System.out.println(intersection.map { i => i -> i.distance })
  }
}
