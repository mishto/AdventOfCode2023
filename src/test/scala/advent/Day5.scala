package advent

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite
import spire.math.Interval

class Day5 extends AnyFunSuite {
  test("part-1") {
    case class Mapping(dest: Long, source: Long, range: Long) {
      def map(value: Long) = {
        if (isMatch(value))
          value + dest - source
        else value
      }
      def isMatch(value: Long) = (value >= source && value < source + range)
    }

    val path = Path(getClass.getResource("/Day5/input.txt").getPath)
    val file = Files[IO].readUtf8Lines(path)
    val seeds = file.head.compile.toList.unsafeRunSync().head
      .split(":").apply(1)
      .split("\\s")
      .filter(_.nonEmpty)
      .map(_.toLong)
      .toList
    val mappings = file.tail
      .filter(_.nonEmpty)
      .split(".*map:".r.findFirstMatchIn(_).nonEmpty)
      .map { chunk =>
        chunk.map { s =>
          val params = s.split("\\s").filter(_.nonEmpty)
          Mapping(params(0).toLong, params(1).toLong, params(2).toLong)
        }
      }

    mappings
      .fold(seeds) { case (seeds, chunk) =>
        seeds.map { seed =>
          chunk.find(_.isMatch(seed)).map(_.map(seed)).getOrElse(seed)
        }
      }
      .map(_.min)
      .evalTap(IO.println)
      .compile
      .drain
      .unsafeRunSync()
  }

  test("part-2") {
    import spire.implicits._

    case class Mapping(dest: Long, source: Interval[Long]) {
      def map(value: Interval[Long]): (Interval[Long], List[Interval[Long]]) = {
        val mapped = source.intersect(value) + dest - source.bottom(1).getOrElse(0L)
        (mapped, value -- source)
      }
    }

    val path = Path(this.getClass.getResource("/Day5/input.txt").getPath)
    val file = Files[IO].readUtf8Lines(path)
    val seeds = file.head.compile.toList.unsafeRunSync().head
      .split(":").apply(1)
      .split("\\s")
      .filter(_.nonEmpty)
      .map(_.toLong)
      .toList
      .grouped(2)
      .map{
        case start :: range :: Nil => Interval.openUpper(start, start + range)
        case _ => Interval.empty[Long]
      }
      .toList

    val mappings = file.tail
      .filter(_.nonEmpty)
      .split(".*map:".r.findFirstMatchIn(_).nonEmpty)
      .map { chunk =>
        chunk.map { s =>
          val params = s.split("\\s").filter(_.nonEmpty)
          val dest = params(0).toLong
          val sourceStart = params(1).toLong
          val sourceRange = params(2).toLong
          Mapping(dest, Interval.openUpper(sourceStart, sourceStart + sourceRange))
        }
      }

    println(s"seeds ${seeds.mkString(" ")}")
    mappings
      .fold(seeds){ case (intervals, chunk) =>
        // for each mapping in the chunk map all seeds that have not yet been mapped
        val newIntervals = chunk
          // nothing has been mapped yet, original seeds need to be mapped
          .foldLeft((List.empty[Interval[Long]], intervals)){
            case ((mapped, unmapped), mapping) =>
              val foo: List[(Interval[Long], List[Interval[Long]])] = unmapped.map(mapping.map)
              val newMapped = foo.map(_._1).filter(_.nonEmpty) ++ mapped
              val newUnmapped = foo.flatMap(_._2).filter(_.nonEmpty)

//              println(s"mapping: ${mapping}")
//              println(s"mapped: ${mapped}")
//              println(s"unmapped: ${unmapped}")
//              println(s"newMapped: ${newMapped}")
//              println(s"newUnmapped: ${newUnmapped}")
//              println()
              (newMapped, newUnmapped)
          }
        newIntervals._1 ++ newIntervals._2
      }
      .map(intervals => intervals
        .map(_.bottom(1).getOrElse(Long.MaxValue))
        .fold(Long.MaxValue)(math.min)
      )
      .evalTap(IO.println)
      .compile
      .drain
      .unsafeRunSync()
  }
}
