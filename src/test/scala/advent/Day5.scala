package advent

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite

class Day5 extends AnyFunSuite {
  case class Mapping(dest: Long, source: Long, range: Long) {
    def map(value: Long) = {
      if (isMatch(value))
        value + dest - source
      else value
    }
    def isMatch(value: Long) = (value >= source && value < source + range)

  }

  test("part-1") {
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

}
