package advent

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import fs2.io.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.net.URL
import scala.util.matching.Regex


class Day3 extends AnyFunSuite with Matchers {
  val symbolsRegex: Regex = """[^\d^.]""".r
  val starRegex: Regex = """\*""".r

  case class Number(value: Int, len: Int, start: Int)

  case class Symbol(start: Int)

  test("part-1") {
    val resource: URL = getClass.getResource("/day3/input.txt")
    symbolsAndNumbersByLine(
      symbolsRegex,
      Files[IO].readUtf8Lines(Path(resource.getPath))
    )
      .map { case (symbolsMap, numbersMap) =>
        numbersMap.flatMap { case (lineNumber, ns) =>
          // filter out the numbers on this line that are not adjacent to symbols
          ns.filter(e => isAdjacentToSymbol(symbolsMap, lineNumber, e))
        }
          .map(_.value)
          .sum
      }
      .evalTap(IO.println)
      .compile
      .drain
      .unsafeRunSync()
  }

  test("part-2") {
    val resource: URL = getClass.getResource("/day3/input.txt")
    symbolsAndNumbersByLine(
      starRegex,
      Files[IO].readUtf8Lines(Path(resource.getPath))
    )
      .map { case (starsMap, numbersMap) =>
        starsMap.flatMap { case (lineNumber, symbols) =>
            // number of adjacent Numbers to this symbol
            symbols.map(symbol => getAdjacentNumbers(lineNumber, symbol, numbersMap))
        }
          .filter(_.size == 2)
          .map(_.foldLeft(1)(_ * _.value))
          .sum
      }
      .evalTap(IO.println)
      .compile
      .drain
      .unsafeRunSync()
  }

  def symbolsAndNumbersByLine(r: Regex, s: Stream[IO, String]): Stream[IO, (Map[Int, List[Symbol]], Map[Int, List[Number]])] = {
    s.map(getSymbolsAndNumbers(r, _))
      .zip(Stream.range(1, Int.MaxValue))
      .map { case ((symbols, numbers), lineNum) =>
        (Map(lineNum -> symbols), Map(lineNum -> numbers))
      }
      .foldMonoid
  }

  def isAdjacentToSymbol(symbol: Map[Int, List[Symbol]], lineNumber: Int, n: Number): Boolean = {
    val symbolIndices = symbol
      .filter { case (symbolsLineNumber, _) => math.abs(symbolsLineNumber - lineNumber) <= 1 }
      .values
      .flatten

    symbolIndices.exists(symbol => symbol.start >= n.start - 1 && symbol.start <= n.start + n.len)
  }

  private def getAdjacentNumbers(lineNumber: Int, symbol: Symbol, numbersMap: Map[Int, List[Number]]) = {
    numbersMap
      // numbers from adjacent rows
      .filter(numberPair => math.abs(numberPair._1 - lineNumber) <= 1)
      .values
      .flatten
      .filter(n => symbol.start >= n.start - 1 && symbol.start <= n.start + n.len)
  }

  private def getSymbolsAndNumbers(symbolRegex: Regex, s: String) = {
    val numbersRegex = """\d+""".r
    (
      symbolRegex.findAllMatchIn(s).map(m => Symbol(m.start)).toList,
      numbersRegex.findAllMatchIn(s)
        .map(m => Number(m.toString().toInt, m.toString().length, m.start))
        .toList
    )
  }
}
