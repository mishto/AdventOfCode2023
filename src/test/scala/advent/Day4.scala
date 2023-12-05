package advent

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import fs2.io.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.net.URL


class Day4 extends AnyFunSuite with Matchers {
  test("day4 - 1") {
    val resource: URL = getClass.getResource("/day4/input.txt")
    Files[IO].readUtf8Lines(Path(resource.getPath))
      .map(parseWinningAndChosenNumbers)
      // count winning numbers per card
      .map { case (w, ns) => ns.count(w.contains) }
      .filter(_ > 0)
      // calculate score per card
      .map(n => math.pow(2, n - 1.0).toInt)
      .foldMonoid
      .evalTap(IO.println)
      .compile
      .drain
      .unsafeRunSync()
  }

  test("day4 - 2") {
    val resource: URL = getClass.getResource("/day4/input.txt")
    Files[IO].readUtf8Lines(Path(resource.getPath))
      .map(parseWinningAndChosenNumbers)
      // count winning numbers per card
      .map { case (w, ns) => ns.count(w.contains) }
      // each card is considered once to start with; sum of all cards is 0
      .scan((Stream.constant(1), 0)) { case ((copies, sum), cardScore) =>
        val cardCopies = copies.head.compile.toList.head
        // for each card copy we win 1 card of each of the following `cardScore` cards.
        val wonCards = Stream.constant(cardCopies).take(cardScore.toLong) ++ Stream.constant(0)
        // add the newly won cards to the existing copies
        val newCopies = copies.tail.zipWith(wonCards)(_ + _)
        // keep track of the total number of cards scanned
        val newSum = sum + cardCopies
        (newCopies, newSum)
      }
      .fold(0) { case (_, (_, n)) => n }
      .evalTap(IO.println)
      .compile
      .drain
      .unsafeRunSync()
  }

  def parseWinningAndChosenNumbers(s: String): (List[String], List[String]) = {
    val r = """Card *\d*:(.*)\|(.*)$""".r
    val m = r.findFirstMatchIn(s)
      .get
    (parseNumbers(m.group(1)), parseNumbers(m.group(2)))
  }

  def parseNumbers(s: String): List[String] = {
    s.split(' ').filter(_.nonEmpty).toList
  }
}
