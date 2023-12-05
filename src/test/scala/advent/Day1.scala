package advent

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.net.URL


class Day1 extends AnyFunSuite with Matchers {
  val wordDigits = Map( "zero" -> 0, "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9)
  val numericDigits = Map( "0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9)
  val allDigits = wordDigits ++ numericDigits

  test("part-1") {
    val resource: URL = getClass.getResource("/day1/input.txt")
    Files[IO].readUtf8Lines(Path(resource.getPath))
      .map(line =>
        for {
          first <- firstDigit(line, numericDigits)
          last <- lastDigit(line, numericDigits)
        } yield first * 10 + last
      )
      .collect { case Some(l) => l }
      .foldMonoid
      .evalTap(IO.println)
      .compile
      .drain
      .unsafeRunSync()
  }

  test("part-2") {
    val resource: URL = getClass.getResource("/day1/input.txt")
    Files[IO].readUtf8Lines(Path(resource.getPath))
      .map(line =>
        for {
          first <- firstDigit(line, allDigits)
          last <- lastDigit(line, allDigits)
        } yield first * 10 + last
      )
      .collect { case Some(l) => l }
      .foldMonoid
      .evalTap(IO.println)
      .compile
      .drain
      .unsafeRunSync()
  }

  def firstDigit(s: String, d: Map[String, Int]) = {
    val digitKey = d.keys
      .map(key => (s.indexOf(key), key))
      .filter(_._1 > -1)
      .min
      ._2
    d.get(digitKey)
  }

  def lastDigit(s: String, d: Map[String, Int]) = {
    val digitKey = d.keys
      .map(key => (s.lastIndexOf(key), key))
      .filter(_._1 > -1)
      .max
      ._2
    d.get(digitKey)
  }
}
