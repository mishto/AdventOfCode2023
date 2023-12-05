package advent

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.net.URL


class Day2 extends AnyFunSuite with Matchers {
  case class Game(id: Int, draws: List[(String, Int)])

  test("part-1") {
    val resource: URL = getClass.getResource("/day2/input.txt")
    Files[IO].readUtf8Lines(Path(resource.getPath))
      .map(parseGame)
      .map(game => (game.id, minByColor(game)))
      .filter { case (_, m) =>
        m.getOrElse("red", 0) <= 12 &&
          m.getOrElse("green", 0) <= 13 &&
          m.getOrElse("blue", 0) <= 14
      }
      .map(_._1)
      .foldMonoid
      .evalTap(a => IO.println(a))
      .compile
      .drain
      .unsafeRunSync()
  }

  test("part-2") {
    val resource: URL = getClass.getResource("/day2/input.txt")
    Files[IO].readUtf8Lines(Path(resource.getPath))
      .map(parseGame)
      .map(game => minByColor(game))
      .map(m => m.getOrElse("red", 0) * m.getOrElse("green", 0) * m.getOrElse("blue", 0))
      .foldMonoid
      .evalTap(a => IO.println(a))
      .compile
      .drain
      .unsafeRunSync()
  }

  private def parseGame(str: String) = {
    val gameRx = """^Game (\d+)""".r
    val setRx = """(\d+) (red|blue|green)""".r
    val gameNumber = gameRx.findFirstMatchIn(str).get.group(1).toInt
    val draws = setRx.findAllMatchIn(str).map(rx => rx.group(2) -> rx.group(1).toInt)
    Game(gameNumber, draws.toList)
  }

  private def minByColor(game: Game): Map[String, Int] = {
    game.draws
      .foldLeft(Map[String, Int]()) { case (m, (color, number)) =>
        m + (color -> math.max(m.getOrElse(color, 0), number))
      }
  }

}
