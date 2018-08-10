package info.daviot.fpmax
import info.daviot.fpmax.stdlib.IO

import scala.util.{Random, Try}

object FpMax extends App {

  val main: IO[Unit] = for {
    _    <- printString("What is your name?")
    name <- readString
    _    <- printString("Hello, " + name + ", welcome to the game!")
    _    <- gameLoop(name)
  } yield ()

  main.unsafeRun()

  private def gameLoop(name: String): IO[Unit] =
    for {
      num   <- nextInt(max = 5).map(_ + 1)
      _     <- printString("Dear " + name + ", please guess a number from 1 to 5:")
      entry <- inputNumber
      _ <- entry match {
        case None        => printString("You failed to enter a number, " + name)
        case Some(`num`) => printString("You guessed right, " + name + "!")
        case _           => printString("You guessed wrong, " + name + "! The number was: " + num)
      }
      _              <- printString("Do you want to continue, " + name + "?")
      wantToContinue <- inputBoolean
      _              <- if (wantToContinue) gameLoop(name) else IO.point(())
    } yield ()

  def printString(s: String): IO[Unit] = IO(() => println(s))
  def readString: IO[String]           = IO(() => readLine())
  def nextInt(max: Int): IO[Int]       = IO(() => Random.nextInt(max))

  private def inputBoolean: IO[Boolean] =
    for {
      _ <- printString("Please enter y or n")
      maybeBool <- inputBooleanOpt
      bool <- maybeBool.fold(inputBoolean)(IO.point(_))
    } yield bool

  private def inputBooleanOpt: IO[Option[Boolean]] =
    readString.map(parseBoolean)

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match {
    case "y" => Some(true)
    case "n" => Some(false)
    case _   => None
  }

  private def inputNumber: IO[Option[Int]] =
    readString.map(s => Try(s.toInt).toOption)
}

object stdlib {
  case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B]         = IO(() => f(self.unsafeRun()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun()).unsafeRun())
  }
  object IO {
    def point[A](a: => A) = IO(() => a)
  }
}
