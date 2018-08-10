package info.daviot.fpmax

import scala.util.Try

import info.daviot.fpmax.StdLib._
object FpMax extends App {
  implicit val randomIO: Random[IO] = max => IO(() => util.Random.nextInt(max))

  implicit val consoleIO: Console[IO] = new Console[IO] {
    override def printString(s: String): IO[Unit] = IO(() => println(s))
    override def readString: IO[String]          = IO(() => readLine())
  }

  implicit val programIO: Program[IO] = new Program[IO] {
    override def finish[A](a: => A): IO[A]                    = IO.point(a)
    override def chain[A, B](fa: IO[A], f: A => IO[B]): IO[B] = fa.flatMap(f)
    override def map[A, B](fa: IO[A], f: A => B): IO[B]       = fa.map(f)
  }

  MyApp.main.unsafeRun()
}

object MyApp {

  def main[F[_]: Program: Random: Console]: F[Unit] =
    for {
      _    <- printString("What is your name?")
      name <- readString
      _    <- printString("Hello, " + name + ", welcome to the game!")
      _    <- gameLoop(name)
    } yield ()

  private def gameLoop[F[_]: Program: Random: Console](name: String): F[Unit] =
    for {
      randomNumber <- nextInt(max = 5)
      hiddenNumber = randomNumber + 1
      _              <- printString("Dear " + name + ", please guess a number from 1 to 5:")
      entry          <- inputNumber
      _              <- handleAnswer(name, hiddenNumber, entry)
      _              <- printString("Do you want to continue, " + name + "?")
      wantToContinue <- inputBoolean
      _              <- if (wantToContinue) gameLoop[F](name) else point(())
    } yield ()

  private def handleAnswer[F[_]: Program: Console](name: String, hidden: Int, entry: Option[Int]): F[Unit] = {
    entry match {
      case None           => printString("You failed to enter a number, " + name)
      case Some(`hidden`) => printString("You guessed right, " + name + "!")
      case _              => printString("You guessed wrong, " + name + "! The number was: " + hidden)
    }
  }

  private def inputBoolean[F[_]: Program: Console]: F[Boolean] =
    for {
      _         <- printString("Please enter y or n")
      maybeBool <- inputBooleanOpt
      bool      <- maybeBool.fold(inputBoolean[F])(point(_))
    } yield bool

  private def inputBooleanOpt[F[_]: Program: Console]: F[Option[Boolean]] =
    readString.map(parseBoolean)

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match {
    case "y" => Some(true)
    case "n" => Some(false)
    case _   => None
  }

  private def inputNumber[F[_]: Program: Console]: F[Option[Int]] =
    readString.map(s => Try(s.toInt).toOption)
}
