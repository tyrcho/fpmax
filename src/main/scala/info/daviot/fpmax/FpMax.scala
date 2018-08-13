package info.daviot.fpmax

import info.daviot.fpmax.OutMessage._

import scala.util.Try
import info.daviot.fpmax.StdLib._
object FpMax extends App {
  implicit val randomIO: Random[IO] = max => IO(() => util.Random.nextInt(max))

  implicit val consumeIO: MessageConsumer[IO] = s => IO(() => println(s.en))

  implicit val provideIO: Provider[IO] = new Provider[IO] {
    override def provide: IO[String] = IO(() => readLine())
  }

  implicit val programIO: Program[IO] = new Program[IO] {
    override def finish[A](a: => A): IO[A]                    = IO.point(a)
    override def chain[A, B](fa: IO[A], f: A => IO[B]): IO[B] = fa.flatMap(f)
    override def map[A, B](fa: IO[A], f: A => B): IO[B]       = fa.map(f)
  }

  MyApp.main.unsafeRun()
}

object MyApp {

  def main[F[_]: Program: Random: Provider: MessageConsumer]: F[Unit] =
    for {
      _    <- printMessage(WhatIsYourName)
      name <- readString
      _    <- printMessage(WelcomeToGame(name))
      _    <- gameLoop(name)
    } yield ()

  private def gameLoop[F[_]: Program: Random: Provider: MessageConsumer](name: String): F[Unit] =
    for {
      randomNumber <- nextInt(max = 5)
      hiddenNumber = randomNumber + 1
      _              <- printMessage(PleaseGuess(name))
      entry          <- inputNumber
      _              <- handleAnswer(name, hiddenNumber, entry)
      _              <- printMessage(DoYouWantToContinue(name))
      wantToContinue <- inputBoolean(name)
      _              <- if (wantToContinue) gameLoop[F](name) else point(())
    } yield ()

  private def handleAnswer[F[_]: MessageConsumer](name: String, hidden: Int, entry: Option[Int]): F[Unit] = {
    entry match {
      case None           => printMessage(ThatIsNotValid(name))
      case Some(`hidden`) => printMessage(YouGuessedRight(name))
      case _              => printMessage(YouGuessedWrong(name, hidden))
    }
  }

  private def inputBoolean[F[_]: Program: Provider: MessageConsumer](name: String): F[Boolean] =
    for {
      maybeBool <- inputBooleanOpt
      bool      <- maybeBool.fold(inputBoolean[F](name))(point(_))
    } yield bool

  private def inputBooleanOpt[F[_]: Program: Provider]: F[Option[Boolean]] =
    readString.map(parseBoolean)

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match {
    case "y" => Some(true)
    case "n" => Some(false)
    case _   => None
  }

  private def inputNumber[F[_]: Program: Provider]: F[Option[Int]] =
    readString.map(s => Try(s.toInt).toOption)
}
