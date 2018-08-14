package info.daviot.fpmax

import cats.Monad
import cats.effect.IO
import info.daviot.fpmax.OutMessage._
import info.daviot.fpmax.StdLib._

import scala.util.Try
object FpMax extends App {
  implicit val randomIO: Random[IO] = max => IO(util.Random.nextInt(max))

  implicit val consumeIO: MessageConsumer[IO] = s => IO(() => println(s.en))

  implicit val provideIO: Provider[IO] = new Provider[IO] {
    override def provide: IO[String] = IO(readLine())
  }

  implicit val program: Monad[IO] = new Monad[IO] {
    override def pure[A](a: A): IO[A]                                  = IO.pure(a)
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B]        = fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???
  }

  MyApp.main.unsafeRunSync()
}

object MyApp {

  def main[F[_]: Monad: Random: Provider: MessageConsumer]: F[Unit] =
    for {
      _    <- printMessage(WhatIsYourName)
      name <- readString
      _    <- printMessage(WelcomeToGame(name))
      _    <- gameLoop(name)
    } yield ()

  private def gameLoop[F[_]: Monad: Random: Provider: MessageConsumer](name: String): F[Unit] =
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

  private def inputBoolean[F[_]: Monad: Provider: MessageConsumer](name: String, failed: Boolean = false): F[Boolean] =
    for {
      _         <- if (failed) printMessage(ThatIsNotValid(name)) else point()
      maybeBool <- inputBooleanOpt
      bool      <- maybeBool.fold(inputBoolean[F](name, failed = true))(point(_))
    } yield bool

  private def inputBooleanOpt[F[_]: Monad: Provider]: F[Option[Boolean]] =
    readString.map(parseBoolean)

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match {
    case "y" => Some(true)
    case "n" => Some(false)
    case _   => None
  }

  private def inputNumber[F[_]: Monad: Provider]: F[Option[Int]] =
    readString.map(s => Try(s.toInt).toOption)
}
