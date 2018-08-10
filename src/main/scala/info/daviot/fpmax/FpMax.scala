package info.daviot.fpmax
import info.daviot.fpmax.stdlib.IO

import scala.util.{Random, Try}

object FpMax extends App {
  println("What is your name?")

  val name = readLine()

  println("Hello, " + name + ", welcome to the game!")

  var exec = true

  while (exec) {
    val num = scala.util.Random.nextInt(5) + 1

    println("Dear " + name + ", please guess a number from 1 to 5:")

    inputNumber() match {
      case None        => println("You failed to enter a number, " + name)
      case Some(`num`) => println("You guessed right, " + name + "!")
      case _           => println("You guessed wrong, " + name + "! The number was: " + num)
    }

    println("Do you want to continue, " + name + "?")

    exec = inputBoolean
  }

  def printString(s: String): IO[Unit] = IO(() => println(s))
  def readString: IO[String]           = IO(() => readLine())
  def nextInt(max: Int): IO[Int]       = IO(() => Random.nextInt(max))

  def inputBoolean: Boolean = {
    var bool = inputBooleanOpt()
    while (bool.isEmpty) {
      println("Please enter y or n")
      bool = inputBooleanOpt()
    }
    bool.get
  }

  private def inputBooleanOpt(): Option[Boolean] = {
    readLine().toLowerCase match {
      case "y" => Some(true)
      case "n" => Some(false)
      case _   => None
    }
  }
  private def inputNumber(): Option[Int] =
    Try {
      readLine().toInt
    }.toOption
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
