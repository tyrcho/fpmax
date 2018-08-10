package info.daviot.fpmax
import scala.util.Try

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

    readLine() match {
      case "y" => exec = true
      case "n" => exec = false
    }
  }
  private def inputNumber(): Option[Int] =
    Try {
      readLine().toInt
    }.toOption
}
