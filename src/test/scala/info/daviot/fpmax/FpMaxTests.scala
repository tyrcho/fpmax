package info.daviot.fpmax

import cats.Monad
import cats.data.State
import info.daviot.fpmax.OutMessage._
import info.daviot.fpmax.StdLib.{Provider, Random}
import org.scalatest._

class FpMaxTests extends FlatSpec with Matchers {
  implicit val randomTestIO: Random[TestState] = _ => State(_.takeNumber)

  implicit val consumerTestIO: MessageConsumer[TestState] = s => State(t => (t.appendOutput(s), ()))

  implicit val providerTestIO: Provider[TestState] = new Provider[TestState] {
    override def provide: TestState[String] = State(_.takeInput)
  }

  implicit val program: Monad[TestState] = new Monad[TestState] {
    override def pure[A](a: A): TestState[A]                                         = State.pure(a)
    override def flatMap[A, B](fa: TestState[A])(f: A => TestState[B]): TestState[B] = fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => TestState[Either[A, B]]): TestState[B] = ???
  }

  val mainTestIO: TestState[Unit] = MyApp.main[TestState]

  it should "answer when guessed wrong" in {
    val name        = "Michel"
    val testData    = TestData(inputs = List(name, "1", "N"), numbers = List(1))
    val (result, _) = mainTestIO.run(testData).value
    result match {
      case TestData(Nil, Nil, messages) =>
        messages.reverse should contain theSameElementsInOrderAs
          Seq(
            WhatIsYourName,
            WelcomeToGame(name),
            PleaseGuess(name),
            YouGuessedWrong(name, 2),
            DoYouWantToContinue(name)
          )
    }
  }

  it should "retry when player does not enter y/n" in {
    val name        = "Michel"
    val testData    = TestData(inputs = List(name, "1", "a", "N"), numbers = List(1))
    val (result, _) = mainTestIO.run(testData).value
    result match {
      case TestData(Nil, Nil, messages) =>
        messages.reverse should contain theSameElementsInOrderAs
          Seq(
            WhatIsYourName,
            WelcomeToGame(name),
            PleaseGuess(name),
            YouGuessedWrong(name, 2),
            DoYouWantToContinue(name),
            ThatIsNotValid(name)
          )
    }
  }

  type TestState[A] = State[TestData, A]
}

case class TestData(inputs: List[String], numbers: List[Int], outputs: List[OutMessage] = Nil) {
  def appendOutput(s: OutMessage): TestData = copy(outputs = s :: outputs)

  def takeInput: (TestData, String) = (copy(inputs = inputs.tail), inputs.head)

  def takeNumber: (TestData, Int) = (copy(numbers = numbers.tail), numbers.head)
}
