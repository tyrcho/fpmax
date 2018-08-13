package info.daviot.fpmax

import info.daviot.fpmax.OutMessage._
import info.daviot.fpmax.StdLib.{Program, Provider, Random, StringConsumer}
import org.scalatest._

class FpMaxTests extends FlatSpec with Matchers {
  implicit val randomTestIO: Random[TestIO] = _ => TestIO(_.takeNumber)

  implicit val consumerTestIO: MessageConsumer[TestIO] = s => TestIO(t => (t.appendOutput(s), ()))

  implicit val providerTestIO: Provider[TestIO] = new Provider[TestIO] {
    override def provide: TestIO[String] = TestIO(_.takeInput)
  }

  implicit val programTestIO: Program[TestIO] = new Program[TestIO] {
    override def finish[A](a: => A): TestIO[A]                            = TestIO.point(a)
    override def chain[A, B](fa: TestIO[A], f: A => TestIO[B]): TestIO[B] = fa.flatMap(f)
    override def map[A, B](fa: TestIO[A], f: A => B): TestIO[B]           = fa.map(f)
  }

  val mainTestIO: TestIO[Unit] = MyApp.main[TestIO]

  it should "answer when guessed wrong" in {
    val name     = "Michel"
    val testData = TestData(inputs = List(name, "1", "N"), numbers = List(1))
    val result   = mainTestIO.eval(testData)
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
    val name     = "Michel"
    val testData = TestData(inputs = List(name, "1", "a", "N"), numbers = List(1))
    val result   = mainTestIO.eval(testData)
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

}

case class TestData(inputs: List[String], numbers: List[Int], outputs: List[OutMessage] = Nil) {
  def appendOutput(s: OutMessage): TestData = copy(outputs = s :: outputs)

  def takeInput: (TestData, String) = (copy(inputs = inputs.tail), inputs.head)

  def takeNumber: (TestData, Int) = (copy(numbers = numbers.tail), numbers.head)
}

//State Monad with state parameter fixed as TestData
case class TestIO[A](run: TestData => (TestData, A)) { self =>
  def map[B](f: A => B): TestIO[B] =
    TestIO(data => self.run(data) match { case (t, a) => (t, f(a)) })

  def flatMap[B](f: A => TestIO[B]): TestIO[B] =
    TestIO(data => self.run(data) match { case (t, a) => f(a).run(t) })

  def eval(t: TestData): TestData = run(t)._1
}

object TestIO {
  def point[A](a: => A): TestIO[A] = TestIO(t => (t, a))
}
