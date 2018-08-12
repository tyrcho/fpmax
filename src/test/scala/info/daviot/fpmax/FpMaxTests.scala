package info.daviot.fpmax

import info.daviot.fpmax.StdLib.{Consumer, Program, Provider, Random}
import org.scalatest._

class FpMaxTests extends FlatSpec with Matchers {
  implicit val randomTestIO: Random[TestIO] = _ => TestIO(_.takeNumber)

  implicit val consumerTestIO: Consumer[TestIO] = s => TestIO(t => (t.appendOutput(s), ()))

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
    val testData = TestData(inputs = List("Michel", "1", "N"), numbers = List(1))
    val result   = mainTestIO.eval(testData)
    result shouldBe TestData(
      Nil,
      Nil,
      List(
        "Please enter y or n",
        "Do you want to continue, Michel?",
        "You guessed wrong, Michel! The number was: 2",
        "Dear Michel, please guess a number from 1 to 5:",
        "Hello, Michel, welcome to the game!",
        "What is your name?"
      )
    )

  }

}

case class TestData(inputs: List[String], numbers: List[Int], outputs: List[String] = Nil) {
  def appendOutput(s: String): TestData = copy(outputs = s :: outputs)

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
