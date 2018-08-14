package info.daviot.fpmax

object StdLib {


  //a Monad
  trait Program[F[_]] {
    def finish[A](a: => A): F[A]
    def chain[A, B](fa: F[A], f: A => F[B]): F[B]
    def map[A, B](fa: F[A], f: A => B): F[B]
  }
  object Program {
    def apply[F[_]](implicit p: Program[F]): Program[F] = p
  }
  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit p: Program[F]): F[B]        = p.map(fa, f)
    def flatMap[B](f: A => F[B])(implicit p: Program[F]): F[B] = p.chain(fa, f)
  }
  def point[F[_], A](a: => A)(implicit p: Program[F]): F[A] = p.finish(a)

  trait Consumer[F[_], T] {
    def consume(s: T): F[Unit]
  }

  object Consumer {
    def apply[F[_], T](implicit c: Consumer[F, T]): Consumer[F, T] = c
  }
  def print[T, F[_]](s: T)(implicit c: Consumer[F, T]): F[Unit] = {
    Consumer[F, T].consume(s)
  }

  type StringConsumer[F[_]] = Consumer[F, String]

  object StringConsumer {
    def apply[F[_]](implicit c: StringConsumer[F]): StringConsumer[F] = c
  }
  def printString[F[_]: StringConsumer](s: String): F[Unit] = StringConsumer[F].consume(s)

  trait Provider[F[_]] {
    def provide: F[String]
  }

  object Provider {
    def apply[F[_]](implicit c: Provider[F]): Provider[F] = c
  }

  def readString[F[_]: Provider]: F[String] = Provider[F].provide

  trait Random[F[_]] {
    def nextInt(max: Int): F[Int]
  }
  object Random {
    def apply[F[_]](implicit r: Random[F]): Random[F] = r
  }

  def nextInt[F[_]: Random](max: Int): F[Int] = Random[F].nextInt(max)

}
