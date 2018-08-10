package info.daviot.fpmax

object StdLib {
  case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B]         = IO(() => f(self.unsafeRun()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun()).unsafeRun())
  }
  object IO {
    def point[A](a: => A) = IO(() => a)
  }

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

  trait Consumer[F[_]] {
    def consume(s: String): F[Unit]
  }

  trait Provider[F[_]] {
    def provide: F[String]
  }

  trait Console[F[_]] extends Consumer[F] with Provider[F]

  object Console {
    def apply[F[_]](implicit c: Console[F]): Console[F] = c
  }

  object Consumer {
    def apply[F[_]](implicit c: Consumer[F]): Consumer[F] = c
  }
  object Provider {
    def apply[F[_]](implicit c: Provider[F]): Provider[F] = c
  }

  def printString[F[_]: Consumer](s: String): F[Unit] = Consumer[F].consume(s)
  def readString[F[_]: Provider]: F[String]           = Provider[F].provide

  trait Random[F[_]] {
    def nextInt(max: Int): F[Int]
  }
  object Random {
    def apply[F[_]](implicit r: Random[F]): Random[F] = r
  }

  def nextInt[F[_]: Random](max: Int): F[Int] = Random[F].nextInt(max)

}
