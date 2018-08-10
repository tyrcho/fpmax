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

  trait Console[F[_]] {
    def printString(s: String): F[Unit]
    def readString: F[String]
  }
  object Console {
    def apply[F[_]](implicit c: Console[F]): Console[F] = c
  }

  def printString[F[_]: Console](s: String): F[Unit] = Console[F].printString(s)
  def readString[F[_]: Console]: F[String]           = Console[F].readString

  trait Random[F[_]] {
    def nextInt(max: Int): F[Int]
  }
  object Random {
    def apply[F[_]](implicit r: Random[F]): Random[F] = r
  }

  def nextInt[F[_]: Random](max: Int): F[Int] = Random[F].nextInt(max)

}
