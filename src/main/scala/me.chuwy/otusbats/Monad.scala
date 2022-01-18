package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(a => a)

  /** Комбинатор в трейте */
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = flatMap(fa)(a => map(fb)((a, _)))
}

object Monad {

  /** Суммонер */
  def apply[F[_]](implicit ev: Monad[F]): Monad[F] = ev

  /** Инстансы */
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def point[A](a: A): Option[A] = Option(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def point[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  type E[A] = Either[_, A]
  implicit val eitherMonad: Monad[E] = new Monad[E] {
    override def flatMap[A, B](fa: E[A])(f: A => E[B]): E[B] = fa match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }

    override def point[A](a: A): E[A] = Right(a)

    override def map[A, B](fa: E[A])(f: A => B): E[B] = fa match {
      case Left(value) => Left(value)
      case Right(value) => Right(f(value))
    }
  }

  /** Синтаксис */
  implicit class MonadOps[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit ev: Monad[F]): F[B] = ev.map(fa)(f)
    def flatMap[B](f: A => F[B])(implicit ev: Monad[F]): F[B] = ev.flatMap(fa)(f)
    def point[A](a: A)(implicit ev: Monad[F]): F[A] = ev.point(a)
    def zip[B](fb: F[B])(implicit ev: Monad[F]): F[(A, B)] = ev.zip(fa, fb)
  }

  /** Комбинатор в объекте */
  def nested[F[_], G[_], A, B](f: A => B)(fga: F[G[A]])(implicit monadF: Monad[F], monadG: Monad[G]): F[G[B]] =
    fga.map(ga => ga.map(f))

}
