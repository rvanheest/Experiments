package experiments.dependencyInjection.deadSimple

import experiments.dependencyInjection.deadSimple.DependecyInjectionFramework.Reader
import experiments.dependencyInjection.deadSimple.DependecyInjectionFramework.Reader._
import experiments.monadics.Functor

import scala.annotation.tailrec

object InitialSetup {

	trait KeyValueStore {
		def put(key: String, value: String): Unit
		def get(key: String): String
		def delete(key: String): Unit
	}

	def modify(key: String, f: String => String): Reader[KeyValueStore, Unit] = {
		for {
			v <- reader[KeyValueStore, String](_.get(key))
			_ <- reader[KeyValueStore, Unit](_.put(key, f(v)))
		} yield ()
	}
}

object LittleLanguage extends App {
	// we define a little language that expresses actions to be taken on a KeyValueStore
	sealed trait KVS[A]
	case class Put[A](key: String, value: String, a: A) extends KVS[A]
	case class Get[A](key: String, f: String => A) extends KVS[A]
	case class Delete[A](key: String, a: A) extends KVS[A]

	// modify will look like this now
	// problem: return type is KVS[KVS[Unit]]
	def modify(k: String, f: String => String): KVS[KVS[Unit]] = {
		Get(k, v => Put(k, f(v), ()))
	}

	// we want to transform KVS[KVS[A]] to KVS[A]
	// solution: Free monads

	case class Done[F[_]: Functor, A](a: A) extends Free[F, A]
	case class More[F[_]: Functor, A](k: F[Free[F, A]]) extends Free[F, A]
	class Free[F[_], A](implicit F: Functor[F]) {
		def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
			case Done(a) => f(a)
			case More(k) => More[F, B](F.map(k)(_ flatMap f))
		}
		def map[B](f: A => B): Free[F, B] = flatMap(x => Done(f(x)))
	}

	implicit val kvsIsFunctor: Functor[KVS] = new Functor[KVS] {
		def map[A, B](kvs: KVS[A])(f: A => B): KVS[B] = kvs match {
			case Put(k, v, a) => Put(k, v, f(a))
			case Get(k, h) => Get(k, f compose h)
			case Delete(k, a) => Delete(k, f(a))
		}
	}

	def put(k: String, v: String): Free[KVS, Unit] = More(Put(k, v, Done(())))
	def get(k: String): Free[KVS, String] = More(Get(k, Done(_)))
	def delete(k: String): Free[KVS, Unit] = More(Delete(k, Done(())))

	def freeModify(k: String, f: String => String): Free[KVS, Unit] = {
		for {
			v <- get(k)
			_ <- put(k, f(v))
		} yield ()
	}

	// then pass this to an interpreter
	@tailrec
	def runKVS[A](kvs: Free[KVS, A], table: Map[String, String]): Map[String, String] = {
		kvs match {
			case More(Put(k, v, a)) => runKVS(a, table + (k -> v))
			case More(Get(k, f)) => runKVS(f(table(k)), table)
			case More(Delete(k, a)) => runKVS(a, table - k)
			case Done(a) => table
		}
	}

	println(runKVS(freeModify("abc", s => s + "123"), Map("abc" -> "xyz")))
}




























