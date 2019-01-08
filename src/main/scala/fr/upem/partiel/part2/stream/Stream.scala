package fr.upem.partiel.part2.stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.{Done, NotUsed}
import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie.Director

import scala.concurrent.Future

object Stream {

  implicit lazy val system = ActorSystem("system")
  implicit lazy val materializer = ActorMaterializer()

  type SimpleSource[A] = Source[A, NotUsed]
  type SimpleFlow[A, B] = Flow[A, B, NotUsed]
  type SimpleSink[A] = Sink[A, Future[Done]]

  // TODO Create a source of Movies
  def sourceOf[A](movies: List[A]): SimpleSource[A] = ???

  // TODO Create a flow to serialize movies, using a Show typeclass instance
  def serialize[A]: SimpleFlow[A, String] = ???

  // TODO Create a flow to transform movies, using a Functor typeclass instance
  def transform[F[_], A, B](f: A => B): SimpleFlow[F[A], F[B]] = ???

  // TODO Implements the simple transformation function
  val movieToDirector: Movie => Director = ???

  // TODO Create a flow to filter movies, with an Eq typeclass instance
  def getOnly[A](a: A): SimpleFlow[A, Option[A]] = ???

  def sink[A]: SimpleSink[A] = Sink.foreach(println)

}
