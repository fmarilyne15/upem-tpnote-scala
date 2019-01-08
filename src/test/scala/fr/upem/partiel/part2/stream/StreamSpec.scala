package fr.upem.partiel.part2.stream

import akka.stream.testkit.scaladsl.TestSink
import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie.Country.{England, France, Italy, UnitedStates}
import fr.upem.partiel.part2.model.Movie._
import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  "Stream" should "run simple movie serialization stream" in {

    import Stream.{materializer, system}

    val source = Stream.sourceOf(
      List(
        movie(title("Alien"), director("Ridley", "Scott"), year(1979), views(1), England),
        movie(title("Nikita"), director("Luc", "Besson"), year(1990), views(2), France),
        movie(title("Taxi Driver"), director("Martin", "Scorsese"), year(1976), views(3), UnitedStates),
        movie(title("The Dreamers"), director("Bernardo", "Bertolucci"), year(2003), views(4), Italy),
      )
    )

    val stream = source.via(Stream.serialize[Movie])

    stream
      .runWith(TestSink.probe[String])
      .request(4)
      .expectNext(
        "Alien is an english movie directed by Ridley Scott in 1979",
        "Nikia is a french movie directed by Luc Besson in 1990",
        "Taxi Driver is an american movie directed by Martin Scorsese in 1976",
        "The Dreamers is an italian movie directed by Bernardo Bertolucci in 2003"
      )
      .expectComplete()
  }

  it should "run simple movie transformation stream" in {

    import Stream.{materializer, system}

    val source = Stream.sourceOf[Option[Movie]](
      List(
        Option(movie(title("Alien"), director("Ridley", "Scott"), year(1979), views(1), England)),
        None,
        Option(movie(title("Nikita"), director("Luc", "Besson"), year(1990), views(2), France)),
        None,
        Option(movie(title("Taxi Driver"), director("Martin", "Scorsese"), year(1976), views(3), UnitedStates)),
        None,
        Option(movie(title("The Dreamers"), director("Bernardo", "Bertolucci"), year(2003), views(4), Italy)),
      )
    )

    val stream = source.via(Stream.transform[Option, Movie, Director](Stream.movieToDirector))

    stream
      .runWith(TestSink.probe[Option[Director]])
      .request(8)
      .expectNext(
        Option(director("Ridley", "Scott")),
        None,
        Option(director("Luc", "Besson")),
        None,
        Option(director("Martin", "Scorsese")),
        None,
        Option(director("Bernardo", "Bertolucci"))
      )
      .expectComplete()
  }

  it should "run simple movie filtered stream" in {

    import Stream.{materializer, system}

    val source = Stream.sourceOf[Movie](
      List(
        movie(title("Alien"), director("Ridley", "Scott"), year(1979), views(1), England),
        movie(title("Nikita"), director("Luc", "Besson"), year(1990), views(2), France),
        movie(title("Taxi Driver"), director("Martin", "Scorsese"), year(1976), views(3), UnitedStates),
        movie(title("The Dreamers"), director("Bernardo", "Bertolucci"), year(2003), views(4), Italy),
      )
    )

    val stream = source.via(Stream.getOnly(movie(title("Nikita"), director("Luc", "Besson"), year(1990), views(2), France)))

    stream
      .runWith(TestSink.probe[Option[Movie]])
      .request(4)
      .expectNext(
        None,
        Option(movie(title("Nikita"), director("Luc", "Besson"), year(1990), views(2), France)),
        None,
        None
      )
      .expectComplete()
  }

}
