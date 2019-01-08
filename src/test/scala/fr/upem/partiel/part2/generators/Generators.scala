package fr.upem.partiel.part2.generators

import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie.Country._
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

object Generators {

  implicit val directorGen = for {
    fn <- oneOf("Steven", "Quentin", "Martin", "Alfred", "Stanley", "Ridley", "Clint", "James", "Christophe", "Tim")
    ls <- oneOf("Spielberg", "Tarantino", "Scorsese", "Hitchcock", "Kubrick", "Scott", "Eastwood", "Cameron", "Nolan", "Burton")
  } yield Movie.director(fn, ls)

  implicit val viewsGen = Gen.choose(1L, 100000000L).map(Movie.views)

  implicit val titleGen = Gen.oneOf(
    "Red Tower",
    "The Frozen Rainbow",
    "Beginning of Legacy",
    "The Birch's Pirates",
    "The Luck of the Touch",
    "Bride in the Heat",
    "Dangerous Serpent",
    "The Broken Death",
    "Lights of Valley",
    "The Nothing's Night",
    "The Way of the Vision",
    "Ice in the Boy",
    "Grey Ashes",
    "The Silent Year",
    "Truth of Legacy",
    "The Touch's Mage",
    "The Witches of the Stars",
    "Flowers in the Thought").map(Movie.title)

  implicit val yearGen = for {
    x <- Gen.oneOf("19", "20")
    a <- Gen.choose(0, 9)
    b <- Gen.choose(0, 9)
    r <- Gen.const(s"$x$a$b".toInt).map(Movie.year)
  } yield r

  implicit val movieGen = for {
    title <- titleGen
    director <- directorGen
    year <- yearGen
    views <- viewsGen
    country <- Gen.oneOf(France, Italy, Germany, England, UnitedStates)
  } yield Movie.movie(title, director, year, views, country)

  def movieGen(views: Long) = for {
    title <- titleGen
    director <- directorGen
    year <- yearGen
    country <- Gen.oneOf(France, Italy, Germany, England, UnitedStates)
  } yield Movie.movie(title, director, year, Movie.views(views), country)

  implicit def tupleGen[T](implicit gen: Gen[T]): Gen[(T, T)] = for {
    t1 <- gen
    t2 <- gen
  } yield (t1, t2)

}