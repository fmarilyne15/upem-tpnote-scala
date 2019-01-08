package fr.upem.partiel.part2.functions

import fr.upem.partiel.part2.generators.Generators.{movieGen, tupleGen}
import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie.Country
import org.scalacheck.Gen.{alphaStr, nonEmptyListOf}
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FunctionsSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = PosInt(50), minSize = 10, maxSize = 20)

  "Functions" should "get director names" in {

    forAll(nonEmptyListOf(tupleGen[String](alphaStr))) { names =>
      val expected = names.map { case (fn, ln) => s"$fn $ln" }
      val movies = names.map { case (fn, ln) => Movie.director(fn, ln) }.map(d => Movie.movie(Movie.title(""), d, Movie.year(1987), Movie.views(1L), Country.Italy))
      Functions.getDirectorNames(movies) should be(expected)
    }
  }

  it should "filter by views" in {

    forAll(nonEmptyListOf(movieGen(10)), nonEmptyListOf(movieGen(100))) { (flops, tops) =>
      val movies = flops ::: tops
      Functions.viewMoreThan(50)(movies) should contain theSameElementsAs tops
    }
  }

  // TODO Implement this unit test
  it should "group by director" in {

  }

}
