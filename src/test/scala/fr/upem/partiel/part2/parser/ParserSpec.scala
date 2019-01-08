package fr.upem.partiel.part2.parser

import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie.{Country, Director, Views, Year}
import org.scalacheck.Gen
import org.scalacheck.Gen.{alphaLowerChar, choose, listOfN, nonEmptyListOf}
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.{JsError, JsNumber, JsString, JsSuccess, Json}

class ParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = PosInt(50), minSize = 10, maxSize = 20)

  "Parser" should "parse a valid year" in {
    import Parser.yearReads

    forAll(Gen.oneOf("1", "2"), listOfN(3, choose(0, 9).map(_.toString))) { (first, next) =>
      val year = (first :: next).mkString
      Json.fromJson[Year](JsString(year)) should be(JsSuccess(Movie.year(year.toInt)))
    }
  }

  val wrongSizeYearGenerator = for {
    h <- choose(1, 9).map(_.toString)
    nTail <- Gen.oneOf(1, 2, 4, 5, 6, 7, 8, 9)
    tail <- listOfN(nTail, choose(0, 9).map(_.toString)).map(_.mkString)
  } yield h + tail
  val wrongMilleniumYearGenerator = for {
    h <- Gen.oneOf(0, 3, 4, 5, 6, 7, 8, 9).map(_.toString)
    tail <- listOfN(3, choose(0, 9).map(_.toString)).map(_.mkString)
  } yield h + tail


  it should "parse a invalid year" in {
    import Parser.yearReads

    forAll(Gen.oneOf(wrongSizeYearGenerator, wrongMilleniumYearGenerator)) { invalidYear =>
      Json.fromJson[Year](JsString(invalidYear)) should be(JsError("Not a valid Year"))
    }
  }

  val nonEmptyStringGenerator = nonEmptyListOf(alphaLowerChar)
    .map(_.mkString)
    .map(_.filter(_ != ' '))

  it should "parse a valid director" in {
    import Parser.directorReads

    forAll(nonEmptyStringGenerator, nonEmptyStringGenerator) { (fn, ln) =>
      Json.fromJson[Director](JsString(s"$fn $ln")) should be(JsSuccess(Movie.director(fn, ln)))
    }
  }

  val invalidDirector = for {
    n <- Gen.oneOf(1, 3, 4, 5, 6, 7, 8, 9)
    d <- listOfN(n, nonEmptyStringGenerator).map(_.mkString(" "))
  } yield d

  it should "parse a invalid director" in {
    import Parser.directorReads

    forAll(invalidDirector) { invalidDirector =>
      Json.fromJson[Director](JsString(invalidDirector)) should be(JsError("Not a valid Director"))
    }
  }

  it should "parse a valid country" in {
    import Parser.countryReads

    Json.fromJson[Country](JsString("FR")) should be(JsSuccess(Country.France))
    Json.fromJson[Country](JsString("UK")) should be(JsSuccess(Country.England))
    Json.fromJson[Country](JsString("GE")) should be(JsSuccess(Country.Germany))
    Json.fromJson[Country](JsString("IT")) should be(JsSuccess(Country.Italy))
    Json.fromJson[Country](JsString("US")) should be(JsSuccess(Country.UnitedStates))
  }

  it should "parse a invalid country" in {
    import Parser.countryReads

    forAll(Gen.alphaNumStr) { invalidCountry =>
      Json.fromJson[Country](JsString(invalidCountry)) should be(JsError("Not a valid Country"))
    }
  }

  it should "parse a valid view number" in {
    import Parser.viewsReads

    forAll(Gen.posNum[Long]) { number =>
      Json.fromJson[Views](JsNumber(number)) should be(JsSuccess(Movie.views(number)))
    }
  }

  it should "parse a invalid view number" in {
    import Parser.viewsReads

    val max = BigDecimal(Long.MaxValue)
    forAll(Gen.posNum[Long].filter(_ > 1)) { number =>
      val overflow = max + BigDecimal(number)
      Json.fromJson[Views](JsNumber(overflow)) should be(JsError("Not a valid Views"))
    }
  }

}
