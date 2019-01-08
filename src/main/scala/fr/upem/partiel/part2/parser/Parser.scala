package fr.upem.partiel.part2.parser

import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie.Country._
import fr.upem.partiel.part2.model.Movie._
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.{Success, Try}

object Parser {

  // TODO
  def toDirector: String => Option[Director] = name => {
        name.split(" ") match {
            case Array(fn, ln) => Option(Director(fn, ln))
            case _ => Option.empty
        }
    }

    // TODO
  def toName: String => Title = name => Title(name)

  // TODO
  def toCountry: String => Option[Country] = country =>
      country match {
          case "FR" => Option(France)
          case "UK" => Option(England)
          case "IT" => Option(Italy)
          case "GE" => Option(Germany)
          case "US" => Option(UnitedStates)
          case _ => Option.empty
      }

    // TODO
  def toYear: String => Option[Year] = year =>
      Try(year.toInt) match {
          case Success(y) if y > 1000 && y < 3000 => Option(Year(y)) //pas jolie mais les tests passent
          case _ => Option.empty
      }

  // TODO
  def toViews: BigDecimal => Option[Views] = view =>
      Try(view.toLong) match {
          case Success(v) if v > 0 => Option(Views(v))
          case _ => Option.empty
      }

  implicit val directorReads = Reads[Director] {
    case JsString(value) => toDirector(value).map(JsSuccess(_)).getOrElse(JsError("Not a valid Director"))
    case _ => JsError("Not a valid type for Director")
  }

  implicit val nameReads = Reads[Title] {
    case JsString(value) => JsSuccess(toName(value))
    case _ => JsError("Not a valid type for Name")
  }

  implicit val countryReads = Reads[Country] {
    case JsString(value) => toCountry(value).map(JsSuccess(_)).getOrElse(JsError("Not a valid Country"))
    case _ => JsError("Not a valid type for Country")
  }

  implicit val yearReads = Reads[Year] {
    case JsString(value) => toYear(value).map(JsSuccess(_)).getOrElse(JsError("Not a valid Year"))
    case _ => JsError("Not a valid type for Year")
  }

  implicit val viewsReads = Reads[Views] {
    case JsNumber(value) => toViews(value).map(JsSuccess(_)).getOrElse(JsError("Not a valid Views"))
    case _ => JsError("Not a valid type for Views")
  }

  implicit val movieReads: Reads[Movie] = (
    (__ \ "title").read[Title] and
      (__ \ "director").read[Director] and
      (__ \ "year").read[Year] and
      (__ \ "views").read[Views] and
      (__ \ "country").read[Country]
    ) (Movie.apply _)

}
