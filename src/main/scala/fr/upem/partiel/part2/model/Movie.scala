package fr.upem.partiel.part2.model

import fr.upem.partiel.part2.model.Movie._

// TODO You have to create all the classes you need for the exam
// TODO Don't forget to read the existing code and the unit tests to get some clues !

class Movie(val title: Title, val director: Director, val year: Year, val views: Views, val country: Country)

object Movie {

  def apply(title: Title, director: Director, year: Year, views: Views, country: Country): Movie = new Movie(title, director, year, views, country)

  // TODO Create this model
  case class Title(name: String)

  // TODO Create this model
  case class Director(fn: String, ln: String)

  // TODO Create this model
  case class Year(value: Int)

  // TODO Create this model
  case class Views(value: Long)

  trait Country

  object Country {

    final case object France extends Country

    final case object England extends Country

    final case object Italy extends Country

    final case object Germany extends Country

    final case object UnitedStates extends Country

  }

  // TODO Create this method
  def movie(title: Title, director: Director, year: Year, views: Views, country: Country): Movie = Movie(title, director, year, views, country)

  // TODO Create this method
  def title(s: String): Title = Title(s)

  // TODO Create this method
  def director(fn: String, ln: String): Director = Director(fn, ln)

  // TODO Create this method
  def year(value: Int): Year = Year(value)

  // TODO Create this method
  def views(value: Long): Views = Views(value)

}