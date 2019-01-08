package fr.upem.partiel.part2.functions

import fr.upem.partiel.part2.model.Movie
import fr.upem.partiel.part2.model.Movie.Director

object Functions {

  // TODO
  lazy val getDirectorNames: List[Movie] => List[String] = {
    case x :: xs => x.director match {
      case Director(fn, ln) => fn + " " + ln :: getDirectorNames(xs)
    }
    case Nil => List()
  }

  // TODO
  lazy val viewMoreThan: Long => List[Movie] => List[Movie] = value => movies => {
    movies.filter(movie => movie.views.value > value)
  }

  // TODO
  lazy val byDirector: List[Movie] => Map[Director, List[Movie]] = movies => {
    movies.groupBy(_.director)
  }

}
