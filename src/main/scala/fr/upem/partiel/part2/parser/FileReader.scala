package fr.upem.partiel.part2.parser

import Parser.movieReads
import play.api.libs.json.Json.parse
import play.api.libs.json.Reads

import scala.io.Source.fromResource

object FileReader {

  private lazy val lines = fromResource("movies.json").getLines().toList.mkString

  lazy val movies = parse(lines).as(Reads.list(movieReads))

}
