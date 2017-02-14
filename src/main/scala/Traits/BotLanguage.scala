package Traits

/**
  * Created by robertMueller on 14.02.17.
  */
import scala.util.parsing.combinator._
trait BotLanguage extends RegexParsers{
  def language: Parser[Any]
}
