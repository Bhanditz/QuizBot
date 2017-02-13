package Quiz
import scala.util
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.JavaTokenParsers
import Types._
/**
  * Created by robertMueller on 13.02.17.
  */

class QuizBotLanguage extends RegexParsers{

  /*=====ACTION MAPPINGS=====*/
  import QuizActor.{NewQuiz, AnswerQuiz, AddQuizQuestion,
  AddQuizAnswers, PublishQuiz, EvaluateQuiz,
  GetPublishedQuizzes, GetScoreboard}

  /*=====PRODUCTION RULES=====*/
  def language = newQuiz|addQuizQuestion|addQuizAnswers|answerQuiz|publishQuiz|evaluateQuiz|getPublishedQuizzes|getScoreboard

  def newQuiz = ("new quiz"|"make quiz") ~> word ^^ {quizName => NewQuiz(quizName)}

  def addQuizQuestion = word ~ opt("with" | "add") ~ "question" ~ opt("=") ~ anyString <~ opt(withUser) ^^ {
    case (quizName ~ _ ~ _ ~_ ~ question) => AddQuizQuestion(quizName, question) }

  def addQuizAnswers = word ~ opt("with" | "add") ~ "answers" ~ opt("=") ~ quizAnswers <~ opt(withUser) ^^ {
    case (quizName ~ _ ~ _ ~ _ ~ answers) => AddQuizAnswers(quizName, answers)
  }

  def quizAnswers: Parser[List[Answer]] = repsep((charOrInt ~ anyString ~  opt(boolean)), " " ) <~ opt(withUser) ^^ { list =>
    list.map({
      case (ci ~ answer ~  Some(b)) => (ci, answer, b)
      case (ci ~ answer ~ None) => (ci, answer, false)
    })
  }

  def answerQuiz = word ~ opt("with" | "add") ~ "answer" ~ charOrInt ~ withUser ^^ {
    case (quizName ~ _ ~ _ ~ usersAnswer ~ userName) => AnswerQuiz(userName, quizName, usersAnswer)
  }

  def publishQuiz = ("publish"|"pub") ~> word ^^ {quizName => PublishQuiz(quizName)}

  def evaluateQuiz = ("eval"|"evaluate"|"remove"|"kill") ~> word ^^ {quizName => EvaluateQuiz(quizName)}

  def getPublishedQuizzes = ("get published quizzes"|"quizzes"|"published quizzes") <~ opt(withUser) ^^  {_=> GetPublishedQuizzes}
  def getScoreboard = ("get scoreboard"|"scoreboard"|"scores"|"score") <~ opt(withUser) ^^ {_=> GetScoreboard}

  def withUser: Parser[String] = user ~> opt("=") ~> word
  def user = ("me"|"username"|"user")
  def anyString: Parser[String] = """.*""".r ^^ {_.toString}
  def word: Parser[String]    = """[a-zA-Z1-9_:-]+""".r ^^ { _.toString }
  def charOrInt: Parser[Either[Int, Char]] = integer ^^ {i => Left(i)}|character ^^ {c => Right(c)}
  def character: Parser[Char] = """[a-zA-Z]""".r ^^ {_.charAt(0)}
  def integer: Parser[Int] =  """\d+""".r ^^ { _.toInt}
  def boolean: Parser[Boolean] = bTrue1|bTrue2|bTrue3|bFalse1|bFalse2|bFalse3
  def bTrue1: Parser[Boolean] = "true" ^^ {_ => true}
  def bTrue2: Parser[Boolean] = "True" ^^ {_ => true}
  def bTrue3: Parser[Boolean] = "TRUE" ^^ {_ => true}
  def bFalse1: Parser[Boolean] = "false" ^^ {_ => false}
  def bFalse2: Parser[Boolean] = "False" ^^ {_ => false}
  def bFalse3: Parser[Boolean] = "FALSE" ^^ {_ => false}
}
