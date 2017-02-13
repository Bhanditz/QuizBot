import Quiz.QuizActor._
import Quiz.QuizManager._
import Quiz._
import akka.actor.{ActorSystem, Props}
import akka.pattern._
import akka.util.Timeout
//import Quiz.QuizActor._
import Quiz._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._

/**
  * Created by robertMueller on 30.01.17.
  */
object Main extends App{
  implicit val timeout = Timeout(2.seconds)
  val quizSystem = ActorSystem("SimpleSystem")
  val quizActor = quizSystem.actorOf(Props[QuizActor], "Quiz-Actor")
  quizActor ! NewQuiz("test-Quiz", "robert")
  quizActor ! AddQuizQuestion("test-Quiz", "How big is china?")
  val answers = List(
    (Left(1), "12.000.000 km^2", false), (Left(2), "9.560.000 km^2", true)
  )
  val answers_ = List(
    (Left(3), "Keine Antwort stimmt", false), (Left(4), "10.500.000 km^2", true)
  )
  quizActor ! AddQuizAnswers("test-Quiz", answers_)
  val published = quizActor ? PublishQuiz("test-Quiz")
  published.foreach(p => println(p))
  val addedAnswer = quizActor ? AnswerQuiz("robert", "test-Quiz", Left(2))
  val addedAnswer_ = quizActor ? AnswerQuiz("jindong", "test-Quiz", Left(3))
  val publishedQuizzes = quizActor ? GetPublishedQuizzes
  publishedQuizzes.foreach(q => print(q))

  val eval = quizActor ? EvaluateQuiz("test-Quiz")
  val scoreboard = quizActor ? GetScoreboard
  scoreboard.foreach(println)

  /*val manager = QuizManager()
  val qname = "tonald_drump"
  val qb = quizInit flatMap(withName(qname))
  val qm_ = quizInPendingQuizzesLens(qname).set(manager, Some(qb))
  val qm__ = quizInPendingQuizzesLens(qname).mod(qm_)(_.map(_ flatMap(withQuestion("how old is he?"))))
  println(qm__.pendingQuizzes.get(qname).get.run(QuizState())._2)
*/

  /*val myQuiz = quizInit flatMap(withName("plotics")) flatMap(withQuestion("How tall is Angela Merkel?"))
  val quiz = myQuiz.run(QuizState())
  println(quiz._1)*/
}
