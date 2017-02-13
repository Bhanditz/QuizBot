package Quiz
 import QuizManager._
 import akka.actor.Actor
 import Quiz._
 import Types._
 import QuizActor._
 import MyLens._
import QuizManager._

 import scalaz.State
 import scalaz._

/**
  * Created by robertMueller on 30.01.17.
  */

object QuizActor{
  //case class setTrueAnswer(quizName: String, trueAnswer: Either[Int, Char])
  case class RequestQuizInfo(name: String)
  case class ResponseQuizInfo(quiz: Map[String,Quiz])
  case class RequestPendingQuizzes(name: String)
  case class ResponsePendingQuizzes(quiz: Quiz)
  case class NewQuiz(name: String)
  case class AddQuizQuestion(name: String, question: String)
  case class AddQuizAnswers(name: String, answers: List[Answer])
  case class MissingQuizParameters(missing: List[String])
  case class PublishQuiz(name: String)
  case object PublishQuizSuccess
  case class PublishQuizFailure(reason: PublishErrors)
  case class EvaluateQuiz(name: String)
  case class QuizEvaluationResults(winners: List[String])
  case class RemoveQuiz(name: String)
  case object GetPendingQuizzes
  case object GetPublishedQuizzes
  case object GetScoreboard
  /*=====USERS ANSWER QUIZ=====*/
  case class AnswerQuiz(userName: String, quizName: String, answer: Either[Int, Char])
  /*=====General Errors=====*/
  case object NoSuchQuiz
  case object NoSuchAnswer
}

/*=====QuizActorState=====*/
//TODO

class QuizActor extends Actor{

  /*=====Queries=====*/
  def getQuizOrErrorMsg[K,V](opt:Option[V]): Option[V] = {
    opt.orElse({
      sender ! QuizActor.NoSuchQuiz
      None
    })
  }
  /*=====THREADSAFE STATE + RECEIVE=====*/
  override def receive = update(QuizManager())

  /*=====STATE UPDATE=====*/
  def update(quizManager: QuizManager): Receive = {

    /*=====CREATING QUIZZES=====*/
    case NewQuiz(name) => {
      val initQuiz = quizInit flatMap(withName(name))
      val updatedManager =  quizInPendingQuizzesLens(name).set(quizManager, Some(initQuiz))
      context become update(updatedManager)
    }

    case AddQuizQuestion(name, question) => {
      val quizWithQuestionLens = quizInPendingQuizzesLens(name)
      getQuizOrErrorMsg (quizWithQuestionLens.get(quizManager))
        .foreach(_ => {
          val updatedManager = quizWithQuestionLens.mod(quizManager)(_.map(_ flatMap(withQuestion(question))))
          context become update(updatedManager)
      })
    }

    case AddQuizAnswers(name, answers) => {
      val quizWithAnswerLens = quizInPendingQuizzesLens(name)
      getQuizOrErrorMsg (quizWithAnswerLens.get(quizManager))
        .foreach(_ => {
          val updatedManager = quizWithAnswerLens.mod(quizManager)(_.map(_ flatMap(withAnswers(answers))))
          context become update(updatedManager)
      })
    }
    case PublishQuiz(name: String) => {
      QuizManager.publish(quizManager)(name) match {
          //NoSuchQuiz / UnspecifiedQuizParameter
        case Left(error) => sender ! PublishQuizFailure(error)
        case Right(updatedManager) => {
          sender ! PublishQuizSuccess
          context become update(updatedManager)
        }
      }
    }

    /*=====GETTING QUIZZES=====*/
    case GetPendingQuizzes => {sender ! quizManager.pendingQuizzes}
    case GetPublishedQuizzes => {sender ! quizManager.publishedQuizzes}
    case GetScoreboard => {sender ! quizManager.scoreboard}

    /*=====INTERACT WITH QUIZZES=====*/
    case AnswerQuiz(userName, quizName, usersAnswer) => {
      val lens =  userQuizAnswersinPublishedQuizzes(quizName)(userName)
      getQuizOrErrorMsg(lens.get(quizManager))
        .foreach(_ => {
        val updatedManager = lens.set(quizManager, Some(usersAnswer))
        context become update(updatedManager)
      })
    }

    case EvaluateQuiz(quizName) => {
      getQuizOrErrorMsg(computeWinners(quizManager, quizName)_1)
        .foreach(winnerList => {
          val updatedManager = evalWinners(Some(winnerList), quizManager)
          sender ! QuizEvaluationResults(winnerList)
          context become update(updatedManager)
      })//Quiz entfernen ...
      }
  }
}
