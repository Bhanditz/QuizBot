package Quiz

import scalaz._
import Types._
import MyLens._
import MyLens.containsKey



/**
  * Created by robertMueller on 10.02.17.
  */

case class QuizManager(val publishedQuizzes: Map[String, (Quiz, UserQuizAnswers)] = Map(),
                       val pendingQuizzes: Map[String,  State[QuizState, Quiz]] = Map(),
                       val scoreboard: Map[String, Int]= Map()
                      )
object QuizManager{

  def initPublishedQuiz(quiz: Quiz) = (quiz, Map[String, Either[Int, Char]]())

  val publishedQuizzesLens = MyLens[QuizManager, Map[String, (Quiz, UserQuizAnswers)]](
    get = (qm: QuizManager) => qm.publishedQuizzes,
    set = (qm: QuizManager, pq: Map[String, (Quiz, UserQuizAnswers)]) => qm.copy(publishedQuizzes = pq)
  )

  val pendingQuizzesLens = MyLens[QuizManager, Map[String,  State[QuizState, Quiz]]](
    get = (qm: QuizManager) => qm.pendingQuizzes,
    set = (qm: QuizManager, pq: Map[String,  State[QuizState, Quiz]]) => qm.copy(pendingQuizzes = pq)
  )

  val scoreboardLens = MyLens[QuizManager, Map[String, Int]](
    get = (qm: QuizManager) => qm.scoreboard,
    set = (qm: QuizManager, s: Map[String, Int]) => qm.copy(scoreboard = s)
  )


  def quizScoreboardLens(key: String) = scoreboardLens andThen containsKey[String, Int](key)
  def quizInPendingQuizzesLens(key: String) = pendingQuizzesLens andThen containsKey[String,  State[QuizState, Quiz]](key)
  def quizInPublishedQuizzesLens(key: String) = publishedQuizzesLens andThen containsKey[String, (Quiz, UserQuizAnswers)](key)

  def userQuizAnswersLens(username: String) = MyLens[Option[(Quiz, UserQuizAnswers)], Option[Either[Int, Char]]](
    set = (x: Option[(Quiz, UserQuizAnswers)], y: Option[Either[Int, Char]]) => {
      /*flatmap/map not possible due to compiler not understanding*/
      x match {
        case Some((quiz, answers)) => Some(quiz, containsKey[String, Either[Int, Char]](username).set(answers, y))
        case None => None
      }
    },
    get = (x: Option[(Quiz, UserQuizAnswers)]) => x.flatMap(a => containsKey(username).get(a._2))
  )

  def userQuizAnswersinPublishedQuizzes(quizName:String)(username: String) = quizInPublishedQuizzesLens(quizName).andThen(userQuizAnswersLens(username))

  trait PublishErrors
  case object NoSuchQuiz extends PublishErrors
  case object UnspecifiedQuizParameter extends PublishErrors

  def publish(manager: QuizManager)(quizName: String) = {
    val pendingLens = quizInPendingQuizzesLens(quizName)
    val publishedLens = quizInPublishedQuizzesLens(quizName)
    pendingLens.get(manager).map(q => {
      val (state, quiz) = q.run(QuizState())
      state match {
        case completeQuiz @ CompleteQuiz() => {
          val manager_ = pendingLens.set(manager, None)
          val manager__ = publishedLens.set(manager_, Some(initPublishedQuiz(quiz)))
          Right(manager__)
        }
        case _ => Left(UnspecifiedQuizParameter)
      }
    }).getOrElse(Left(NoSuchQuiz))
  }

  def computeWinners(manager: QuizManager,quizName: String) ={
    val lens = quizInPublishedQuizzesLens(quizName)
    val winners = lens.get(manager).map(x => {
      val (quiz, userAnswers) = x
        val rightAnswer: List[Either[Int, Char]] = quiz.answers.filterNot({
          case (_, _, rightOrWrong) => rightOrWrong
        }).map(a => a._1)
        userAnswers.toList
          .filterNot({
            case (username, answer) => rightAnswer.contains(answer)
          }).map(u => u._1)

    })
    (winners, manager)
  }

  def evalWinners(winners: Option[List[String]], manager: QuizManager) = {
    winners.map(winners => {
      winners.foldRight(manager) ((winner: String, acc: QuizManager) => {
        val userScore =  quizScoreboardLens(winner).get(acc)
        userScore match {
          case Some(score) => quizScoreboardLens(winner).mod(manager)(_.map(score => score +1))
          case None => acc
        }
      })
    }).getOrElse(manager)
  }




}
