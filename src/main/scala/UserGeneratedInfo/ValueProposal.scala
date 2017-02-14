package UserGeneratedInfo

import MyLens.MyLens
import MyLens.containsKey
import Quiz.Quiz
import _root_.Quiz.Types._

/**
  * Created by robertMueller on 13.02.17.
  */
//case class UserGeneratedInfoGroup(val infoGroupName: String, val info: Map[String, List[ValueProposal]] = Map())
case class ValueProposal(val infoName: String, val infoValue: String, val message: Option[String] = None,
                         val upvotedBy: Set[String] = Set.empty, val downvotedBy: Set[String] = Set.empty)


object ValueProposal {
  /*
  val infoGroupLens = MyLens[UserGeneratedInfoGroup, Map[String, List[ValueProposal]]](
    get = (infoGroup: UserGeneratedInfoGroup) => infoGroup.info,
    set = (infoGroup: UserGeneratedInfoGroup, newInfo: Map[String, List[ValueProposal]]) => infoGroup.copy(info = newInfo)
  )*/

  //def infoLens(infoKey: String) = infoGroupLens andThen containsKey[String, List[ValueProposal]](infoKey)

  /*def proposalLens(infoKey: String) = infoLens(infoKey).andThen(MyLens[Option[List[ValueProposal]], ValueProposal](
  get = (proposals: Option[List[ValueProposal]]) => ???,
  set = (optList: Option[List[ValueProposal]], newProposal: ValueProposal) => Some(optList.getOrElse(Nil) ++: List(newProposal))
))*/

  /*Sets do not allow duplicated*/
  def upvote(userName: String)(proposal: ValueProposal): ValueProposal = proposal.copy(upvotedBy = proposal.upvotedBy + userName , downvotedBy = proposal.downvotedBy - userName)
  def downvote(userName: String)(proposal: ValueProposal): ValueProposal = proposal.copy(upvotedBy = proposal.upvotedBy - userName , downvotedBy = proposal.downvotedBy + userName)
  def withMessage(proposal: ValueProposal, msg : String): ValueProposal = proposal.copy(message = Some(msg))
  def upvoteDownvoteRate(proposal: ValueProposal): Double = proposal.upvotedBy.size/proposal.downvotedBy.size
  def upvoteDownvoteCount(proposal: ValueProposal): Int = proposal.upvotedBy.size+proposal.downvotedBy.size
  val minCountForDeletion = 5
  val maxRateForDeletion = 1.5
  def mayBeDeleted(proposal: ValueProposal) = upvoteDownvoteRate(proposal) <= maxRateForDeletion && upvoteDownvoteCount(proposal) > minCountForDeletion
  def rename(proposal: ValueProposal, version: Int) = proposal.copy(infoName = s"${proposal.infoName}($version)")
  /*Recursion*/
  def renameWhileTaken(myProposal: ValueProposal, proposals: List[ValueProposal], startWithVersion:Int = 0): ValueProposal = {
    val duplicates = proposals.filterNot(p=>p.infoName == myProposal.infoName)
    val isDuplicate = duplicates.size > 0
    isDuplicate match {
      case false => myProposal
      case true => renameWhileTaken(rename(myProposal, startWithVersion + 1), proposals)
    }
  }
}