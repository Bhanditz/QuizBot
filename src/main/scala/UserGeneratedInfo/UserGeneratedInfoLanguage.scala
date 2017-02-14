package UserGeneratedInfo
import Traits.BotLanguage
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by robertMueller on 14.02.17.
  */
class UserGeneratedInfoLanguage extends BotLanguage {

  /*=====ACTION MAPPINGS=====*/
  import UserGeneratedInfoActor.{GetAllProposals, AllProposals,
    AddProposal,
    Upvote, Downvote, DeleteProposal
  }

  override def language = addProposal|upvote|downvote|delete

  def addProposal = ("create info"|"new info"|"make info")~>("name"|"key")~>opt("=")~>word~("info"|"value")~opt("=")~anyString~opt("message"|"msg" ~> opt("=") ~> anyString) ^^ {
    case (infoName~_~_~value~message) => AddProposal(ValueProposal(infoName, value, message))
  }

  def upvote = ("+1"|"+"|"upvote"|"up")~>word~withUser ^^ {
    case(infoName~userName)=> Upvote(infoName, userName)
  }

  def downvote = ("-1"|"-"|"downvote"|"down")~>word~withUser ^^ {
    case(infoName~userName)=> Downvote(infoName, userName)
  }

  def delete = ("delete"|"remove"|"kill")~>word ^^ {
    case(infoName)=> DeleteProposal(infoName)
  }

  def word: Parser[String]    = """[a-zA-Z1-9_:-]+""".r ^^ { _.toString }
  def anyString: Parser[String] = """.*""".r ^^ {_.toString}
  def user = ("me"|"username"|"user")
  def withUser: Parser[String] = user ~> opt("=") ~> word
}
