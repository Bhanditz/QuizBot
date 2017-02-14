package UserGeneratedInfo

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by robertMueller on 14.02.17.
  */
class UserGeneratedInfoLanguage extends RegexParsers {

  /*=====ACTION MAPPINGS=====*/
  import UserGeneratedInfoActor.{GetAllProposals, AllProposals,
    AddProposal, AddedProposal,
    Upvote, Downvote, DeleteProposal
  }
}
