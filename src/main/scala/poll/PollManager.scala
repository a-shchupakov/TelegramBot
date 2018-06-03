package poll


import java.util.Calendar

import content.Question

object PollManager {
  def setState(poll: Poll, newState: Boolean): Poll = {
    poll.copy(state = newState)
  }
  def addQuestion(poll: Poll, question: Question): (Int, Poll) = {
    val id = poll.getNextQuestionId
    val newPoll = poll.copy(questions = poll.questions + (id -> question))
    id -> newPoll
  }
  def deleteQuestion(poll: Poll, id: Int): Poll = poll.copy(questions = poll.questions - id)
}
