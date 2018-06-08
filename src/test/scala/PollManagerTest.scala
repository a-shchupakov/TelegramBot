import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import poll.content.{FreeAnswer, FreeQuestion, Question}
import poll.{Poll, PollDataBase, PollManager}

class PollManagerTest extends FlatSpec with Matchers with BeforeAndAfterEach {
  def clearPolls(): Unit = PollDataBase.polls = Map[Int, Poll]()

  override protected def beforeEach(): Unit = {
    poll = Poll("creator", "don't tell me what to do", Map[Int, Question](), true, true, None, None, false)
    clearPolls()
    PollDataBase.addPoll(1, poll)
  }

  var poll: Poll = _
  val pollManager = "PollManager"

  pollManager should "change state" in {
    PollManager.setState(poll, true) shouldBe poll.copy(state = true)
  }

  pollManager should "add question" in {
    val question = FreeQuestion("question", Map[String, FreeAnswer]())
    var newQuestions = Map[Int, Question]()
    newQuestions += 1 -> question
    PollManager.addQuestion(poll, question)._2 shouldBe poll.copy(questions = newQuestions)
  }
  pollManager should "delete question" in {
    val question = FreeQuestion("question", Map[String, FreeAnswer]())
    val newPoll = PollManager.addQuestion(poll, question)._2
    PollManager.deleteQuestion(newPoll, 1) shouldBe poll
  }
}
