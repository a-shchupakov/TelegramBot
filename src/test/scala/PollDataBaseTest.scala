import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import poll.{Poll, PollDataBase}
import poll.content.Question

class PollDataBaseTest extends FlatSpec with Matchers with BeforeAndAfterEach{
  def clearPolls(): Unit = PollDataBase.polls = Map[Int, Poll]()

  override protected def beforeEach(): Unit = {
    poll = Poll("creator", "don't tell me what to do", Map[Int, Question](), true, true, None, None, false)
    clearPolls()
  }

  var poll: Poll = _
  val pollDataBase = "PollDataBase"

  pollDataBase should "add and delete poll" in {
    PollDataBase.addPoll(1, poll)
    PollDataBase.polls.size shouldBe 1
    PollDataBase.deletePoll(1)
    PollDataBase.polls.size shouldBe 0
  }

  pollDataBase should "change state" in {
    PollDataBase.addPoll(1, poll)
    PollDataBase.setState(1, state = true)
    PollDataBase.polls(1) shouldBe poll.copy(state = true)
  }

  pollDataBase should "select and unselect poll" in {
    val user = "user"
    PollDataBase.addPoll(1, poll)
    PollDataBase.openPoll(user, 1)
    PollDataBase.selectedPollMap.size shouldBe 1
    PollDataBase.closePoll(user)
    PollDataBase.selectedPollMap.size shouldBe 0
  }

  pollDataBase should "give correct opened poll id" in {
    val user = "user"
    PollDataBase.addPoll(1, poll)
    PollDataBase.openPoll(user, 1)
    PollDataBase.getOpenedPollId(user) shouldBe 1
    PollDataBase.closePoll(user)
    PollDataBase.getOpenedPollId(user) shouldBe -1
  }
}
