import java.text.SimpleDateFormat
import java.util.Locale

import command._
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import poll.{Poll, PollDataBase}
import poll.content.{FreeAnswer, FreeQuestion, Question}

class CommandTest extends FlatSpec with Matchers with BeforeAndAfterEach{
  val parser: String = "CommandParser"
  val executor: String = "CommandExecutor"

  def clearPolls(): Unit = PollDataBase.polls = Map[Int, Poll]()

  override protected def beforeEach(): Unit = {
    poll = Poll(sender, "don't tell me what to do", Map[Int, Question](), true, true, None, None, false)
    clearPolls()
  }

  var poll: Poll = _
  val question = FreeQuestion("question", Map[String, FreeAnswer]())
  val format = new SimpleDateFormat("kk:mm:ss yy:MM:dd", Locale.ENGLISH)
  val date = "19:36:28 19:06:06"
  val sender = "sender"
  val and = " and "

  parser + and + executor should "handle create_poll" in {
    val command = CommandParser.parseCommand(s"/create_poll (don't tell me what to do) yes (continuous)").get
    command shouldBe a[CreatePoll]
    command match {
      case command: CreatePoll => {
        command.pollName shouldBe "don't tell me what to do"
        command.anonymous shouldBe Option(true)
        command.visible shouldBe Option(true)
        command.begin shouldBe None
        command.end shouldBe None

        CommandExecutor.execute(sender, command) shouldBe 1.toString
        PollDataBase.polls.size shouldBe 1
      }
    }
  }

  parser + and + executor should "handle start_poll" in {
    PollDataBase.addPoll(1, poll)
    val command = CommandParser.parseCommand(s"/start_poll 1").get
    command shouldBe a[StartPoll]
    command match {
      case command: StartPoll => {
        command.id shouldBe 1

        CommandExecutor.execute(sender, command)
        PollDataBase.polls.size shouldBe 1

      }
    }
  }

  parser + and + executor should "handle stop_poll 1" in {
    PollDataBase.addPoll(1, poll)
    val command = CommandParser.parseCommand(s"/stop_poll 1").get
    command shouldBe a[StopPoll]
    command match {
      case command: StopPoll => {
        command.id shouldBe 1

        CommandExecutor.execute(sender, command)
        PollDataBase.polls.size shouldBe 0 //не хочет работать

      }
    }
  }

  parser + and + executor should "handle delete_poll" in {
    PollDataBase.addPoll(1, poll)
    val command = CommandParser.parseCommand("/delete_poll 1").get
    command shouldBe a[DeletePoll]
    command match {
      case command: DeletePoll => {
        command.id shouldBe 1

        PollDataBase.polls.size shouldBe 1
        CommandExecutor.execute(sender, command)
        PollDataBase.polls.size shouldBe 0
      }
    }
  }

  parser + and + executor should "handle open_poll" in {
    PollDataBase.addPoll(1, poll)
    val command = CommandParser.parseCommand("/begin 1").get
    command shouldBe a[OpenPoll]
    command match {
      case command: OpenPoll => {
        command.id shouldBe 1

        CommandExecutor.execute(sender, command)
        PollDataBase.selectedPollMap.size shouldBe 1
      }
    }
  }
  parser + and + executor should "handle close_poll" in {
    PollDataBase.addPoll(1, poll)
    val command = CommandParser.parseCommand("/end").get
    command shouldBe a[ClosePoll]
    command match {
      case command: ClosePoll => {
        PollDataBase.selectedPollMap += (sender -> 1)
        CommandExecutor.execute(sender, command)
        PollDataBase.selectedPollMap.size shouldBe 0
      }
    }
  }
}
