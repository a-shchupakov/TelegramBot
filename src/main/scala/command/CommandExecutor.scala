package command

import java.util.Calendar

import poll.{Poll, PollDataBase, PollManager}
import poll.content._


object CommandExecutor {

  private def setAnswer(executor: String, command: SetAnswer): String = {
    doIfOpened(executor, poll => {
      doIfStarted(poll, _ => {
        doIfQuestionExists(poll, command.id, question => {
          doIfNotAnswered(executor, poll, question, _ => {
            question match {
              case question: FreeQuestion => command.answer.createFreeAnswer().map(answer => {
                val newAnswers = question.answers + (executor -> answer)
                val newQuestion = question.copy(answers = newAnswers)
                val newQuestions = poll.questions + (command.id -> newQuestion)
                val newPoll = poll.copy(questions = newQuestions)
                PollDataBase.addPoll(PollDataBase.getOpenedPollId(executor), newPoll)
                "Question answered"
              }).getOrElse("Wrong answer format")
              case question: ChoiceQuestion => command.answer.createChoiceAnswer().map(answer => {
                if (question.answerVariants.contains(answer.answer)){
                  val newAnswers = question.answers + (executor -> answer)
                  val newQuestion = question.copy(answers = newAnswers)
                  val newQuestions = poll.questions + (command.id -> newQuestion)
                  val newPoll = poll.copy(questions = newQuestions)
                  PollDataBase.addPoll(PollDataBase.getOpenedPollId(executor), newPoll)
                  "Question answered"
                }
                else "Wrong answer number"
              }).getOrElse("Wrong answer format")
              case question: MultiQuestion => command.answer.createMultiAnswer().map(answer => {
                if (answer.answer.forall(id => question.answerVariants.contains(id))){
                  val newAnswers = question.answers + (executor -> answer)
                  val newQuestion = question.copy(answers = newAnswers)
                  val newQuestions = poll.questions + (command.id -> newQuestion)
                  val newPoll = poll.copy(questions = newQuestions)
                  PollDataBase.addPoll(PollDataBase.getOpenedPollId(executor), newPoll)
                  "Question answered"
                }
                else "Wrong answer number"
              }).getOrElse("Wrong answer format")
            }
          })
        })
      })
    })
  }

  private def doIfPollExists(id: Int, function: Poll => String): String = {
    PollDataBase.polls
      .get(id)
      .map(function)
      .getOrElse(s"Poll with $id is not found")
  }

  private def doIfQuestionExists(poll: Poll, id: Int, function: Question => String): String = {
    poll.questions
      .get(id)
      .map(function)
      .getOrElse(s"Question with $id is not found")
  }

  private def doIfOpened(executor: String, function: Poll => String): String = {
    PollDataBase.getOpenedPoll(executor).map(function).getOrElse("Select poll first")
  }

  private def doIfOwner(poll: Poll, executor: String, function: Poll => String): String = {
    if (poll.creator.equals(executor))
      function(poll)
    else
      "Access denied: you are not owner of poll"
  }

  private def doIfStarted(poll: Poll, function: Poll => String): String = {
    if (poll.state)
      return function(poll)
    else if (poll.begin.isDefined){
      val today = Calendar.getInstance().getTime
      if (today after poll.begin.get) {
        if (poll.end.isDefined) {
          if (today before poll.end.get)
            return function(poll)
        }
        else
          return "Access denied: poll is not started"
      }
    }
    "Access denied: poll is not started"
  }

  private def doIfNotStarted(poll: Poll, function: Poll => String): String = {
    if (poll.begin.isDefined) {
      val today = Calendar.getInstance().getTime
      if (today before poll.begin.get)
        return function(poll)
    }
    else if (!poll.state)
      return function(poll)
    "Access denied: poll is started"
  }

  private def doIfNotAnswered(executor: String, poll: Poll, question: Question, function: Poll => String): String = {
    if (!question.answers.contains(executor))
      function(poll)
    else
      "Access denied: you have already answered this question"
  }

  private def getContent(executor: String, command: GetContent): String = {
    doIfOpened(executor, poll => {
      poll.questions.foldLeft(s"Poll '${poll.name}' with id (${PollDataBase.getOpenedPollId(executor)}):") {
        case (result, (id, question)) =>
          val answers = question match {
            case _: FreeQuestion => s"Question $id: ${question.question}"
            case question: ChoiceQuestion =>
              question.answerVariants.foldLeft(s"Question $id: ${question.question}") {
                case (innerResult, (innerId, answer)) => s"$innerResult\n\t$innerId: $answer"
              }
            case question: MultiQuestion =>
              question.answerVariants.foldLeft(s"Question $id: ${question.question}") {
                case (innerResult, (innerId, answer)) => s"$innerResult\n\t$innerId: $answer"
              }
          }
          val temp = answers.split("\n").map(line => s"\t$line").mkString("\n")
          s"$result\n$temp"
      }
    })
  }

  private def createPoll(executor: String, command: CreatePoll): String = {
    val correct =
      if (command.begin.isDefined && command.end.isDefined)
        command.begin.get before command.end.get
      else
        true

    if (!correct)
      return "Incorrect date"

    val poll = Poll(executor,
      command.pollName,
      Map[Int, Question](),
      command.anonymous.getOrElse(true),
      command.visible.getOrElse(false),
      command.begin, command.end,
      state = false)

    val id = PollDataBase.getNextPollId
    PollDataBase.addPoll(id, poll)
    id.toString
  }

  private def createQuestion(executor: String, command: CreateQuestion): String = {
    doIfOpened(executor: String, poll => {
      doIfNotStarted(poll, poll => {
        doIfOwner(poll, executor, (poll: Poll) => {
          val question = command._type.getOrElse("free") match {
            case "free" => FreeQuestion(command.question, Map[String, FreeAnswer]())
            case "choice" =>
              val answersMap = command.answers.zip(Stream.from(1)).map(data => data._2 -> data._1).toMap
              ChoiceQuestion(command.question, answersMap, Map[String, ChoiceAnswer]())
            case "multi" =>
              val answersMap = command.answers.zip(Stream.from(1)).map(data => data._2 -> data._1).toMap
              MultiQuestion(command.question, answersMap, Map[String, MultiChoiceAnswer]())
          }
          val idAndPoll = PollManager.addQuestion(poll, question)
          PollDataBase.addPoll(PollDataBase.getOpenedPollId(executor), idAndPoll._2)
          idAndPoll._1.toString
        })
      })
    })
  }

  def execute(executor: String, command: Command): String = {
    command match {
      case command: CreatePoll => createPoll(executor, command)

      case command: CreateQuestion => createQuestion(executor, command)

      case command: DeletePoll => doIfPollExists(command.id, poll => {
        doIfOwner(poll, executor, _ => {
          PollDataBase.deletePoll(command.id)
          s"Poll with ${command.id} has been deleted"
        })
      })

      case command: DeleteQuestion => doIfOpened(executor, poll => {
        doIfNotStarted(poll, _ => {
          doIfOwner(poll, executor, _ => {
            doIfQuestionExists(poll, command.id, _ => {
              val newPoll = PollManager.deleteQuestion(poll, command.id)
              PollDataBase.addPoll(PollDataBase.getOpenedPollId(executor), newPoll)
              s"Question with ${command.id} has been deleted"
            })
          })
        })
      })

      case command: StartPoll => doIfPollExists(command.id, poll => {
        doIfOwner(poll, executor, _ => {
          doIfNotStarted(poll, _ => {
            if (poll.begin.isDefined)
              return "Cannot start manually"
            PollDataBase.setState(command.id, state = true)
            s"Poll with ${command.id} has been started"
          })
        })
      })

      case command: StopPoll => doIfPollExists(command.id, poll => {
        doIfOwner(poll, executor, _ => {
          doIfStarted(poll, _ => {
            if (poll.end.isDefined)
              return "Cannot end manually"
            PollDataBase.setState(command.id, state = false)
            s"Poll with ${command.id} has been stopped"
          })
        })
      })

      case command: OpenPoll => doIfPollExists(command.id, _ => {
        PollDataBase.openPoll(executor, command.id)
        s"Poll with ${command.id} opened"
      })

      case _: ClosePoll => doIfPollExists(PollDataBase.getOpenedPollId(executor), _ => {
        PollDataBase.closePoll(executor)
        s"Poll closed"
      })

      case command: SetAnswer => setAnswer(executor, command)

      case _: GetPolls => PollDataBase.polls.foldLeft("Available polls:") {
        case (result, (id, poll)) => s"$result\n\t$id: ${poll.name}"
      }

      case command: GetContent => getContent(executor, command)

      case command: GetResults => doIfPollExists(command.id, poll => {
        if (poll.visible)
          poll.getResults
        else
          doIfNotStarted(poll, poll => poll.getResults)
      })
    }
  }
}
