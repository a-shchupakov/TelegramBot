package poll

import java.util.Date

import content.{ChoiceAnswer, FreeAnswer, MultiChoiceAnswer, Question}

case class Poll(creator: String,
                name: String,
                questions: Map[Int, Question],
                anonymous: Boolean,
                visible: Boolean,
                begin: Option[Date], end: Option[Date],
                state: Boolean) {
  private val questionIdGenerator: Iterator[Int] = Stream.from(questions.size + 1).iterator
  def getNextQuestionId: Int = questionIdGenerator.next
  def getResults: String = {
    val result = questions.foldLeft("Questions:") {
      case (result_, (id, question)) => {
        val innerResult = if (question.answers.isEmpty) "No answers" else question.answers.foldLeft("Answers:") {
          case (questionResult, (sender, answer)) => {
            val answerResults = answer match {
              case freeAnswer: FreeAnswer => freeAnswer.answer
              case choiceAnswer: ChoiceAnswer => choiceAnswer.answer.toString
              case multiAnswer: MultiChoiceAnswer => multiAnswer.answer.mkString(" ")
            }
            s"$questionResult\r\n\t${if(anonymous) "" else sender + ":"} $answerResults"
          }
        }
        val temp = innerResult.split("\n").map(line => s"\t$line").mkString("\n")
        s"${result_}\r\n\t${question.question} (id: $id):$temp"
      }
    }
    s"Poll results:\r\n$result"
  }
}
