package poll

import java.util.Date

import content.Question

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
    "results" //todo: format results.
  }
}

