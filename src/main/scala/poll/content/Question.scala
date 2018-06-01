package poll.content

sealed trait Question {
  def question: String
  def answers: Map[String, Answer]
}

case class FreeQuestion(question: String,
                        answers: Map[String, FreeAnswer]) extends Question
case class ChoiceQuestion(question: String,
                          answerVariants: Map[Int, String],
                          answers: Map[String, ChoiceAnswer]) extends Question
case class MultiQuestion(question: String,
                         answerVariants: Map[Int, String],
                         answers: Map[String, MultiChoiceAnswer]) extends Question
