package poll.content

import scala.util.{Success, Try}

sealed trait Answer

case class FreeAnswer(answer: String) extends Answer
case class ChoiceAnswer(answer: Int) extends Answer
case class MultiChoiceAnswer(answer: List[Int]) extends Answer

case class AnswerInstance(answer: String){
  def createFreeAnswer(): Option[FreeAnswer] = Option(FreeAnswer(answer))

  def createChoiceAnswer(): Option[ChoiceAnswer] = {
    Try(ChoiceAnswer(Integer.parseInt(answer))) match {
      case Success(createdAnswer) => Option(createdAnswer)
      case _ => None
    }
  }
  def createMultiAnswer(): Option[MultiChoiceAnswer] = {
    Try(MultiChoiceAnswer(answer.split(" ").map(id => Integer.parseInt(id)).toList)) match {
      case Success(createdAnswer) => Option(createdAnswer)
      case _ => None
    }
  }
}