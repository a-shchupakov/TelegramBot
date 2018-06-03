package command

import java.util.Date

import poll.content.AnswerInstance

sealed trait Command


case class CreatePoll(pollName: String,
                      anonymous: Option[Boolean],
                      visible: Option[Boolean],
                      begin: Option[Date], end: Option[Date]) extends Command

case class CreateQuestion(question: String, _type: Option[String], answers: List[String]) extends Command

case class DeletePoll(id: Int) extends Command

case class DeleteQuestion(id: Int) extends Command

case class StartPoll(id: Int) extends Command

case class StopPoll(id: Int) extends Command

case class OpenPoll(id: Int) extends Command

case class ClosePoll() extends Command

case class SetAnswer(id: Int, answer: AnswerInstance) extends Command

case class GetPolls() extends Command

case class GetContent() extends Command

case class GetResults(id: Int) extends Command

