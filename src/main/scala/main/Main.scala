package main

import command.CommandExecutor
import poll.content.AnswerInstance

object Main extends App {

  val freeQuestion = AnswerInstance("some answer").createFreeAnswer()
  val choiceQuestion = AnswerInstance("3").createChoiceAnswer()
  val multiQuestion = AnswerInstance("1 2 dsa").createMultiAnswer()

  val sender: String = "Antony"
}
