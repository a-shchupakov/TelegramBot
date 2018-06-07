package command

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import poll.content.AnswerInstance

import scala.util.parsing.combinator.RegexParsers

object CommandParser extends RegexParsers {
  def number: Parser[Int] = """\d+""".r ^^ {_.toInt}
  def word: Parser[String] = """[\w\s,.:!?;'"-]+""".r
  def visible: Parser[Boolean] = ("afterstop" | "continuous") ^^ {_ == "afterstop"}
  def anonymous: Parser[Boolean] = ("yes" | "no") ^^ {_ == "yes"}
  def questionType: Parser[String] = "open" | "choice" | "multi"
  def date: Parser[Date] = {
    val format = new SimpleDateFormat("kk:mm:ss yy:MM:dd", Locale.ENGLISH)
    val threeValues: Parser[String] = (number <~ ":") ~ (number <~ ":") ~ number ^^ { case num1 ~ num2 ~ num3 => Array(num1, num2, num3).mkString(":") }
    threeValues ~ threeValues ^^ {case time ~ date => format.parse(time + " " + date)}
  }
  def brackets[T](parser: Parser[T]): Parser[T] = "(" ~> parser <~ ")"

  def createPoll: Parser[CreatePoll] =
    "/create_poll" ~>
    brackets(word) ~
    brackets(anonymous).? ~
    brackets(visible).? ~
    brackets(date).? ~
    brackets(date).? ^^
    {case name ~ anonymous ~ visible ~ start ~ end => CreatePoll(name, anonymous, visible, start, end)}

  def createQuestion: Parser[CreateQuestion] =
    "/add_question" ~>
      brackets(word) ~
      brackets(questionType).? ~
      brackets(word).* ^^
      {case question ~ type_ ~ questions => CreateQuestion(question, type_, questions)}

  def deletePoll: Parser[DeletePoll] = "/delete_poll" ~> brackets(number) ^^ DeletePoll
  def deleteQuestion: Parser[DeleteQuestion] = "/delete_question" ~> brackets(number) ^^ DeleteQuestion
  def startPoll: Parser[StartPoll] = "/start_poll" ~> brackets(number) ^^ StartPoll
  def stopPoll: Parser[StopPoll] = "/stop_poll" ~> brackets(number) ^^ StopPoll
  def openPoll: Parser[OpenPoll] = "/begin" ~> brackets(number) ^^ OpenPoll
  def closePoll: Parser[ClosePoll] = "/end" ^^ {_ => new ClosePoll}
  def setAnswer: Parser[SetAnswer] = "/answer" ~> brackets(number) ~ brackets(word) ^^ {case id ~ answer => SetAnswer(id, AnswerInstance(answer))}
  def getPolls: Parser[GetPolls] = "/list" ^^ {_ => new GetPolls}
  def getContent: Parser[GetContent] = "/view" ^^ {_ => new GetContent}
  def getResults: Parser[GetResults] = "results" ~> brackets(number) ^^ GetResults

  def anyCommand: Parser[Command] =
    createPoll | createQuestion |
    deletePoll | deleteQuestion |
    startPoll | stopPoll |
    openPoll | closePoll |
    setAnswer |
    getPolls | getContent | getResults

  def parseCommand(input: String): ParseResult[Command] = parse(anyCommand, input)
}
