package main

import scala.io.Source

object FileExecutor extends App {
  val path: String = "D:\\IT\\Scala\\Projects\\TelegramBot\\input.txt"
  Source.fromFile(path).getLines().foreach(message => {
    val temp = message.split("> ")
    val sender = temp(0)
    val command = temp(1)
    val result = Bot.execute(sender, command)
    println(s"Sending '$command' (from $sender). Answer: $result")
  })
}
