package main

import info.mukel.telegrambot4s._
import api._
import command.{CommandExecutor, CommandParser}
import methods._
import models._
import declarative._


object Bot extends TelegramBot with Polling with Commands {

  override def token: String = ""

  override def receiveMessage(msg: Message): Unit = {
    val text = msg.text.getOrElse("")
    val sender = msg.from.map(user => user.username.getOrElse("unknown_sender")).getOrElse("unknown_sender")
    val answer = execute(sender, text)
    request(SendMessage(msg.source, answer))
  }

  def main(args: Array[String]): Unit = {
    run()
  }

  def execute(sender: String, command: String): String = {
    CommandParser.parseCommand(command) match {
      case CommandParser.Success(cmd, _) => CommandExecutor.execute(sender, cmd)
      case _ => s"Error: Command not recognized"
    }
  }
}