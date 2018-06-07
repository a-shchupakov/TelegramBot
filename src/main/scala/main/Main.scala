package main


import command._

object Main extends App {
//  val sender: String = "Antony"
//  println(CommandExecutor.execute(sender, CreatePoll("Test poll", Option(true), Option(true), None, None)))
//  println(CommandExecutor.execute(sender, OpenPoll(1)))
//  println(CommandExecutor.execute(sender, CreateQuestion("How are you?", Option("free"), List.empty)))
//  println(CommandExecutor.execute(sender, CreateQuestion("What is your name?", Option("free"), List.empty)))
//  println(CommandExecutor.execute(sender, CreateQuestion("What is your gender?", Option("choice"), List[String]("male", "female"))))
//  println(CommandExecutor.execute(sender, GetContent()))

  val command = CommandParser.parseCommand("/create_poll (test_poll) (yes) (afterstop) (12:24:15 18:06:06)")
  println(command.get)
}
