package poll

object PollDataBase {
  var polls: Map[Int, Poll] = Map[Int, Poll]()
  var selectedPollMap: Map[String, Int] = Map[String, Int]()

  private val pollIdGenerator = Stream.from(1).iterator
  def getNextPollId: Int = pollIdGenerator.next

  def addPoll(id: Int, poll: Poll): Unit = polls += id -> poll
  def deletePoll(id: Int): Unit = {
    polls -= id
    selectedPollMap
      .keys
      .filter(user => selectedPollMap(user) == id)
      .foreach(user => closePoll(user))
  }
  def openPoll(user: String, id: Int): Unit = selectedPollMap += user -> id
  def closePoll(user: String): Unit = selectedPollMap -= user
  def getOpenedPoll(user: String): Option[Poll] = {
    getOpenedPollId(user) match {
      case -1 => None
      case id => polls.get(id)
    }
  }
  def getOpenedPollId(user: String): Int = selectedPollMap.getOrElse(user, -1)
  def setState(id: Int, state: Boolean): Unit = addPoll(id, PollManager.setState(polls(id), state))
}
