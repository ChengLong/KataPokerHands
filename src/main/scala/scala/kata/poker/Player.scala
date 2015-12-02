package scala.kata.poker

case class Player(name: String, hand: Hand)

object Player {
  private val ValidPlayer = """(\w+):\s*([\w\s]+)""".r

  def apply(s: String): Player = s match {
    case ValidPlayer(name, hand) => Player(name, Hand(hand))
    case _ => throw new IllegalArgumentException(s"Invalid player: ${s}")
  }
}