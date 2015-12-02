package scala.kata.poker

case class Card(value: Char, suit: Char) extends Ordered[Card] {
  def rank: Int = value match {
    case 'T' => 10
    case 'J' => 11
    case 'Q' => 12
    case 'K' => 13
    case 'A' => 14
    case a => a.asDigit
  }

  override def compare(that: Card): Int = this.rank - that.rank
}

object Card {
  private val ValidCard = """([2-9TJQKA])([HDSC])""".r

  def apply(s: String): Card = s match {
    case ValidCard(value, suit) => Card(value.head, suit.head)
    case _ => throw new IllegalArgumentException(s"Invalid card: ${s}")
  }
}