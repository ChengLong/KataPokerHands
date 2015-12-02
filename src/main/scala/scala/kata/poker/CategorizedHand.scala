package scala.kata.poker

/**
  * Created by chengl on 4/12/15.
  */
sealed trait CategorizedHand {
  def category: Class[_ <: CategorizedHand] = this.getClass

  def categoryRank: Int

  def differentiators: Seq[Card]
}

case class StraightFlush(differentiators: Seq[Card]) extends CategorizedHand {
  override def categoryRank: Int = 9
}

case class FourOfKind(differentiators: Seq[Card]) extends CategorizedHand {
  override def categoryRank: Int = 8
}

case class FullHouse(differentiators: Seq[Card]) extends CategorizedHand {
  override def categoryRank: Int = 7
}

case class Flush(differentiators: Seq[Card]) extends CategorizedHand {
  override def categoryRank: Int = 6
}

case class Straight(differentiators: Seq[Card]) extends CategorizedHand {
  override def categoryRank: Int = 5
}

case class ThreeOfKind(differentiators: Seq[Card]) extends CategorizedHand {
  override def categoryRank: Int = 4
}

case class TwoPairs(differentiators: Seq[Card]) extends CategorizedHand {
  override def categoryRank: Int = 3
}

case class OnePair(differentiators: Seq[Card]) extends CategorizedHand {
  override def categoryRank: Int = 2
}

case class HighCard(differentiators: Seq[Card]) extends CategorizedHand {
  override def categoryRank: Int = 1
}