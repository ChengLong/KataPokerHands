package scala.kata.poker

case class Hand(cards: Seq[Card]) {
  lazy val categorizedHand: CategorizedHand = this match {
    case StraightFlushExtractor(categorized) => categorized
    case FourOfKindExtractor(categorized) => categorized
    case FullHouseExtractor(categorized) => categorized
    case FlushExtractor(categorized) => categorized
    case StraightExtractor(categorized) => categorized
    case ThreeOfKindExtractor(categorized) => categorized
    case TwoPairsExtractor(categorized) => categorized
    case OnePairExtractor(categorized) => categorized
    case HighCardExtractor(categorized) => categorized
    case _ => throw new RuntimeException(s"Can't compute score of the hand: ${cards}")
  }

  def categoryRank = categorizedHand.categoryRank

  def category = categorizedHand.category

  def differentiators = categorizedHand.differentiators
}

object Hand {
  val CardInOneHand = 5
  private val ValidCard = """[2-9TJQKA][HDSC]""".r
  private val ValidHand = ((1 to CardInOneHand).map(_ => s"(${ValidCard})".r)).mkString("\\s*").r

  def apply(s: String): Hand = s match {
    case ValidHand(cards@_*) => new Hand(cards.map(Card(_)))
    case _ => throw new IllegalArgumentException(s"Invalid hand: ${s}")
  }
}

object StraightFlushExtractor {
  def unapply(h: Hand): Option[CategorizedHand] = {
    val cards = h.cards.sorted.reverse
    val isFlush = cards.forall(_.suit == cards.head.suit)
    val isStraight = cards.map(_.rank - cards.last.rank) == (0 until Hand.CardInOneHand).reverse

    if (!(isFlush && isStraight)) None
    else Some(StraightFlush(Seq(cards.head)))
  }
}

object FourOfKindExtractor {
  def unapply(h: Hand): Option[CategorizedHand] = {
    val cards = h.cards.sorted.reverse
    val fourOfKind = cards.collectFirst { case c if cards.count(_.rank == c.rank) == 4 => c }

    if (fourOfKind.isEmpty) None
    else Some(FourOfKind(Seq(fourOfKind.get)))
  }
}

object FullHouseExtractor {
  def unapply(h: Hand): Option[CategorizedHand] = {
    val cards = h.cards.sorted.reverse
    val grouped = cards.groupBy(_.rank)
    val three = grouped.collectFirst { case (_, cards) if cards.size == 3 => cards }
    val two = grouped.collectFirst { case (_, cards) if cards.size == 2 => cards }

    if (three.isEmpty || two.isEmpty) None
    else Some(FullHouse(Seq(three.get.head)))
  }
}

object FlushExtractor {
  def unapply(h: Hand): Option[CategorizedHand] = {
    val cards = h.cards.sorted.reverse
    if (!cards.forall(_.suit == cards.head.suit)) None
    else Some(Flush(cards))
  }
}

object StraightExtractor {
  def unapply(h: Hand): Option[CategorizedHand] = {
    val cards = h.cards.sorted.reverse
    val isStraight = cards.map(_.rank - cards.last.rank) == (0 until Hand.CardInOneHand).reverse

    if (!isStraight) None
    else Some(Straight(Seq(cards.head)))
  }
}

object ThreeOfKindExtractor {
  def unapply(h: Hand): Option[CategorizedHand] = {
    val cards = h.cards.sorted.reverse
    val threeOfKind = cards.collectFirst { case c if cards.count(_.rank == c.rank) == 3 => c }

    if (threeOfKind.isEmpty) None
    else Some(ThreeOfKind(Seq(threeOfKind.get)))
  }
}

object TwoPairsExtractor {
  def unapply(h: Hand): Option[CategorizedHand] = {
    val cards = h.cards.sorted.reverse
    val grouped = cards.groupBy(_.rank)
    val pairs = grouped.filter(_._2.size == 2)

    if (pairs.size != 2) None
    else {
      val pairCards = pairs.values.flatten.toSeq.sorted.reverse
      val cardsInOrder = pairCards ++ (cards diff pairCards)
      Some(TwoPairs(cardsInOrder))
    }
  }
}

object OnePairExtractor {
  def unapply(h: Hand): Option[CategorizedHand] = {
    val cards = h.cards.sorted.reverse
    val grouped = cards.groupBy(_.rank)
    val pairs = grouped.filter(_._2.size == 2)

    if (pairs.size != 1) None
    else {
      val pairCards = pairs.values.flatten.toSeq
      val cardsInOrder = pairCards ++ (cards diff pairCards)
      Some(OnePair(cardsInOrder))
    }
  }
}

object HighCardExtractor {
  def unapply(h: Hand): Option[CategorizedHand] = {
    val cards = h.cards.sorted.reverse
    Some(HighCard(cards))
  }
}
