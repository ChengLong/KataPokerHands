package scala.kata.poker

/**
  * Created by chengl on 4/12/15.
  */
case class Game(player1: Player, player2: Player) {

  import scala.math.Ordering.Implicits._

  private val equalHandComparator: PartialFunction[(Player, Player), Result] = {
    case (Player(_, h1), Player(_, h2)) if h1.category == h2.category && getHighCard(h1.differentiators, h2.differentiators).isEmpty =>
      Tie
  }

  private val differentCategoryComparator: PartialFunction[(Player, Player), Result] = {
    case (Player(n1, h1), Player(n2, h2)) if h1.category != h2.category =>
      if (h1.categoryRank > h2.categoryRank) Winner(n1, h1.category.getSimpleName)
      else Winner(n2, h2.category.getSimpleName)
  }

  private val highCardComparator: PartialFunction[(Player, Player), Result] = {
    case (Player(n1, h1), Player(n2, h2)) if h1.categorizedHand.isInstanceOf[HighCard] =>
      compareByHighCard(n1, h1.differentiators, n2, h2.differentiators)
  }

  private val onePairComparator: PartialFunction[(Player, Player), Result] = {
    case (p1@Player(n1, h1), p2@Player(n2, h2)) if h1.categorizedHand.isInstanceOf[OnePair] =>
      compareByConstructsFirstThenHighCard(p1, p2, 2, "bigger pair of ")
  }

  private val twoPairsComparator: PartialFunction[(Player, Player), Result] = {
    case (p1@Player(n1, h1), p2@Player(n2, h2)) if h1.categorizedHand.isInstanceOf[TwoPairs] =>
      compareByConstructsFirstThenHighCard(p1, p2, 4, "bigger pair of ")
  }

  private val threeOfKindComparator: PartialFunction[(Player, Player), Result] = {
    case (p1@Player(n1, h1), p2@Player(n2, h2)) if h1.categorizedHand.isInstanceOf[ThreeOfKind] =>
      compareByConstructsFirstThenHighCard(p1, p2, 3, "bigger three of ")
  }

  private val straightComparator: PartialFunction[(Player, Player), Result] = {
    case (p1@Player(n1, h1), p2@Player(n2, h2)) if h1.categorizedHand.isInstanceOf[Straight] =>
      compareByConstructsFirstThenHighCard(p1, p2, 1, "bigger straight of ")
  }

  private val flushComparator: PartialFunction[(Player, Player), Result] = {
    case (p1@Player(n1, h1), p2@Player(n2, h2)) if h1.categorizedHand.isInstanceOf[Flush] =>
      compareByConstructsFirstThenHighCard(p1, p2, Hand.CardInOneHand, "bigger flush with high card ")
  }

  private val fullHouseComparator: PartialFunction[(Player, Player), Result] = {
    case (p1@Player(n1, h1), p2@Player(n2, h2)) if h1.categorizedHand.isInstanceOf[FullHouse] =>
      compareByConstructsFirstThenHighCard(p1, p2, 3, "bigger full house with three of ")
  }

  private val fourOfKindComparator: PartialFunction[(Player, Player), Result] = {
    case (p1@Player(n1, h1), p2@Player(n2, h2)) if h1.categorizedHand.isInstanceOf[FourOfKind] =>
      compareByConstructsFirstThenHighCard(p1, p2, 1, "bigger four of ")
  }

  private val straightFlushComparator: PartialFunction[(Player, Player), Result] = {
    case (p1@Player(n1, h1), p2@Player(n2, h2)) if h1.categorizedHand.isInstanceOf[StraightFlush] =>
      compareByConstructsFirstThenHighCard(p1, p2, 1, "bigger straight flush of ")
  }

  private val comparator =
    equalHandComparator orElse
      differentCategoryComparator orElse
      straightFlushComparator orElse
      fourOfKindComparator orElse
      fullHouseComparator orElse
      flushComparator orElse
      straightComparator orElse
      threeOfKindComparator orElse
      twoPairsComparator orElse
      onePairComparator orElse
      highCardComparator

  def judge: Result = {
    comparator(player1, player2)
  }

  private def getHighCard(diff1: Seq[Card], diff2: Seq[Card]): Option[Card] = {
    (diff1 zip diff2).collectFirst { case (c1, c2) if c1.rank != c2.rank => Seq(c1, c2).max }
  }

  private def compareByHighCard(name1: String, diff1: Seq[Card], name2: String, diff2: Seq[Card]): Winner = {
    val high = getHighCard(diff1, diff2)
    if (diff1 < diff2) Winner(name2, s"high card ${high.get.value}")
    else Winner(name1, s"high card ${high.get.value}")
  }

  private def compareByConstructsFirstThenHighCard(p1: Player, p2: Player, cardsToConsider: Int, message: String): Result = {
    val (diff1, diff2) = (p1.hand.differentiators, p2.hand.differentiators)
    getHighCard(diff1.take(cardsToConsider), diff2.take(cardsToConsider)) match {
      case None => compareByHighCard(p1.name, diff1.drop(cardsToConsider), p2.name, diff2.drop(cardsToConsider))
      case Some(highCard) =>
        val winner = if (diff1 > diff2) p1.name else p2.name
        Winner(winner, message + highCard.value)
    }
  }
}

object Game {
  private val ValidGame = """([\w\s:]+)\s{2,}([\w\s:]+)""".r

  def apply(input: String): Game = input match {
    case ValidGame(player1, player2) => Game(Player(player1), Player(player2))
    case _ => throw new IllegalArgumentException(s"Invalid Game: ${input}")
  }
}
