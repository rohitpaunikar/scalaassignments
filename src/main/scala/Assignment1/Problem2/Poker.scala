package Assignment1.Problem2

import java.io.File

import scala.io.Source

object Poker extends App {

  /**
   * Enum for representing suit of the cards
   * H - Hearts, C - Clubs, S - Spades, D - Diamonds
   */
  object Suit extends Enumeration {
    val H,C,S,D  = Value
  }

  /**
   * Enum for representing Face of the cards
   * TWO - 2, THREE - 3, FOUR - 4, FIVE - 5, SIX - 6, SEVEN - 7, EIGHT - 8, NINE - 9,
   * TEN - 10(T), JACK - J, QUEEN - Q, KING - K, ACE - A
   */
  object FaceOfTheCard extends Enumeration {
    val TWO  = Value("2"); val THREE = Value("3"); val FOUR = Value("4"); val FIVE = Value("5");
    val SIX = Value("6"); val SEVEN = Value("7"); val EIGHT = Value("8"); val NINE = Value("9");
    val TEN = Value("T"); val JACK = Value("J"); val QUEEN = Value("Q"); val KING = Value("K");
    val ACE = Value("A")
  }

  /**
   * An implicit ordering implementation for sorting the Face of the cards in descending order
   */
  implicit object DescendingOrderingFace extends Ordering[FaceOfTheCard.Value] {
    override def compare(a:FaceOfTheCard.Value, b:FaceOfTheCard.Value) :Int = b.id compare a.id
  }

  /**
   * Card represents a single card
   * @param card String representation of a card. for e.g., if the card is King of Hearts, its
   *             string representation would be KH i.e. Face + Suit
   */
  case class Card(card: String){
    private val length = card.length
    val face = FaceOfTheCard.withName(card.substring(0,1))
    val suit = Suit.withName(card.substring(1, 2))
  }

  /**
   * An implicit ordering implementation for sorting the cards in descending order based on the
   * Face of the card
   */
  implicit object DescendingOrderingCard extends Ordering[Card] {
    override def compare(a:Card, b:Card) :Int = b.face.id compare a.face.id
  }

  /**
   * Hand represents a hand of the poker. Each hand would contain five cards
   * @param cards List of 5 cards
   */

  case class Hand(cards: List[Card]){

    private lazy val sortedCards = cards.sorted

    private lazy val countByFace = cards.groupBy(c => c.face).map(e => (e._1 -> e._2.count(_ => true))).toList

    private def areALLCardsSameSuit = cards.forall(card => card.suit == cards.head.suit)

    private def areCardsSeq(highestCard:Card, remainingCards: List[Card]): Boolean =
      remainingCards match {
        case Nil => true
        case h :: t => if (h.face.id == (highestCard.face.id - 1)) areCardsSeq(h,t)
        else false
      }

    private def isRoyalFlush = areALLCardsSameSuit && areCardsSeq(sortedCards.head, sortedCards.tail) &&
                        sortedCards.head.face == FaceOfTheCard.ACE

    private def isStraightFlush = areALLCardsSameSuit && areCardsSeq(sortedCards.head, sortedCards.tail)

    private def isFourOfAKind = countByFace.exists(t => t._2 == 4)

    private def isFullHouse = countByFace.exists(t => t._2 == 3) &&
                              countByFace.exists(t => t._2 == 2)

    private def isFlush = areALLCardsSameSuit

    private def isStraight = areCardsSeq(sortedCards.head, sortedCards.tail)

    private def isThreeOfaKind = countByFace.exists(t => t._2 == 3)

    private def isTwoPairs = countByFace.toMap.values.count(count => count == 2) == 2

    private def isOnePair = countByFace.exists(t => t._2 == 2)

    /**
     * A score calculated based on the hand. These scores are used to determine the winner
     */
    val score: Int = {
      if (isRoyalFlush) 200
      else if (isStraightFlush) 180 + sortedCards.head.face.id
      else if (isFourOfAKind) 160 + countByFace.filter(p => p._2 == 4).head._1.id
      else if (isFullHouse) 140 + countByFace.filter(p => p._2 == 3).head._1.id
      else if (isFlush) 120 + sortedCards.head.face.id
      else if (isStraight) 100 + sortedCards.head.face.id
      else if (isThreeOfaKind) 80 + countByFace.filter(p => p._2 == 3).head._1.id
      else if (isTwoPairs) 60 + countByFace.filter(p => p._2 == 2).sorted.head._1.id
      else if (isOnePair) 40 + countByFace.filter(p => p._2 == 2).head._1.id
      else sortedCards.head.face.id
    }
  }
  object Hand{

    /**
     * If both the Player's hands have same score, it recursively checks for high cards
     * for all the possible combinations
     * @param hand1
     * @param hand2
     * @return 0 if both the hands are same
     *         -1 if hand1 < hand2
     *         1 if hand1 > hand2
     */
    def compareHandsWithSameScore(hand1: Hand, hand2: Hand): Int = {
      def compareEachCard(cards1:List[FaceOfTheCard.Value], cards2:List[FaceOfTheCard.Value]) : Int = {
        cards1 match {
          case Nil => 0
          case h1 :: t1 =>
            if (h1.id > cards2.head.id) 1
            else if (h1.id < cards2.head.id) -1
            else compareEachCard(t1, cards2.tail)
        }
      }

      if ((hand1.score >= 180 && hand1.score < 200) || (hand1.score >= 120 && hand1.score < 140) || (hand1.score >= 0 && hand1.score < 40))
        compareEachCard(hand1.sortedCards.tail.map(c => c.face), hand2.sortedCards.tail.map(c => c.face))
      else if (hand1.score >= 60 && hand1.score < 80)
        compareEachCard(hand1.countByFace.filter(p => p._2 == 2).sorted.tail.head._1 :: List(hand1.countByFace.filter(p => p._2 == 1).head._1),
                          hand2.countByFace.filter(p => p._2 == 2).sorted.tail.head._1 :: List(hand2.countByFace.filter(p => p._2 == 1).head._1))
      else if (hand1.score >= 40 && hand1.score < 60)
        compareEachCard(hand1.countByFace.filter(p => p._2 == 1).map(e => e._1).sorted,hand2.countByFace.filter(p => p._2 == 1).map(e => e._1).sorted)
      else 0
    }
  }

  /**
   * Player represents a player of poker game
   * @param name name of the player
   * @param hand hand of the player
   */
  case class Player(name:String, hand: Hand)

  /**
   * Game represents a poker game between 2 players
   * @param player1
   * @param player2
   */
  case class Game(player1: Player, player2:Player){
    val player1Score  = player1.hand.score
    val player2Score  = player2.hand.score

    /**
     * returns the winner player based on the scores of their hands
     * @return Some[Player] if there is a clear winner
     *         None if there is a draw
     */
    def getWinner:Option[Player] = {
      if (player1Score > player2Score) Some(player1)
      else if (player2Score > player1Score) Some(player2)
      else {
        val comparsionResult = Hand.compareHandsWithSameScore(player1.hand,player2.hand)
        if (comparsionResult < 0) Some(player2)
        else if (comparsionResult > 0) Some(player1)
        else None
      }
    }

  }

  println( "Player 1 won " +
    Source.fromFile(new File(System.getProperty("user.dir") + "\\src\\resources\\poker.txt")).getLines().map(
      line => {
        Game (
          Player("Player1", Hand(line.split(" ").take(5).map(s => Card(s)).toList)),
          Player("Player2", Hand(line.split(" ").takeRight(5).map(s => Card(s)).toList))
        ).getWinner.get
      }
    ).count(p => p.name == "Player1")
  + " games")
}
