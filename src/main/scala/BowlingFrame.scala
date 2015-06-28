/**
 * Created by dylangrald on 6/25/15.
 */
abstract class BowlingFrame {
  def firstRoll: Int
  def secondRoll: Int
  def isStrike: Boolean
  def isSpare: Boolean

  def total: Int = firstRoll + secondRoll
}

class RegularBowlingFrame(val firstRoll: Int, val secondRoll: Int) extends BowlingFrame {
  override def isStrike = {
    firstRoll == 10
  }

  override def isSpare = {
    firstRoll + secondRoll == 10 && !isStrike
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[RegularBowlingFrame]

  override def equals(other: Any): Boolean = other match {
    case that: RegularBowlingFrame =>
      (that canEqual this) &&
        firstRoll == that.firstRoll &&
        secondRoll == that.secondRoll
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(firstRoll, secondRoll)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString = s"RegularBowlingFrame($firstRoll, $secondRoll)"
}

class BonusBowlingFrame(val firstRoll: Int) extends BowlingFrame {
  override def secondRoll: Int = 0

  override def isStrike: Boolean = false

  override def isSpare: Boolean = false

  def canEqual(other: Any): Boolean = other.isInstanceOf[BonusBowlingFrame]

  override def equals(other: Any): Boolean = other match {
    case that: BonusBowlingFrame =>
      (that canEqual this) &&
        firstRoll == that.firstRoll
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(firstRoll)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
