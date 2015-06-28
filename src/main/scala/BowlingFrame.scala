/**
 * Created by dylangrald on 6/25/15.
 */

object BowlingFrame {
  def strike: BowlingFrame = new RegularBowlingFrame(10, 0)
  def gutterBall: BowlingFrame = new RegularBowlingFrame(0, 0)
}

abstract class BowlingFrame {
  def firstRoll: Int
  def secondRoll: Int
  def isStrike: Boolean
  def isSpare: Boolean
}

class RegularBowlingFrame(val firstRoll: Int, val secondRoll: Int) extends BowlingFrame {
  override def isStrike = {
    firstRoll == 10
  }

  override def isSpare = {
    firstRoll + secondRoll == 10 && !isStrike
  }
}

class BonusBowlingFrame(val firstRoll: Int, val secondRollOption: Option[Int]) extends BowlingFrame {
  override def secondRoll: Int = 0

  override def isStrike: Boolean = false

  override def isSpare: Boolean = false
}
