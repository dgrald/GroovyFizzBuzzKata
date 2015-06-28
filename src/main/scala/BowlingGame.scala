/**
 * Created by dylangrald on 6/27/15.
 */
object BowlingGame {
  def getInstance(): BowlingGame = getInstance(BowlingScorer.getInstance)
  def getInstance(calculator: BowlingScorer): BowlingGame = new BowlingGameImplementation(calculator)
}

abstract class BowlingGame {
  def roll(rollScore: Int): Unit
  def score: Int
}

private class BowlingGameImplementation(calculator: BowlingScorer) extends BowlingGame {

  private var rolls = List[Int]()
  private var frames = List[BowlingFrame]()
  private var frameNum = 1
  private var rollNum = 1

  override def score: Int = {
    calculator.calculateScores(frames)
  }

  override def roll(rollScore: Int): Unit = {
    if(frameNum <= 10) {
      addRegularFrameScore(rollScore)
    } else {
      addScoreForBonusFrame(rollScore)
    }
    rolls = rolls :+ rollScore
  }

  private def isStrike(rollScore: Int): Boolean = {
    rollScore == 10 && rollNum == 1
  }

  private def addScoreForBonusFrame(rollScore: Int) = {
    if(rollNum == 1) {
      val tenthFrame = frames.last
      if(tenthFrame.isStrike) {
        rollNum = 2
      } else if (tenthFrame.isSpare) {
        frames = frames :+ new BonusBowlingFrame(rollScore, None)
      }
    } else {
      frames = frames :+ new BonusBowlingFrame(rolls.last, Some(rollScore))
    }
  }

  private def addRegularFrameScore(rollScore: Int): Unit = {
    if (isStrike(rollScore)) {
      frames = frames :+ new RegularBowlingFrame(10, 0)
      frameNum += 1
    } else {
      if (rollNum == 1) {
        rollNum = 2
      } else {
        frames = frames :+ new RegularBowlingFrame(rolls.last, rollScore)
        rollNum = 1
        frameNum += 1
      }
    }
  }
}
