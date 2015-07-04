import scala.annotation.tailrec

/**
 * Created by dylangrald on 6/25/15.
 */
object BowlingScorer {
  def apply(): BowlingScorer = new BowlingScorerImplementation()
}

abstract class BowlingScorer {
  def calculateScores(scores: Seq[BowlingFrame]): Int
}

private class BowlingScorerImplementation extends BowlingScorer {
  override def calculateScores(scores: Seq[BowlingFrame]): Int = {
    calculateScoresRecursively(scores, 0, 1)
  }

  @tailrec
  private def calculateScoresRecursively(scores: Seq[BowlingFrame], totalScore: Int, frameNum: Int): Int = scores match {
    case Seq() => totalScore
    case firstFrame +: restOfFrames =>
      val scoreForFrame = scoreFrame(firstFrame, restOfFrames)
      val newTotalScore = totalScore + scoreForFrame
      if(scoreOfGameIsComplete(frameNum, restOfFrames)) {
        newTotalScore
      } else {
        calculateScoresRecursively(scores.tail, newTotalScore, frameNum + 1)
      }
  }

  private def scoreFrame(firstFrame: BowlingFrame, restOfFrames: Seq[BowlingFrame]): Int = restOfFrames match {
    case Seq() => firstFrame.total
    case _ =>
      if(firstFrame.isStrike) {
        firstFrame.total + totalBonusForStrike(restOfFrames)
      } else if(firstFrame.isSpare) {
        firstFrame.total + totalBonusForSpare(restOfFrames)
      } else {
        firstFrame.total
      }
  }

  private def totalBonusForStrike(framesRelevantToScore: Seq[BowlingFrame]): Int = framesRelevantToScore match {
    case Seq() => 0
    case Seq(oneFrame) => oneFrame.total
    case first +: second +: _ =>
      if(first.isStrike) {
        first.total + second.firstRoll
      } else {
        first.total
      }
  }

  private def totalBonusForSpare(framesRelevantToScore: Seq[BowlingFrame]): Int = framesRelevantToScore match {
    case Seq() => 0
    case firstFrame +: _ => firstFrame.firstRoll
  }

  private def scoreOfGameIsComplete(frameNum: Int, restOfFrames: Seq[BowlingFrame]): Boolean = {
    frameNum >= 10 && restOfFrames.size <= 1
  }
}