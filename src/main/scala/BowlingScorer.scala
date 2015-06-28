import scala.annotation.tailrec

/**
 * Created by dylangrald on 6/25/15.
 */
object BowlingScorer {
  def getInstance: BowlingScorer = new BowlingScorerImplementation()
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
      if(frameNum == 10) {
        totalScore + totalLastFrame(firstFrame, restOfFrames)
      } else {
        val scoreForFrame = scoreFrame(firstFrame, restOfFrames)
        calculateScoresRecursively(scores.tail, totalScore + scoreForFrame, frameNum + 1)
      }
  }

  private def scoreFrame(firstFrame: BowlingFrame, restOfFrames: Seq[BowlingFrame]): Int = restOfFrames match {
    case Seq() => firstFrame.total
    case _ =>
      if(firstFrame.isStrike) {
        firstFrame.total + totalFrameForStrike(restOfFrames)
      } else if(firstFrame.isSpare) {
        firstFrame.total + totalFrameForSpare(restOfFrames)
      } else {
        firstFrame.total
      }
  }

  def totalFrameForStrike(framesRelevantToScore: Seq[BowlingFrame]): Int = framesRelevantToScore match {
    case Seq() => 0
    case Seq(oneFrame) => oneFrame.firstRoll + oneFrame.secondRoll
    case first +: second +: _ => scoreFrame(first, List(second))
  }

  def totalFrameForSpare(frames: Seq[BowlingFrame]): Int = frames match {
    case Seq() => 0
    case first +: _ =>
      if(first.isStrike) {
        scoreFrame(first, List())
      } else {
        first.firstRoll
      }
  }


  def totalLastFrame(frame: BowlingFrame, frames: Seq[BowlingFrame]): Int = {
    if(frame.isStrike || frame.isSpare) {
      frames match {
        case firstBonus +: secondBonus +: _ => frame.total + firstBonus.firstRoll + secondBonus.firstRoll
        case Seq(oneBonus) => frame.total + oneBonus.firstRoll
        case _ => frame.total
      }
    } else {
      frame.total
    }
  }

}