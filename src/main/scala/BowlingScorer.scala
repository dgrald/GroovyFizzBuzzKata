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
    case firstFrame +: restOfFrames => {
      if(frameNum == 10) {
        return totalScore + totalLastFrame(firstFrame, restOfFrames)
      }
      val scoreForFrame = scoreFrame(firstFrame, restOfFrames)
      calculateScoresRecursively(scores.tail, totalScore + scoreForFrame, frameNum + 1)
    }
  }

  private def scoreFrame(firstFrame: BowlingFrame, restOfFrames: Seq[BowlingFrame]): Int = {

    val totalForFrame = firstFrame.firstRoll + firstFrame.secondRoll

    restOfFrames match {
      case Seq() => totalForFrame
      case _ =>
        if(firstFrame.isStrike) {
          return totalForFrame + totalFrameForStrike(restOfFrames)
        }

        if(firstFrame.isSpare) {
          return totalForFrame + totalFrameForSpare(restOfFrames)
        }
        totalForFrame
    }
  }

  def totalFrameForStrike(frames: Seq[BowlingFrame]): Int = frames match {
    case Seq() => 0
    case Seq(oneFrame) => oneFrame.firstRoll + oneFrame.secondRoll
    case first +: second +: _ => {
      scoreFrame(first, List(second))
    }
  }

  def totalFrameForSpare(frames: Seq[BowlingFrame]): Int = frames match {
    case Seq() => 0
    case first +: _ => {
      if(first.isStrike) {
        return scoreFrame(first, List())
      }
      first.firstRoll
    }
  }


  def totalLastFrame(frame: BowlingFrame, frames: Seq[BowlingFrame]): Int = {
    val lastFrameTotal = frame.firstRoll + frame.secondRoll
    if(frame.isStrike || frame.isSpare) {
      frames match {
        case Seq(someValue) => someValue match {
          case bonus: BonusBowlingFrame =>
            if(frame.isStrike) return lastFrameTotal + bonus.firstRoll + bonus.secondRollOption.getOrElse(0)
            return lastFrameTotal + bonus.firstRoll
          case _ => throw new IllegalArgumentException(s"The 11th frame must be a bonus frame but was: ${someValue}")
        }
      }
    }
    lastFrameTotal
  }

}