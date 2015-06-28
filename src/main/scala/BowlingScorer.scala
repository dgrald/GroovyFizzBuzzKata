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
      val scoreForFrame = scoreFrame(firstFrame, restOfFrames)
      if(scoreOfGameIsComplete(frameNum, restOfFrames)) {
        totalScore + scoreForFrame
      } else {
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

  private def totalFrameForStrike(framesRelevantToScore: Seq[BowlingFrame]): Int = framesRelevantToScore match {
    case Seq() => 0
    case Seq(oneFrame) => oneFrame.total
    case first +: second +: _ => scoreFrame(first, List(second))
  }

  private def totalFrameForSpare(frames: Seq[BowlingFrame]): Int = frames match {
    case Seq() => 0
    case firstFrame +: _ =>
      if(firstFrame.isStrike) {
        scoreFrame(firstFrame, List())
      } else {
        firstFrame.firstRoll
      }
  }

  private def scoreOfGameIsComplete(frameNum: Int, restOfFrames: Seq[BowlingFrame]): Boolean = {
    frameNum >= 10 && restOfFrames.size <= 1
  }

}