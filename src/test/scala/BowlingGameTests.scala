import org.scalatest.mock.MockitoSugar
import org.scalatest.{Matchers, FlatSpec}
import org.mockito.Mockito._

/**
 * Created by dylangrald on 6/27/15.
 */
class BowlingGameTests extends FlatSpec with Matchers with MockitoSugar {

  "The game" should "return populate the frames correctly" in {
    val expectedScores = List.fill(10)(BowlingFrame(4,5))
    val expectedScore = 90
    val scorerMock = mock[BowlingScorer]
    when(scorerMock.calculateScores(expectedScores)).thenReturn(expectedScore)

    val bowlingGame = BowlingGame(scorerMock)
    (1 to 20).foreach(roll => if(roll % 2 == 1)bowlingGame.roll(4) else bowlingGame.roll(5))

    val actualScore = bowlingGame.score

    assert(actualScore == expectedScore)
  }

  it should "create the bonus frame correctly correctly when there is a strike on the last frame" in {
    val expectedScores = List.fill(10)(BowlingFrame(10,0)) :+ BowlingFrame(10) :+ BowlingFrame(10)
    val expectedScore = 300
    val scorerMock = mock[BowlingScorer]
    when(scorerMock.calculateScores(expectedScores)).thenReturn(expectedScore)

    val bowlingGame = BowlingGame(scorerMock)
    (1 to 12).foreach(roll => bowlingGame.roll(10))

    val actualScore = bowlingGame.score

    assert(actualScore == expectedScore)
  }

  it should "create the bonus frame correctly when there is a spare on the last frame" in {
    val expectedScores = List.fill(10)(BowlingFrame(5,5)) :+ BowlingFrame(5)
    val expectedScore = 150
    val scorerMock = mock[BowlingScorer]
    when(scorerMock.calculateScores(expectedScores)).thenReturn(expectedScore)

    val bowlingGame = BowlingGame(scorerMock)
    (1 to 21).foreach(roll => bowlingGame.roll(5))

    val actualScore = bowlingGame.score

    assert(actualScore == expectedScore)
  }

}
