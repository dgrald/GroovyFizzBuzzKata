import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by dylangrald on 6/25/15.
 */
class BowlingFrameTests extends FlatSpec with Matchers {

  "The bowling frame" should "return the correct pins knocked down" in {
    val firstRoll = 1
    val secondRoll = 2
    val frame = new RegularBowlingFrame(1,2)

    assert(frame.firstRoll == firstRoll)
    assert(frame.secondRoll == secondRoll)
  }

  it should "determine whether it's a spare" in {
    val spare = new RegularBowlingFrame(9, 1)

    assert(spare.isSpare)
    assert(!spare.isStrike)
  }

  it should "determine whether it's a strike" in {
    val strike = new RegularBowlingFrame(10, 0)

    assert(strike.isStrike)
    assert(!strike.isSpare)
  }
}
