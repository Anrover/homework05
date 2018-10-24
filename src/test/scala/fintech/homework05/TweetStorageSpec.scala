package fintech.homework05

import org.scalatest.{FlatSpec, Matchers, Inside}
import Inside._

class TweetStorageSpec extends FlatSpec with Matchers {
  val tweet1 = Tweet("1", "Vova", "firstTweetVova", likes = 0)
  val tweet1Copy = Tweet("1", "Vova", "firstTweetVova", likes = 0)
  val tweet1WithNewLikes = Tweet("1", "Vova", "firstTweetVova", likes = 5)
  val tweet2 = Tweet("2", "Vova", "secondTweetVova", likes = 0)
  val tweet3 = Tweet("3", "Roma", "firstTweetRoma", likes = 0)

  "LocalStorage" should "correct add tweet in Storage" in {
    val storage = new LocalStorage
    storage.putTweet(tweet1) should matchPattern { case Success(_) => }
    storage.putTweet(tweet2) should matchPattern { case Success(_) => }
    storage.putTweet(tweet1) should matchPattern { case Error(_) => }
  }

  it should "correct update tweet in Storage" in {
    val storage = new LocalStorage
    storage.putTweet(tweet1)

    val resUpdate = storage.updateTweet(tweet1WithNewLikes)
    resUpdate should matchPattern { case Success(_) => }
    inside (resUpdate) { case Success(tweet) => tweet.likes should be (5) }

    storage.updateTweet(tweet2) should matchPattern { case Error(_) => }
  }

  it should "correct return tweet from Storage" in {
    val storage = new LocalStorage
    storage.putTweet(tweet1)

    val resGet = storage.getTweet("1")
    resGet should matchPattern { case Success(_) => }
    inside (resGet) { case Success(tweet) => tweet should be (tweet1Copy) }

    storage.getTweet("123") should matchPattern { case Error(_) => }
  }
}
