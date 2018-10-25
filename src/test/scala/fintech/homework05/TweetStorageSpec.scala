package fintech.homework05

import org.scalatest.{FlatSpec, Matchers, Inside}
import Inside._

class TweetStorageSpec extends FlatSpec with Matchers {
  val tweet1 = Tweet("1", "Vova", "firstTweetVova", likes = 0)
  val tweet2 = Tweet("2", "Vova", "secondTweetVova", likes = 0)

  "LocalStorage" should "correct add tweet in Storage" in {
    val storage = new LocalStorage
    storage.putTweet(tweet1) should matchPattern { case Success(_) => }
    storage.putTweet(tweet2) should matchPattern { case Success(_) => }
  }
  it should "return error, when create tweet, if tweet with this id exist in Storage" in {
    val storage = new LocalStorage
    val messageError = s"A tweet with this id: ${tweet1.id} is already in storage"

    storage.putTweet(tweet1)
    val response = storage.putTweet(tweet1)
    response should matchPattern { case Error(_) => }
    inside(response) { case Error(message) => message should be (messageError) }
  }

  it should "correct update tweet in Storage" in {
    val storage = new LocalStorage
    val tweet1WithNewLikes = tweet1.copy(likes = 5)
    storage.putTweet(tweet1)

    val resUpdate = storage.updateTweet(tweet1WithNewLikes)
    resUpdate should matchPattern { case Success(_) => }
    inside (resUpdate) { case Success(tweet) => tweet.likes should be (5) }
  }
  it should "return error, when update tweet, if tweet with this id does not exist in Storage" in {
    val storage = new LocalStorage
    val messageError = s"A tweet with this id: ${tweet2.id} is not in storage"

    storage.putTweet(tweet1)
    val response = storage.updateTweet(tweet2)
    response should matchPattern { case Error(_) => }
    inside(response) { case Error(message) => message should be (messageError) }
  }

  it should "correct return tweet from Storage" in {
    val storage = new LocalStorage
    storage.putTweet(tweet1)

    val resGet = storage.getTweet("1")
    resGet should matchPattern { case Success(_) => }
    inside (resGet) { case Success(tweet) => tweet should be (tweet1.copy()) }
  }
  it should "return error, when getting tweet, if tweet with this id does not exist in Storage" in {
    val storage = new LocalStorage
    val nonexistentId = "123"
    val messageError = s"No tweet for id: $nonexistentId"

    storage.putTweet(tweet1)
    val response = storage.getTweet(nonexistentId)
    response should matchPattern { case Error(_) => }
    inside(response) { case Error(message) => message should be (messageError) }
  }
}
