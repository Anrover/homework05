package fintech.homework05
import org.scalatest.{FlatSpec, Matchers, Inside}
import Inside._

class TweetAppSpec extends FlatSpec with Matchers {
  val text1 = "Hello, Vasya #qwe#abc dfsaf #qwer123@mail_ru.com"
  val userName = "vova"

  "TweetApi" should "correct create tweet" in {
    val tweetApi = new TweetApi(new LocalStorage)
    val successTweet = tweetApi.createTweet(CreateTweetRequest(text1, userName))

    successTweet should matchPattern { case Success(_) => }
    inside (successTweet) { case Success(tweet) =>
      tweet should matchPattern { case Tweet(_, `userName`,
      `text1`, Seq("#qwe", "#abc", "#qwer123@mail_ru"), _, 0) => }
    }
  }
  it should "return error, when create tweet, if length Text > MaxLenTweet" in {
    val tweetApi = new TweetApi(new LocalStorage)
    val messageError = "The text length has been exceeded"
    val bigText: String = "abc"*300

    val response = tweetApi.createTweet(CreateTweetRequest(bigText, userName))
    response should matchPattern { case Error(_) => }
    inside(response) { case Error(message) => message should be (messageError) }
  }

  it should "correct like tweet" in {
    val tweetApi = new TweetApi(new LocalStorage)

    val response = tweetApi.createTweet(CreateTweetRequest(text1, userName))
    response match {
      case Success(tweet) =>
        inside(tweetApi.likeTweet(LikeRequest(tweet.id))) {
          case Success(tweetAfterLike) =>
            tweetAfterLike should be (tweet.copy(likes = tweet.likes + 1))
        }
      case _ => throw new Exception("Something went wrong")
    }
  }
  it should "return error, when like tweet, if tweet with this id does not exist in Storage" in {
    val tweetApi = new TweetApi(new LocalStorage)
    val nonexistentId = "123"
    val messageError = s"No tweet for id: $nonexistentId"

    val response = tweetApi.likeTweet(LikeRequest(nonexistentId))
    response should matchPattern { case Error(_) => }
    inside(response) { case Error(message) => message should be (messageError) }
  }

  it should "correct return tweet by id" in {
    val tweetApi = new TweetApi(new LocalStorage)

    val response = tweetApi.createTweet(CreateTweetRequest(text1, userName))
    response match {
      case Success(tweet) =>
        val successTweet2 = tweetApi.getTweet(GetTweetRequest(tweet.id))
        successTweet2 should matchPattern { case Success(_) => }
        inside(successTweet2) { case Success(tweet2) => tweet2 should be (tweet) }
      case _ => throw new Exception("Something went wrong")
    }
  }
  it should "return error, when getting tweet, if tweet with this id does not exist in Storage" in {
    val tweetApi = new TweetApi(new LocalStorage)
    val nonexistentId = "1244121"
    val messageError = s"No tweet for id: $nonexistentId"

    val response = tweetApi.getTweet(GetTweetRequest(nonexistentId))
    response should matchPattern { case Error(_) => }
    inside(response) { case Error(message) => message should be (messageError) }
  }
}
