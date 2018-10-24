package fintech.homework05
import org.scalatest.{FlatSpec, Matchers, Inside}
import Inside._

class TweetAppSpec extends FlatSpec with Matchers {
  val text1 = "Hello, Vasya #qwe#abc dfsaf #qwer123@mail_ru.com"
  val bigText: String = "abc"*300
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
  it should "return error, if length Text > MaxLenTweet" in {
    val tweetApi = new TweetApi(new LocalStorage)

    tweetApi.createTweet(CreateTweetRequest(bigText, userName)) should
      matchPattern { case Error(_) => }
  }

  it should "correct like tweet" in {
    val tweetApi = new TweetApi(new LocalStorage)

    val response = tweetApi.createTweet(CreateTweetRequest(text1, userName))
    response match {
      case Success(tweet) =>
        inside(tweetApi.likeTweet(LikeRequest(tweet.id))) {
          case Success(tweetAfterLike) =>
            tweetAfterLike should be (Tweet(tweet.id, tweet.user, tweet.text,
              tweet.hashTags, tweet.createdAt, tweet.likes + 1))
        }
      case _ => throw new Exception("Something went wrong")
    }
  }
  it should "return error, when like tweet, if tweet with this id does not exist in Storage" in {
    val tweetApi = new TweetApi(new LocalStorage)

    tweetApi.likeTweet(LikeRequest("1244121")) should matchPattern { case Error(_) => }
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

    tweetApi.getTweet(GetTweetRequest("1244121")) should matchPattern { case Error(_) => }
  }
}
