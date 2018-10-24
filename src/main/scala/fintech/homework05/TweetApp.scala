package fintech.homework05

import java.util.UUID.randomUUID
import java.time.Instant
import scala.util.matching.Regex

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнение
  */

sealed trait Result[+T]
final case class Success[T](result: T) extends Result[T]
final case class Error[T](message: String) extends Result[T]

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

trait TweetStorage {
  def putTweet(tweet: Tweet): Result[Tweet]
  def getTweet(id: String): Result[Tweet]
  def updateTweet(tweet: Tweet): Result[Tweet]
}

class LocalStorage extends TweetStorage{
  private var tweets: Map[String, Tweet] = Map.empty
  override def putTweet(tweet: Tweet): Result[Tweet] = tweets.get(tweet.id) match {
    case Some(_) => Error(s"A tweet with this id: ${tweet.id} is already in storage")
    case None =>
      tweets += (tweet.id -> tweet)
      Success(tweet)
  }

  override def getTweet(id: String): Result[Tweet] = tweets.get(id) match {
    case Some(tweet) => Success(tweet)
    case _ => Error(s"No tweet for id: $id")
  }

  override def updateTweet(tweet: Tweet): Result[Tweet] = tweets.get(tweet.id) match {
    case Some(_) =>
      tweets += (tweet.id -> tweet)
      Success(tweet)
    case None => Error(s"A tweet with this id: ${tweet.id} is not in storage")
  }
}

class TweetApi(storage: TweetStorage) {
  private val MaxLenTweet = 280
  private val RegExHashTags = "#[A-Za-z_@0-9]+".r

  def createTweet(request: CreateTweetRequest): Result[Tweet] = request match {
    case CreateTweetRequest(text, _) if text.length > MaxLenTweet =>
      Error("The text length has been exceeded")
    case CreateTweetRequest(text, user) =>
      storage.putTweet(Tweet(randomUUID().toString, user, text,
        getHashTags(text), Some(Instant.now), 0))
  }

  private def getHashTags(text: String): Seq[String] =
    RegExHashTags.findAllIn(text).toSeq

  def likeTweet(request: LikeRequest): Result[Tweet] =
    storage.getTweet(request.id) match {
      case Success(tweet) =>
        storage.updateTweet(Tweet(tweet.id, tweet.user, tweet.text,
          tweet.hashTags, tweet.createdAt, tweet.likes + 1))
      case Error(message) => Error(message)
    }

  def getTweet(request: GetTweetRequest): Result[Tweet] =
    storage.getTweet(request.id)
}

object TweetApiExample extends App {

  val storage: TweetStorage = new LocalStorage()
  val app = new TweetApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response match {
    case Success(value) => println(s"Created tweet with id: ${value.id}")
    case Error(message) => println(s"Failed to create tweet: $message")
  }
}
