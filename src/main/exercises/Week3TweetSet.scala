abstract class TweetSet {

  def incl(x: Tweet): TweetSet

  def contains(x: Tweet): Boolean

  def filter(p: Tweet => Boolean): TweetSet

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet

  def mostRetweeted(): Option[Tweet]
}

case class Tweet(user: String, text: String, retweets: Int)

class Empty extends TweetSet {

  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)

  def contains(x: Tweet): Boolean = false

  def filter(p: Tweet => Boolean): TweetSet = {
    this
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    acc
  }

  def union(that: TweetSet): TweetSet = {
    that
  }

  def mostRetweeted(): None.type = None
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (x.text > elem.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def contains(x: Tweet): Boolean = {
    if (x.text < elem.text) left.contains(x)
    else if (x.text > elem.text) right.contains(x)
    else true
  }

  def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p, new Empty)
  }

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if (p(elem)) {
      right.filterAcc(p, left.filterAcc(p, acc.incl(elem)))
    } else {
      right.filterAcc(p, left.filterAcc(p, acc))
    }
  }

  def union(that: TweetSet): TweetSet = {
    left.union(right.union(that.incl(elem)))
  }


  def mostRetweeted(): Option[Tweet] = {
    val leftMost = left.mostRetweeted()
    val rightMost = right.mostRetweeted()

    val tweetsToCompare = List(Some(elem), leftMost, rightMost).flatten

    Some(tweetsToCompare.maxBy(_.retweets))
  }


}