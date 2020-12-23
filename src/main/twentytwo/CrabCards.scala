package twentytwo

object CrabCards extends App {
  def countWinnerScore(input: String): Long = {
    val split = input.split("\n\n")
    var one = parseDeck(split(0))
    var two = parseDeck(split(1))

    while (!(one.isEmpty || two.isEmpty)) {
      val cardOne = one.head
      val cardTwo = two.head
      if (cardOne > cardTwo) {
        val tuple = winRound(one, two, cardOne, cardTwo)
        one = tuple._1
        two = tuple._2
      } else {
        val tuple = winRound(two, one, cardTwo, cardOne)
        one = tuple._2
        two = tuple._1
      }
    }
    getScore(one, two)
  }

  private def winRound(winner: List[Int], loser: List[Int], winning: Int, losing: Int): (List[Int], List[Int]) = {
    var win = winner
    var lose = loser
    win = win.slice(1, win.length) :+ winning
    win = win :+ losing
    lose = lose.slice(1, lose.length)
    (win, lose)
  }

  private def getScore(one: List[Int], two: List[Int]): Long = {
    val winnerDeck = if (one.isEmpty) {
      two
    } else {
      one
    }
    winnerDeck.reverse.zipWithIndex.map(p => (p._2 + 1)*p._1).sum
  }

  def countRecursiveScore(input: String): Long = {
    val split = input.split("\n\n")
    var one = parseDeck(split(0))
    var two = parseDeck(split(1))
    val res = performGame(one, two)
    one = res._1
    two = res._2
    getScore(one, two)
  }

  private def performGame(oneIn: List[Int], twoIn: List[Int]): (List[Int], List[Int]) = {
    var one = oneIn
    var two = twoIn
    var knownConfigurations = Set[(String, String)]()
    while (!(one.isEmpty || two.isEmpty)) {
      val configuration = (one.sorted.mkString(","), two.sorted.mkString(","))
      if (knownConfigurations.contains(configuration)) {
        return (one, List())
      }
      knownConfigurations += configuration
      val cardOne = one.head
      val cardTwo = two.head
      if (cardOne < one.length && cardTwo < two.length) {
        if (performGame(one.map(i => i).slice(1, cardOne + 1), two.map(i => i).slice(1, cardTwo + 1))._1.isEmpty) {
          val result = winRound(two, one, cardTwo, cardOne)
          one = result._2
          two = result._1
        } else {
          val result = winRound(one, two, cardOne, cardTwo)
          one = result._1
          two = result._2
        }
      } else if (cardOne > cardTwo) {
        val tuple = winRound(one, two, cardOne, cardTwo)
        one = tuple._1
        two = tuple._2
      } else {
        val tuple = winRound(two, one, cardTwo, cardOne)
        one = tuple._2
        two = tuple._1
      }
    }
    (one, two)
  }

  private def parseDeck(input: String): List[Int] = {
    val split = input.split("\n")
    split.slice(1, split.length).map(_.toInt).toList
  }

  val input = "Player 1:\n41\n48\n12\n6\n1\n25\n47\n43\n4\n35\n10\n13\n23\n39\n22\n28\n44\n42\n32\n31\n24\n50\n34\n29\n14\n\nPlayer 2:\n36\n49\n11\n16\n20\n17\n26\n30\n18\n5\n2\n38\n7\n27\n21\n9\n19\n15\n8\n45\n37\n40\n33\n46\n3"
  println(countWinnerScore(input))
  println(countRecursiveScore(input))
}
