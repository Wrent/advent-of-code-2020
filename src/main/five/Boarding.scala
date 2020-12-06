package five

object Boarding extends App {
  def parseSeat(id: String): (Int, Int) = {
    val row = id.substring(0, 7).replace('F', '0').replace('B', '1')
    val seat = id.substring(7).replace('R', '1').replace('L', '0')
    (Integer.parseInt(row, 2), Integer.parseInt(seat, 2))
  }

  def getSeatId(id: String): Int = {
    val (row, column) = parseSeat(id)
    row * 8 + column
  }

  def getHighestSeatId(input: String): Int = {
    getSeatIds(input).max
  }

  private def getSeatIds(input: String) = {
    input.split("\n").map(getSeatId)
  }

  def getMissingSeatId(input: String): Int = {
    val sorted = getSeatIds(input).sorted
    var diff = 0
    var prev = sorted(0)
    for (elem <- sorted) {
      diff = elem - prev
      if (diff > 1) return elem - 1
      prev = elem
    }
    -1
  }

  val input = "BFFBFBBRLR\nFFFFBBBLRR\nFFFFBBFLRL\nBFFBBFFLRR\nBFFBFBBLRR\nFBBBBBFLLR\nBBFFBFFRRR\nBFFBBBBLLL\nBBFFFBFRLL\nBFBFFBBRRR\nFFFBBBFLLR\nBFFFBFBRRR\nFBFBFFFRLR\nBFFBFBBRLL\nFFBBBFBRRR\nBBFBFBBRRR\nBFFBFFFLLL\nFBBFFBBRLL\nFBFFBFBRRR\nBFFFBFBRLL\nFBFBBFBRRL\nFBFFBFBRLL\nFBFFFFBLRR\nBFBBBFBLLR\nFFBBBBBLRR\nBFBBBFBRRR\nFFBFBBFLLL\nBFFFFFBLRL\nFBBBBFBRRR\nFBBBFBFLRL\nBBFFFFFRLR\nBBFFBFBLLL\nBBFBBFFRLL\nFBBFFBFRRL\nBFBBFFFRLL\nBFFBFFBLRL\nBFBBBBFLLR\nBBFFFFBLLR\nFBBBFBBRLR\nBFFFBFFLLR\nBFBBFBFRLL\nFBBBBFBLLL\nFBFBBBFLLL\nFFFBBBBRLR\nFBBBFFBRLL\nBBFFBFBLRR\nFBFFBFFRRL\nFBBBFFFLLR\nFBBFFFFRLR\nBFBFFBFLLR\nFFBFBBBRRL\nFBBFFBBLLL\nFFBFFBBRLL\nFFBBFFFRRR\nFBBFBFBRRR\nFBBFFFFLLR\nFBFBFBFLLL\nBFBFFFBLRL\nFFBBBBFLRR\nFFBFFFFLRR\nFBFBFFBRRR\nFFFBBBFRRL\nFBBBFBBLRL\nBFFBBBFLLL\nFFFBFBFRLR\nBFBFBFBLLL\nFBBBFFFRRL\nFBBBBFFRLR\nFFBFFBFLRR\nBFBFFBFLRL\nBFFBFBFRRR\nBFBFFBFRRR\nBBFFBBFLRL\nBFFFBBBLLL\nFBFFBBBLRL\nFFBBFBBLRL\nBFBBFFBRLL\nFFBFFFFRRL\nFBFFBBBRLL\nFBBFBFBRLL\nBBFBFBBRLR\nFBFFFBFLRR\nBFBBFFFLRR\nBFBBBFFRRR\nFFBFFBBRLR\nBFBBBBBRRL\nFFBFFBFLRL\nFFFFBFFRLL\nFBFBBBBRLL\nFBFBBFFLLL\nFFBBFBFRLR\nFBBFFBBRLR\nBBFFFBBLLR\nFBBFFBBRRL\nFFBFFBBRRL\nBFFFFBBRLL\nFBFBFFBRLR\nFFFBFFBRRL\nBFFBFFFRLL\nFFBBBFFLLL\nFBFFFBFRRR\nBFBFBBFRRR\nBFBFFBFLLL\nFBFBBFFLLR\nFBBBFFBLRL\nFFBFFFFLLR\nFFFBBBFRLR\nFBBFBFFRLL\nFFBFBFBLRL\nBFBFBBBLRR\nBBFBBBFLLR\nFFFBBFBRRL\nFBFBBFBLRL\nBFFFBBBLRL\nFFFBFFBRRR\nFBBBFFFLRL\nFBBBBBBRRL\nBFBBBBBRRR\nFFBFBBFRLL\nFFBBBBFRRR\nFBBFFBFLRL\nFBBBBFBLLR\nBBFFBBFLLL\nFBBBBFBLRR\nBFFFFFFLRL\nFBBFFFBRLL\nBBFFFBBLRL\nFFBBFFFRLL\nBFBBFBBLLR\nBFBFBBBRLR\nBFFFFBFLRR\nFFBBFFBRRR\nBFFFBBBRRL\nFBFFBBBLRR\nFFFFBFFLRR\nFFBBFBFLLR\nBFBBFFBLRL\nBFFBFFFLLR\nFFBBBFBLLL\nBBFFFBBRLR\nFBBBFBBRLL\nFBFFBBFRLR\nBBFBFBBLLR\nBFBFFFBLLL\nBBFBBFFLRR\nBFBBFBFRRL\nFFFBBFBRRR\nBFBFBBFLLL\nFFFFBFBRLL\nFBFFFFBRLL\nFBBFFBBLLR\nFBBBFFBLRR\nFBFFFFFLLL\nBBFBFFFLLR\nBFFFBBFLRR\nFBBBBFFRRL\nBFFBBBBLLR\nFFBBFBBLLR\nBFFBBBBRRR\nBFFBFFBRRL\nBBFBBFBLRL\nFFBFFFBRRL\nBBFBBFBLLL\nFFBFBFBLLR\nFFBBFBFRRL\nBFFFFFBRLL\nFBBBBBBRLL\nFBBFBBBLLR\nFFFFBBFRRR\nFBBBFBFLLL\nFBFFBFBRRL\nBBFFFFBRLL\nFBBBBFBLRL\nBBFFBBBRRL\nBFFBFFFRLR\nFFBFBFFLLL\nFFBBBBFLRL\nBFFBBBFRRR\nBFFBFFBLLL\nFFBFFBBLRL\nFBFFBBFRLL\nFBFBBBFRRR\nFBFFBFFLLR\nBBFFBBFRRL\nBFBFFBFRLR\nFBBBFFBRRR\nFFFBBBFRRR\nBFFBBFBLRL\nBBFFFBFLRL\nFBBFFBBLRL\nFBFBBFFRLR\nBFFBBBFLRR\nBBFFFFBLLL\nBBFBBFBLLR\nFFBBBBBRRR\nFBFBFFFLRR\nFBFBFFFLLR\nFFBFFFBLLR\nFFBFFBFRLR\nBFFBFBBRRR\nBBFFFBBLRR\nBFBFBFFRLR\nFFBBBBFLLR\nFFBBFFBLRR\nBBFBBFFRLR\nBBFFFBFLLR\nBBFFBBBLRL\nBFFBBFBRRL\nFFFBFBBLLR\nBBFFBFFRLR\nBFFFFBBLLR\nFBBFBFFLLR\nFFFFBFBRLR\nFBBFFFFLLL\nBFBFBBFRLR\nBFFBFFBRLR\nFFFBFFFLLR\nFFBBBBFRLR\nFBBBFBBLLL\nFFBFBFBRRR\nFBFFFBBLRR\nBFBBFFFLRL\nFBFFBFBRLR\nBFBFFBFLRR\nBBFFFBFLLL\nFFBFFBBLRR\nBFBFFFFRRR\nBFFFBBBRLL\nBBFBBFBLRR\nFFFFBFBRRL\nBFFBBFFRLL\nFBFFBBBRRL\nBBFFFFFLRL\nBFFBBBFRLR\nBFFFBFFRRL\nBFBFFFBRLR\nFFFBFBFLLL\nBFBFFBBLLL\nFBFBBBFRLR\nFBFFBBFLLL\nBFBFBBBLRL\nFFBFBFBLRR\nBFBBBBBLLL\nBFFBBBBRRL\nFFFFBBBRLR\nBFBFBBFLLR\nFBBBBBFRRR\nFBBFBFFRLR\nBBFBBFBRRL\nFFBFBBBLRL\nBFBFBFBLRL\nFBFBBBFLRL\nFBFBBBBLLL\nFBFBBBBRRR\nFBBFBFBLLR\nFBFBFBBRLR\nFFBFFFBLRR\nBFFBBFBRLL\nBFFBFBBLLL\nFBFBFFBLRL\nBFFFFFFRLR\nBBFFFFFRLL\nFFBFBFBRRL\nFBBFFBFRRR\nBFFFFBBLRL\nFBBFBBFLRL\nFBBFBBFRLL\nBBFBFBBLRR\nFBFBFBBLRL\nBFBBBFFRLL\nFBBBBBBLLL\nFBBBBBBLRR\nBBFFBFBRRL\nFBFBFBFRRR\nFBBFBFFRRR\nBBFBBFFRRR\nFFBFBBFLRL\nFFBFBBBRRR\nBFFFFFFLRR\nBFBFFBBLRR\nBFBFBFBRLL\nFBFBBBBLLR\nBFFFFBBLLL\nFBBFFFBLLR\nBFBFBFBRLR\nBBFFFFBLRL\nBFBFBBBLLR\nFFFBBBBRRL\nFBFFFBFRLL\nFBBFBBBLRL\nBFBFFBBRLR\nFBBFFFBLRR\nFBBBBBFRLL\nBBFBFFBLRL\nBFFFBFFRLL\nFBFBFFBLLR\nBFFFFFBRRR\nFBBFBBBRRL\nFFFBBFFRRR\nBFBBFBFLLL\nBFFBBFBRLR\nFFBBBBBLLL\nFBFFBBBRLR\nFFFBFFBRLR\nBFBFFFFLRR\nBFFFBFFLRR\nBFFFFBBRLR\nFBFBBBFLLR\nBFFBFBBRRL\nFBBFBBBRLL\nFBBFBBFRLR\nFFBFBFFLRR\nFFFBFFFLLL\nBFBFBFFLLL\nFFFBFBFRRR\nBFFBFFFLRL\nFFBFBFBLLL\nBFFFBFFLRL\nBFBBBFFLRL\nFFFFBFBLLL\nFBBBBFFLLR\nFBFBBFBRRR\nFFFBBBBLRR\nBBFBBFFLRL\nFBFFBFFRLR\nFFFBFBBLRL\nBBFBFBFLLR\nFFBFBFFRRL\nBFFFFBFRLL\nFFBFBFFLRL\nFBBBBFBRLR\nFBFBFFFRRR\nBFBBBBFRRL\nBBFFFFBLRR\nFFFFBFBRRR\nFFFBFFFRRR\nBFBBBFFLLR\nBBFBFBFRLR\nFFFBFBBLLL\nFFBFBBBRLR\nBBFFBFBLRL\nFFBFFFBLLL\nFBFFFFFLRR\nFBBFBBBRLR\nFBFFFBBLLR\nFFBFBFBRLR\nFFBBFBFLRL\nFBFBFBBLRR\nFBBFFBFLRR\nBBFFBBBLLR\nFFFBFFFRRL\nFFBFFFFRLL\nBFBFBFFRRL\nFBFBBBFLRR\nFFBBFBBRRL\nBFBBBFBLRL\nFFFFBFFRLR\nFFFBBBBLLR\nFBBBFFFLLL\nBFFBFFFRRR\nFFFFBBBLRL\nBFFFBBFLLR\nFBFBFFFLRL\nBFFFFBFLRL\nFBFFBBBLLL\nFFBBBFFRLL\nBFBFFFBRRL\nBFFFFFBLLR\nFFBFBBFRRR\nFBFBFBFRLR\nBFBFBFFLLR\nBFBBBBBLRL\nFFFBBFFLRL\nFBFFBFFRLL\nFFFFBFFRRR\nFBFFFBFLLL\nBBFBFBFRLL\nFBBFBFBLLL\nFBFFFBBRRR\nFFBBBFFRRL\nFBBFBBFLRR\nFBBBFFBRLR\nFFFBBFFRRL\nFFFBFFFRLR\nFBFFBFBLLL\nFFBFBFFRRR\nBFFBBBBRLR\nBBFFBBFLLR\nBBFBFFBRLR\nFBBFBBFLLL\nFBBFBBFRRL\nBBFBBFBRLL\nBFFFFBFRRL\nFBFBBFFRRR\nBFFFFBFLLL\nBBFFFBFLRR\nFBBBFBFRLR\nFFFBFFBLRR\nFBBFBBFLLR\nFBBFFFFLRR\nBFBBBBFRRR\nFBFFFFBRLR\nFBBBBBFLRL\nBFBFFBBLRL\nBFBBBBFLRL\nFBFBFBBRRR\nFFFFBBBRRR\nFBBFFFBRRL\nFBFFBFFLRL\nFFBFFFBRRR\nBFFBBFFLLR\nFBBBBBFRRL\nFBBBFBBRRL\nFFFBFBBRLL\nBBFFBFBRRR\nFBFBFBFLRL\nFBBFFBFLLR\nFFBFFFFRRR\nBFFBBFFLLL\nBFBBBFFLRR\nFFBFBBFRLR\nBBFFBFFLLL\nBBFBFBFRRR\nBFFBFFBRRR\nBBFFBBBRLR\nBFBFBBBRLL\nBBFBFFFRLR\nFFFFBBBLLR\nFBBFBFBRLR\nFFBBFBBLRR\nFBFFFBBLRL\nBBFFBFBRLL\nBFFBFFFRRL\nFFBBBBBRRL\nFBFFFFFRLR\nFFBFFFBRLR\nFFFBFBBRRR\nBFFBFBBLLR\nFFFBBFFRLL\nBBFFBFFLRL\nFFFBBFFLLR\nBFFFBFBLRR\nFBBFBFBLRL\nBFBBFFFRRR\nBFBBBFBLRR\nFFBBBFBRRL\nFFBBBBFRRL\nFFBFFFBLRL\nBFBFBBFLRL\nBFFBBFFRLR\nFFBBBFFLRL\nBFBFFFBLRR\nFFFBBFBLRL\nFFFBFFBLLL\nBFBFBFBLRR\nFBFBBFBRLR\nBBFFBBBRRR\nFFFBFBBRLR\nBFFBBFFRRL\nBFBBBBBRLL\nBBFBFFFLRL\nFFFFBFBLRL\nBFBBFBFLLR\nFBFBFFFRLL\nBFBBFBFLRR\nFBFBFBFLRR\nBBFFFBFRRL\nFFFFBFFRRL\nFBFBFFBLRR\nBFBFBFFLRR\nFFFBBBBLLL\nBBFBBFBRRR\nBFFBBBFLRL\nBBFFBFBLLR\nBFBBFBBLLL\nFFFBFBFRRL\nFBBBFBBRRR\nFFFBBFFLLL\nBFBBBFBLLL\nFBFFBFBLRL\nBBFFFFBRRL\nBBFFBFFLRR\nFBFFFFBRRL\nFBBFFBFRLR\nFBFBBBBLRR\nBBFFBBBLLL\nBBFFFFFLLR\nFBFFBBFRRR\nFBBFFBFLLL\nFBFFBFBLLR\nBBFFFFBRRR\nFBFFFBFRLR\nBFBFBFBRRR\nBFBBBFFLLL\nFBBFFBBRRR\nFFBBFBBRLL\nFFBBFBBRRR\nBBFBFFFLLL\nFBFFBBFRRL\nBFFFBFBLLR\nFBFBBBFRRL\nFBFBFBBRRL\nFBFFBBBRRR\nBBFBFBFLLL\nFFBBFFFRLR\nFBBFFFBLLL\nFFBFFBBRRR\nBFFFBBFLLL\nFBFBFBBLLL\nFBBFFFFRRL\nFFBBFBFRLL\nFBBBBBFRLR\nBFBBFBBLRL\nBFFFFBFRLR\nBFFFFFFRRR\nFBFBBFFLRL\nFFBBFFBLRL\nFBBFFFBLRL\nFBBBBFFRLL\nBBFBFFFRLL\nFBFFFFFRLL\nFFBFFBBLLL\nFBFFFFBLRL\nBFBFBBBLLL\nBBFBBFBRLR\nBBFFFBFRRR\nFBBFBFBRRL\nBFFBFFFLRR\nBFBBFFBRRR\nBFBBBFBRLL\nBFBBBFFRRL\nFBFBFBFLLR\nFFBFBFBRLL\nFFBBBFFLLR\nBFBFBBBRRR\nBBFFBFBRLR\nFFBBBFBRLL\nFFFBBFFLRR\nBFBFFFBLLR\nFFBFBBFRRL\nBBFFBBFRLR\nBBFBFBBLLL\nFBFBBFBLRR\nBBFBFBFLRL\nFBBFFFBRRR\nFBFBBFFRRL\nFBBBFBFRRR\nBFFFFFFRRL\nBBFFFFFRRR\nBFBBBFBRLR\nBBFFFBBRRR\nFBFFFFFRRR\nBBFFFFFLRR\nFFFBFBBRRL\nFFBBBFBRLR\nFFFBFFBLRL\nFFFBBBBRLL\nFBFBBBBLRL\nBFBBBBBRLR\nFBFBFFBRRL\nBFBBFFBLLR\nBFFFFFBLLL\nBFBBBBFLRR\nFFBFBBBLLR\nBBFBBFFLLR\nBFBFFBFRLL\nFBBBBFBRRL\nFBFFFBFLRL\nFFBBFFBLLR\nBFFBFFBLLR\nFFBBBFBLRL\nFBBBBFFLRR\nBFFBFBFLRL\nBFFBFFBLRR\nFFBFBBBLLL\nFBFBFFBRLL\nFBBBFFFRRR\nFBBBBBBLRL\nFFBFFBFLLL\nBBFFBFFRRL\nFFFBBBFLRL\nBFBBFBBRLR\nFFFBFBFLRR\nFBBBFBFRLL\nBFBBFFFLLR\nFBFBFFFLLL\nBFFBBBFLLR\nBBFBFFBRRL\nFFBBBBBLLR\nFFFBBBFLRR\nBFBFBBBRRL\nBFFFFFBRRL\nFBFFBBBLLR\nBBFBBBFLRR\nBFFBBBBLRR\nBBFBFFFLRR\nFBBBFBFLLR\nBBFFBFFRLL\nFFBBFFFLRL\nBBFBFFBLRR\nBFFBBFBLLR\nBFBBBBBLLR\nBFFFBBFRLL\nFBFBBBBRRL\nFFBFBFFLLR\nBFFFBFBLLL\nFFFBBBFRLL\nFBFFBBFLLR\nFFFBBFBRLL\nBFBFFFBRLL\nBFFFBFFLLL\nBBFFFFBRLR\nFBBFFBBLRR\nBBFFBBBRLL\nBFBFFFFRLL\nFBBBBBBRLR\nFFFBFFBLLR\nFFFBBBBRRR\nFFFBBBBLRL\nFBBBFFBLLR\nBFBFBFBRRL\nFFFFBBFRLR\nFFBFFBFLLR\nFFFFBBBRRL\nFBBFFFFRLL\nBFFBBBFRRL\nFFFFBBFRRL\nBFBFFBBRLL\nFBFFFFFLRL\nFFFBBFBLRR\nFBFFFBFRRL\nBFBFFBBLLR\nFFFFBBFLLL\nFBFFFBBRRL\nBFFBBBFRLL\nBFFFBFBRLR\nBBFBFBBRLL\nFBFBFBBRLL\nBFBFFFBRRR\nBFFFBBBLRR\nFFBFBFFRLL\nBFBBFBFRRR\nBBFBBFFRRL\nBBFFFBBLLL\nFBFBFFBLLL\nFBFFFFBLLR\nFBFBBFFRLL\nBBFBBBFLLL\nFFBFBBFLRR\nBFFBFBFLLR\nBFFBBBBLRL\nFFBFBBBLRR\nFBBBFFFRLL\nFFBFFFFLLL\nFBFFFFBLLL\nFBBBBBBRRR\nBFBFFBBRRL\nFFBBFFFLLL\nFFBBBFBLRR\nFFBBBBBLRL\nBBFFBBFRLL\nFFBBFBFLLL\nBFFFFFFLLL\nBFBBFBFRLR\nBBFFFFFRRL\nFFBBFFFRRL\nFFFFBFBLRR\nBFFFBBBLLR\nFBFFBFFRRR\nBFBFFFFLLL\nFBBFBFFLRR\nFFBBBFFRRR\nFBBBFFFRLR\nFFFFBBFLRR\nFBFBFBBLLR\nBFFFFFBRLR\nBFFBFBFRLR\nFBFFBFBLRR\nFBFFFBFLLR\nFBFFBBFLRR\nFFBBFBBRLR\nBFFFBFBRRL\nBFBBFBBRLL\nBBFFFFFLLL\nFFBBBBBRLR\nBBFBFFBLLL\nBBFFFBBRLL\nBBFBFFBRLL\nBFFBFBFRRL\nFFBFFBFRRL\nBFBBFFBRRL\nBFFFFFFRLL\nFFBBBBFRLL\nFFFBFBFLRL\nFBBBBFFLLL\nBBFBBFFLLL\nBFFFFBBRRL\nBFFFBFBLRL\nFFFBFBBLRR\nBFFFBFFRRR\nFFBBBFFRLR\nBFBFBBFRRL\nBFFBBFFRRR\nBFFBBFFLRL\nFBBBBFBRLL\nFFFFBBFLLR\nBFBFFFFRLR\nBBFFBFFLLR\nBBFBFFBLLR\nFFFBBFBLLL\nBFBFBFFRRR\nFBBFBFFLRL\nBFFBFBFRLL\nFFBBFFBLLL\nFBBBBFFLRL\nFFBBFFFLLR\nFFFBBFBLLR\nBFBBFFFLLL\nBFFBFBFLLL\nBFFBBFBLRR\nBFFBFBFLRR\nFBBFFFFRRR\nBFBBFFBRLR\nBFFFBBFRRR\nBFBBBFBRRL\nFFBBFBBLLL\nFBBBFBBLLR\nBBFBFBBRRL\nFFBFFBFRLL\nFFFFBBFRLL\nBBFFBBFLRR\nFBFBFBFRRL\nFBBBFBBLRR\nFBFFBFFLRR\nFFFBFBFLLR\nFBBFBFFRRL\nFBFFFBBRLL\nFFBFFBBLLR\nFFBFBFFRLR\nFFBFBBBRLL\nFFFBFFFLRL\nFFFBBFFRLR\nFBFBBFBRLL\nBFFFBBFLRL\nBFBBFBBRRR\nFBBFBBFRRR\nFFBBFBFRRR\nBFFBFBBLRL\nFFFFBBBLLL\nBFFFFFBLRR\nFBBFBBBLRR\nFBBBBBFLLL\nFFBFFBFRRR\nBBFFBBFRRR\nFFBFBBFLLR\nFFBBBFFLRR\nBFFFBBFRRL\nBFBFBFFLRL\nBFBBFBFLRL\nFFBBFBFLRR\nFFFBBFBRLR\nFBFBBBBRLR\nBFFFBBFRLR\nFFFBFFFLRR\nBFBBFFFRRL\nBFBBBBBLRR\nFBFFFFBRRR\nFBBFBBBLLL\nFFFFBBBRLL\nBBFFFBBRRL\nBBFBFFFRRR\nFFBFFFFRLR\nFFBBFFBRLL\nFBFFFBBLLL\nBBFBFBFLRR\nFBBBFBFLRR\nBFBFBFFRLL\nBBFBBBFLRL\nFBFFFFFLLR\nBFBFBFBLLR\nBBFBBBFRLR\nBFBBBFFRLR\nFFBBFFFLRR\nFBFFFBBRLR\nFFBFFFBRLL\nBFBBFBBRRL\nFFBBFFBRLR\nBFBBFFBLLL\nFFBBBBFLLL\nBBFBFFFRRL\nFBFBBBFRLL\nFBFBBFFLRR\nFBBFBFFLLL\nBFBFFBFRRL\nFBFBFFFRRL\nBFBFBBFRLL\nBFFFBFFRLR\nFBFBFBFRLL\nBFBBFBBLRR\nFBBFBFBLRR\nBFFBFFBRLL\nFFFBFFFRLL\nFFBFFFFLRL\nFBBBFFFLRR\nFFFBFFBRLL\nBFFFBBBRLR\nFBFBBFBLLR\nBFBBBBFRLL\nBBFFBBBLRR\nFBFFFFFRRL\nBFBFFFFRRL\nFBBBBBBLLR\nFBBFFFBRLR\nFBBBFFBLLL\nBBFBBBFRLL\nBFFBBFBLLL\nBFFFFFFLLR\nFBBFBBBRRR\nBFFFFBBLRR\nBBFFFBFRLR\nFFFBBBFLLL\nBFFBBBBRLL\nBFBBBBFLLL\nBFBFBBFLRR\nBFBFFFFLLR\nBBFBFBFRRL\nFFBBFFBRRL\nBFBBBBFRLR\nFBBBFFBRRL\nBBFBFBBLRL\nBFBBFFBLRR\nFBBFFFFLRL\nFBBBBFFRRR\nFFBBBFBLLR\nFBBFFBFRLL\nFBFBBFBLLL\nFFFFBFBLLR\nFBBBFBFRRL\nFBFFBBFLRL\nBFBBFFFRLR\nBFFFFBBRRR\nFFBBBBBRLL\nBFFFBBBRRR\nFBBBBBFLRR\nBFFFFBFRRR\nBFBFFFFLRL\nFFFBFBFRLL\nBFFFFBFLLR\nBBFBFFBRRR\nFBFFBFFLLL"
  println(getHighestSeatId(input))
  println(getMissingSeatId(input))
}