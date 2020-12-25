package twentyfive

object Encryption extends App {
  def getEncryptionKey(card: BigInt, door: BigInt): BigInt = {
    val cardLoop = getLoopSize(card)

    var current: BigInt = 1
    for (i <- 0L until cardLoop.toLong) {
      current = performLoop(current, door)
    }
    current
  }

  def getLoopSize(publicKey: BigInt): BigInt = {
    var current: BigInt = 1
    var i = 0
    while (current != publicKey) {
      current = performLoop(current, 7)
      i += 1
    }
    i
  }

  private def performLoop(current: BigInt, subjectNumber: BigInt) = {
    val value = current * subjectNumber
    value % 20201227
  }

  println(getEncryptionKey(11562782, 18108497))
}
