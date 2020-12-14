package twelve

import three.Coord

object Ferry extends App {

  def getFinalSpot(input: String): Int = {
    val instructions = input.split("\n")
    var current = (Coord(0, 0), "E")
    instructions.foreach(instr => current = getNext(current, instr))
    Math.abs(current._1.x) + Math.abs(current._1.y)
  }

  def getWaypointSpot(input: String): Int = {
    val instructions = input.split("\n")
    var current = (Coord(0, 0), Coord(10, -1), "E")
    instructions.foreach(instr => current = getNextWaypoint(current, instr))
    Math.abs(current._1.x) + Math.abs(current._1.y)
  }

  private def getNext(current: (Coord, String), instr: String): (Coord, String) = {
    val direction = instr.substring(0, 1)
    val number = instr.substring(1).toInt
    direction match {
      case "F" => (current._1.get(current._2, number), current._2)
      case "L" => (current._1, turn(current._2, number, turnLeft))
      case "R" => (current._1, turn(current._2, number, turnRight))
      case "N" => (current._1.get("N", number), current._2)
      case "S" => (current._1.get("S", number), current._2)
      case "E" => (current._1.get("E", number), current._2)
      case "W" => (current._1.get("W", number), current._2)
    }
  }

  private def getNextWaypoint(current: (Coord, Coord, String), instr: String): (Coord, Coord, String) = {
    val direction = instr.substring(0, 1)
    val number = instr.substring(1).toInt
    direction match {
      case "F" => (Coord(current._1.x + number * current._2.x, current._1.y + current._2.y * number), current._2, current._3)
      case "L" => (current._1, turn(current._2, number, rotateLeft), current._3)
      case "R" => (current._1, turn(current._2, number, rotateRight), current._3)
      case "N" => (current._1, current._2.get("N", number), current._3)
      case "S" => (current._1, current._2.get("S", number), current._3)
      case "E" => (current._1, current._2.get("E", number), current._3)
      case "W" => (current._1, current._2.get("W", number), current._3)
    }
  }

  private def turn[A](input: A, degrees: Int, fn: A => A): A = {
    val times = degrees / 90
    var res = input
    for (i <- 0 until times) {
      res = fn(res)
    }
    res
  }

  def rotateRight(coord: Coord): Coord = {
    Coord(coord.y * -1, coord.x)
  }

  def rotateLeft(coord: Coord): Coord = {
    Coord(coord.y, coord.x * -1)
  }

  def turnRight(direction: String): String = {
    direction match {
      case "N" => "E"
      case "E" => "S"
      case "S" => "W"
      case "W" => "N"
    }
  }

  def turnLeft(direction: String): String = {
    direction match {
      case "N" => "W"
      case "W" => "S"
      case "S" => "E"
      case "E" => "N"
    }
  }

  val input = "F20\nL90\nE5\nS1\nR180\nF67\nS3\nF75\nL180\nW4\nN4\nF88\nL90\nS2\nE2\nL180\nS4\nF3\nL90\nN3\nL180\nN5\nE2\nN1\nW5\nL180\nE3\nF50\nE1\nF84\nS4\nW3\nL90\nW1\nN1\nL90\nF7\nL90\nN5\nR90\nF35\nE2\nF100\nE5\nR90\nW1\nF85\nR90\nW4\nS4\nR180\nF20\nR90\nN5\nW1\nS3\nF77\nR90\nN1\nW2\nR90\nN5\nF25\nE2\nR90\nE5\nW5\nS3\nF59\nN3\nL90\nF1\nN5\nF31\nR90\nS5\nR90\nE1\nF81\nS3\nL90\nF79\nS3\nW1\nF25\nE2\nN4\nR90\nF16\nR180\nF29\nS5\nW1\nL90\nF50\nE5\nL90\nW2\nL90\nN2\nW1\nR90\nF65\nE3\nF21\nW3\nS5\nL90\nN4\nR180\nN4\nF37\nW1\nF40\nW1\nF78\nS1\nL90\nE2\nF12\nL90\nW3\nF16\nN1\nL90\nR90\nN2\nR90\nN2\nF5\nR90\nF43\nL90\nE5\nF89\nN3\nE3\nS3\nW1\nF48\nE2\nN2\nL180\nF78\nN5\nL90\nF14\nN3\nR180\nE4\nF27\nN5\nR90\nF68\nL270\nW5\nF59\nW1\nF98\nE3\nF47\nR270\nF43\nL90\nF79\nL90\nF94\nW1\nF40\nR90\nW4\nS3\nF13\nE3\nS5\nL180\nS3\nL270\nW4\nR90\nN3\nF64\nE2\nR90\nF4\nE5\nS3\nR90\nW1\nE3\nR90\nE5\nS1\nR180\nW1\nF36\nE1\nF45\nL90\nF92\nW3\nN3\nW2\nR90\nW2\nF79\nE2\nR90\nS4\nN4\nE1\nN4\nR90\nF71\nE3\nS4\nL90\nE1\nF10\nN3\nF53\nE5\nS5\nR90\nF85\nN2\nW4\nR90\nF64\nW1\nS2\nL90\nN1\nW1\nF40\nF7\nS3\nF20\nS3\nF63\nF97\nN5\nF23\nN3\nF20\nL90\nS3\nE3\nF54\nN5\nF79\nN1\nF50\nL90\nF10\nR90\nW3\nS1\nR180\nF93\nE1\nF73\nL90\nE3\nN3\nL180\nF1\nE1\nN2\nW2\nL90\nW2\nL90\nN4\nF97\nW2\nS1\nF89\nE3\nL90\nS5\nR90\nN3\nE2\nL90\nF59\nR90\nS4\nF53\nW3\nS3\nR90\nF35\nR180\nW1\nF32\nN2\nW3\nL90\nF55\nN3\nE3\nR90\nF50\nN5\nL90\nS3\nE3\nR90\nE4\nS2\nR90\nN4\nW1\nR90\nF44\nR90\nF56\nW3\nS3\nL90\nS2\nE4\nF91\nS2\nR90\nN3\nR90\nW1\nS1\nF4\nL90\nE3\nL180\nN5\nF67\nF50\nS3\nF71\nL90\nF81\nR90\nE1\nF27\nW2\nN5\nE5\nF99\nR90\nF30\nF98\nL90\nF20\nS2\nE2\nN2\nE4\nR180\nW2\nS5\nL90\nN5\nF59\nE1\nN3\nF42\nE2\nN4\nW1\nR90\nE4\nL180\nF92\nR90\nN4\nW3\nL180\nS3\nW2\nN2\nL90\nF26\nS1\nE5\nR90\nE2\nL90\nW4\nF96\nE5\nF4\nF98\nE3\nF77\nR180\nE4\nF28\nE3\nW2\nN3\nF23\nN3\nL90\nW5\nR90\nL90\nN3\nW3\nF97\nR90\nE3\nF22\nL180\nS2\nF22\nW2\nS5\nW5\nF40\nE3\nL90\nE1\nS3\nL90\nW3\nE5\nF69\nL90\nW5\nN4\nL90\nN3\nF49\nS2\nE2\nF41\nW2\nF61\nE3\nR90\nW5\nL180\nE4\nF52\nE2\nF86\nR270\nF27\nW5\nR90\nE1\nS4\nF3\nR90\nE3\nF28\nF31\nS4\nF81\nS5\nF89\nE5\nN2\nF21\nE5\nL180\nS4\nL180\nS3\nE3\nR180\nF58\nE5\nF8\nW2\nR90\nN3\nL270\nS1\nF67\nW4\nN2\nL180\nL90\nE5\nL180\nS3\nW2\nR180\nF70\nR90\nS5\nF40\nS1\nR90\nN1\nR90\nS3\nR90\nE2\nR90\nF86\nR90\nF33\nW2\nN5\nR180\nW5\nS4\nF1\nE2\nL90\nS3\nF68\nE3\nR90\nS4\nR90\nW2\nF51\nL90\nW1\nN2\nL90\nF40\nN1\nR90\nW1\nS5\nF39\nL90\nF61\nL90\nN4\nW5\nF5\nE2\nN3\nF67\nS4\nF44\nR180\nF4\nL180\nN2\nL90\nE5\nL270\nE1\nL90\nF99\nR90\nN2\nE4\nR90\nF96\nE1\nN4\nL90\nW5\nR270\nE2\nL90\nF33\nR90\nF11\nN1\nR90\nE5\nR90\nW1\nF61\nR90\nF98\nR180\nF86\nN5\nL180\nW4\nS3\nR180\nF98\nE5\nS4\nF33\nN2\nE4\nL90\nF36\nS1\nE1\nF92\nF48\nW3\nN4\nF2\nE4\nF98\nW5\nF67\nS3\nF60\nN5\nR90\nS2\nL90\nN5\nL180\nW2\nN4\nL90\nN4\nL90\nF90\nE5\nL90\nS1\nW1\nN2\nF76\nS4\nE5\nF5\nS4\nR90\nF41\nE5\nN5\nR90\nN5\nE2\nF13\nW2\nN5\nL180\nN5\nL90\nS3\nW1\nS1\nE1\nE3\nS5\nR90\nS1\nW3\nR90\nE2\nF37\nL90\nN3\nE4\nF85\nS1\nF27\nS5\nF10\nS2\nL90\nE1\nS3\nF6\nN5\nE5\nR90\nW2\nF2\nN4\nF73\nR90\nS5\nL90\nF87\nL90\nF100\nL90\nN3\nE3\nF90\nR90\nN5\nN3\nF80\nN2\nF88\nR90\nS5\nL90\nF88\nR90\nW2\nS4\nN2\nF9\nS3\nE4\nR180\nF60\nW2\nF93\nE2\nF4\nL90\nF20\nR180\nF87\nW2\nF75\nS3\nL180\nW3\nR180\nW1\nR90\nE1\nR90\nN4\nW2\nR90\nW1\nF74\nS1\nW4\nS3\nF59\nR270\nW1\nN5\nF42\nF34\nW3\nR270\nE1\nL90\nW3\nR270\nF57\nN2\nE3\nL270\nF57\nR90\nF68\nE1\nL90\nE2\nF4\nN2\nF28\nN4\nL90\nN4\nE5\nN2\nR90\nF89\nR270\nN4\nL90\nW4\nL90\nW4\nF92\nS1\nF77\nN2\nE1\nR90\nF72\nN5\nR90\nW1\nW3\nF25\nE1\nS4\nE3\nF95\nW3\nF72\nS3\nE5\nN4\nE1\nR180\nF73\nN1\nW2\nS5\nE3\nR180\nF68\nF4"
  println(getFinalSpot(input))
  println(getWaypointSpot(input))
}
