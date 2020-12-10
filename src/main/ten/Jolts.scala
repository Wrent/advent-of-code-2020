package ten

object Jolts extends App {
  def countDifferences(input: String): Int = {
    val adapters = parseAdapterNumbers(input)
    var diffs = Map[Int, Int](1 -> 0, 2 -> 0, 3 -> 0)
    for (i <- 0 until adapters.length - 1) {
      val diff = adapters(i + 1) - adapters(i)
      diffs = diffs + (diff -> (diffs(diff) + 1))
    }
    diffs(1) * diffs(3)
  }

  private def parseAdapterNumbers(input: String): List[Int] = {
    val adapters = 0 :: input.split("\n").map(_.toInt).sorted.toList
    adapters :+ (adapters.max + 3)
  }

  def countPossibilities(input: String): BigInt = {
    val adapters = parseAdapters(input)
    adapters(0).countPossibilities(adapters)
  }

  def parseAdapters(input: String) = {
    val adaptersNumbers = parseAdapterNumbers(input)
    val adapters = adaptersNumbers.zipWithIndex
      .map(entry => (entry._2, parseAdapter(entry._2, adaptersNumbers)))
      .toMap
    adapters
  }

  private def parseAdapter(index: Int, adapters: List[Int]): Adapter = {
    var children = List[Int]()
    children = updateChildren(index, 3, adapters, children)
    children = updateChildren(index, 2, adapters, children)
    children = updateChildren(index, 1, adapters, children)
    new Adapter(adapters(index), children)
  }

  private def updateChildren(index: Int, diff: Int, adapters: List[Int], children: List[Int]): List[Int] = {
    if (index + diff < adapters.length && adapters(index + diff) - 3 <= adapters(index)) {
      return index + diff :: children
    }
    children
  }

  val input = "99\n104\n120\n108\n67\n136\n80\n44\n129\n113\n158\n157\n89\n60\n138\n63\n35\n57\n61\n153\n116\n54\n7\n22\n133\n130\n5\n72\n2\n28\n131\n123\n55\n145\n151\n42\n98\n34\n140\n146\n100\n79\n117\n154\n9\n83\n132\n45\n43\n107\n91\n163\n86\n115\n39\n76\n36\n82\n162\n6\n27\n101\n150\n30\n110\n139\n109\n1\n64\n56\n161\n92\n62\n69\n144\n21\n147\n12\n114\n18\n137\n75\n164\n33\n152\n23\n68\n51\n8\n95\n90\n48\n29\n26\n165\n81\n13\n126\n14\n143\n15"
  println(countDifferences(input))
  println(countPossibilities(input))
}

class Adapter(val jolts: Int, val children: List[Int], var cache: BigInt = null) {
  def countPossibilities(adapters: Map[Int, Adapter]): BigInt = {
    if (cache != null) {
      return cache
    }
    val childrenResult: BigInt = children.map(adapters(_).countPossibilities(adapters)).sum
    val result: BigInt = if (childrenResult == 0 && children.nonEmpty) children.size else childrenResult
    if (cache == null) {
      cache = result
    }
    result
  }
}
