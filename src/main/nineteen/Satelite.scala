package nineteen

import java.util.regex.Pattern

object Satelite extends App {

  def countMatches(input: String): Int = {
    val split = input.split("\n\n")
    val rules = split(0).split("\n").map(parseRuleTuple).toMap
    val lines = split(1).split("\n")
    val strings = lines.filter(line => rules("0").matches(line, rules))
    strings.length
  }

  def countMatchesLoops(input: String): Int = {
    val split = input.split("\n\n")
    var rules = split(0).split("\n").map(parseRuleTuple).toMap
    rules = rules + ("8" -> OrRule(List("42"), List("42", "8"), "8"))
    rules = rules + ("11" -> OrRule(List("42", "31"), List("42", "11", "31"), "11"))
    val lines = split(1).split("\n")
    //    val regex = rules("0").toRegex(rules)
    //    println(regex)
    //    val strings = lines.filter(line => line.matches(regex))
    val strings = lines.filter(line => {
      line.matches(getRegex(rules, 1)) ||
        line.matches(getRegex(rules, 2)) ||
        line.matches(getRegex(rules, 3)) ||
        line.matches(getRegex(rules, 4)) ||
        line.matches(getRegex(rules, 5)) ||
        line.matches(getRegex(rules, 6)) ||
        line.matches(getRegex(rules, 7)) ||
        line.matches(getRegex(rules, 8)) ||
        line.matches(getRegex(rules, 10))
    })
    //      .filter(!isWrongEleven(_, rules))
    strings.length
  }

  private def getRegex(rules: Map[String, Rule], cnt: Int) = {
    "(" + rules("42").toRegex(rules) + ")+" + rules("42").toRegex(rules).repeat(cnt) + rules("31").toRegex(rules).repeat(cnt)
  }

  private def isWrongEleven(line: String, rules: Map[String, Rule]): Boolean = {
    val pattern = Pattern.compile(rules("31").toRegex(rules))
    val matcher = pattern.matcher(line)
    val thirtyOne = matcher.results().count
    val pattern2 = Pattern.compile(rules("42").toRegex(rules))
    val matcher2 = pattern2.matcher(line)
    val fortyTwo = matcher2.results().count()
    println(line + ": " + fortyTwo + " " + thirtyOne)
    !(fortyTwo - thirtyOne >= 1)
  }

  private def parseRuleTuple(line: String): (String, Rule) = {
    val colonIndex = line.indexOf(':')
    val id = line.substring(0, colonIndex)
    val rule = line.substring(colonIndex + 2)
    (id, parseRule(rule, id))
  }

  private def parseRule(input: String, id: String): Rule = {
    if (input(0) == '"') {
      return LiteralRule(input(1).toString)
    }
    if (input.contains("|")) {
      val split = input.split(" \\| ")
      return OrRule(split(0).split(" ").toList, split(1).split(" ").toList, id)
    }
    ChainRule(input.split(" ").toList)
  }

  val input = "3: 7 45 | 10 39\n120: 109 45 | 16 39\n84: 96 39 | 104 45\n6: 120 39 | 113 45\n111: 45 93 | 39 45\n13: 17 45 | 96 39\n74: 122 45 | 17 39\n94: 66 45 | 119 39\n127: 39 84 | 45 132\n129: 45 128 | 39 35\n112: 39 35 | 45 58\n24: 45 76 | 39 112\n43: 39 17 | 45 96\n2: 45 5 | 39 77\n71: 100 45\n11: 42 31\n51: 77 45\n4: 124 39 | 85 45\n45: \"a\"\n78: 111 39 | 128 45\n8: 42\n104: 45 39 | 39 93\n29: 122 39 | 66 45\n42: 63 45 | 20 39\n41: 73 45 | 19 39\n110: 39 98 | 45 114\n55: 45 104 | 39 122\n0: 8 11\n53: 39 34 | 45 89\n39: \"b\"\n61: 77 45 | 104 39\n121: 45 65 | 39 1\n105: 45 44 | 39 99\n113: 9 39 | 103 45\n117: 96 93\n125: 39 108 | 45 43\n69: 45 39\n56: 50 45 | 12 39\n73: 39 35 | 45 100\n87: 39 100 | 45 111\n10: 45 13 | 39 73\n19: 39 69 | 45 58\n100: 39 45\n66: 93 93\n46: 45 82 | 39 74\n76: 66 45 | 111 39\n103: 45 52 | 39 115\n77: 45 39 | 45 45\n52: 82 39 | 112 45\n15: 45 47 | 39 132\n68: 39 77 | 45 17\n1: 45 47 | 39 61\n65: 37 45 | 51 39\n98: 45 2 | 39 80\n35: 39 45 | 45 45\n93: 39 | 45\n126: 111 39 | 77 45\n32: 45 36 | 39 37\n37: 45 69 | 39 119\n90: 39 17 | 45 69\n21: 39 66 | 45 58\n22: 39 56 | 45 64\n7: 123 39 | 48 45\n60: 45 102 | 39 26\n107: 45 29 | 39 71\n58: 39 45 | 39 39\n70: 128 39 | 111 45\n81: 27 45 | 129 39\n67: 5 39 | 100 45\n96: 45 45 | 39 39\n116: 39 87 | 45 55\n106: 39 51 | 45 92\n14: 45 128 | 39 58\n48: 39 104 | 45 5\n72: 45 35 | 39 111\n130: 118 45 | 28 39\n115: 45 91 | 39 87\n31: 39 6 | 45 22\n30: 79 45 | 57 39\n9: 125 39 | 49 45\n122: 39 39 | 45 93\n23: 101 45 | 78 39\n47: 39 100 | 45 58\n28: 45 111 | 39 122\n101: 45 77 | 39 66\n33: 39 5 | 45 111\n95: 39 5\n27: 58 45 | 17 39\n16: 15 39 | 116 45\n80: 45 119 | 39 66\n92: 45 111 | 39 58\n57: 39 73 | 45 86\n123: 45 58 | 39 77\n5: 39 39\n128: 45 45\n124: 101 39 | 126 45\n108: 45 122 | 39 119\n119: 45 39 | 39 39\n50: 81 45 | 106 39\n99: 130 45 | 46 39\n132: 17 45 | 119 39\n49: 70 45 | 117 39\n63: 131 39 | 83 45\n85: 39 33 | 45 97\n18: 45 60 | 39 25\n83: 39 54 | 45 4\n38: 62 45 | 21 39\n64: 39 30 | 45 121\n118: 45 17\n91: 39 17 | 45 100\n82: 39 100 | 45 119\n86: 119 39 | 111 45\n89: 39 119 | 45 77\n44: 39 127 | 45 107\n88: 45 95 | 39 89\n17: 39 45 | 45 39\n131: 39 3 | 45 110\n12: 39 24 | 45 23\n26: 39 108 | 45 90\n36: 58 45 | 100 39\n97: 45 111\n25: 39 88 | 45 32\n62: 96 39 | 111 45\n59: 39 119 | 45 96\n34: 45 35 | 39 100\n79: 68 39 | 94 45\n40: 39 72 | 45 14\n20: 39 105 | 45 18\n75: 111 45 | 69 39\n114: 67 45 | 59 39\n54: 38 39 | 53 45\n102: 39 75 | 45 80\n109: 39 40 | 45 41\n\naaaabbaabbaaabaaabbaaaaa\naaababaabaaaabaaabababbabbbbaabbabbbbaababbaaabaababababababbabbabbaaabb\nbbbbbbbaabbaabaaaaabbaababbbaaabbababbabaababbaa\nbbbbbbabbabaababbababababbbabbabbbabaabaabbaaaba\nabbbbbaabaabbaabbabaaaba\naaaabbaaabbbbbabaaabaabaaaaaaaab\nbbabbababaababaabababaaaaabbbbbabbbbabbbabbbaabbbbabbbbb\naaabaaaaaaaabbaaabaaabbb\nabbabaaabbaaabbabbbabbba\nbababbaaaaaabbababbabaaa\naaaaabaaaababbbabbaabaaa\nbabaabbaaababbbabbaaaaaaaabbbabbbabbbaaabbabaabbaababbaabaabbbbb\nbaaaabbbbaaabaababaaaaaabbabaaaabbbaabbabbabaabbabbbbbabbaabbbabaaababba\naaaaabbbaabbbbbbbbabaaaababaaaab\nabaabaaabaabbbabaaabbbaaabaabbbabbaaaaaabaaabababbaaabbabbbbabaa\nabbbbbbabbaabbababaabaabbabaaabaabbbaaaa\nbabbbaaabaaaababababbbba\nbabbbabbbaaaababaaabaababbaababbababbaabaabaaababaabbbaaaabbababaaabaabbaababbaabbabbaab\nbbaabaabaabababbbaabaabb\nbaaaaabbabbbabbabbbababbbabababb\nbaaabbbbaababbabbababababbbbbaabbabbabba\naaaaabbbaabaaabbbbbbabbbbbaaaaaaabbaaaab\nbbaababababbbbbaabaaabbb\nabaabbababaabaaabaababab\nabbabbabababbababaababaababbababbbababaaabbbbbbaaaababaa\nbbaaaabababbbbaabaaaaaabbabbabbbbabbabbbbaabbaab\naabaaabbbabaabbabbabbbbb\nbaaabbabaabbabbbbaaababb\nababaababbaabbbbbbbaaabaaaabbbbb\naaaaabbaaaababbbabaaabab\nabbbaaabbaaabbbaababbabbbbbababbbbbbabba\naaaabaaabbbbbbabbaaabaaababbbabb\nbbaabbabbababaabaaaabbaaaaaababbabbbaabaaabbaabaabaaaabaaaabbbbbbaabbabb\nbbaabbaabbabaaabababbbaaaababbababbbabbbbaaaabbabaabbbaabaabbabbaabbabba\naaabbbbabbbbbbaabaabbbbb\nababbbaaabaaaaaaabaaaaab\naabaaabbbaaabbabbbbbbabababaaabbbabbabbaaaabbbbb\nabababbaaaabaaaaabbaaaab\nbbbaabaaaabbbbabbbaaaabababbbaaaaaaaabbbbabbbbbaabaaabbaabbababbaaaaababbbababbbaabaabba\nbbbbbbabaabaaababaaabbabbbaabbbbababbaaaaaabbaaabbabaabb\naaaaaabaaaaabababaaababa\nabaaabaabbaaaaabaaabbbabaabbbaabbaababbbbbbbaaab\naabbbbbabbbbbabbabbbbbab\nbbabaabaaababbabbaababab\naabbaaababaaaaaabbabbabababbabbb\nbbabbabbaaaaabbaabbbaaaa\naabababaaaabbaabbbbbbabb\naababbabaaabbaababaaaaba\naabbaaababbbaabaaabaabbb\nbbbbaaaaabaababbabbaabaa\nabaabbabbbbbabbababbbabb\nbaaabbbabaaabaaaabaaaaab\naaababbbaabbabaababbaabaabaabababaabbabbababbaaaaaaaababbaabbaba\naaaaaabbaababbbabbababbb\naabaabaaaaaaaabbbbbaababaabbabab\nabbbbbbbaabbbababaaaaabbbababbba\nbabbbbaaabbbbabbaabbbbaa\nbaaabbabbbaabaabaabbabab\naaaabbabaabababbbbabbaab\nbabaabbbbbbbaabaababaabaabaaaaba\nbabbbaabbbababbabaabbbbb\nbbaabababaaabbbbaabbbabbbbbaabbbbbbbbaba\nbbbbabbbbaaaababbbabbbba\naabaaabbabbbaaababbaabab\nabaababbbbbaabbaaaaabbaabbbaabaababaaabb\nbbaabbbbaaabaaaaabaaaaba\naaaabaabbbaaababaababaaa\nabaabaaaaabbaabbaababaab\nbbaaabaabbabbbaaabaaabba\naabbbbbbabaabbabbbaababb\nbbbbaaaaabbbbabaababbbab\nbabbbaaaaaaaaabbbabbaaab\naabbbabbaaaabbabbaabbaaa\nabbbaabaaabaaabbababbaba\nbaaabbbaabbbbabbbabaabbbbbbbaabaabbabbbb\nbabbbbabbbaabbaaabaabaaabaaaababaababaab\nabbbaabbbabbbabaababbaba\nbabbbbabbbbbabaabababbba\naaababbbababaaaaaababaaa\nbaaaabaababaabaaabaababbbbaaaaaabbbbbbaaaabaabba\nbbababbaaababababababbbabbbbbbabbbbbaaaabaaaaabaaaabaabaababaaba\nabbbbabaabaabbabbbbaaababaaabaaaabbbaabb\nbababbbbbabbbababbbbbbba\nbaaaaaaabbaabababbaababb\nbbabbaaabbabbbbaabbaaaab\nbbaabbabbbbbbabbaaaaaaba\nabbbbaaaabbbaaabbabbabbb\nbbbabbababbbbaaababaaaab\naabbbbbbababbbbbaabbbaaa\nbabaabbaabaababbbbaababb\nababaaaabbaababaaabbbbab\nbbbabbaabaaaabbbaabbabab\naabaaabbaaaabbabbababaababaaabbbabababab\nbaaabaaaabbbbaaaaabbabbbbaaaabba\nbbbaabbaabbabbbabbabbbba\nbabaababbaaabbabbbbaabbb\nbabbbaabbbabbababbbbaabb\nbabbbabababbbbbabbaaaaab\nbaaabaaabaaaabaabbabaabaababaabbaaababbbabbabbabbabaaaaaabbaaaabbbaabaaa\nbbabbaabbbabbabaaaababababbabbbaaaababaaaabbbbaaababaabb\nbaababaabbbbbbabababbaab\naaabbaababaabbabaaaaabab\naaabbbaabbbbbabbaaaaabaabbaaaabb\nabbbbabaaaaababbaaabbaaa\nabbbbbbaabbbbbaabaaabbbbaababaaa\nbaabbaabbbbabbabbabbabaa\nababbbaaaabbaaabbaabbbba\nbabbbbbaabbbababbbbabababbaababbababababababbbba\naabbbbbbbbabaaaababababb\naababbbabbbbabbbbbaabbba\naababbabbbbababbbabbaaab\nabbabbaabaaaaaabbaabaaab\nbababaaabbabababaabbabaaabbabbbbbaabaaaa\nbababababbaabbaabababbba\naaaabbababbbaaaaabbaaabaababbabb\naaaabbbbaaaaaabbbaabbbbb\naaabbbbabbbaaabaaabaabba\nbabaababbbbbabaaabaaaaba\nbbbaaabaabbaabbaabbbbbab\naaaabbababbbbaaaabbbbabababbbbbbababbaaabaabaaaa\naaabbaabbabbbbababaaaaba\nabbabbaabbabbbaaaaaaaaba\nabaabbaaaaaaabaaababaababaabbaaa\nbabbbaabbbbaaabbaaaaaaab\naaaabbbbaaaabbbbbaabaaba\nababbbaaaaaabaaaaabaabaa\nabababaaaabbbbbbbaaaaaba\nbabbbaabaaaabaababaaaaba\nbbaaabaaaaabbaababaaaaab\nbabbbaaabbbaaabaaababbbabbbbabbbbabaaabbabababab\nbbbaaabaababbabbaabaabbb\nbababaaaaabbbabaababbaba\nbaabababbbbaabbbabaabbbbbabbbaababbabbbaabbaabbbbbbbabbabbbbaaabbababbba\nabbbabbabaaabaabbaaaabaaabbabaabbaabaabb\nbbbabbbbbaaaaabbabbbabaa\nbbaaabbbbbbbabaaabababab\nbbababbabbbabbabbbaaaaab\naababbbabbbbabbbbbbbabab\nbbbbabbabbbbbabbaabbaaba\naabbbbbaaabbabaaaaaabbaaababbbaaabbbaabbbaabbbbabbaababb\naabbabbbaabbbbabbbbaabbbabaabaabbaaaaaaaaaaababbabbaaaabaabbaabb\naabaaaaaaabaabbabababbbabaababbb\nbbbbbbbbbabbababbababaabbaabababababbbbbaaababaa\naaabaaabbaaaabababbabbaaabbababaababbaab\nbababaaaabbabbaabbbabaab\nababbabbbbabaabaabaababa\naabbbabbaabaaabbabbaaaab\nbbaaabaaaabbaabbbbbabaaa\naabababbbaaabbabbbaabaabaaaaaaaaaaaabaabbaabaaabaaabaaba\naabaaabaaaaababbabaaabab\nbbabbababbaabaabbaabaaba\naaababababababbababababb\naaaaabbbaaaabbabaabbabab\nbabaababbbbbaabababababb\nbabbbbaabbaabbbabaabaabb\naaaabbaaababbbaabbabbbba\nbbbbaabbbababbbbaaabaaabbbbbaaababaaabbababaabbbbaabaabbbabbaaab\nabbbbbaabaaabbbbabababaabbabaaaaababbaba\nbbbbabbabbabaabaabbbabab\nbaaaababbbbbabbabaabaaab\naabbbbbabaaaabbbbbabbaab\nbbabbabbaaaabbbbbbbabbbbabababab\naaaababbaabababbbbaabbbbbaaaaaaaabaaaaba\naaabaaaababaabbababbbbaabbababaa\nbaaabbabbaababbaabbbbaab\nbabbbbaaababaabaabbababb\nbaabbabaabaaaabaaaabbaaabaabaaaaaaababbaabbaabaaaabbbaaaaaaababbaaaabaabaaaabbaaabbabbbaaabbaaaa\naaaaaabbbababbbbaaaaabab\nabbbabbabbaaabbbababbbbbbbbbaaabbaabbbba\naaababaabbbbabaaaabababa\nbabbabababaaaaaababbbbaabbabbabbbbabbbbb\nbbabaaababaabaabaaaababa\naabbaabbbbbbabbababbababbabbaabbabaaaabbbaaabbaa\nbaaabaaaaaaaabbababbbabababbbabb\naabbbabbabbaabbaaabaaaaa\nbbaaabaaabbbabbabbbbabab\nabaaaaaaaabbbbbbbbbaabbb\nbabbbbabbabbaababaabaaab\nababaabbaabaaababaaaaaba\nbaababaabababbbbbbbbaabb\nbbbbbaabbaababaaaaabbaaaabaaaabaaaaabbbaaaaababbbaabbbba\nbababbbbaabbbabababbbbaabaabbbbbababbaaaabbabababaabaabb\nabababaaaabbbababaaaabba\nbbaaaaaaabaaaaaabbbbbbbb\nabaabbbbaabbaaabbbbbaaabaabbabbaaabbbababbbbbbaaaabaaaaabbbbaaba\nabbbabbabaaaaaabaabaabba\naabababbbaababaaabaabbbb\nbaababbababbbbbabbabbabbbbbbaabb\nbababbaaaabbbabbaaaabbaaabaababaabbabbab\nabbbbabbabbbbaaaabaaabaa\nabababbaaaabaaababaaabaa\nbabbababbbabaaaaabbbaabbaaaaaaab\nabbbbaaaabbbbbbaaabaaaab\nbaaaabaaaabaabaaabbaaabb\nbbbbbbabbbaaabababbaabbaaabbbbba\naaaabbaaaabaabaabbaabbaaabbbabaaaabaabab\nbabaabababbbbababbababaa\nbaabbbabbaabbbaaaababbaa\nbbbabbbbabbbaabaabaabbabbaaabbaa\nbbaaabbbaabbbababaaaabaabaababab\nbabbaabaaaaaaaaaaaabaaba\nbbabbabaaabaaabbabbaabab\nbabaababaaabbbbaabbaaabb\naaaabbabaabbbbbabbbabbba\nbbbbbbabaaabbaababbaababbaabbbbbabbbabbb\naaabbbbabbbbbabbaabbbaab\nbbbbaabaaaaabaababbabaab\nabbbaabbbbbbabaaaabaabba\naabbbbaaabababbbaaaabababbaabbbaaabaabbb\naaaaabbaaaaabbababbbaaaa\naaababbbabaaaabbbbabbaaaababaaabaabaaaab\naaabaaabbbaaabbbbbbaaaab\nabaabaabbbbaababbbbaabaa\naaaabaaaaababbababababab\naabababbbabbbbbbabbbbabb\nabbabbaababaabbbabbaaaaa\nbabbbaabababaabbbbbaababbaaaaabbbbbbbbbaaabbaaaa\nbaaabbbbaaabababaaabaabb\nbaaabbbabbbaaabaaabaaaaa\nababaababbbbabaaabaababa\nababbbaaaaabbbbabbababbb\nabbbbabaabbbbbaabbaaaaab\nbaaabbbbababbabbaaabbaaa\nbabaabaaaaabbbbabbbbbbaaabaabbabbbbbabbaabaabbbaabaaabbb\nbbbababbabbbbbbabbbbabaaaabaaaabbaababbb\nbbabbabbbbbaaabaaabbabaabaaaabba\nbaaabaaabbaaabbbaaaaaababbabbbab\naaaabbabaaaabaababaaabbb\nabbbbbbaabbaaabbabbbaaaaabbbabab\nabaabaaabbbbbbaaaababaaa\naaabbaabbabbbaaaabbaaaba\nbaaaaaabaaababaaababaaaabbbbaabb\nbaaabaabbbaabbaaabbbabababbbbaab\naaabbbaabbbbabbababbbabb\nbaaaababbaababaaaaaababa\nbabbbbbababbbaabbabbaaab\nbabbaabbbbabbaaabbbaabababaababbbabbbbbbbbbabaabbaaaaabaababaaba\nababbaaaabbabaabbabbabbbbbbaaaab\nbbaaaaaabbbabbaaaaaaaaba\nbababaaabaaabbbabbbbbbbb\nbbabaaaabbbababbbabbaaaa\nbbaabbbbbaaabaaaababaaab\nbabbabababaababbbbbbbaaa\naabbaabbabbabbbaaaaaabaaabbaabbaaabaaabbbbabbababaabaabb\naabababbbaaaabbbaaaaabab\naabbbbbbabbbbabbaababbbababaabbbabbabbbbbaabbabaabababbb\nbbbbaaaaabaabbaaaababbaa\nbaaabbababbbaaabaaabbaba\naaababbbbabbbbbaaaabaaba\nbabaabaaabababaaaaaababbabbbabab\nbbaaaaaaaabbabbbaaababba\nbbbabbbbbaaaaaaabababbaaabababbaaabbbbbabaababaabbaaaaab\nbbabbbaaababaaaaabaaabbb\nababbbaaababbbbbaabaaaaa\nabaababbbabbbaaaababbaaa\nbbbbabbbaaaaaaaababaaaba\nbbbbaabaabababaabbbbaabb\nbbaabaababbbbbabaaaaabababbabbababababbbabbababbbbaababb\naaaaabbaabababbabaaaaaba\naabbbabbbbabbabaaaabbabb\nbabbbaabaaabaaabbbaabbabbaaabbbbbbbbbabbbabbbabbbaabaababbabbaaa\nbabbbaabbaaaabbbbaabbaba\nbbbababbbaaabbbbabbbbbaaababbbbb\nbbbbabbbbbabbabaaaaaaaaaabaaabaaaababaaa\nababaabbaaaaabbaabbababb\nbababaaaabababaaababbaaa\nbbaabababaaabbbbabbaaabb\nbabbbbaabaaabbbaababaaababaaababbbbbbbbbbaaababb\nbaaaabbbaaabbaabbabbbabb\nababbbbbbbbbaaaaaabaabab\nabaabbaabaaaababbaabbabb\naaaabbababaabaaababaaaab\nbaaabaabaaaabaababbbbbbbbabbaaaa\nbbabaaaabaaabbbbaababbbb\nababbbaaaabbaabbababbbbbaababbaaaaababba\nabbaabbaaaaaaabbbabaaaaa\naaabababababbabbbabaabbabbabbaababbbbabbbabaaabbaaababab\nbbaababaaaaabaaaabaaaaab\nbbbaababbabbbbbabbbbaabb\nababaaaabbbbbabbaaabaabb\nbabaabbabbbbabaabbbaaaaa\nbbbbbabbabaabaaaabbbabaa\nbaaaaaaaaabababbbbbaaabaaabbbabbabbababa\nbaaabbbabbaabaabbabbababbaaaabbbbbbbbbbabababbab\nabbbabbabababababaabaaab\nbabaababbbaabababbabaabb\nbbabababaabababbbaabbabb\nabaabaabbabaabaaabbaabaa\naaaaabaaabbaabaaaabbaaababaaabbababbbabbabaabbbbbabbbbbabbaaabababbabbaa\nbbabbbabaababababbbbabbbabaaaabbabbbabbaaabaaaaaababbaba\nbabbbbaababbbbaaabbbaaabababbbababaaaabb\nbbbabbbabbabbbababbabaab\naababbabaaabbbbaaabababa\nabbbbababbabababbababbab\naaabbaabaabaabaaaabaabab\nbbabbaabbabbbbbbabaaaaaa\nbbabbbaabaabbaabbaabbbbb\nbbbbbabaaabaaabaababbbbababbbbaabaaaabbaababaaaabbababaabbaaabbababbbbbb\nbbaabaaaababbabbababbbaaaaabbabbaabbbabaaaabaaabbabbbbbabaababbbabbbbbbbbabbbbba\naaabbbbaabaabbaaabbaabbb\nababbbaaaabbbaaabbbbbaaabbbbabbbbabbabbaababbaabaaababba\nbbaaaaaaababaaaabbaaaaaabbaaabaabbbabaaaababbabaaabaabba\nabbabbaaaaaaaaaaaaaababa\nabbbbabbbaaabbabbbaabaaa\nbbbbaaaabaaabaaabbbabbbaabbabaab\nbbaaababbbbabbbbbbbabaab\nabbaaabbbbaaaaababaaaabaabbaaaaabaabaaab\naaabaaaababbbbaaaababbaa\naabbabbbbbbaabbabbabbbab\naabaabbbabaababaaabaaaaa\nabbbbbbbaaabaaabaaabbaab\nbabbaabababbbaababababab\nbabbbaaabbababbabaabbbaa\nabbaabbaabbbaabbaababaaa\nababbbbbbbbbbbaabaaababb\nababaaaabbbbabaabbaaaaaabbbaabaababababb\nbaababaaaaababbbbaaaabbbbabbababbaabbbaaaabbaaaa\nbaaaababbbbbbabbbabaaaab\nbbabbabaabaababbababbbab\nabbbbabaabaabbbaabbabbbbbaabbbab\nababaaaabbaabbbbbabaaaba\naabaaababaababbaabbaabab\nbabaabaaabaababbabbaabbb\naabbbbbaababaabaabbaabaa\nbaaaaabbbababbaaabbbbbbbbbabbaabaaabaabbaaaaaaabaaabbabb\nbabababaaaaaaaabbbabaabaaaabbaaaaabaaabbbbababbbababaabbaaaabbbababbbaab\nbabbaababbbbaababaabbaba\nbaabbabaaaabbbbbbababbabbaabaaab\nabaabaaabaaabbbbbabbabba\nababbbaaaabaaabbabbbbbab\naaaaabbbabbaabbaaabaaaab\naaaaaaaaabbbbbabbaaabbabbaaabbaabaaabbabaaabaaaa\naaaabbbbbbaabaabbababbab\nbabababaabbbbaaaaaaaabbabbbbbababababbab\naaaabbaaabbbaaabbaabaaab\naabaabaaabbabbbbaababbaa\nabaabbabbabbbababbbbaabaabbbbbaabaaabbbbabbaaaaa\nbababbbbbbbbabbbbbbaababbabbbbaaaabbababaababbbb\naaabbbaabbaabbbbabaaabbb\nabbbbbbbbbbaaabbababbabbbaababbb\naaaabbbbaabbabaaaaabbabb\naaaaaaaababbbaaaabbaaabb\nbabbaabababbaababbbabbbbbbbbbbababbbababbaabbbab\nbbbbabbbbbbabbaabaabbbaa\naaaabbbbbaaaaaaabaaaaabbabaababaabbbbbab\naabaaaabbbabbbaaabbabbaaabaabbbaaabbbbbaababbabbabaaabbabbabaaab\naaaaabbbbbabaaaaabaabbbb\nabaabbaabbbbbbaabaaabbaaababaaababbaabababbbbbbbbaabbbbbbabbaaabbabbbbaaaaaaaaaaaaaabbaa\nabaabaabbbaaabbbabbaabaa\nbabbaababaabbbaabbabbaaa\nababaaabbababbaabaaaaababaabbababbabaabbabbbbaaa\naaababaabbbbbabbbbabaaaaaabaaaaaaabaabab\naaaababbbbbbaababababaabaababaaaababbaba\nbbabababababbbaaaababaaa\nbbbabbabaabbbabbabbaabaa\nbbaaababbaaabaaabaaaaababbabaabb\nabbbbbbabaababaaaabbabab\naaaaaaaaaaaabbaabbabbaab\nbbaaabaaabbabbbaaababaab\naabaabbbbbbbbbbbbbbabbabaabaabbbabbbbbbbbbbbababbbbbabbbaabbbbbabaaaabba\nabbabbbababbbbbaaabbbaaa\nbaaaaaaabbabaaabbaaabaabbbabaabbabbbaabbabbababbaaabababbabaabbaaabbbbabbababbab\nbbbaabaaabaabbabaabaabbabbbbaaaa\nbaaabaaabaababaaabbbbaab\nbbbabbaababbbbababaaaaab\naabbabbbaaabaaaabbbaaaab\naabbabbbaabbaabbabaabaaabaaabbbb\nbababbbbaaaaabbbaabababbabaabbaababbbaabbabbababaaaaaaabbaabbabbbaabaabb\nbababbaaababbabbbbbbbbba\naabababbaabaabaabbabbbbb\nbbbbaaaaabbbaababbbbbbba\nbababbbbaabbbbbbabbabaaa\naaabbbaaaaaaaaaabaabbbab\nbaaaabaaaaababbabbbbababbbbaaaab\nabbbaabbbabbbaaabbbabbba\nbaaaabbbbabababaabbbabab\naaaabbabbbababbababbbbaa\naabbabaabababaabaababbbb\nbbaabbbbbaabbaababbabbbaaaaabbabbbaabbba\nabbbaaabbaaaabbbaabbbaab\nabaaababbaabbbabababaaabbbbaabbbbbaaaaabaaabaaabbabbbabaabaaababbabaabbbaaaabaaa\naaaaabbbbababaababbaaabb\nbbbbabbbbbabaabaaabaabab\naababbbababaabbbbbaabbabaabaaabbbabbaaabaaababba\nabaaababababbbbbbbababbabbbabbbbbbaabbbabaaabbbbabaababb\nbabbbbbababaabaabbbaabaa\nbbbbabaaaabbbabaaaabaaaaabaaaaaabbababaa\nbaaaabaabaaaabbbbabaabbaaabaaababbababbabaababab\nbabbbbababbbbaaaabaaaabb\nabaabaaaabbaabbabaaaabbbbaaaaaababbaaabb\nabbaabbabababbaaaaababba\nabbbbbbababaabbabbbbbaab\nbbaabaabaababbbabaaabbabbbaaaaaababaabbbbbbbabbaababbababaabbaaa\nbbaaababbbbababbbbbbaaaaaabaabbb\nbbbbbbaaabbbaabaabbbabbabbbbbbaaabbababbabbbabbbbbabaabb\naabbaababbaababbbbbababbaabaababbbaaabbaababbbbbbbbaaaaabbabaaaababbabbaaaababbbaababbbb\nbabbbaabbbabbabbbbbabbaaabbaaaab\nabbbaabbbbabababaaaababa\nababbbbbbbaaabaabbbabaaa\nababaaaabaaaabababbaaaba\nababbbbbbababababbbbbabbbbbbbbba\nbbbbabaabaaabaaabaabaaaa\nbaaabaaaabbbbabbababaababbbbbbbbababbbababaaaaabbbbbabababbbbbaaaabbbbababbaaaaaaabaaababaaaabbb\nbbabababaaaabaaaababbbba\nbbaaababaaaabaabaabababa\nbbbaababaaabbaabbbaabbba\naabbabbabaaaabbabbaaaaabaabaabaababbbbabbaabaaba\nabbabaabbbbabbbabbaabbaabbabbaabbababbbb\nabbbabbaabbbbabbbbbbbaab\naabababbabaababbbaaabbbbaabaaababbaabaaa\nbbbbaababbbabbababbbabbb\naaabaaabbbaabbbbbababbba\nbbbbaabaaaabbabababaaaaabaaaaabaabaaabab\nbaabbaabababbbaababbaaaa\naababbabaaababbbbaaabbaa\nbabbbaaaababbbbbabababbb\nbaaaaaaabbbaababababbbba\nbbabaaabaaaababbbaabaabb\nabbbbabaabbbabbabbaaaabb\nbbaabbbbbabbbaabbaaabbaa\naaabbbaaabbababbbbbbabab\nbabbbaaaaabbabbbabbaababbbaaababaabababbabbababaaaabbbbbaaabaabb\nbbbababbbbbbbbbaabbabbababaabbbb\nbaaaabaabaaabbabbbbbaaab\nbabababaabbbbaaabbbababbaaabaabb\nbabaaaabbabbabbbbbbabbaaababbaaabaabaaaabaaaabaabbabbbaabaabaaabababbbbb\nbbabbbaababbbbbaaaaaaaaaaabbabab\nbbaabaaaaababaabaabbababaaaabbababaabbbabbbaaababbababbb\nabaabaabbbbbbbabaabbabab\naabbbababbaaababababbbab\nbbbabbaabbbababbbaabaabb\nbabbbabaaabbbbbaaaaaabbaababbbbbabaaabbb\naaaabaaabbaaabbbababbabbbaaabaaabbbaababaabbbbaa\nabbabbbbbaaaababbaabbaba\nbbaaabbbbbbabbabababaabaabbbbbbbababbbba\nbbbbabbaaabaaabbaaabaaabaabaaaab\nbabaababbabaabbbaaababba\nababaababaaaaaabbbbabbabaabbbaaa\nbabaababbbbbabababaaabbaabaaaaaaabbabbababbabaaaabbbabbbaaababaabbbbabaaababbbba\nabbabbaaaaaaabbabbbbaaaaaaaaabbb\naaabbbbabbaaabbbaababbabbabababb\nbbaabbbbababbaaababaabaabbbbabaaaaabaabbaaabbbaaaaaabbba\nababaabaaabbababbbbabbabaabaaabbabaabbab\nbaaabbbabbbbabbababbbbabaaabaabaaaaabbab\nbaaabbbbaabababbbaaaaaba\nabbabbbbbaaaaabbaabaaaaa\nbbabbabbaaaaabbabbabbbaabaabbbaa\nbbbaaabbaaaababbbabbbbaababaabbbabbbaaababababbb\nbabbaabbbbbbaaaaababbaba\nabbaabbaabababbaaababbbb\nabbbbaaabaabaabbaabbabbabaaabaabbbabbaabaaabababaabbaabababaaaaabbbbabbaaabbbbaaaaaaabba\nabbbaabaabbbbbbbbabaaabb\nbaaaabaabaaaaaabbaaaababaaabbbaabbbabbabbaabbaaa\nbaaabbabbbabaaaaaabaabba\nbabbaabaaaababbbbaaaabaabaababab\nbbbaabbababaabbbbaaaaaba\naaaabbbbaabbabaaabaaaabb\naabbaaabbbbaabbaaaabbaab\nbabbbaaaababaaaabbababbb\nabbabbaabbabaaabbbaaaabb\naaaabaaababbbbbbaababbbb\nbbbababbbbaaaaaaabbbbbab\nbbbabababaabaaabbaaabaaabbabaababbabbaabbbaababbaaaabaababbbabab\naaaababbbaaabbababababbb\nabaabbabbbaaabaabbaaaaabbbaabbbbbbabaaaabbaaaaabaaaabbabbbababaa\nbbaabbaaaabaabaabbaababb\naaababbbbaaaaabbbbaaababaabaaaaaabbabaab\nbbbabbbbbaaaaaababaabbbaaaababbbbaabbbaaabbbabaa\nbabbbbbbaaababbbbaaabbaa\nbbbbabbaabaaaabbaabbabaababbbbaabbabaabbbbbabbbbabbabaab\nbaaaabbbaaaaabbbaababbaa"
  println(countMatches(input))
  println(countMatchesLoops(input))
}

trait Rule {
  def matches(line: String, rules: Map[String, Rule]): Boolean

  def size(rules: Map[String, Rule]): Int

  def toRegex(rules: Map[String, Rule]): String
}

case class LiteralRule(literal: String) extends Rule {
  override def matches(line: String, rules: Map[String, Rule]): Boolean = {
    literal == line
  }

  override def size(rules: Map[String, Rule]): Int = {
    literal.length
  }

  override def toRegex(rules: Map[String, Rule]): String = {
    literal
  }
}

case class ChainRule(children: List[String]) extends Rule {
  override def matches(line: String, rules: Map[String, Rule]): Boolean = {
    val s: Int = size(rules)
    val rulesIterator = children.iterator
    var current = 0
    var res = true
    while (current < s && res) {
      val nextId = rulesIterator.next()
      val nextRule = rules(nextId)
      val nextRuleSize = nextRule.size(rules)
      val substring = line.substring(current, current + nextRuleSize)
      res = nextRule.matches(substring, rules)
      current = current + nextRuleSize
    }
    res && current == line.length
  }

  override def size(rules: Map[String, Rule]): Int = {
    children.map(rules(_)).map(_.size(rules)).sum
  }

  override def toRegex(rules: Map[String, Rule]): String = {
    children.map(rules(_)).map(_.toRegex(rules)).mkString
  }
}

case class OrRule(a: List[String], b: List[String], id: String) extends Rule {
  override def matches(line: String, rules: Map[String, Rule]): Boolean = {
    ChainRule(a).matches(line, rules) || ChainRule(b).matches(line, rules)
  }

  override def size(rules: Map[String, Rule]): Int = {
    Math.max(ChainRule(a).size(rules), ChainRule(b).size(rules))
  }

  override def toRegex(rules: Map[String, Rule]): String = {
    //8: 42 | 42 8
    //11: 42 31 | 42 11 31
    if (id == "8") {
      return "(" + ChainRule(List("42")).toRegex(rules) + ")+"
    }
    if (id == "11") {
      return "(" + ChainRule(List("42")).toRegex(rules) + ")+(" + ChainRule(List("31")).toRegex(rules) + ")+"
    }
    "(" + ChainRule(a).toRegex(rules) + "|" + ChainRule(b).toRegex(rules) + ")"
  }

}
