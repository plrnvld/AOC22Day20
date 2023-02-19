import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("Input.txt")
    val numbers = source.getLines().map(_.toInt).toList
    val numberList = new NumberList(numbers)

    for (i <- 0 until numbers.length) {
    // for (i <- 0 until 2) {
        numberList.mix(i)
        // numberList.printEntries
        // println("=============")

        if (i % 100 == 0)
            println(i)
    }

    numberList.printEntries
    numberList.printResult
  }
}

class NumberList(val nums: List[Int]) {
    val entries: ListBuffer[Entry] = nums.to(ListBuffer).zipWithIndex.map{ case (n, i) => Entry(n, i) }
    val count = nums.length

    def mix(originalIndex: Int) = {
        val currIndex = mod(entries.indexWhere(entry => entry.originalIndex == originalIndex))
        val entry = entries(currIndex)
        val nextIndex = if (currIndex + entry.num <= 0) { // ################## Sus
            mod((currIndex + entry.num) - 1)
        } else if (currIndex + entry.num >= count - 1) { // ################## Sus
            mod((currIndex + entry.num) + 1)
        } else { 
            mod((currIndex + entry.num)) 
        }

        val leftVal = entries(nextIndex).num
        val rightVal = entries(mod(nextIndex + 1, count)).num
        // println(s"${entry.num} moves between $leftVal and $rightVal:")

        if (nextIndex > currIndex) {
            for(i <- currIndex until nextIndex) { 
                entries(i) = entries(i + 1)    
            }
            entries(nextIndex) = entry
        } else if (nextIndex < currIndex ) {
            for(i <- currIndex until nextIndex by -1) { 
                entries(i) = entries(i - 1)    
            }
            entries(nextIndex) = entry
        } else {
            // println(s"Don't move $entry")
        }
    }

    def printEntries = {
        println(entries.map(e => e.num.toString).mkString(", "))
    }

    def printEntry(index: Int) = {
        println(entries(index))
    }

    def printResult = {
        val indexOfZero = entries.indexWhere(entry => entry.num == 0)

        val index1000 = mod(indexOfZero + 1000)
        val index2000 = mod(indexOfZero + 2000)
        val index3000 = mod(indexOfZero + 3000)
        val entry1000 = entries(index1000).num
        val entry2000 = entries(index2000).num
        val entry3000 = entries(index3000).num

        val res = entry1000 + entry2000 + entry3000
        println(s"$entry1000 + $entry2000 + $entry3000 = $res")
    }

    def mod(x: Int, m: Int = count): Int = (x % m + m) % m
    
    def length: Int = entries.length
}

case class Entry(val num: Int, val originalIndex: Int) {
    
}

// -5555 (-651 + 1528 -6432) not right
