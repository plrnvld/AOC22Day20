import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("Example.txt")
    val numbers = source.getLines().map(_.toInt).toList
    val circle = new CircularList(numbers)

    circle.printEntries

    for (i <- 0 until circle.size) {
        val entry = circle.find(i)
        println(s"--> Mixing with index $i: (${entry.num}, ${entry.originalIndex})")
        circle.mix(i)
        circle.printEntries
    }

    // circle.printEntries
    circle.printResult
  }
}

class CircularList(numbers: List[Int]) {
    val size = numbers.length
    private val first = numbers.head
    private var index = 0

    var start = new ListEntry(first, index, null, null)
    start.next = start
    start.prev = start
    index += 1
    private var curr = start
    
    for (num <- numbers.tail) {
        println(s"> Adding $num")
        var afterCurr = curr.next
        val newEntry = new ListEntry(num, index, null, null)
        newEntry.prev = curr
        newEntry.next = afterCurr
        curr.next = newEntry
        afterCurr.prev = newEntry
        
        index += 1
        curr = curr.next
    }

    def mix(originalIndex: Int) {
        val entryToMix = find(originalIndex)
        val beforeOld = entryToMix.prev
        val afterOld = entryToMix.next
        val steps = mod(entryToMix.num)

        var curr = entryToMix
        if (steps != 0) {
            curr = move(curr, steps)

            val beforeNew = curr
            val afterNew = curr.next

            entryToMix.prev = beforeNew
            entryToMix.next = afterNew

            beforeOld.next = afterOld
            afterOld.prev = beforeOld
        }        
    }

    def mod(x: Int, m: Int = size): Int = (x % m + m) % m

    def printEntries = {
        var n = 0
        var curr = start
        
        while (n < size) {
            print(s"${curr.num}, ")
            curr = curr.next
            n += 1
        }

        println()
        println()
    }

    def move(entry: ListEntry, steps: Int): ListEntry = {
        var n = 0
        var curr = entry
        val stepsMod = mod(steps)
        if (stepsMod == 0) {
            curr
        } else if (steps > 0) {
            while (n < steps) {
                curr = curr.next
                n += 1
            }

            curr
        } else { // steps < 0
            while (n < steps) {
                curr = curr.prev
                n -= 1
            }

            curr
        }            
    }

    def find(originalIndex: Int): ListEntry = {
        var curr = start
        var n = 1

        while (n <= size) {
            if (curr.originalIndex == originalIndex) {
                return curr
            }

            curr = curr.next
            n += 1
        }

        throw new Exception(s"Entry with original index $originalIndex not found.") 
    }

    def printResult = {
    
    }
}

class ListEntry(val num: Int, val originalIndex: Int, var next: ListEntry, var prev: ListEntry) {
    
}

// -5555 (-651 + 1528 -6432) not right
