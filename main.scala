import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("Example.txt")
    val numbers = source.getLines().map(_.toInt).toList
    val circle = new CircularList(numbers)

    println("\nInitial arrangement:")
    circle.printEntries

    for (i <- 0 until circle.size) {
        val entry = circle.findOriginalIndex(i)
        circle.mix(i)
        circle.printEntries
    }

    // circle.printEntries
    circle.printResult
  }
}

class CircularList(numbers: List[Int]) {
    val size = numbers.length
    private var index = 0

    var start = new ListEntry(numbers.head, index, null, null)
    start.next = start
    start.prev = start
    index += 1
    
    private var curr = start
    
    for (num <- numbers.tail) {
        var afterCurr = curr.next
        val newEntry = new ListEntry(num, index, null, null)
        newEntry.prev = curr
        newEntry.next = afterCurr
        curr.next = newEntry
        afterCurr.prev = newEntry
        
        index += 1
        curr = move(curr, 1)
    }

    def mix(originalIndex: Int) {
        val entryToMix = findOriginalIndex(originalIndex)
        val beforeOld = entryToMix.prev
        val afterOld = entryToMix.next
        val steps = normalizeSteps(entryToMix.num)

        var curr = entryToMix
        if (steps != 0) {
            val actualSteps = if (steps > 0) steps else steps - 1 // Step one further when going left
            curr = move(curr, actualSteps)

            println(s"${entryToMix.num} moves $steps step(s) between ${curr.num} and ${curr.next.num}:")

            rem(entryToMix)
            insert(entryToMix, curr)            
        } else {
            println(s"${entryToMix.num} does not move:")
        }  
    }

    def rem(entry: ListEntry) = {
        val left = entry.prev
        val right = entry.next
        left.next = right
        right.prev = left
    }

    def insert(entry: ListEntry, after: ListEntry) = {
        val left = after
        val right = after.next
        
        left.next = entry
        entry.prev = left
        
        right.prev = entry
        entry.next = right
    }

    def mod(x: Int, m: Int = size): Int = (x % m + m) % m

    def printEntries = {
        var n = 0
        var curr = start
        
        while (n < size) {
            print(s"${curr.num}, ")
            curr = move(curr, 1)
            n += 1
        }

        println()
        println()
    }

    def move(entry: ListEntry, steps: Int): ListEntry = {
        var n = 0
        var curr = entry
        val stepsNorm = normalizeSteps(steps)

        if (steps == 1000) {
            println(s"Steps norm = $stepsNorm");
        }
        
        if (stepsNorm > 0) {
            while (n < stepsNorm) {
                curr = curr.next
                n += 1
            }
        } else if (stepsNorm < 0) {
            while (n > stepsNorm) {
                curr = curr.prev
                n -= 1
            }    
        }

        curr
    }

    def normalizeSteps(steps: Int): Int = {
        if (steps >= 0) {
            steps % size
        } else {
            -(-steps % size)
        }
    }

    def findOriginalIndex(originalIndex: Int): ListEntry = {
        var curr = start
        var n = 0

        while (n < size) {
            if (curr.originalIndex == originalIndex) {
                return curr
            }

            curr = move(curr, 1)
            n += 1
        }

        throw new Exception(s"Entry with original index $originalIndex not found.") 
    }

    def findZero: ListEntry = {
        var curr = start
        var n = 0

        while (n < size) {
            if (curr.num == 0) {
                return curr
            }

            curr = move(curr, 1)
            n += 1
        }

        throw new Exception(s"Entry with value 0 not found.")
    }

    def printResult = {
        val zero = findZero
        val res1 = move(zero, 1000)
        val res2 = move(res1, 1000)
        val res3 = move(res2, 1000)
        val total = res1.num + res2.num + res3.num

        println(s"${res1.num} + ${res2.num} + ${res3.num} = $total")        
    }
}

class ListEntry(val num: Int, val originalIndex: Int, var next: ListEntry, var prev: ListEntry) {
    
}

// -5555 (-651 + 1528 -6432) not right
