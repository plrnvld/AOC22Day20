import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("Input.txt")
    val numbers = source.getLines().map(_.toInt).toList
    val circle = new CircularList(numbers)

    println("\nInitial arrangement:")
    circle.printEntries

    for (i <- 0 until circle.size) {
        val entry = circle.findOriginalIndex(i)
        circle.mix(i)
        // circle.printEntries
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
        curr = move(curr, 1, forSwitching = false)
    }

    def mix(originalIndex: Int) {
        var forSwitching = true
        val entryToMix = findOriginalIndex(originalIndex)
        val beforeOld = entryToMix.prev
        val afterOld = entryToMix.next
        val steps = normalizeSteps(entryToMix.num, forSwitching)

        var curr = entryToMix
        if (steps != 0) {
            curr = move(curr, steps, forSwitching)

            rem(entryToMix)

            if (steps > 0) {
                println(s"${entryToMix.num} moves $steps step(s) between ${curr.num} and ${curr.next.num}:")
                insert(entryToMix, curr)
            }
            else {
                println(s"${entryToMix.num} moves $steps step(s) between ${curr.prev.num} and ${curr.num}:")
                insert(entryToMix, curr.prev) // Going left you need `.prev` to end up at the right spot
            }
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
            curr = move(curr, 1, forSwitching = false)
            n += 1
        }

        println()
        println()
    }

    def move(entry: ListEntry, steps: Int, forSwitching: Boolean): ListEntry = {
        var n = 0
        var curr = entry
        val stepsNorm = normalizeSteps(steps, forSwitching)

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

    def normalizeSteps(steps: Int, forSwitching: Boolean): Int = {
        val modulo = if (forSwitching) size - 1 else size
        
        if (steps >= 0) {
            steps % modulo
        } else {
            -(-steps % modulo)
        }
    }

    def findOriginalIndex(originalIndex: Int): ListEntry = {
        var curr = start
        var n = 0

        while (n < size) {
            if (curr.originalIndex == originalIndex) {
                return curr
            }

            curr = move(curr, 1, forSwitching = false)
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

            curr = move(curr, 1, forSwitching = false)
            n += 1
        }

        throw new Exception(s"Entry with value 0 not found.")
    }

    def printResult = {
        val zero = findZero
        val res1 = move(zero, 1000, forSwitching = false)
        val res2 = move(res1, 1000, forSwitching = false)
        val res3 = move(res2, 1000, forSwitching = false)
        val total = res1.num + res2.num + res3.num

        println(s"${res1.num} + ${res2.num} + ${res3.num} = $total")        
    }
}

class ListEntry(val num: Int, val originalIndex: Int, var next: ListEntry, var prev: ListEntry) {
    
}

// -5555 (-651 + 1528 -6432) not right
// 10106 (2674 + 8689 - 1257) too low
