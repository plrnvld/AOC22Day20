import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("Input.txt")
    val numbers = source.getLines().map(_.toInt).toList
    val numberList = new NumberList(numbers)

    println(s"Count = ${numberList.length}")

    val dedupped = numbers.distinct
    println(s"Distinct = ${dedupped.length}")
  }
}

class NumberList(val numbers: List[Int]) {

    
    def length: Int = numbers.length
}
