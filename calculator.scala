/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  *
  * You should finish this procedure according to
  * the reference described in `README.md` to complete
  * the assignment.
  */
import scala.util.control.Breaks._

@main def calculator(commands: String*): Unit = {
  /** Converts given string `s` to integer.
   *
   * Throws [[NumberFormatException]] if `s` can't be converted to integer,
   * but you shouldn't worry about it at this moment.
   */
  def parseInt(s: String): Int = s.toInt

  var acc: Int = 0
  var A: Int = 0
  var B: Int = 0
  var blink: Boolean = false
  // define additional registers here

  def readNumber(number: String): Unit = {
    if (blink) {
      B = parseInt(number)
    } else {
      A = parseInt(number)
    }
    blink = !blink
  }

  def sum(): Unit = {
    acc = A + B
    blink = false
  }

  def subtraction(): Unit = {
    acc = A - B
    blink = false
  }

  def multiply(): Unit = {
    acc = A * B
    blink = false
  }

  def division(): Unit =  {
    if (B == 0) {
      A = 0
      acc = 0
    } else {
      acc = A / B
    }
    blink = false
  }

  def swap(): Unit = {
    var C: Int = A
    A = B
    B = C
  }

  def blinkFunc(): Unit =  {
    blink = !blink
  }

  def accFunc(): Unit =  {
    if (blink) {
      B = acc
    } else {
      A = acc
    }
    blink = !blink
  }
  breakable {
    for (c <- commands) {
      c match
        case "+" => sum()
        case "-" => subtraction()
        case "*" => multiply()
        case "/" => division()
        case "swap" => swap()
        case "blink" => blinkFunc()
        case "acc" => accFunc()
        case "break" => break
        case _ => readNumber(c)
    }
  }

  println(acc)
}
