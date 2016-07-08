package perft

import java.io.{BufferedReader, FileReader, IOException}

import model.{FenToPositionS, Move, PositionS, UndoMove}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.control.Breaks._

object PerftCompare {

  @throws[IOException]
  def main(args: Array[String]) {
    val maxDepth: Int = 4
    val fileReader: FileReader = new FileReader("D:\\Documents\\ECHECS\\GCLEv1.1.4\\perftsuite.epd")
    val reader: BufferedReader = new BufferedReader(fileReader)
    var line: String = null
    var passes: Int = 0
    var fails: Int = 0
    line = reader.readLine
    while (line != null) {
      val parts: Array[String] = line.split(";")
      if (parts.length < 3) {
        // ?? //continue //todo: continue is not supported
      }
      val fen: String = parts(0).trim
      var i: Int = 1
      breakable {
        while (i < parts.length) {
          {
            if (i > maxDepth) {
              break //todo: break is not supported
            }
            val entry: String = parts(i).trim
            val entryParts: Array[String] = entry.split(" ")
            val perftResult: Int = entryParts(1).toInt
            val position: PositionS = FenToPositionS.toPositionS(fen)
            val result: PerftResult = Perft.perft(position, i)
            if (perftResult == result.moveCount) {
              passes += 1
              System.out.println("PASS: " + fen + ". Moves " + result.moveCount + ", depth " + i)
            }
            else {
              fails += 1
              System.out.println("FAIL: " + fen + ". Moves " + result.moveCount + ", depth " + i)
              break //todo: break is not supported
            }
          }
          i += 1
        }
      }
      line = reader.readLine
    }
    System.out.println("Passed: " + passes)
    System.out.println("Failed: " + fails)
  }


}

class PerftResult {
  var moveCount: Long = 0
  var timeTaken: Long = 0
}

object Perft {
  def perft(gp: PositionS, depth: Int): PerftResult = {
    val result: PerftResult = new PerftResult
    if (depth == 0) {
      result.moveCount += 1
      return result
    }
    val moves: ArrayBuffer[Int] = gp.coupsValides()
    var i: Int = 0
    while (i < moves.size) {
      {
        val ui: UndoMove = new UndoMove
        gp.exec(moves.apply(i), ui)
          val subPerft: PerftResult = perft(gp, depth - 1)
          gp.unexec(ui)
          result.moveCount += subPerft.moveCount

      }
      {
        i += 1
      }
    }
    result
  }

  def divide(gp: PositionS, depth: Int): mutable.HashMap[String, Long] = {
    val result: mutable.HashMap[String, Long] = new mutable.HashMap[String, Long]
    val moves: ArrayBuffer[Int] = gp.coupsValides()
    var i: Int = 0
    while (i < moves.size) {
      {
        val ui: UndoMove= new UndoMove
        gp.exec(moves.apply(i), ui)
          val subPerft: PerftResult = perft(gp, depth - 1)
          gp.unexec(ui)
          result.put(toString(moves.apply(i)), subPerft.moveCount)

      }

      i += 1

    }
    result
  }

  def toString(gc: Int): String = {
    Move.getString(gc)
  }
}
