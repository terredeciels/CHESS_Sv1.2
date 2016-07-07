package model

import model.PositionS._
import Move._

import scala.collection.mutable.ArrayBuffer

object Main extends TGenS {
  def main(args: Array[String]): Unit = {
    val g = new GenS(PosInit, white)
    val moves = g.search()
    println(moves)
    println(moves.size)

   val movesStr = ArrayBuffer.empty[String]
    moves.foreach(move =>{
      movesStr += getString(cO(move)) += getString(cX(move)) +"/"
    }
    )
    println(movesStr)
  }
}
