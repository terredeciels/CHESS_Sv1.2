package model

import model.PositionS._

object Main extends TGenS {
  def main(args: Array[String]): Unit = {
    val g = new GenS(PosInit, white)
    val moves = g.search
    println(moves)
    println(moves.size)
  }
}
