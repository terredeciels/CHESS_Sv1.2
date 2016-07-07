package model

import scala.collection.mutable.ArrayBuffer

class PositionS(pawnFlag:Boolean) extends TGenS {

  var caseEP :Int = -1
  var castles : ArrayBuffer[Boolean]=  ArrayBuffer.empty[Boolean]

  var colors: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  var pieces: ArrayBuffer[Int] = ArrayBuffer.empty[Int]

  //initialisation
  (0 until 64).foreach(cO => colors += EMPTY)
  (0 until 64).foreach(cO => pieces += EMPTY)
  (0 until 4).foreach(c => castles += true)
}

object PositionS {

  val PosInit = new PositionS(false) {

    castles = ArrayBuffer(true,true,true,true)

    lastrank(black).foreach(cO => colors(cO) = black)
    rank2(black).foreach(cO => colors(cO) = black)

    lastrank(white).foreach(cO => colors(cO) = white)
    rank2(white).foreach(cO => colors(cO) = white)

    rank2(black).foreach(cO => pieces(cO) = PAWN) // couleur donnée par color !
    rank2(white).foreach(cO => pieces(cO) = PAWN)
    List(a1, a8, h1, h8).foreach(cO => pieces(cO) = ROOK)
    List(b1, g1, b8, g8).foreach(cO => pieces(cO) = KNIGHT)
    List(c1, f1, c8, f8).foreach(cO => pieces(cO) = BISHOP)
    List(d1, d8).foreach(cO => pieces(cO) = QUEEN)
    List(e1, e8).foreach(cO => pieces(cO) = KING)
  }
}

