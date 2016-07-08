package model

import org.chesspresso.Chess
import org.chesspresso.position.Position

import scala.Seq.range
import scala.collection.mutable.ArrayBuffer

object FenToGPosition extends TGenS{

  def toGPosition(fen: String): PositionS = toGPosition(new Position(fen))

  private def toGPosition(position: Position) = {
    val gp = new PositionS(false)
    val cp_etats = new Array[Int](64)

    val PAWN = 1 //5
    val KNIGHT = 2 //1
    val BISHOP = 3 //2
    val ROOK = 4 //3
    val QUEEN = 5 //4
    val KING = 6 //6

    // TODO tester : cases et pieces equivalentes ?

   // val etats = new Array[Int](NB_CELLULES)
//    var colors: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
//    var pieces: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
//    (0 until 64).foreach(cO => colors += EMPTY)
//    (0 until 64).foreach(cO => pieces += EMPTY)

    (0 until 64).foreach(caseO => cp_etats(caseO) = position.getStone(caseO))
    //range(0, NB_CELLULES).foreach(caseO => etats(caseO) = OUT)
    (0 until 64).foreach(caseO => gp.pieces(caseO) = cp_etats(caseO))
    (0 until 64).foreach(caseO => gp.colors(caseO) =
      if (cp_etats(caseO)<0) white else black  )

    //gp.pieces = pieces
    gp.side = if (position.getToPlay == Chess.WHITE) white else black
    val cp_roques = position.getCastles
    gp.castles(0) = (2 & cp_roques) == 2
    gp.castles(1) = (1 & cp_roques) == 1
    gp.castles(2) = (8 & cp_roques) == 8
    gp.castles(3) = (4 & cp_roques) == 4
    gp.caseEP = if (position.getSqiEP == -1) -1 else position.getSqiEP
    gp
  }

}