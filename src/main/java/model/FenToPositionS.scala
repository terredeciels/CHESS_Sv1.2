package model

import org.chesspresso.Chess
import org.chesspresso.position.Position

object FenToPositionS extends TGenS {

  def toPositionS(fen: String): PositionS = toPositionS(new Position(fen))

  private def toPositionS(p: Position) = {
    val gp = new PositionS(false)

    (0 until 64).foreach(caseO => gp.pieces(caseO) =
      if (abs(p.getStone(caseO)) == 0) 0 else if (abs(p.getStone(caseO)) == 5) 1
      else if (abs(p.getStone(caseO)) == 6) 6 else abs(p.getStone(caseO))+1 )

    (0 until 64).foreach(caseO => gp.colors(caseO) =
      if (p.getStone(caseO) < 0) black else if(p.getStone(caseO) >0) white else EMPTY)

    gp.side = if (p.getToPlay == Chess.WHITE) white else black
    val cp_roques = p.getCastles
    gp.castles(0) = (2 & cp_roques) == 2
    gp.castles(1) = (1 & cp_roques) == 1
    gp.castles(2) = (8 & cp_roques) == 8
    gp.castles(3) = (4 & cp_roques) == 4
    gp.caseEP = if (p.getSqiEP == -1) -1 else p.getSqiEP
    gp
  }

}