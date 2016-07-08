package model

import scala.collection.mutable.ArrayBuffer
import Move._
import Castle._

class PositionS(var pawnFlag: Boolean) extends TGenS {
  val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"


  var side: Int = _

  val R = new Castle
  var caseEP: Int = -1
  var castles: ArrayBuffer[Boolean] = ArrayBuffer.empty[Boolean]

  var colors: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  var pieces: ArrayBuffer[Int] = ArrayBuffer.empty[Int]

  def setPawnFlag(flag: Boolean) = pawnFlag = flag

  //initialisation
  (0 until 64).foreach(cO => colors += EMPTY)
  (0 until 64).foreach(cO => pieces += EMPTY)
  (0 until 4).foreach(c => castles += true)

  def unexec(ug: UndoMove) = {
    (0 until 64).foreach(e => pieces(e) = ug.pieces(e))
    (0 until 64).foreach(e => colors(e) = ug.colors(e))
    (0 to 4).foreach(e => castles(e) = ug.castles(e))
    caseEP = ug.caseEP
    side = -side
  }

  def exec(m: Int, ug: UndoMove) = {
    // copy(etats, 0, ug.etats, 0, NB_CELLULES)
    (0 until 64).foreach(e => ug.pieces(e) = pieces(e))
    (0 until 64).foreach(e => ug.colors(e) = colors(e))
    ug.setKQkq(castles)
    ug.caseEP = caseEP
    val O = cO(m)
    val X = cX(m)

    //val p = m.piece

    var t = flag(m)
    val cpiece = captpiece(m)
    caseEP = -1
    R.side = side
    if (pieces(O) == PAWN && abs(X - O) == abs(nord - sud)) {
      caseEP = if (side == black) X + 10 else X - 10
    }
    val piecePromotion = t

    val Deplacement = 0
    val KingCastle = 2
    val QueenCastle = 3
    val Prise = 4
    val EnPassant = 5
    val Promotion = 8
    t = if (t >= 8) Promotion else t
    t match {
      // ! attention repeter code pour Deplacement et Prise
      case Deplacement =>
        //        e(X, O)
        //        e(O)
        pieces(O) = pieces(X)
        pieces(O) = EMPTY
        colors(X) = side
        colors(O) = EMPTY
        valideDroitRoque(m)
      case Prise =>
        //        e(X, O)
        //        e(O)
        pieces(O) = pieces(X)
        pieces(O) = EMPTY
        colors(X) = side
        colors(O) = EMPTY
        valideDroitRoque(m)
      case EnPassant =>
        //        e(X, O)
        //        e(O)
        pieces(O) = pieces(X)
        pieces(O) = EMPTY
        colors(X) = side
        colors(O) = EMPTY

        pieces(X + nord * side) = EMPTY
        colors(X + nord * side) = EMPTY
      // e(X + nord * side)
      case Promotion =>
        piecePromotion match {
          case 8 => pieces(X) = KNIGHT
          case 9 => pieces(X) = BISHOP
          case 10 => pieces(X) = ROOK
          case 11 => pieces(X) = QUEEN
        }
        colors(X) = side
        //e(O)
        pieces(O) = EMPTY
        colors(O) = EMPTY
      case KingCastle | QueenCastle =>
        //        e(X, O)
        //        e(O)
        pieces(O) = pieces(X) // Roi
        pieces(O) = EMPTY
        colors(X) = side
        colors(O) = EMPTY
        //        e(caseXTour(m), caseOTour(m))
        //        e(caseOTour(m))
        pieces(caseOTour(m)) = pieces(caseXTour(m))
        colors(caseOTour(m)) = EMPTY
        R.unsetRoque()
      case _ =>
    }
    // side = -side
    side = -side
  }
  //TODO
  def caseOTour(m: Int): Int = if (cO(m) == e1 && flag(m) == 2) h1
  else if (cO(m) == e1 && flag(m) == 3) a1
  else if (cO(m) == e8 && flag(m) == 2) h8
  else if (cO(m) == e8 && flag(m) == 3) a8 else -1 //bug

  def caseXTour(m: Int): Int = if (cO(m) == e1 && flag(m) == 2) f1
  else if (cO(m) == e1 && flag(m) == 3) d1
  else if (cO(m) == e8 && flag(m) == 2) f8
  else if (cO(m) == e8 && flag(m) == 3) d8 else -1 // bug

  def coupsValides(): ArrayBuffer[Int] = {
    val generateur = new GenS(this, side)
    val coupsvalides = generateur.search()
    val estEnEchec = generateur.estEnEchec
    coupsvalides
  }

  def valideDroitRoque(m: Int) {
    val caseO = cO(m)
    val piece = pieces(caseO) // ?
    piece match {
      case KING =>
        R.unsetRoque()
      case ROOK =>
        if (caseO == caseTourH(side)) unsetK(side)
        if (caseO == caseTourA(side)) unsetQ(side)
      case _ =>
    }
    if (colors(caseTourA(side)) != side && pieces(caseTourA(side)) != ROOK ||
      colors(caseRoi(side)) != side && pieces(caseRoi(side)) != KING) unsetQ(side)
    if (colors(caseTourH(side)) != side && pieces(caseTourH(side)) != ROOK ||
      colors(caseRoi(side)) != side && pieces(caseRoi(side)) != KING) unsetK(side)
  }
}


object PositionS {

  val PosInit = new PositionS(false) {

    castles = ArrayBuffer(true, true, true, true)

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

