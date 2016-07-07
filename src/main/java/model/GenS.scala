package model

import model.Move._

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

class GenS(val pos: PositionS, val side: Int) extends TGenS {

  var moves: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  var isCheck: Boolean = false

  def search(): ArrayBuffer[Int] = {
    searchMoves
    searchEPMoves
    castlingMoves
    checkMoves
    moves
  }

  def xside = if (side == -1) 1 else -1

  def searchEPMoves :ArrayBuffer[Int]= {
    val caseEP = pos.caseEP
    // prise en passant (avant recherche d'échecs)
    if (caseEP != -1) {
      epPawn(caseEP, est)
      epPawn(caseEP, ouest)
    }
    moves
  }

  //  def genPawnMove(cO: Int) = {
  //    // println("pion: " + cO)
  //    searchPawnMoves(cO, recherchePionAttaqueRoque = false)
  //  }

  def epPawn(caseEP: Int, estouest: Int) {
    val caseEstOuest = caseEP + side * nord + estouest;
    if (isSidePawn(caseEstOuest, side)) {
      val flag = 5 // ep capture
      val captpiece = 0 // ?
      moves += move(caseEstOuest, caseEP, flag, captpiece)
      // pseudoCoups.add(new GCoups(couleur * PION, caseEstOuest, caseEP, 0, 0, 0, EnPassant, 0));
    }
  }

  def isSidePawn(s: Int, side: Int): Boolean =
    pos.pieces(s) == PAWN && pos.colors(s) == side

  /**
    * attention: -couleur
    */
  def castlingMoves :ArrayBuffer[Int] = {
    // List<GCoups> coupsAttaque = new GPosition(true).pseudoC(gp, -couleur);
    val pos = new PositionS(true)
    val gen = new GenS(pos, -side)
    val coupsAttaque = gen.searchMoves
    val piece = pos.pieces
    val color = pos.colors
    // for (Integer type = 0; type < 4; type++) {
    (0 until 4).foreach { t => {
      val _c0 = o_o(t)(0)
      val _c1 = o_o(t)(1)
      val _c2 = o_o(t)(2)
      val _c3 = o_o(t)(3)
      val e_c4 = if (t == 1 || t == 3) color(o_o(t)(4)) else EMPTY
      // int e_c4 = type == 1 || type == 3 ? e[o_o[type][4]] : VIDE;

      if (pos.castles(t)) {
        if (((piece(_c0) == KING && color(_c0) == side) && (piece(_c2) == ROOK && color(_c2) == side)
          && color(_c3) == EMPTY && color(_c1) == EMPTY && e_c4 == EMPTY)
          && !fAttaque(_c0, _c3, _c1, coupsAttaque)) {

          // val flag king(2) or queen castle(3)
          val captpiece = 0
          if (abs(_c1 - _c0) < 2) moves += move(_c0, _c1, flag = 2, captpiece)
          else moves += move(_c0, _c1, flag = 3, captpiece)
          // pseudoCoups.add(new GCoups(ROI, _c0, _c1, _c2, _c3, 0, Roque, 0));

        }
      }
    }
    }
   moves
  }

  def checkMoves: ArrayBuffer[Int] = {
    // val caseRoiCouleur: Int =OUT
    val it = moves.iterator
    while (it.hasNext) {
      val coups = it.next
      val positionSimul: PositionS = fPositionSimul(coups, side)
      positionSimul.setPawnFlag(true)
      val caseRoiCouleur = pCaseRoi(positionSimul, side)

      val gen = new GenS(positionSimul, -side)
      val pseudoCoupsPosSimul: ArrayBuffer[Int] = gen.searchMoves
      // val pseudoCoupsPosSimul = new PositionS(true).pseudoC(positionSimul, -side);

      isCheck = fAttaque(caseRoiCouleur, -1, -1, pseudoCoupsPosSimul)
      if (isCheck) {
        moves -= coups
        // aRetirer.add(coups);
      }
    }
    moves
  }

  def searchMoves: ArrayBuffer[Int] = {
    val colors = pos.colors
    val pieces = pos.pieces
    val xside = if (side == -1) 1 else -1 // ou autre
    val loop = new Breaks // se place ici ?
    def opposingPiece(square: Int) = colors(square) == xside
    def emptySquare(square: Int) = colors(square) == EMPTY
    def isTrait(square: Int) = colors(square) == side

    (0 until 64).foreach { cO => {
      if (isTrait(cO)) {
        val piece = pieces(cO)
        if (isPawn(piece)) {
          searchPawnMoves(cO, recherchePionAttaqueRoque = false)
          //genPawnMove(cO)
        } else {
          (0 until directions(piece - 1)).foreach { dir => {
            var cX = cO
            loop.breakable {
              while (true) {
                cX = nextSquare(cX, piece, dir)
                if (cX == -1) loop.break
                if (emptySquare(cX)) {
                  genMove(cO, cX, capture = 0)
                  if (!slide(piece - 1)) loop.break
                } else {
                  if (opposingPiece(cX)) genMove(cO, cX, capture = 1)
                  loop.break
                }

              }
            }
          }
          }
        }
      }
    }
    }
    moves
  }

  //  def diagonalePionAttaqueRoque(caseO: Int, orientation: Int, estOuOuest: Int) {
  //    val caseX: Int = caseO + orientation + estOuOuest
  //    if (etats(caseX) != OUT) {
  //    }
  //  }

  def genMove(cO: Int, cX: Int, capture: Int) = {
    // println("piece" + cO + "," + cX + "," + capture)
    val flag = if (capture == 0) 0 else 4
    val captpiece = if (capture == 0) 0 else pos.pieces(cX)
    moves += move(cO, cX, flag, captpiece)
  }

  def searchPawnMoves(caseO: Int, recherchePionAttaqueRoque: Boolean): ArrayBuffer[Int] = {
    // var caseX = caseO + NS.orientation(side)
    var caseX = mailbox(mailbox64(caseO) + NS.orientation)
    if (pos.colors(caseX) == EMPTY) {
      if (lastrank(side).contains(caseX)) {
        moves ++= addPromotionMove(caseO, caseX, 0) // sans prise
      }
      else {
        val flag = 0 // quiet move 0
        val captpiece = 0
        moves += move(caseO, caseX, flag, captpiece)
        // pseudoCoups.add(new GCoups(couleur * PION, caseO, caseX, 0, 0, 0, Deplacement, 0));
      }
      if (rank2(side).contains(caseO)) {
        caseX = mailbox(mailbox64(caseO) + 2 * NS.orientation)
        if (pos.colors(caseX) == EMPTY) {
          val flag = 0 // quiet move 0
          val captpiece = 0
          moves += move(caseO, caseX, flag, captpiece)
          //  pseudoCoups.add(new GCoups(couleur * PION, caseO, caseX, 0, 0, 0, Deplacement, 0));
        }
      }
    }
    if (!recherchePionAttaqueRoque) {
      // caseX = caseO + NS.orientation(side) + est
      caseX = mailbox(mailbox64(caseO) + NS.orientation + est)
      if (caseX != OUT) {
        if (opposingPiece(caseX)) {
          if (lastrank(side).contains(caseX)) {
            moves ++= addPromotionMove(caseO, caseX, pos.pieces(caseX)) // avec prise
          }
          else {
            val flag = 4 // capture 0100
            val captpiece = pos.pieces(caseX)
            moves += move(caseO, caseX, flag, captpiece)
            // pseudoCoups.add(new GCoups(couleur * PION, caseO, caseX, 0, 0, etats[caseX], Prise, 0));
          }
        }
      }
      // caseX = caseO + NS.orientation(side) + ouest
      caseX = mailbox(mailbox64(caseO) + NS.orientation + ouest)
      if (caseX != OUT) {
        if (opposingPiece(caseX)) {
          if (lastrank(side).contains(caseX)) {
            moves ++= addPromotionMove(caseO, caseX, pos.pieces(caseX))
          }
          else {
            val flag = 4
            val captpiece = pos.pieces(caseX)
            moves += move(caseO, caseX, flag, captpiece)
            //   pseudoCoups.add(new GCoups(couleur * PION, caseO, caseX, 0, 0, etats[caseX], Prise, 0));
          }
        }
      }
    }
    //    else {
    //      diagonalePionAttaqueRoque(caseO, orientation, est)
    //      diagonalePionAttaqueRoque(caseO, orientation, ouest)
    //    }
    moves
  }

  def opposingPiece(square: Int) = pos.colors(square) == -side

  def addPromotionMove(caseO: Int, caseX: Int, captpiece: Int): ArrayBuffer[Int] = {
    var flag = 8 // 1000 a 1011(11)
    List(KNIGHT, BISHOP, ROOK, QUEEN).foreach(p => {
      moves += move(caseO, caseX, flag, captpiece)
      flag += 1
    })
    // pseudoCoups.add(new GCoups(couleur * PAWN, caseO, caseX, 0, 0, pieceprise, Promotion, couleur * p)))
    moves
  }

  def fAttaque(caseRoi: Int, F1ouF8: Int, G1ouG8: Int, _moves: ArrayBuffer[Int]): Boolean = {
    val it: Iterator[Int] = _moves.iterator
    while (it.hasNext) {
      val caseX: Int = it.next()
      if (caseX == caseRoi || caseX == F1ouF8 || caseX == G1ouG8) return true
    }
    false
  }

  def pCaseRoi(pos: PositionS, side: Int): Int =
    (0 until 64).find(cO => pos.pieces(cO) == KING && pos.colors(cO) == side).get

  def fPositionSimul(m: Int, couleur: Int): PositionS = {
    val p = new PositionS(false)
    (0 until 64).foreach(e => p.pieces(e) = pos.pieces(e))
    (0 until 64).foreach(e => p.colors(e) = pos.colors(e))
    // copy(etats, 0, p.etats, 0, NB_CELLULES)
    val O = cO(m)
    val X = cX(m)
    var t = flag(m)
    val cpiece  = captpiece(m)
    val piecePromotion = t
    val Deplacement = 0
    val Prise = 4
    val EnPassant = 5
    val Promotion = 8
    t = if (t >= 8) Promotion else t
    t match {
      case Deplacement | Prise | EnPassant =>
        // e(p, X, O) //e(p, O)
        p.pieces(O) = p.pieces(X)
        p.pieces(O) = EMPTY
        p.colors(X) = couleur
        p.colors(O) = EMPTY
        if (t == EnPassant)
        //e(p, X + nord * couleur)
         { p.pieces(X + nord * couleur) = EMPTY
        p.colors(X + nord * couleur) = EMPTY}
      case Promotion => {
        piecePromotion match {
          case 0 => p.pieces(X) = KNIGHT
          case 1 => p.pieces(X) = BISHOP
          case 2 => p.pieces(X) = ROOK
          case 3 => p.pieces(X) = QUEEN
        }
        p.colors(X) = couleur
        //e(p, O)
        p.pieces(O) = EMPTY
        p.colors(O) = EMPTY
      }
      case _ =>
    }
    p
  }

  object NS {
    val orientation = if (side == white) nord else sud
  }
}
