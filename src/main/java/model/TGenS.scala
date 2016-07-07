package model

trait TGenS {

  // Move encoding 4 x 8 bits(byte) = 32 bits
  //
  //  byte  meaning        example
  //  ====================================
  //  1    from-square  36 (A5)
  //  2      to-square  49 (B6)
  //  3          flags  32 (en-passant capture)
  //  4 captured-piece   3 (black pawn)

  // https://chessprogramming.wikispaces.com/Encoding+Moves


  val EMPTY = 0
  val black = 1
  val white = -1

  val OUT = -1
  val PAWN = 1
  val KNIGHT = 2
  val BISHOP = 3
  val ROOK = 4
  val QUEEN = 5
  val KING = 6
  val a8 = 0
  val h8 = 7
  val a7 = 8
  val h7 = 15
  val a1 = 56
  val h1 = 63
  val a2 = 48
  val h2 = 55
  val b1 = 57
  val g1 = 62
  val b8 = 1
  val g8 = 6
  val c1 = 58
  val f1 = 61
  val c8 = 2
  val f8 = 5
  val d1 = 59
  val d8 = 3
  val e1 = 60
  val e8 = 4

  val o_o = Vector(Vector(e1, g1, h1, f1), Vector(e1, c1, a1, d1, b1), Vector(e8, g8, h8, f8), Vector(e8, c8, a8, d8, b8))

  val directions = Vector(0, 8, 4, 4, 8, 8)
  val mailbox64 = Vector[Int](
    21, 22, 23, 24, 25, 26, 27, 28,
    31, 32, 33, 34, 35, 36, 37, 38,
    41, 42, 43, 44, 45, 46, 47, 48,
    51, 52, 53, 54, 55, 56, 57, 58,
    61, 62, 63, 64, 65, 66, 67, 68,
    71, 72, 73, 74, 75, 76, 77, 78,
    81, 82, 83, 84, 85, 86, 87, 88,
    91, 92, 93, 94, 95, 96, 97, 98
  )
  val mailbox = Vector[Int](
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 0, 1, 2, 3, 4, 5, 6, 7, -1,
    -1, 8, 9, 10, 11, 12, 13, 14, 15, -1,
    -1, 16, 17, 18, 19, 20, 21, 22, 23, -1,
    -1, 24, 25, 26, 27, 28, 29, 30, 31, -1,
    -1, 32, 33, 34, 35, 36, 37, 38, 39, -1,
    -1, 40, 41, 42, 43, 44, 45, 46, 47, -1,
    -1, 48, 49, 50, 51, 52, 53, 54, 55, -1,
    -1, 56, 57, 58, 59, 60, 61, 62, 63, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
  )
  val FILES = "abcdefgh"
  val slide = Vector(false, false, true, true, true, false)
  val nord = -10
  val sud = +10
  val est = +1
  val ouest = -1
  val offset = Vector(
    Vector(0, 0, 0, 0, 0, 0, 0, 0), //0
    Vector(-21, -19, -12, -8, 8, 12, 19, 21), /* KNIGHT =2 (1)*/
    Vector(-11, -9, 9, 11, 0, 0, 0, 0), /* BISHOP =3 */
    Vector(-10, -1, 1, 10, 0, 0, 0, 0), /* ROOK = 4*/
    Vector(-11, nord, -9, ouest, est, 9, sud, 11), /* QUEEN =5 */
    Vector(-11, nord, -9, ouest, est, 9, sud, 11) /* KING =6  (5)*/
  )

  def getString(square: Int): String = fileStr(square) + rankStr(square)

  def fileStr(square: Int): String = FILES(file(square)).toString

  def file(square: Int): Int = square % 8 //0 a 7

  def rankStr(square: Int): String = (rank(square) + 1).toString

  def rank(square: Int): Int = 7 - square / 8 //0 a 7

  def lastrank(couleur: Int) = if (couleur == white) a1 to h1 else a8 to h8

  def rank2(couleur: Int) = if (couleur == white) a2 to h2 else a7 to h7

  def nextSquare(square: Int, piece: Int, direction: Int): Int =
    mailbox(mailbox64(square) + offset(piece - 1)(direction))

  def isPawn(piece: Int) = piece == PAWN

  def abs(x: Int) = if (x < 0) -x else x


}
