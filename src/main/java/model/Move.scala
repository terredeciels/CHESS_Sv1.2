package model

object Move {
  def getString(gc: Int): _root_.scala.Predef.String = ???

  def move(cO: Int, cX: Int, flag: Int, captpiece: Int): Int = {
    var d: Int = cO << 8 | cX
    d |= flag << 16
    d |= captpiece << 20
    d
  }

  def cO(move: Int) = move >> 8 & 255

  def cX(move: Int) = move & 255

  def flag(move: Int) = move >> 16 & 15

  def captpiece(move: Int) = move >> 20 & 15


}

