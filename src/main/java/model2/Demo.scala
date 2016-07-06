package model2

object Demo {
  def main(args: Array[String]) {
    val cO: Int = 3 /* 3 = 0000 0011 */
    val cX: Int = 31 /* 13 = 0001 1111 */
    val flag = 4
    val capt_piece = 3

    var d: Int = cO << 8 | cX
    d |= flag << 16
    d |= capt_piece << 20

    val ccapt_piece = d >> 20 & 15
    val fflag = d >> 16 & 15
    val ccO = d >> 8 & 255
    val ccX = d & 255

    println("code= " + d)
    println("capt_piece= " + ccapt_piece)
    println("flag= " + fflag)
    println("cO= " + ccO)
    println("cX= " + ccX)


    //
    //    c = a & cX;            /* 12 = 0000 1100 */
    //    println("a & cX = " + c );
    //
    //    c = a | cX;            /* 61 = 0011 1101 */
    //    println("a | cX = " + c );
    //
    //    c = a ^ cX;            /* 49 = 0011 0001 */
    //    println("a ^ cX = " + c );
    //
    //    c = ~a;               /* -61 = 1100 0011 */
    //    println("~a = " + c );
    //
    //    c = a << 2;           /* 240 = 1111 0000 */
    //    println("a << 2 = " + c );
    //
    //    c = a >> 2;           /* 215 = 1111 */
    //    println("a >> 2  = " + c );
    //
    //    c = a >>> 2;          /* 215 = 0000 1111 */
    //    println("a >>> 2 = " + c );
  }
}
