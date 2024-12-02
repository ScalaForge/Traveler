package traveler.pdts

trait NumConvert[To, From]:
  def apply(from: From): To

object NumConvert:
  given byte2Byte: NumConvert[Byte, Byte] with
    def apply(from: Byte): Byte = from
  given byte2Short: NumConvert[Short, Byte] with
    def apply(from: Byte): Short = from.toShort
  given byte2Int: NumConvert[Int, Byte] with
    def apply(from: Byte): Int = from.toInt
  given byte2Float: NumConvert[Float, Byte] with
    def apply(from: Byte): Float = from.toFloat
