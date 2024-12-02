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
  given byte2Long: NumConvert[Long, Byte] with
    def apply(from: Byte): Long = from.toLong

  given short2Short: NumConvert[Short, Short] with
    def apply(from: Short): Short = from

  given short2Int: NumConvert[Int, Short] with
    def apply(from: Short): Int = from.toInt

  given short2Long: NumConvert[Long, Short] with
    def apply(from: Short): Long = from.toLong

  given int2Int: NumConvert[Int, Int] with
    def apply(from: Int): Int = from

  given int2Long: NumConvert[Long, Int] with
    def apply(from: Int): Long = from.toLong

  given long2Long: NumConvert[Long, Long] with
    def apply(from: Long): Long = from
