package traveler.pdts
import scala.compiletime.erasedValue
import traveler.pdts.NumericPDT.NumericTypes

opaque type NumericData[A <: PDTNumeric.NumericTypes] <: Long = Long

object NumericData:
  extension [A <: PDTNumeric.NumericTypes](num: NumericData[A])
    def getByte: Byte =
      num.toByte
    def getShort: Short =
      num.toShort
    def getInt: Int =
      num.toInt
    def getLong: Long =
      num
    def getFloat: Float =
      java.lang.Float.intBitsToFloat(num.toInt)
    def getDouble: Double =
      java.lang.Double.longBitsToDouble(num)

    def reveal(using ext: Extract[A]): A = ext(num)
    // def as[B <: NumericPDT.NumericTypes]: NumericData = num.asInstanceOf[NumericData]

  trait Extract[A <: PDTNumeric.NumericTypes]:
    def apply(num: NumericData[A]): A

  given Extract[Byte] with
    def apply(num: NumericData[Byte]): Byte = num.getByte

  given Extract[Short] with
    def apply(num: NumericData[Short]): Short = num.getShort

  given Extract[Int] with
    def apply(num: NumericData[Int]): Int = num.getInt

  given Extract[Long] with
    def apply(num: NumericData[Long]): Long = num.getLong

  given Extract[Float] with
    def apply(num: NumericData[Float]): Float = num.getFloat

  given Extract[Double] with
    def apply(num: NumericData[Double]): Double = num.getDouble

  def apply[A <: PDTNumeric.NumericTypes](value: A): NumericData[A] =
    value match
      case b: Byte   => b.toLong
      case s: Short  => s.toLong
      case i: Int    => i.toLong
      case l: Long   => l
      case f: Float  => java.lang.Float.floatToRawIntBits(f).toLong
      case d: Double => java.lang.Double.doubleToRawLongBits(d).toLong
