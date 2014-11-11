package java

import scala.scalajs.js.typedarray._

package object nio {

  // Life is pretty simple in single-threaded mode
  private[this] val pkgarraybuf = new ArrayBuffer(8)
  private[this] val pkgdbl      = new Float64Array(pkgarraybuf)
  private[this] val pkgfloat    = new Float32Array(pkgarraybuf)
  private[this] val pkgint      = new Int8Array(pkgarraybuf)

  private[nio] def doubleToRawLongBits(s: Double): Long = doubleToLongBits(s)
  private[nio] def doubleToLongBits(s: Double): Long = {
    pkgdbl(0) = s
    val b0 = pkgint(0).toLong
    val b1 = pkgint(1).toLong
    val b2 = pkgint(2).toLong
    val b3 = pkgint(3).toLong
    val b4 = pkgint(4).toLong
    val b5 = pkgint(5).toLong
    val b6 = pkgint(6).toLong
    val b7 = pkgint(7).toLong
    if (ByteOrder.nativeOrder eq ByteOrder.LITTLE_ENDIAN)
      b7 | (b6<<0x8) | (b5<<0x10) | (b4<<0x18) | (b3<<0x20) | (b2<<0x28) | (b1<<0x30) | (b0<<0x38)
    else
      b0 | (b1<<0x8) | (b2<<0x10) | (b3<<0x18) | (b4<<0x20) | (b5<<0x28) | (b6<<0x30) | (b7<<0x38)
  }

  private[nio] def longBitsToDouble(s: Long): Double = {
    if (ByteOrder.nativeOrder eq ByteOrder.LITTLE_ENDIAN) {
      pkgint(7) = s.toByte
      pkgint(6) = (s >> 0x8).toByte
      pkgint(5) = (s >> 0x10).toByte
      pkgint(4) = (s >> 0x18).toByte
      pkgint(3) = (s >> 0x20).toByte
      pkgint(2) = (s >> 0x28).toByte
      pkgint(1) = (s >> 0x30).toByte
      pkgint(0) = (s >> 0x38).toByte
    } else {
      pkgint(0) = s.toByte
      pkgint(1) = (s >> 0x8).toByte
      pkgint(2) = (s >> 0x10).toByte
      pkgint(3) = (s >> 0x18).toByte
      pkgint(4) = (s >> 0x20).toByte
      pkgint(5) = (s >> 0x28).toByte
      pkgint(6) = (s >> 0x30).toByte
      pkgint(7) = (s >> 0x38).toByte
    }
    pkgdbl(0)
  }

  private[nio] def floatToRawIntBits(s: Float): Int = floatToIntBits(s)
  private[nio] def floatToIntBits(s: Float): Int = {
    pkgfloat(0) = s
    val b0 = pkgint(0).toInt
    val b1 = pkgint(1).toInt
    val b2 = pkgint(2).toInt
    val b3 = pkgint(3).toInt
    if (ByteOrder.nativeOrder eq ByteOrder.LITTLE_ENDIAN)
      b3 | (b2<<0x8) | (b1<<0x10) | (b0<<0x18)
    else
      b0 | (b1<<0x8) | (b2<<0x10) | (b3<<0x18)
  }

  private[nio] def intBitsToFloat(s: Int): Float = {
    if (ByteOrder.nativeOrder eq ByteOrder.LITTLE_ENDIAN) {
      pkgint(3) = s.toByte
      pkgint(2) = (s >> 0x8).toByte
      pkgint(1) = (s >> 0x10).toByte
      pkgint(0) = (s >> 0x18).toByte
    } else {
      pkgint(0) = s.toByte
      pkgint(1) = (s >> 0x8).toByte
      pkgint(2) = (s >> 0x10).toByte
      pkgint(3) = (s >> 0x18).toByte
    }
    pkgfloat(0)
  }
}
