package java.nio

import scala.scalajs.js.typedarray._

final class ByteOrder private[ByteOrder] (val name: String) {
  override def toString = name
  override def equals(o: Any) = o match {
    case p: ByteOrder => p eq this
    case _ => false
  }
}

object ByteOrder {
  final val BIG_ENDIAN: ByteOrder =
    new ByteOrder("BIG_ENDIAN")

  final val LITTLE_ENDIAN: ByteOrder =
    new ByteOrder("LITTLE_ENDIAN")

  final val NATIVE_ORDER: ByteOrder = {
    val b = new ArrayBuffer(4)
    val a = new Uint32Array(b)
    val c = new Uint8Array(b)
    a(0) = 0x12345678
    if (c(0) == 0x78)
      LITTLE_ENDIAN
    else
      BIG_ENDIAN
  }

  def nativeOrder = NATIVE_ORDER
}
