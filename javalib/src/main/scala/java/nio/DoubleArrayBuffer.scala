package java.nio

abstract class DoubleArrayBuffer private[nio] (capacity: Int, _backingArray: Array[Double], _offset: Int)
    extends DoubleBuffer(capacity) {

  protected[nio] final def backingArray = _backingArray
  protected[nio] final def offset = _offset

  private[nio] def this(array: Array[Double]) {
    this(array.length, array, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Double](capacity), 0)
  }

  final override def get(): Double = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = backingArray(offset + position)
    _position += 1
    r
  }

  final override def get(index: Int): Double = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index)
  }

  final override def get(dest: Array[Double], off: Int, len: Int): DoubleBuffer = {
    val length = dest.length
    if ((off < 0) || (len < 0) || off.toDouble + len.toDouble > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferUnderflowException
    System.arraycopy(backingArray, offset + position, dest, off, len)
    _position += len
    this
  }

  final override def isDirect: Boolean =
    false

  final override def order: ByteOrder =
    ByteOrder.nativeOrder
}
