package java.nio

abstract class FloatArrayBuffer private[nio] (capacity: Int, _backingArray: Array[Float], _offset: Int)
    extends FloatBuffer(capacity) {

  protected[nio] final def backingArray = _backingArray
  protected[nio] final def offset = _offset

  private[nio] def this(array: Array[Float]) {
    this(array.length, array, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Float](capacity), 0)
  }

  final override def get(): Float = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = backingArray(offset + position)
    _position += 1
    r
  }

  final override def get(index: Int): Float = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index)
  }

  final override def get(dest: Array[Float], off: Int, len: Int): FloatBuffer = {
    val length = dest.length
    if ((off < 0) || (len < 0) || off.toFloat + len.toFloat > length)
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
