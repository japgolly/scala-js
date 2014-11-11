package java.nio

abstract class IntArrayBuffer private[nio] (capacity: Int, _backingArray: Array[Int], _offset: Int)
    extends IntBuffer(capacity) {

  protected[nio] final def backingArray = _backingArray
  protected[nio] final def offset = _offset

  private[nio] def this(array: Array[Int]) {
    this(array.length, array, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Int](capacity), 0)
  }

  final override def get(): Int = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = backingArray(offset + position)
    _position += 1
    r
  }

  final override def get(index: Int): Int = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index)
  }

  final override def get(dest: Array[Int], off: Int, len: Int): IntBuffer = {
    val length = dest.length
    if ((off < 0) || (len < 0) || off.toInt + len.toInt > length)
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
