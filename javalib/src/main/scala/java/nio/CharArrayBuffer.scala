package java.nio

abstract class CharArrayBuffer private[nio] (capacity: Int, _backingArray: Array[Char], _offset: Int)
    extends CharBuffer(capacity) {

  protected[nio] final def backingArray = _backingArray
  protected[nio] final def offset = _offset

  private[nio] def this(array: Array[Char]) {
    this(array.length, array, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Char](capacity), 0)
  }

  final override def get(): Char = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = backingArray(offset + position)
    _position += 1
    r
  }

  final override def get(index: Int): Char = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index)
  }

  final override def get(dest: Array[Char], off: Int, len: Int): CharBuffer = {
    val length = dest.length
    if ((off < 0) || (len < 0) || off.toLong + len.toLong > length)
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

  final override def subSequence(start: Int, end: Int): CharSequence = {
    if (start < 0 || end < start || end > remaining)
      throw new IndexOutOfBoundsException
    val result: CharBuffer = duplicate
    result.limit(position + end)
    result.position(position + start)
    result
  }

  final override def toString: String =
    String.copyValueOf(backingArray, offset + position, remaining)
}
