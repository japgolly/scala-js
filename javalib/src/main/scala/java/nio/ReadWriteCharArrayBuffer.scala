package java.nio

object ReadWriteCharArrayBuffer {
  private[nio] def copy(other: CharArrayBuffer, markOfOther: Int): ReadWriteCharArrayBuffer = {
    val buf = new ReadWriteCharArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadWriteCharArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Char], _arrayOffset: Int)
    extends CharArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  private[nio] def this(backingArray: Array[Char]) {
    this(backingArray.length, backingArray, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Char](capacity), 0)
  }

  def asReadOnlyBuffer: CharBuffer =
    ReadOnlyCharArrayBuffer.copy(this, _mark)

  def compact: CharBuffer = {
    System.arraycopy(backingArray, position + offset, backingArray, offset, remaining)
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  def duplicate: CharBuffer =
    ReadWriteCharArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    false

  protected[nio] def protectedArray: Array[Char] =
    backingArray

  protected[nio] def protectedArrayOffset: Int =
    offset

  protected[nio] def protectedHasArray: Boolean =
    true

  def put(c: Char): CharBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    backingArray(offset + position) = c
    _position += 1
    this
  }

  def put(index: Int, c: Char): CharBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index) = c
    this
  }

  override def put(src: Array[Char], off: Int, len: Int): CharBuffer = {
    val length = src.length
    if (off < 0 || len < 0 || len.toLong + off.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    System.arraycopy(src, off, backingArray, offset + position, len)
    _position += len
    this
  }

  def slice: CharBuffer =
    new ReadWriteCharArrayBuffer(remaining, backingArray, offset + position)
}
