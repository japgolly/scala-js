package java.nio

object ReadWriteShortArrayBuffer {
  private[nio] def copy(other: ShortArrayBuffer, markOfOther: Int): ReadWriteShortArrayBuffer = {
    val buf = new ReadWriteShortArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadWriteShortArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Short], _arrayOffset: Int)
    extends ShortArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  private[nio] def this(backingArray: Array[Short]) {
    this(backingArray.length, backingArray, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Short](capacity), 0)
  }

  def asReadOnlyBuffer: ShortBuffer =
    ReadOnlyShortArrayBuffer.copy(this, _mark)

  def compact: ShortBuffer = {
    System.arraycopy(backingArray, position + offset, backingArray, offset, remaining)
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  def duplicate: ShortBuffer =
    ReadWriteShortArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    false

  protected[nio] def protectedArray: Array[Short] =
    backingArray

  protected[nio] def protectedArrayOffset: Int =
    offset

  protected[nio] def protectedHasArray: Boolean =
    true

  def put(c: Short): ShortBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    backingArray(offset + position) = c
    _position += 1
    this
  }

  def put(index: Int, c: Short): ShortBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index) = c
    this
  }

  override def put(src: Array[Short], off: Int, len: Int): ShortBuffer = {
    val length = src.length
    if (off < 0 || len < 0 || len.toShort + off.toShort > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    System.arraycopy(src, off, backingArray, offset + position, len)
    _position += len
    this
  }

  def slice: ShortBuffer =
    new ReadWriteShortArrayBuffer(remaining, backingArray, offset + position)
}
