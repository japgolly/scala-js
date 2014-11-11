package java.nio

object ReadWriteLongArrayBuffer {
  private[nio] def copy(other: LongArrayBuffer, markOfOther: Int): ReadWriteLongArrayBuffer = {
    val buf = new ReadWriteLongArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadWriteLongArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Long], _arrayOffset: Int)
    extends LongArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  private[nio] def this(backingArray: Array[Long]) {
    this(backingArray.length, backingArray, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Long](capacity), 0)
  }

  def asReadOnlyBuffer: LongBuffer =
    ReadOnlyLongArrayBuffer.copy(this, _mark)

  def compact: LongBuffer = {
    System.arraycopy(backingArray, position + offset, backingArray, offset, remaining)
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  def duplicate: LongBuffer =
    ReadWriteLongArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    false

  protected[nio] def protectedArray: Array[Long] =
    backingArray

  protected[nio] def protectedArrayOffset: Int =
    offset

  protected[nio] def protectedHasArray: Boolean =
    true

  def put(c: Long): LongBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    backingArray(offset + position) = c
    _position += 1
    this
  }

  def put(index: Int, c: Long): LongBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index) = c
    this
  }

  override def put(src: Array[Long], off: Int, len: Int): LongBuffer = {
    val length = src.length
    if (off < 0 || len < 0 || len.toLong + off.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    System.arraycopy(src, off, backingArray, offset + position, len)
    _position += len
    this
  }

  def slice: LongBuffer =
    new ReadWriteLongArrayBuffer(remaining, backingArray, offset + position)
}
