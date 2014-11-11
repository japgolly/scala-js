package java.nio

object ReadWriteDoubleArrayBuffer {
  private[nio] def copy(other: DoubleArrayBuffer, markOfOther: Int): ReadWriteDoubleArrayBuffer = {
    val buf = new ReadWriteDoubleArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadWriteDoubleArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Double], _arrayOffset: Int)
    extends DoubleArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  private[nio] def this(backingArray: Array[Double]) {
    this(backingArray.length, backingArray, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Double](capacity), 0)
  }

  def asReadOnlyBuffer: DoubleBuffer =
    ReadOnlyDoubleArrayBuffer.copy(this, _mark)

  def compact: DoubleBuffer = {
    System.arraycopy(backingArray, position + offset, backingArray, offset, remaining)
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  def duplicate: DoubleBuffer =
    ReadWriteDoubleArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    false

  protected[nio] def protectedArray: Array[Double] =
    backingArray

  protected[nio] def protectedArrayOffset: Int =
    offset

  protected[nio] def protectedHasArray: Boolean =
    true

  def put(c: Double): DoubleBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    backingArray(offset + position) = c
    _position += 1
    this
  }

  def put(index: Int, c: Double): DoubleBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index) = c
    this
  }

  override def put(src: Array[Double], off: Int, len: Int): DoubleBuffer = {
    val length = src.length
    if (off < 0 || len < 0 || len.toDouble + off.toDouble > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    System.arraycopy(src, off, backingArray, offset + position, len)
    _position += len
    this
  }

  def slice: DoubleBuffer =
    new ReadWriteDoubleArrayBuffer(remaining, backingArray, offset + position)
}
