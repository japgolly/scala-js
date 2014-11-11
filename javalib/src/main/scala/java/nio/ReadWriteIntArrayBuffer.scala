package java.nio

object ReadWriteIntArrayBuffer {
  private[nio] def copy(other: IntArrayBuffer, markOfOther: Int): ReadWriteIntArrayBuffer = {
    val buf = new ReadWriteIntArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadWriteIntArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Int], _arrayOffset: Int)
    extends IntArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  private[nio] def this(backingArray: Array[Int]) {
    this(backingArray.length, backingArray, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Int](capacity), 0)
  }

  def asReadOnlyBuffer: IntBuffer =
    ReadOnlyIntArrayBuffer.copy(this, _mark)

  def compact: IntBuffer = {
    System.arraycopy(backingArray, position + offset, backingArray, offset, remaining)
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  def duplicate: IntBuffer =
    ReadWriteIntArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    false

  protected[nio] def protectedArray: Array[Int] =
    backingArray

  protected[nio] def protectedArrayOffset: Int =
    offset

  protected[nio] def protectedHasArray: Boolean =
    true

  def put(c: Int): IntBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    backingArray(offset + position) = c
    _position += 1
    this
  }

  def put(index: Int, c: Int): IntBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index) = c
    this
  }

  override def put(src: Array[Int], off: Int, len: Int): IntBuffer = {
    val length = src.length
    if (off < 0 || len < 0 || len.toInt + off.toInt > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    System.arraycopy(src, off, backingArray, offset + position, len)
    _position += len
    this
  }

  def slice: IntBuffer =
    new ReadWriteIntArrayBuffer(remaining, backingArray, offset + position)
}
