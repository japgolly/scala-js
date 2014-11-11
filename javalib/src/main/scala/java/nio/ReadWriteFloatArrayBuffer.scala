package java.nio

object ReadWriteFloatArrayBuffer {
  private[nio] def copy(other: FloatArrayBuffer, markOfOther: Int): ReadWriteFloatArrayBuffer = {
    val buf = new ReadWriteFloatArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadWriteFloatArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Float], _arrayOffset: Int)
    extends FloatArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  private[nio] def this(backingArray: Array[Float]) {
    this(backingArray.length, backingArray, 0)
  }

  private[nio] def this(capacity: Int) {
    this(capacity, new Array[Float](capacity), 0)
  }

  def asReadOnlyBuffer: FloatBuffer =
    ReadOnlyFloatArrayBuffer.copy(this, _mark)

  def compact: FloatBuffer = {
    System.arraycopy(backingArray, position + offset, backingArray, offset, remaining)
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  def duplicate: FloatBuffer =
    ReadWriteFloatArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    false

  protected[nio] def protectedArray: Array[Float] =
    backingArray

  protected[nio] def protectedArrayOffset: Int =
    offset

  protected[nio] def protectedHasArray: Boolean =
    true

  def put(c: Float): FloatBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    backingArray(offset + position) = c
    _position += 1
    this
  }

  def put(index: Int, c: Float): FloatBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index) = c
    this
  }

  override def put(src: Array[Float], off: Int, len: Int): FloatBuffer = {
    val length = src.length
    if (off < 0 || len < 0 || len.toFloat + off.toFloat > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    System.arraycopy(src, off, backingArray, offset + position, len)
    _position += len
    this
  }

  def slice: FloatBuffer =
    new ReadWriteFloatArrayBuffer(remaining, backingArray, offset + position)
}
