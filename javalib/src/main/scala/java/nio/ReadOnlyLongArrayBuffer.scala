package java.nio

object ReadOnlyLongArrayBuffer {
  private[nio] def copy(other: LongArrayBuffer, markOfOther: Int): ReadOnlyLongArrayBuffer = {
    val buf = new ReadOnlyLongArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadOnlyLongArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Long], _arrayOffset: Int)
    extends LongArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  def asReadOnlyBuffer: LongBuffer =
    duplicate

  def compact: LongBuffer =
    throw new ReadOnlyBufferException

  def duplicate: LongBuffer =
    ReadOnlyLongArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    true

  protected[nio] def protectedArray: Array[Long] =
    throw new ReadOnlyBufferException

  protected[nio] def protectedArrayOffset: Int =
    throw new ReadOnlyBufferException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Long): LongBuffer =
    throw new ReadOnlyBufferException

  def put(index: Int, c: Long): LongBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: Array[Long], off: Int, len: Int): LongBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: LongBuffer): LongBuffer =
    throw new ReadOnlyBufferException

  def slice: LongBuffer =
    new ReadOnlyLongArrayBuffer(remaining, backingArray, offset + position)
}
