package java.nio

object ReadOnlyShortArrayBuffer {
  private[nio] def copy(other: ShortArrayBuffer, markOfOther: Int): ReadOnlyShortArrayBuffer = {
    val buf = new ReadOnlyShortArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadOnlyShortArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Short], _arrayOffset: Int)
    extends ShortArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  def asReadOnlyBuffer: ShortBuffer =
    duplicate

  def compact: ShortBuffer =
    throw new ReadOnlyBufferException

  def duplicate: ShortBuffer =
    ReadOnlyShortArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    true

  protected[nio] def protectedArray: Array[Short] =
    throw new ReadOnlyBufferException

  protected[nio] def protectedArrayOffset: Int =
    throw new ReadOnlyBufferException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Short): ShortBuffer =
    throw new ReadOnlyBufferException

  def put(index: Int, c: Short): ShortBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: Array[Short], off: Int, len: Int): ShortBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: ShortBuffer): ShortBuffer =
    throw new ReadOnlyBufferException

  def slice: ShortBuffer =
    new ReadOnlyShortArrayBuffer(remaining, backingArray, offset + position)
}
