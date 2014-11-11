package java.nio

object ReadOnlyIntArrayBuffer {
  private[nio] def copy(other: IntArrayBuffer, markOfOther: Int): ReadOnlyIntArrayBuffer = {
    val buf = new ReadOnlyIntArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadOnlyIntArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Int], _arrayOffset: Int)
    extends IntArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  def asReadOnlyBuffer: IntBuffer =
    duplicate

  def compact: IntBuffer =
    throw new ReadOnlyBufferException

  def duplicate: IntBuffer =
    ReadOnlyIntArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    true

  protected[nio] def protectedArray: Array[Int] =
    throw new ReadOnlyBufferException

  protected[nio] def protectedArrayOffset: Int =
    throw new ReadOnlyBufferException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Int): IntBuffer =
    throw new ReadOnlyBufferException

  def put(index: Int, c: Int): IntBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: Array[Int], off: Int, len: Int): IntBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: IntBuffer): IntBuffer =
    throw new ReadOnlyBufferException

  def slice: IntBuffer =
    new ReadOnlyIntArrayBuffer(remaining, backingArray, offset + position)
}
