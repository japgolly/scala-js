package java.nio

object ReadOnlyFloatArrayBuffer {
  private[nio] def copy(other: FloatArrayBuffer, markOfOther: Int): ReadOnlyFloatArrayBuffer = {
    val buf = new ReadOnlyFloatArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadOnlyFloatArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Float], _arrayOffset: Int)
    extends FloatArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  def asReadOnlyBuffer: FloatBuffer =
    duplicate

  def compact: FloatBuffer =
    throw new ReadOnlyBufferException

  def duplicate: FloatBuffer =
    ReadOnlyFloatArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    true

  protected[nio] def protectedArray: Array[Float] =
    throw new ReadOnlyBufferException

  protected[nio] def protectedArrayOffset: Int =
    throw new ReadOnlyBufferException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Float): FloatBuffer =
    throw new ReadOnlyBufferException

  def put(index: Int, c: Float): FloatBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: Array[Float], off: Int, len: Int): FloatBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: FloatBuffer): FloatBuffer =
    throw new ReadOnlyBufferException

  def slice: FloatBuffer =
    new ReadOnlyFloatArrayBuffer(remaining, backingArray, offset + position)
}
