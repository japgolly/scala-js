package java.nio

object ReadOnlyDoubleArrayBuffer {
  private[nio] def copy(other: DoubleArrayBuffer, markOfOther: Int): ReadOnlyDoubleArrayBuffer = {
    val buf = new ReadOnlyDoubleArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadOnlyDoubleArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Double], _arrayOffset: Int)
    extends DoubleArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  def asReadOnlyBuffer: DoubleBuffer =
    duplicate

  def compact: DoubleBuffer =
    throw new ReadOnlyBufferException

  def duplicate: DoubleBuffer =
    ReadOnlyDoubleArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    true

  protected[nio] def protectedArray: Array[Double] =
    throw new ReadOnlyBufferException

  protected[nio] def protectedArrayOffset: Int =
    throw new ReadOnlyBufferException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Double): DoubleBuffer =
    throw new ReadOnlyBufferException

  def put(index: Int, c: Double): DoubleBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: Array[Double], off: Int, len: Int): DoubleBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: DoubleBuffer): DoubleBuffer =
    throw new ReadOnlyBufferException

  def slice: DoubleBuffer =
    new ReadOnlyDoubleArrayBuffer(remaining, backingArray, offset + position)
}
