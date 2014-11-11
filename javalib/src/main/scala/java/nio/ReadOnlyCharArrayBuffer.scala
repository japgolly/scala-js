package java.nio

object ReadOnlyCharArrayBuffer {
  private[nio] def copy(other: CharArrayBuffer, markOfOther: Int): ReadOnlyCharArrayBuffer = {
    val buf = new ReadOnlyCharArrayBuffer(other.capacity, other.backingArray, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf
  }
}

final class ReadOnlyCharArrayBuffer private[nio] (_capacity: Int, _backingArray: Array[Char], _arrayOffset: Int)
    extends CharArrayBuffer(_capacity, _backingArray, _arrayOffset) {

  def asReadOnlyBuffer: CharBuffer =
    duplicate

  def compact: CharBuffer =
    throw new ReadOnlyBufferException

  def duplicate: CharBuffer =
    ReadOnlyCharArrayBuffer.copy(this, _mark)

  def isReadOnly: Boolean =
    true

  protected[nio] def protectedArray: Array[Char] =
    throw new ReadOnlyBufferException

  protected[nio] def protectedArrayOffset: Int =
    throw new ReadOnlyBufferException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Char): CharBuffer =
    throw new ReadOnlyBufferException

  def put(index: Int, c: Char): CharBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: Array[Char], off: Int, len: Int): CharBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: CharBuffer): CharBuffer =
    throw new ReadOnlyBufferException

  override def put(src: String, start: Int, end: Int): CharBuffer = {
    if ((start < 0) || (end < 0) || start.toLong + end.toLong > src.length)
      throw new IndexOutOfBoundsException
    throw new ReadOnlyBufferException
  }

  def slice: CharBuffer =
    new ReadOnlyCharArrayBuffer(remaining, backingArray, offset + position)
}
