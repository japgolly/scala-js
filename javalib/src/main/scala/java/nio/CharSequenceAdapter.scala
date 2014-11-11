package java.nio

object CharSequenceAdapter {
  private[nio] def copy(other: CharSequenceAdapter): CharSequenceAdapter = {
    val buf = new CharSequenceAdapter(other.sequence)
    buf._limit = other._limit
    buf._position = other._position
    buf._mark = other._mark
    buf
  }
}

final class CharSequenceAdapter private[nio] (chseq: CharSequence) extends CharBuffer(chseq.length) {
  private[nio] final val sequence = chseq

  def asReadOnlyBuffer: CharBuffer =
    duplicate

  def compact: CharBuffer =
    throw new ReadOnlyBufferException

  def duplicate: CharBuffer =
    CharSequenceAdapter.copy(this)

  def get: Char = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = sequence.charAt(position)
    _position += 1
    r
  }

  def get(index: Int): Char = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    sequence.charAt(index)
  }

  final override def get(dest: Array[Char], off: Int, len: Int): CharBuffer = {
    val length = dest.length
    if ((off < 0) || (len < 0) || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferUnderflowException
    val newPosition = position + len
    sequence.toString.getChars(position, newPosition, dest, off)
    _position = newPosition
    this
  }

  def isDirect: Boolean =
    false

  def isReadOnly: Boolean =
    true

  def order: ByteOrder =
    ByteOrder.nativeOrder

  protected[nio] def protectedArray: Array[Char] =
    throw new UnsupportedOperationException

  protected[nio] def protectedArrayOffset: Int =
    throw new UnsupportedOperationException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Char): CharBuffer =
    throw new ReadOnlyBufferException

  def put(index: Int, c: Char): CharBuffer =
    throw new ReadOnlyBufferException

  final override def put(src: Array[Char], off: Int, len: Int): CharBuffer = {
    if ((off < 0) || (len < 0) || off.toLong + len.toLong > src.length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    throw new ReadOnlyBufferException
  }

  override def put(src: String, start: Int, end: Int): CharBuffer = {
    if ((start < 0) || (end < 0) || start.toLong + end.toLong > src.length)
      throw new IndexOutOfBoundsException
    throw new ReadOnlyBufferException
  }

  def slice: CharBuffer =
    new CharSequenceAdapter(sequence.subSequence(position, limit))

  def subSequence(start: Int, end: Int): CharSequence = {
    if (end < start || start < 0 || end > remaining)
      throw new IndexOutOfBoundsException
    val result = CharSequenceAdapter.copy(this)
    result._position = position + start
    result._limit = position + end
    result
  }
}
