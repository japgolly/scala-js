package java.nio

object CharToByteBufferAdapter {
  private[nio] def wrap(byteBuffer: ByteBuffer): CharBuffer =
    new CharToByteBufferAdapter(byteBuffer.slice)
}

final class CharToByteBufferAdapter private[nio] (byteBuffer: ByteBuffer)
    extends CharBuffer(byteBuffer.capacity >> 1) {

  byteBuffer.clear

  def asReadOnlyBuffer: CharBuffer = {
    val buf = new CharToByteBufferAdapter(byteBuffer.asReadOnlyBuffer)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def compact: CharBuffer = {
    if (byteBuffer.isReadOnly)
      throw new ReadOnlyBufferException
    byteBuffer.limit(limit << 1)
    byteBuffer.position(position << 1)
    byteBuffer.compact
    byteBuffer.clear
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  def duplicate: CharBuffer = {
    val buf = new CharToByteBufferAdapter(byteBuffer.duplicate)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def get: Char = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = byteBuffer.getChar(position << 1)
    _position += 1
    r
  }

  def get(index: Int): Char = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.getChar(index << 1)
  }

  def isDirect: Boolean =
    byteBuffer.isDirect

  def isReadOnly: Boolean =
    byteBuffer.isReadOnly

  def order: ByteOrder =
    byteBuffer.order

  protected[nio] def protectedArray: Array[Char] =
    throw new UnsupportedOperationException

  protected[nio] def protectedArrayOffset: Int =
    throw new UnsupportedOperationException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Char): CharBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    byteBuffer.putChar(position << 1, c)
    _position += 1
    this
  }

  def put(index: Int, c: Char): CharBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.putChar(index << 1, c)
    this
  }

  def slice: CharBuffer = {
    byteBuffer.limit(limit << 1)
    byteBuffer.position(position << 1)
    val result = new CharToByteBufferAdapter(byteBuffer.slice)
    byteBuffer.clear
    result
  }

  def subSequence(start: Int, end: Int): CharSequence = {
    if (start < 0 || end < start || end > remaining)
      throw new IndexOutOfBoundsException
    val result = duplicate
    result.limit(position + end)
    result.position(position + start)
    result
  }
}
