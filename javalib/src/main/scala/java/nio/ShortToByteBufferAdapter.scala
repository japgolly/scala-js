package java.nio

object ShortToByteBufferAdapter {
  private[nio] def wrap(byteBuffer: ByteBuffer): ShortBuffer =
    new ShortToByteBufferAdapter(byteBuffer.slice)
}

final class ShortToByteBufferAdapter private[nio] (byteBuffer: ByteBuffer)
    extends ShortBuffer(byteBuffer.capacity >> 1) {

  byteBuffer.clear

  def asReadOnlyBuffer: ShortBuffer = {
    val buf = new ShortToByteBufferAdapter(byteBuffer.asReadOnlyBuffer)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def compact: ShortBuffer = {
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

  def duplicate: ShortBuffer = {
    val buf = new ShortToByteBufferAdapter(byteBuffer.duplicate)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def get: Short = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = byteBuffer.getShort(position << 1)
    _position += 1
    r
  }

  def get(index: Int): Short = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.getShort(index << 1)
  }

  def isDirect: Boolean =
    byteBuffer.isDirect

  def isReadOnly: Boolean =
    byteBuffer.isReadOnly

  def order: ByteOrder =
    byteBuffer.order

  protected[nio] def protectedArray: Array[Short] =
    throw new UnsupportedOperationException

  protected[nio] def protectedArrayOffset: Int =
    throw new UnsupportedOperationException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Short): ShortBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    byteBuffer.putShort(position << 1, c)
    _position += 1
    this
  }

  def put(index: Int, c: Short): ShortBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.putShort(index << 1, c)
    this
  }

  def slice: ShortBuffer = {
    byteBuffer.limit(limit << 1)
    byteBuffer.position(position << 1)
    val result = new ShortToByteBufferAdapter(byteBuffer.slice)
    byteBuffer.clear
    result
  }
}
