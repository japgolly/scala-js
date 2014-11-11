package java.nio

object LongToByteBufferAdapter {
  private[nio] def wrap(byteBuffer: ByteBuffer): LongBuffer =
    new LongToByteBufferAdapter(byteBuffer.slice)
}

final class LongToByteBufferAdapter private[nio] (byteBuffer: ByteBuffer)
    extends LongBuffer(byteBuffer.capacity >> 3) {

  byteBuffer.clear

  def asReadOnlyBuffer: LongBuffer = {
    val buf = new LongToByteBufferAdapter(byteBuffer.asReadOnlyBuffer)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def compact: LongBuffer = {
    if (byteBuffer.isReadOnly)
      throw new ReadOnlyBufferException
    byteBuffer.limit(limit << 3)
    byteBuffer.position(position << 3)
    byteBuffer.compact
    byteBuffer.clear
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  def duplicate: LongBuffer = {
    val buf = new LongToByteBufferAdapter(byteBuffer.duplicate)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def get: Long = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = byteBuffer.getLong(position << 3)
    _position += 1
    r
  }

  def get(index: Int): Long = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.getLong(index << 3)
  }

  def isDirect: Boolean =
    byteBuffer.isDirect

  def isReadOnly: Boolean =
    byteBuffer.isReadOnly

  def order: ByteOrder =
    byteBuffer.order

  protected[nio] def protectedArray: Array[Long] =
    throw new UnsupportedOperationException

  protected[nio] def protectedArrayOffset: Int =
    throw new UnsupportedOperationException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Long): LongBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    byteBuffer.putLong(position << 3, c)
    _position += 1
    this
  }

  def put(index: Int, c: Long): LongBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.putLong(index << 3, c)
    this
  }

  def slice: LongBuffer = {
    byteBuffer.limit(limit << 3)
    byteBuffer.position(position << 3)
    val result = new LongToByteBufferAdapter(byteBuffer.slice)
    byteBuffer.clear
    result
  }
}
