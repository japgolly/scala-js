package java.nio

object FloatToByteBufferAdapter {
  private[nio] def wrap(byteBuffer: ByteBuffer): FloatBuffer =
    new FloatToByteBufferAdapter(byteBuffer.slice)
}

final class FloatToByteBufferAdapter private[nio] (byteBuffer: ByteBuffer)
    extends FloatBuffer(byteBuffer.capacity >> 2) {

  byteBuffer.clear

  def asReadOnlyBuffer: FloatBuffer = {
    val buf = new FloatToByteBufferAdapter(byteBuffer.asReadOnlyBuffer)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def compact: FloatBuffer = {
    if (byteBuffer.isReadOnly)
      throw new ReadOnlyBufferException
    byteBuffer.limit(limit << 2)
    byteBuffer.position(position << 2)
    byteBuffer.compact
    byteBuffer.clear
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  def duplicate: FloatBuffer = {
    val buf = new FloatToByteBufferAdapter(byteBuffer.duplicate)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def get: Float = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = byteBuffer.getFloat(position << 2)
    _position += 1
    r
  }

  def get(index: Int): Float = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.getFloat(index << 2)
  }

  def isDirect: Boolean =
    byteBuffer.isDirect

  def isReadOnly: Boolean =
    byteBuffer.isReadOnly

  def order: ByteOrder =
    byteBuffer.order

  protected[nio] def protectedArray: Array[Float] =
    throw new UnsupportedOperationException

  protected[nio] def protectedArrayOffset: Int =
    throw new UnsupportedOperationException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Float): FloatBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    byteBuffer.putFloat(position << 2, c)
    _position += 1
    this
  }

  def put(index: Int, c: Float): FloatBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.putFloat(index << 2, c)
    this
  }

  def slice: FloatBuffer = {
    byteBuffer.limit(limit << 2)
    byteBuffer.position(position << 2)
    val result = new FloatToByteBufferAdapter(byteBuffer.slice)
    byteBuffer.clear
    result
  }
}
