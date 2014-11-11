package java.nio

object DoubleToByteBufferAdapter {
  private[nio] def wrap(byteBuffer: ByteBuffer): DoubleBuffer =
    new DoubleToByteBufferAdapter(byteBuffer.slice)
}

final class DoubleToByteBufferAdapter private[nio] (byteBuffer: ByteBuffer)
    extends DoubleBuffer(byteBuffer.capacity >> 3) {

  byteBuffer.clear

  def asReadOnlyBuffer: DoubleBuffer = {
    val buf = new DoubleToByteBufferAdapter(byteBuffer.asReadOnlyBuffer)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def compact: DoubleBuffer = {
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

  def duplicate: DoubleBuffer = {
    val buf = new DoubleToByteBufferAdapter(byteBuffer.duplicate)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def get: Double = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = byteBuffer.getDouble(position << 3)
    _position += 1
    r
  }

  def get(index: Int): Double = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.getDouble(index << 3)
  }

  def isDirect: Boolean =
    byteBuffer.isDirect

  def isReadOnly: Boolean =
    byteBuffer.isReadOnly

  def order: ByteOrder =
    byteBuffer.order

  protected[nio] def protectedArray: Array[Double] =
    throw new UnsupportedOperationException

  protected[nio] def protectedArrayOffset: Int =
    throw new UnsupportedOperationException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Double): DoubleBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    byteBuffer.putDouble(position << 3, c)
    _position += 1
    this
  }

  def put(index: Int, c: Double): DoubleBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.putDouble(index << 3, c)
    this
  }

  def slice: DoubleBuffer = {
    byteBuffer.limit(limit << 3)
    byteBuffer.position(position << 3)
    val result = new DoubleToByteBufferAdapter(byteBuffer.slice)
    byteBuffer.clear
    result
  }
}
