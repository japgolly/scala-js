package java.nio

object IntToByteBufferAdapter {
  private[nio] def wrap(byteBuffer: ByteBuffer): IntBuffer =
    new IntToByteBufferAdapter(byteBuffer.slice)
}

final class IntToByteBufferAdapter private[nio] (byteBuffer: ByteBuffer)
    extends IntBuffer(byteBuffer.capacity >> 2) {

  byteBuffer.clear

  def asReadOnlyBuffer: IntBuffer = {
    val buf = new IntToByteBufferAdapter(byteBuffer.asReadOnlyBuffer)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def compact: IntBuffer = {
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

  def duplicate: IntBuffer = {
    val buf = new IntToByteBufferAdapter(byteBuffer.duplicate)
    buf._limit = limit
    buf._position = position
    buf._mark = _mark
    buf
  }

  def get: Int = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = byteBuffer.getInt(position << 2)
    _position += 1
    r
  }

  def get(index: Int): Int = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.getInt(index << 2)
  }

  def isDirect: Boolean =
    byteBuffer.isDirect

  def isReadOnly: Boolean =
    byteBuffer.isReadOnly

  def order: ByteOrder =
    byteBuffer.order

  protected[nio] def protectedArray: Array[Int] =
    throw new UnsupportedOperationException

  protected[nio] def protectedArrayOffset: Int =
    throw new UnsupportedOperationException

  protected[nio] def protectedHasArray: Boolean =
    false

  def put(c: Int): IntBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    byteBuffer.putInt(position << 2, c)
    _position += 1
    this
  }

  def put(index: Int, c: Int): IntBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    byteBuffer.putInt(index << 2, c)
    this
  }

  def slice: IntBuffer = {
    byteBuffer.limit(limit << 2)
    byteBuffer.position(position << 2)
    val result = new IntToByteBufferAdapter(byteBuffer.slice)
    byteBuffer.clear
    result
  }
}
