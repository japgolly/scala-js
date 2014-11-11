package java.nio

object ReadOnlyHeapByteBuffer {
  private[nio] def copy(other: HeapByteBuffer, markOfOther: Int): ReadOnlyHeapByteBuffer = {
    val buf = new ReadOnlyHeapByteBuffer(other.backingArray, other.capacity, other.offset)
    buf._limit = other.limit
    buf._position = other.position
    buf._mark = markOfOther
    buf.order(other.order)
    buf
  }
}

final class ReadOnlyHeapByteBuffer private[nio] (_backingArray: Array[Byte], _capacity: Int, _arrayOffset: Int)
    extends HeapByteBuffer(_backingArray, _capacity, _arrayOffset) {

  override def asReadOnlyBuffer: ByteBuffer =
    ReadOnlyHeapByteBuffer.copy(this, _mark)

  override def compact: ByteBuffer =
    throw new ReadOnlyBufferException

  override def duplicate: ByteBuffer =
    ReadOnlyHeapByteBuffer.copy(this, _mark)

  override def isReadOnly: Boolean =
    true

  protected[nio] def protectedArray: Array[Byte] =
    throw new ReadOnlyBufferException

  protected[nio] def protectedArrayOffset: Int =
    throw new ReadOnlyBufferException

  protected[nio] def protectedHasArray: Boolean =
    false

  override def put(b: Byte): ByteBuffer =
    throw new ReadOnlyBufferException

  override def put(index: Int, b: Byte): ByteBuffer =
    throw new ReadOnlyBufferException

  override def put(src: Array[Byte], off: Int, len: Int): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putDouble(value: Double): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putDouble(index: Int, value: Double): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putFloat(value: Float): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putFloat(index: Int, value: Float): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putInt(value: Int): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putInt(index: Int, value: Int): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putLong(index: Int, value: Long): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putLong(value: Long): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putShort(index: Int, value: Short): ByteBuffer =
    throw new ReadOnlyBufferException

  override def putShort(value: Short): ByteBuffer =
    throw new ReadOnlyBufferException

  override def put(buf: ByteBuffer): ByteBuffer =
    throw new ReadOnlyBufferException

  override def slice: ByteBuffer = {
    val slice = new ReadOnlyHeapByteBuffer(backingArray, remaining, offset + position)
    slice._order = order
    slice
  }
}
