package java.nio

object ReadWriteHeapByteBuffer {
  private[nio] def copy(other: HeapByteBuffer, markOfOther: Int): ReadWriteHeapByteBuffer = {
    val buf = new ReadWriteHeapByteBuffer(other.backingArray, other.capacity, other.offset)
    buf._limit = other._limit
    buf._position = other._position
    buf._mark = markOfOther
    buf.order(other.order)
    buf
  }
}

final class ReadWriteHeapByteBuffer private[nio] (backingArray: Array[Byte], capacity: Int, arrayOffset: Int)
    extends HeapByteBuffer(backingArray, capacity, arrayOffset) {

  private[nio] def this(backingArray: Array[Byte]) {
    this(backingArray, backingArray.length, 0)
  }

  private[nio] def this(capacity: Int) {
    this(new Array[Byte](capacity), capacity, 0)
  }

  override def asReadOnlyBuffer: ByteBuffer =
    ReadOnlyHeapByteBuffer.copy(this, _mark)

  override def compact: ByteBuffer = {
    System.arraycopy(backingArray, position + offset, backingArray, offset, remaining)
    _position = limit - position
    _limit = capacity
    _mark = Buffer.UNSET_MARK
    this
  }

  override def duplicate: ByteBuffer =
    ReadWriteHeapByteBuffer.copy(this, _mark)

  override def isReadOnly: Boolean =
    false

  protected[nio] def protectedArray: Array[Byte] =
    backingArray

  protected[nio] def protectedArrayOffset: Int =
    offset

  protected[nio] def protectedHasArray: Boolean =
    true

  override def put(b: Byte): ByteBuffer = {
    if (position == limit)
      throw new BufferOverflowException
    backingArray(offset + position) = b
    _position += 1
    this
  }

  override def put(index: Int, b: Byte): ByteBuffer = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index) = b
    this
  }

  override def put(src: Array[Byte], off: Int, len: Int): ByteBuffer = {
    if (off < 0 || len < 0 || off.toLong + len.toLong > src.length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    if (isReadOnly)
      throw new ReadOnlyBufferException
    System.arraycopy(src, off, backingArray, offset + position, len)
    _position += len
    this
  }

  override def putDouble(value: Double): ByteBuffer =
    putLong(doubleToRawLongBits(value))

  override def putDouble(index: Int, value: Double): ByteBuffer =
    putLong(index, doubleToRawLongBits(value))

  override def putFloat(value: Float): ByteBuffer =
    putInt(floatToIntBits(value))

  override def putFloat(index: Int, value: Float): ByteBuffer =
    putInt(index, floatToIntBits(value))

  override def putInt(value: Int): ByteBuffer = {
    val newPosition: Int = position + 4
    if (newPosition > limit)
      throw new BufferOverflowException
    store(position, value)
    _position = newPosition
    this
  }

  override def putInt(index: Int, value: Int): ByteBuffer = {
    if (index < 0 || index.toLong + 4 > limit)
      throw new IndexOutOfBoundsException
    store(index, value)
    this
  }

  override def putLong(index: Int, value: Long): ByteBuffer = {
    if (index < 0 || index.toLong + 8 > limit)
      throw new IndexOutOfBoundsException
    store(index, value)
    this
  }

  override def putLong(value: Long): ByteBuffer = {
    val newPosition: Int = position + 8
    if (newPosition > limit)
      throw new BufferOverflowException
    store(position, value)
    _position = newPosition
    this
  }

  override def putShort(index: Int, value: Short): ByteBuffer = {
    if (index < 0 || index.toLong + 2 > limit)
      throw new IndexOutOfBoundsException
    store(index, value)
    this
  }

  override def putShort(value: Short): ByteBuffer = {
    val newPosition: Int = position + 2
    if (newPosition > limit)
      throw new BufferOverflowException
    store(position, value)
    _position = newPosition
    this
  }

  override def slice: ByteBuffer = {
    val slice = new ReadWriteHeapByteBuffer(backingArray, remaining, offset + position)
    slice._order = order
    slice
  }
}
