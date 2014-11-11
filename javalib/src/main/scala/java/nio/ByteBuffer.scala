package java.nio

object ByteBuffer {

  def allocate(capacity: Int): ByteBuffer = {
    if (capacity < 0)
      throw new IllegalArgumentException()
    BufferFactory.newByteBuffer(capacity)
  }

  // def allocateDirect(capacity: Int): ByteBuffer = {
    // if (capacity < 0)
      // throw new IllegalArgumentException()
    // BufferFactory.newDirectByteBuffer(capacity)
  // }

  def wrap(array: Array[Byte]): ByteBuffer =
    BufferFactory.newByteBuffer(array)

  def wrap(array: Array[Byte], start: Int, len: Int): ByteBuffer = {
    val length = array.length
    if ((start < 0) || (len < 0) || start.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException()
    val buf = BufferFactory.newByteBuffer(array)
    buf._position = start
    buf._limit = start + len
    buf
  }
}

abstract class ByteBuffer private[nio] (capacity: Int) extends Buffer(capacity) with Comparable[ByteBuffer] {

  private[nio] var _order: ByteOrder =
    ByteOrder.BIG_ENDIAN

  final def array(): Array[Byte] =
    protectedArray()

  final def arrayOffset(): Int =
    protectedArrayOffset()

  def asCharBuffer(): CharBuffer

  def asDoubleBuffer(): DoubleBuffer

  def asFloatBuffer(): FloatBuffer

  def asIntBuffer(): IntBuffer

  def asLongBuffer(): LongBuffer

  def asReadOnlyBuffer(): ByteBuffer

  def asShortBuffer(): ShortBuffer

  def compact(): ByteBuffer

  def compareTo(otherBuffer: ByteBuffer): Int = {
    var compareRemaining = if (remaining < otherBuffer.remaining) remaining else otherBuffer.remaining
    var thisPos = position
    var otherPos = otherBuffer.position
    var thisByte: Byte = 0
    var otherByte: Byte = 0
    while (compareRemaining > 0) {
      thisByte = get(thisPos)
      otherByte = otherBuffer.get(otherPos)
      if (thisByte != otherByte)
        return if (thisByte < otherByte) -1 else 1
      thisPos += 1
      otherPos += 1
      compareRemaining -= 1
    }
    remaining - otherBuffer.remaining
  }

  def duplicate(): ByteBuffer

  override def equals(other: Any): Boolean = other match {
    case otherBuffer: ByteBuffer =>
      if (remaining() != otherBuffer.remaining())
        false
      else {
        var myPosition = position
        var otherPosition = otherBuffer.position
        var equalSoFar = true
        while (equalSoFar && (myPosition < limit)) {
          equalSoFar = get(myPosition) == otherBuffer.get(otherPosition)
          myPosition += 1
          otherPosition += 1
        }
        equalSoFar
      }
    case _ => false
  }

  def get(): Byte

  def get(dest: Array[Byte]): ByteBuffer =
    get(dest, 0, dest.length)

  def get(dest: Array[Byte], off: Int, len: Int): ByteBuffer = {
    val length = dest.length
    if ((off < 0) || (len < 0) || ((off: Long) + (len: Long) > length))
      throw new IndexOutOfBoundsException()
    if (len > remaining())
      throw new BufferUnderflowException()
    for (i <- (off until off+len))
      dest(i) = get()
    this
  }

  def get(index: Int): Byte

  def getChar(): Char

  def getChar(index: Int): Char

  def getDouble(): Double

  def getDouble(index: Int): Double

  def getFloat(): Float

  def getFloat(index: Int): Float

  def getInt(): Int

  def getInt(index: Int): Int

  def getLong(): Long

  def getLong(index: Int): Long

  def getShort(): Short

  def getShort(index: Int): Short

  final def hasArray(): Boolean =
    protectedHasArray()

  override def hashCode(): Int = {
    var myPosition = position
    var hash = 0
    while (myPosition < limit) {
      hash += get(myPosition)
      myPosition += 1
    }
    hash
  }

  def isDirect(): Boolean

  final def order(): ByteOrder = _order

  final def order(byteOrder: ByteOrder): ByteBuffer =
    orderImpl(byteOrder)

  private[nio] def orderImpl(byteOrder: ByteOrder): ByteBuffer = {
    _order = byteOrder
    this
  }

  private[nio] def protectedArray(): Array[Byte]

  private[nio] def protectedArrayOffset(): Int

  private[nio] def protectedHasArray(): Boolean

  def put(b: Byte): ByteBuffer

  final def put(src: Array[Byte]): ByteBuffer =
    put(src, 0, src.length)

  def put(src: Array[Byte], off: Int, len: Int): ByteBuffer = {
    val length = src.length
    if ((off < 0) || (len < 0) || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException()
    if (len > remaining)
      throw new BufferOverflowException()
    for (i <- (off until (off + len)))
      put(src(i))
    this
  }

  def put(src: ByteBuffer): ByteBuffer = {
    if (src eq this)
      throw new IllegalArgumentException()
    if (src.remaining > remaining)
      throw new BufferOverflowException()
    val contents = new Array[Byte](src.remaining)
    src.get(contents)
    put(contents)
    this
  }

  def put(index: Int, b: Byte): ByteBuffer

  def putChar(value: Char): ByteBuffer

  def putChar(index: Int, value: Char): ByteBuffer

  def putDouble(value: Double): ByteBuffer

  def putDouble(index: Int, value: Double): ByteBuffer

  def putFloat(value: Float): ByteBuffer

  def putFloat(index: Int, value: Float): ByteBuffer

  def putInt(value: Int): ByteBuffer

  def putInt(index: Int, value: Int): ByteBuffer

  def putLong(value: Long): ByteBuffer

  def putLong(index: Int, value: Long): ByteBuffer

  def putShort(value: Short): ByteBuffer

  def putShort(index: Int, value: Short): ByteBuffer

  def slice(): ByteBuffer

  override def toString(): String = {
    val buf = new StringBuilder()
    buf.append(getClass().getName())
    buf.append(", status: capacity=")
    buf.append(capacity())
    buf.append(" position=")
    buf.append(position())
    buf.append(" limit=")
    buf.append(limit())
    return buf.toString()
  }
}
