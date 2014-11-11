package java.nio

abstract class HeapByteBuffer private[nio] (_backingArray: Array[Byte], _capacity: Int, _offset: Int)
    extends ByteBuffer(_capacity) {
  protected[nio] final def backingArray = _backingArray
  protected[nio] final def offset = _offset
  if (offset + capacity > backingArray.length)
    throw new IndexOutOfBoundsException

  private[nio] def this(backingArray: Array[Byte]) {
    this(backingArray, backingArray.length, 0)
  }

  private[nio] def this(capacity: Int) {
    this(new Array[Byte](capacity), capacity, 0)
  }

  final override def get(dest: Array[Byte], off: Int, len: Int): ByteBuffer = {
    val length = dest.length
    if (off < 0 || len < 0 || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferUnderflowException
    System.arraycopy(backingArray, offset + position, dest, off, len)
    _position += len
    this
  }

  final def get: Byte = {
    if (position == limit)
      throw new BufferUnderflowException
    val r = backingArray(offset + position)
    _position += 1
    r
  }

  final def get(index: Int): Byte = {
    if (index < 0 || index >= limit)
      throw new IndexOutOfBoundsException
    backingArray(offset + index)
  }

  final def getDouble: Double =
    longBitsToDouble(getLong)

  final def getDouble(index: Int): Double =
    longBitsToDouble(getLong(index))

  final def getFloat: Float =
    intBitsToFloat(getInt)

  final def getFloat(index: Int): Float =
    intBitsToFloat(getInt(index))

  final def getInt: Int = {
    val newPosition = position + 4
    if (newPosition > limit)
      throw new BufferUnderflowException
    val result = loadInt(position)
    _position = newPosition
    result
  }

  final def getInt(index: Int): Int = {
    if (index < 0 || index + 4 > limit)
      throw new IndexOutOfBoundsException
    loadInt(index)
  }

  final def getLong: Long = {
    val newPosition = position + 8
    if (newPosition > limit)
      throw new BufferUnderflowException
    val result = loadLong(position)
    _position = newPosition
    result
  }

  final def getLong(index: Int): Long = {
    if (index < 0 || index + 8 > limit)
      throw new IndexOutOfBoundsException
    loadLong(index)
  }

  final def getShort: Short = {
    val newPosition = position + 2
    if (newPosition > limit)
      throw new BufferUnderflowException
    val result = loadShort(position)
    _position = newPosition
    result
  }

  final def getShort(index: Int): Short = {
    if (index < 0 || index + 2 > limit)
      throw new IndexOutOfBoundsException
    loadShort(index)
  }

  final def isDirect: Boolean =
    false

  protected final def loadInt(index: Int): Int = {
    val baseOffset = offset + index
    var bytes = 0
    if (order eq ByteOrder.BIG_ENDIAN) {
      var i = 0
      while (i < 4) {
        bytes = bytes << 8
        bytes = bytes | (backingArray(baseOffset + i) & 0xFF)
        i += 1
      }
    } else {
      var i = 3
      while (i >= 0) {
        bytes = bytes << 8
        bytes = bytes | (backingArray(baseOffset + i) & 0xFF)
        i -= 1
      }
    }
    bytes
  }

  protected final def loadLong(index: Int): Long = {
    val baseOffset = offset + index
    var bytes = 0L
    if (order eq ByteOrder.BIG_ENDIAN) {
      var i = 0
      while (i < 8) {
        bytes = bytes << 8
        bytes = bytes | (backingArray(baseOffset + i) & 0xFF)
        i += 1
      }
    } else {
      var i = 7
      while (i >= 0) {
        bytes = bytes << 8
        bytes = bytes | (backingArray(baseOffset + i) & 0xFF)
        i -= 1
      }
    }
    bytes
  }

  protected final def loadShort(index: Int): Short = {
    val baseOffset = offset + index
    var bytes: Short = 0
    if (order eq ByteOrder.BIG_ENDIAN) {
      bytes = (backingArray(baseOffset) << 8).toShort
      bytes = (bytes | (backingArray(baseOffset + 1) & 0xFF)).toShort
    } else {
      bytes = (backingArray(baseOffset + 1) << 8).toShort
      bytes = (bytes | (backingArray(baseOffset) & 0xFF)).toShort
    }
    bytes
  }

  protected final def store(index: Int, _value: Int): Unit = {
    val baseOffset = offset + index
    var value = _value
    if (order eq ByteOrder.BIG_ENDIAN) {
      var i = 3
      while (i >= 0) {
        backingArray(baseOffset + i) = (value & 0xFF).toByte
        value = value >> 8
        i -= 1
      }
    } else {
      var i = 0
      while (i <= 3) {
        backingArray(baseOffset + i) = (value & 0xFF).toByte
        value = value >> 8
        i += 1
      }
    }
  }

  protected final def store(index: Int, _value: Long): Unit = {
    val baseOffset = offset + index
    var value = _value
    if (order eq ByteOrder.BIG_ENDIAN) {
      var i = 7
      while (i >= 0) {
        backingArray(baseOffset + i) = (value & 0xFF).toByte
        value = value >> 8
        i -= 1
      }
    } else {
      var i = 0
      while (i <= 7) {
        backingArray(baseOffset + i) = (value & 0xFF).toByte
        value = value >> 8
        i += 1
      }
    }
  }

  protected final def store(index: Int, value: Short): Unit = {
    val baseOffset: Int = offset + index
    if (order eq ByteOrder.BIG_ENDIAN) {
      backingArray(baseOffset) = ((value >> 8) & 0xFF).toByte
      backingArray(baseOffset + 1) = (value & 0xFF).toByte
    }
    else {
      backingArray(baseOffset + 1) = ((value >> 8) & 0xFF).toByte
      backingArray(baseOffset) = (value & 0xFF).toByte
    }
  }

  final def asCharBuffer: CharBuffer =
    CharToByteBufferAdapter.wrap(this)

  final def asDoubleBuffer: DoubleBuffer =
    DoubleToByteBufferAdapter.wrap(this)

  final def asFloatBuffer: FloatBuffer =
    FloatToByteBufferAdapter.wrap(this)

  final def asIntBuffer: IntBuffer =
    IntToByteBufferAdapter.wrap(this)

  final def asLongBuffer: LongBuffer =
    LongToByteBufferAdapter.wrap(this)

  final def asShortBuffer: ShortBuffer =
    ShortToByteBufferAdapter.wrap(this)

  final def getChar: Char =
    getShort.toChar

  final def getChar(index: Int): Char =
    getShort(index).toChar

  final def putChar(value: Char): ByteBuffer =
    putShort(value.toShort)

  final def putChar(index: Int, value: Char): ByteBuffer =
    putShort(index, value.toShort)
}
