package java.nio

object ShortBuffer {
  def allocate(capacity: Int): ShortBuffer = {
    if (capacity < 0)
      throw new IllegalArgumentException
    BufferFactory.newShortBuffer(capacity)
  }

  def wrap(array: Array[Short]): ShortBuffer =
    wrap(array, 0, array.length)

  def wrap(array: Array[Short], start: Int, len: Int): ShortBuffer = {
    if (array == null)
      throw new NullPointerException
    if (start < 0 || len < 0 || start.toLong + len.toLong > array.length)
      throw new IndexOutOfBoundsException
    val buf = BufferFactory.newShortBuffer(array)
    buf._position = start
    buf._limit = start + len
    buf
  }
}

abstract class ShortBuffer private[nio] (capacity: Int) extends Buffer(capacity) with Comparable[ShortBuffer] {

  final def array: Array[Short] =
    protectedArray

  final def arrayOffset: Int =
    protectedArrayOffset

  def asReadOnlyBuffer: ShortBuffer

  def compact: ShortBuffer

  def compareTo(otherBuffer: ShortBuffer): Int = {
    var compareRemaining = if ((remaining < otherBuffer.remaining)) remaining else otherBuffer.remaining
    var thisPos = position
    var otherPos = otherBuffer.position
    var thisByte: Short = 0
    var otherByte: Short = 0
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

  def duplicate: ShortBuffer

  override def equals(other: Any): Boolean = other match {
    case otherBuffer: ShortBuffer =>
      if (remaining != otherBuffer.remaining)
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

  def get(): Short

  def get(dest: Array[Short]): ShortBuffer =
    get(dest, 0, dest.length)

  def get(dest: Array[Short], off: Int, len: Int): ShortBuffer = {
    val length: Int = dest.length
    if (off < 0 || len < 0 || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferUnderflowException
    for (i <- (off until off+len))
      dest(i) = get()
    this
  }

  def get(index: Int): Short

  final def hasArray: Boolean =
    protectedHasArray

  override def hashCode: Int = {
    var myPosition = position
    var hash: Int = 0
    while (myPosition < limit) {
      hash = hash + get(myPosition)
      myPosition += 1
    }
    hash
  }

  def isDirect: Boolean

  def order: ByteOrder

  private[nio] def protectedArray: Array[Short]

  private[nio] def protectedArrayOffset: Int

  private[nio] def protectedHasArray: Boolean

  def put(s: Short): ShortBuffer

  final def put(src: Array[Short]): ShortBuffer =
    put(src, 0, src.length)

  def put(src: Array[Short], off: Int, len: Int): ShortBuffer = {
    val length: Int = src.length
    if (off < 0 || len < 0 || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    for (i <- (off until off+len))
      put(src(i))
    this
  }

  def put(src: ShortBuffer): ShortBuffer = {
    if (src eq this)
      throw new IllegalArgumentException
    if (src.remaining > remaining)
      throw new BufferOverflowException
    val contents = new Array[Short](src.remaining)
    src.get(contents)
    put(contents)
    this
  }

  def put(index: Int, s: Short): ShortBuffer

  def slice: ShortBuffer

  override def toString: String = {
    val buf = new StringBuilder
    buf.append(getClass.getName)
    buf.append(", status: capacity=")
    buf.append(capacity)
    buf.append(" position=")
    buf.append(position)
    buf.append(" limit=")
    buf.append(limit)
    buf.toString
  }
}
