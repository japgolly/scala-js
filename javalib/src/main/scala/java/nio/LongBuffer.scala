package java.nio

object LongBuffer {
  def allocate(capacity: Int): LongBuffer = {
    if (capacity < 0)
      throw new IllegalArgumentException
    BufferFactory.newLongBuffer(capacity)
  }

  def wrap(array: Array[Long]): LongBuffer =
    wrap(array, 0, array.length)

  def wrap(array: Array[Long], start: Int, len: Int): LongBuffer = {
    if (array == null)
      throw new NullPointerException
    if (start < 0 || len < 0 || len.toLong + start.toLong > array.length)
      throw new IndexOutOfBoundsException
    val buf = BufferFactory.newLongBuffer(array)
    buf._position = start
    buf._limit = start + len
    buf
  }
}

abstract class LongBuffer private[nio] (capacity: Int) extends Buffer(capacity) with Comparable[LongBuffer] {

  final def array: Array[Long] =
    protectedArray

  final def arrayOffset: Int =
    protectedArrayOffset

  def asReadOnlyBuffer: LongBuffer

  def compact: LongBuffer

  def compareTo(otherBuffer: LongBuffer): Int = {
    var compareRemaining = if ((remaining < otherBuffer.remaining)) remaining else otherBuffer.remaining
    var thisPos = position
    var otherPos = otherBuffer.position
    var thisByte: Long = 0L
    var otherByte: Long = 0L
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

  def duplicate: LongBuffer

  override def equals(other: Any): Boolean = other match {
    case otherBuffer: LongBuffer =>
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

  def get(): Long

  def get(dest: Array[Long]): LongBuffer =
    get(dest, 0, dest.length)

  def get(dest: Array[Long], off: Int, len: Int): LongBuffer = {
    val length: Int = dest.length
    if (off < 0 || len < 0 || len.toLong + off.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferUnderflowException
    for (i <- (off until off+len))
      dest(i) = get
    this
  }

  def get(index: Int): Long

  final def hasArray: Boolean =
    protectedHasArray

  override def hashCode: Int = {
    var myPosition = position
    var hash: Int = 0
    while (myPosition < limit) {
      val l = get(myPosition)
      hash = hash + (l.toInt) ^ ((l >> 32).toInt)
      myPosition += 1
    }
    hash
  }

  def isDirect: Boolean

  def order: ByteOrder

  private[nio] def protectedArray: Array[Long]

  private[nio] def protectedArrayOffset: Int

  private[nio] def protectedHasArray: Boolean

  def put(l: Long): LongBuffer

  final def put(src: Array[Long]): LongBuffer =
    put(src, 0, src.length)

  def put(src: Array[Long], off: Int, len: Int): LongBuffer = {
    val length = src.length
    if (off < 0 || len < 0 || len.toLong + off.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    for (i <- (off until off+len))
      put(src(i))
    this
  }

  def put(src: LongBuffer): LongBuffer = {
    if (src eq this)
      throw new IllegalArgumentException
    if (src.remaining > remaining)
      throw new BufferOverflowException
    val contents = new Array[Long](src.remaining)
    src.get(contents)
    put(contents)
    this
  }

  def put(index: Int, l: Long): LongBuffer

  def slice: LongBuffer

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
