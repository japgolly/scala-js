package java.nio

object DoubleBuffer {
  def allocate(capacity: Int): DoubleBuffer = {
    if (capacity < 0)
      throw new IllegalArgumentException
    BufferFactory.newDoubleBuffer(capacity)
  }

  def wrap(array: Array[Double]): DoubleBuffer =
    wrap(array, 0, array.length)

  def wrap(array: Array[Double], start: Int, len: Int): DoubleBuffer = {
    val length: Int = array.length
    if (start < 0 || len < 0 || start.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    val buf = BufferFactory.newDoubleBuffer(array)
    buf._position = start
    buf._limit = start + len
    buf
  }
}

abstract class DoubleBuffer private[nio] (capacity: Int) extends Buffer(capacity) with Comparable[DoubleBuffer] {

  final def array: Array[Double] =
    protectedArray

  final def arrayOffset: Int =
    protectedArrayOffset

  def asReadOnlyBuffer: DoubleBuffer

  def compact: DoubleBuffer

  def compareTo(otherBuffer: DoubleBuffer): Int = {
    var compareRemaining: Int = if ((remaining < otherBuffer.remaining)) remaining else otherBuffer.remaining
    var thisPos = position
    var otherPos = otherBuffer.position
    var thisDouble: Double = 0.0
    var otherDouble: Double = 0.0
    while (compareRemaining > 0) {
      thisDouble = get(thisPos)
      otherDouble = otherBuffer.get(otherPos)
      if ((thisDouble != otherDouble) && ((thisDouble == thisDouble) || (otherDouble == otherDouble)))
        return if (thisDouble < otherDouble) -1 else 1
      thisPos += 1
      otherPos += 1
      compareRemaining -= 1
    }
    remaining - otherBuffer.remaining
  }

  def duplicate: DoubleBuffer

  override def equals(other: Any): Boolean = other match {
    case otherBuffer: CharBuffer =>
      if (remaining != otherBuffer.remaining)
        false
      else {
        var myPosition = position
        var otherPosition = otherBuffer.position
        var equalSoFar = true
        while (equalSoFar && (myPosition < limit)) {
          val a = get(myPosition)
          val b = otherBuffer.get(otherPosition)
          myPosition += 1
          otherPosition += 1
          equalSoFar = a == b || (a != a && b != b)
        }
        equalSoFar
      }
    case _ => false
  }

  def get(): Double

  def get(dest: Array[Double]): DoubleBuffer =
    get(dest, 0, dest.length)

  def get(dest: Array[Double], off: Int, len: Int): DoubleBuffer = {
    val length: Int = dest.length
    if (off < 0 || len < 0 || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferUnderflowException
    for (i <- (off until off+len))
      dest(i) = get()
    this
  }

  def get(index: Int): Double

  final def hasArray: Boolean =
    protectedHasArray

  override def hashCode: Int = {
    var myPosition = position
    var hash: Int = 0
    while (myPosition < limit) {
      val l = doubleToLongBits(get(myPosition))
      myPosition += 1
      hash = hash + (l.toInt) ^ ((l >> 32).toInt)
    }
    hash
  }

  def isDirect: Boolean

  def order: ByteOrder

  private[nio] def protectedArray: Array[Double]

  private[nio] def protectedArrayOffset: Int

  private[nio] def protectedHasArray: Boolean

  def put(d: Double): DoubleBuffer

  final def put(src: Array[Double]): DoubleBuffer =
    put(src, 0, src.length)

  def put(src: Array[Double], off: Int, len: Int): DoubleBuffer = {
    val length: Int = src.length
    if (off < 0 || len < 0 || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    for (i <- (off until off+len))
      put(src(i))
    this
  }

  def put(src: DoubleBuffer): DoubleBuffer = {
    if (src eq this)
      throw new IllegalArgumentException
    if (src.remaining > remaining)
      throw new BufferOverflowException
    val doubles = new Array[Double](src.remaining)
    src.get(doubles)
    put(doubles)
    this
  }

  def put(index: Int, d: Double): DoubleBuffer

  def slice: DoubleBuffer

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
