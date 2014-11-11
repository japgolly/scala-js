package java.nio

object FloatBuffer {
  def allocate(capacity: Int): FloatBuffer = {
    if (capacity < 0)
      throw new IllegalArgumentException
    BufferFactory.newFloatBuffer(capacity)
  }

  def wrap(array: Array[Float]): FloatBuffer =
    wrap(array, 0, array.length)

  def wrap(array: Array[Float], start: Int, len: Int): FloatBuffer = {
    if (array == null)
      throw new NullPointerException
    if (start < 0 || len < 0 || start.toLong + len.toLong > array.length)
      throw new IndexOutOfBoundsException
    val buf = BufferFactory.newFloatBuffer(array)
    buf._position = start
    buf._limit = start + len
    buf
  }
}

abstract class FloatBuffer private[nio] (_capacity: Int) extends Buffer(_capacity) with Comparable[FloatBuffer] {

  final def array: Array[Float] =
    protectedArray

  final def arrayOffset: Int =
    protectedArrayOffset

  def asReadOnlyBuffer: FloatBuffer

  def compact: FloatBuffer

  def compareTo(otherBuffer: FloatBuffer): Int = {
    var compareRemaining = if ((remaining < otherBuffer.remaining)) remaining else otherBuffer.remaining
    var thisPos = position
    var otherPos = otherBuffer.position
    var thisFloat: Float = 0
    var otherFloat: Float = 0
    while (compareRemaining > 0) {
      thisFloat = get(thisPos)
      otherFloat = otherBuffer.get(otherPos)
      if ((thisFloat != otherFloat) && ((thisFloat == thisFloat) || (otherFloat == otherFloat)))
        return if (thisFloat < otherFloat) -1 else 1
      thisPos += 1
      otherPos += 1
      compareRemaining -= 1
    }
    remaining - otherBuffer.remaining
  }

  def duplicate: FloatBuffer

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

  def get(): Float

  def get(dest: Array[Float]): FloatBuffer =
    get(dest, 0, dest.length)

  def get(dest: Array[Float], off: Int, len: Int): FloatBuffer = {
    val length: Int = dest.length
    if (off < 0 || len < 0 || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferUnderflowException
    for (i <- (off until off+len))
      dest(i) = get()
    this
  }

  def get(index: Int): Float

  final def hasArray: Boolean =
    protectedHasArray

  override def hashCode: Int = {
    var myPosition = position
    var hash: Int = 0
    while (myPosition < limit) {
      hash += floatToIntBits(get(myPosition))
      myPosition += 1
    }
    hash
  }

  def isDirect: Boolean

  def order: ByteOrder

  private[nio] def protectedArray: Array[Float]

  private[nio] def protectedArrayOffset: Int

  private[nio] def protectedHasArray: Boolean

  def put(f: Float): FloatBuffer

  final def put(src: Array[Float]): FloatBuffer =
    put(src, 0, src.length)

  def put(src: Array[Float], off: Int, len: Int): FloatBuffer = {
    val length: Int = src.length
    if (off < 0 || len < 0 || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    for (i <- (off until off+len))
      put(src(i))
    this
  }

  def put(src: FloatBuffer): FloatBuffer = {
    if (src eq this)
      throw new IllegalArgumentException
    if (src.remaining > remaining)
      throw new BufferOverflowException
    val contents = new Array[Float](src.remaining)
    src.get(contents)
    put(contents)
    this
  }

  def put(index: Int, f: Float): FloatBuffer

  def slice: FloatBuffer

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
