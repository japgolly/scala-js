package java.nio

object IntBuffer {
  def allocate(capacity: Int): IntBuffer = {
    if (capacity < 0)
      throw new IllegalArgumentException
    BufferFactory.newIntBuffer(capacity)
  }

  def wrap(array: Array[Int]): IntBuffer =
    wrap(array, 0, array.length)

  def wrap(array: Array[Int], start: Int, len: Int): IntBuffer = {
    if (array == null)
      throw new NullPointerException
    if (start < 0 || len < 0 || len.toLong + start.toLong > array.length)
      throw new IndexOutOfBoundsException
    val buf = BufferFactory.newIntBuffer(array)
    buf._position = start
    buf._limit = start + len
    buf
  }
}

abstract class IntBuffer private[nio] (capacity: Int) extends Buffer(capacity) with Comparable[IntBuffer] {

  final def array: Array[Int] =
    protectedArray

  final def arrayOffset: Int =
    protectedArrayOffset

  def asReadOnlyBuffer: IntBuffer

  def compact: IntBuffer

  def compareTo(otherBuffer: IntBuffer): Int = {
    var compareRemaining: Int = if ((remaining < otherBuffer.remaining)) remaining else otherBuffer.remaining
    var thisPos = position
    var otherPos = otherBuffer.position
    var thisByte: Int = 0
    var otherByte: Int = 0
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

  def duplicate: IntBuffer

  override def equals(other: Any): Boolean = other match {
    case otherBuffer: IntBuffer =>
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

  def get(): Int

  def get(dest: Array[Int]): IntBuffer =
    get(dest, 0, dest.length)

  def get(dest: Array[Int], off: Int, len: Int): IntBuffer = {
    val length = dest.length
    if (off < 0 || len < 0 || len.toLong + off.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferUnderflowException
    for (i <- (off until off+len))
      dest(i) = get
    this
  }

  def get(index: Int): Int

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

  private[nio] def protectedArray: Array[Int]

  private[nio] def protectedArrayOffset: Int

  private[nio] def protectedHasArray: Boolean

  def put(i: Int): IntBuffer

  final def put(src: Array[Int]): IntBuffer =
    put(src, 0, src.length)

  def put(src: Array[Int], off: Int, len: Int): IntBuffer = {
    val length = src.length
    if (off < 0 || len < 0 || len.toLong + off.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    for (i <- (off until off+len))
      put(src(i))
    this
  }

  def put(src: IntBuffer): IntBuffer = {
    if (src eq this)
      throw new IllegalArgumentException
    if (src.remaining > remaining)
      throw new BufferOverflowException
    val contents = new Array[Int](src.remaining)
    src.get(contents)
    put(contents)
    this
  }

  def put(index: Int, i: Int): IntBuffer

  def slice: IntBuffer

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
