package java.nio

import java.io.IOException

object CharBuffer {
  def allocate(capacity: Int): CharBuffer = {
    if (capacity < 0)
      throw new IllegalArgumentException
    BufferFactory.newCharBuffer(capacity)
  }

  def wrap(array: Array[Char]): CharBuffer =
    wrap(array, 0, array.length)

  def wrap(array: Array[Char], start: Int, len: Int): CharBuffer = {
    val length: Int = array.length
    if ((start < 0) || (len < 0) || start.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    val buf = BufferFactory.newCharBuffer(array)
    buf._position = start
    buf._limit = start + len
    buf
  }

  def wrap(chseq: CharSequence): CharBuffer =
    BufferFactory.newCharBuffer(chseq)

  def wrap(chseq: CharSequence, start: Int, end: Int): CharBuffer = {
    if (chseq == null)
      throw new NullPointerException
    if (start < 0 || end < start || end > chseq.length)
      throw new IndexOutOfBoundsException
    val result: CharBuffer = BufferFactory.newCharBuffer(chseq)
    result._position = start
    result._limit = end
    result
  }
}

abstract class CharBuffer private[nio] (capacity: Int) extends Buffer(capacity) with Comparable[CharBuffer] with CharSequence with Appendable with Readable {

  final def array: Array[Char] =
    protectedArray

  final def arrayOffset: Int =
    protectedArrayOffset

  def asReadOnlyBuffer: CharBuffer

  final def charAt(index: Int): Char = {
    if (index < 0 || index >= remaining)
      throw new IndexOutOfBoundsException
    get(position + index)
  }

  def compact: CharBuffer

  def compareTo(otherBuffer: CharBuffer): Int = {
    var compareRemaining: Int = if ((remaining < otherBuffer.remaining)) remaining else otherBuffer.remaining
    var thisPos = position
    var otherPos = otherBuffer.position
    var thisByte: Char = 0
    var otherByte: Char = 0
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

  def duplicate: CharBuffer

  override def equals(other: Any): Boolean = other match {
    case otherBuffer: CharBuffer =>
      if (remaining != otherBuffer.remaining)
        false
      else {
        var myPosition = position
        var otherPosition = otherBuffer.position
        var equalSoFar: Boolean = true
        while (equalSoFar && (myPosition < limit)) {
          equalSoFar = get(myPosition) == otherBuffer.get(otherPosition)
          myPosition += 1
          otherPosition += 1
        }
        equalSoFar
      }
    case _ => false
  }

  def get(): Char

  def get(dest: Array[Char]): CharBuffer =
    get(dest, 0, dest.length)

  def get(dest: Array[Char], off: Int, len: Int): CharBuffer = {
    val length = dest.length
    if ((off < 0) || (len < 0) || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferUnderflowException
    for (i <- (off until off+len))
      dest(i) = get()
    this
  }

  def get(index: Int): Char

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

  final def length: Int =
    remaining

  def order: ByteOrder

  private[nio] def protectedArray: Array[Char]

  private[nio] def protectedArrayOffset: Int

  private[nio] def protectedHasArray: Boolean

  def put(c: Char): CharBuffer

  final def put(src: Array[Char]): CharBuffer =
    put(src, 0, src.length)

  def put(src: Array[Char], off: Int, len: Int): CharBuffer = {
    val length = src.length
    if ((off < 0) || (len < 0) || off.toLong + len.toLong > length)
      throw new IndexOutOfBoundsException
    if (len > remaining)
      throw new BufferOverflowException
    for (i <- (off until off+len))
      put(src(i))
    this
  }

  def put(src: CharBuffer): CharBuffer = {
    if (src eq this)
      throw new IllegalArgumentException
    if (src.remaining > remaining)
      throw new BufferOverflowException
    val contents = new Array[Char](src.remaining)
    src.get(contents)
    put(contents)
    this
  }

  def put(index: Int, c: Char): CharBuffer

  final def put(str: String): CharBuffer =
    put(str, 0, str.length)

  def put(str: String, start: Int, end: Int): CharBuffer = {
    val length: Int = str.length
    if (start < 0 || end < start || end > length)
      throw new IndexOutOfBoundsException
    if (end - start > remaining)
      throw new BufferOverflowException
    for (i <- (start until end))
      put(str.charAt(i))
    this
  }

  def slice: CharBuffer

  def subSequence(start: Int, end: Int): CharSequence

  override def toString: String = {
    val strbuf: StringBuilder = new StringBuilder
    for (i <- (position until limit))
      strbuf.append(get(i))
    return strbuf.toString
  }

  def append(c: Char): CharBuffer =
    put(c)

  def append(csq: CharSequence): CharBuffer =
    if (csq != null)
      put(csq.toString)
    else
      put("null")

  def append(_csq: CharSequence, start: Int, end: Int): CharBuffer = {
    val csq = if (_csq == null) "null" else _csq
    val cs = csq.subSequence(start, end)
    if (cs.length > 0)
      put(cs.toString)
    this
  }

  def read(target: CharBuffer): Int = {
    var r = remaining
    if (target eq this) {
      if (r == 0)
        return -1
      else
        throw new IllegalArgumentException
    }
    if (r == 0)
      return if (limit > 0 && target.remaining == 0) 0 else -1
    r = Math.min(target.remaining, r)
    if (r > 0) {
      val chars = new Array[Char](r)
      get(chars)
      target.put(chars)
    }
    r
  }
}
