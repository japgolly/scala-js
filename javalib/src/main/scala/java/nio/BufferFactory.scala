package java.nio

object BufferFactory {

  def newByteBuffer(array: Array[Byte]): ByteBuffer =
    new ReadWriteHeapByteBuffer(array)

  def newByteBuffer(capacity: Int): ByteBuffer =
    new ReadWriteHeapByteBuffer(capacity)

  def newCharBuffer(array: Array[Char]): CharBuffer =
    new ReadWriteCharArrayBuffer(array)

  def newCharBuffer(chseq: CharSequence): CharBuffer =
    new CharSequenceAdapter(chseq)

  def newCharBuffer(capacity: Int): CharBuffer =
    new ReadWriteCharArrayBuffer(capacity)

  // def newDirectByteBuffer(capacity: Int): ByteBuffer =
    // new ReadWriteDirectByteBuffer(capacity)

  def newDoubleBuffer(array: Array[Double]): DoubleBuffer =
    new ReadWriteDoubleArrayBuffer(array)

  def newDoubleBuffer(capacity: Int): DoubleBuffer =
    new ReadWriteDoubleArrayBuffer(capacity)

  def newFloatBuffer(array: Array[Float]): FloatBuffer =
    new ReadWriteFloatArrayBuffer(array)

  def newFloatBuffer(capacity: Int): FloatBuffer =
    new ReadWriteFloatArrayBuffer(capacity)

  def newIntBuffer(capacity: Int): IntBuffer =
    new ReadWriteIntArrayBuffer(capacity)

  def newIntBuffer(array: Array[Int]): IntBuffer =
    new ReadWriteIntArrayBuffer(array)

  def newLongBuffer(capacity: Int): LongBuffer =
    new ReadWriteLongArrayBuffer(capacity)

  def newLongBuffer(array: Array[Long]): LongBuffer =
    new ReadWriteLongArrayBuffer(array)

  def newShortBuffer(capacity: Int): ShortBuffer =
    new ReadWriteShortArrayBuffer(capacity)

  def newShortBuffer(array: Array[Short]): ShortBuffer =
    new ReadWriteShortArrayBuffer(array)
}
