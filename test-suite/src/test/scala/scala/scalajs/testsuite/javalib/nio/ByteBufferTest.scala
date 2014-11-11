package scala.scalajs.testsuite.javalib.nio

import org.scalajs.jasminetest.JasmineTest
import java.nio._
import java.util.Arrays
import java.lang.{Float => JFloat, Double => JDouble}
import scala.scalajs.js.typedarray._

object ByteBufferTest extends AbstractBufferTest {
  val SMALL_TEST_LENGTH = 5
  val BUFFER_LENGTH = 250

  var buf: ByteBuffer = null

  def testBody(suite: => Unit): Unit = describe("java.nio.ByteBuffer") {

    beforeEach {
      buf = ByteBuffer.allocate(10)
      loadTestData1(buf)
      baseBuf = buf
    }

    it("testArray") {
      if (buf.hasArray) {
        val array: Array[Byte] = buf.array
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
        loadTestData1(array, buf.arrayOffset, buf.capacity)
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
        loadTestData2(array, buf.arrayOffset, buf.capacity)
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
        loadTestData1(buf)
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
        loadTestData2(buf)
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
      }
      else
        expect(() => buf.array).toThrow()
    }

    it("testArrayOffset") {
      if (buf.hasArray) {
        val array: Array[Byte] = buf.array
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
        loadTestData1(array, buf.arrayOffset, buf.capacity)
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
        loadTestData2(array, buf.arrayOffset, buf.capacity)
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
        loadTestData1(buf)
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
        loadTestData2(buf)
        assertContentEquals(buf, array, buf.arrayOffset, buf.capacity)
      }
      else
        expect(() => buf.arrayOffset).toThrow()
    }

    it("testAsReadOnlyBuffer") {
      buf.clear
      buf.mark
      buf.position(buf.limit)
      val readonly: ByteBuffer = buf.asReadOnlyBuffer
      expect(buf eq readonly).toBe(false)
      expect(readonly.isReadOnly).toBe(true)
      expect(buf.position).toEqual(readonly.position)
      expect(buf.limit).toEqual(readonly.limit)
      // expect(buf.isDirect).toEqual(readonly.isDirect)
      expect(buf.order == readonly.order).toBe(true)
      assertContentEquals(buf, readonly)
      readonly.reset
      expect(readonly.position).toEqual(0)
      readonly.clear
      expect(buf.position).toEqual(buf.limit)
      buf.reset
      expect(buf.position).toEqual(0)
    }

    it("testCompact") {
      if (buf.isReadOnly) {
        expect(() => buf.compact).toThrow()
        return
      }
      buf.clear
      buf.mark
      loadTestData1(buf)
      var ret: ByteBuffer = buf.compact
      expect(ret eq buf).toBe(true)
      expect(buf.position).toEqual(buf.capacity)
      expect(buf.limit).toEqual(buf.capacity)
      assertContentLikeTestData1(buf, 0, 0.toByte, buf.capacity)
      expect(() => buf.reset).toThrow()
      buf.position(0)
      buf.limit(0)
      buf.mark
      ret = buf.compact
      expect(ret eq buf).toBe(true)
      expect(buf.position).toEqual(0)
      expect(buf.limit).toEqual(buf.capacity)
      assertContentLikeTestData1(buf, 0, 0.toByte, buf.capacity)
      expect(() => buf.reset).toThrow()
      expect(buf.capacity > ByteBufferTest.SMALL_TEST_LENGTH).toBe(true)
      buf.position(1)
      buf.limit(ByteBufferTest.SMALL_TEST_LENGTH)
      buf.mark
      ret = buf.compact
      expect(ret eq buf).toBe(true)
      expect(buf.position).toEqual(4)
      expect(buf.limit).toEqual(buf.capacity)
      assertContentLikeTestData1(buf, 0, 1.toByte, 4)
      expect(() => buf.reset).toThrow()
    }

    it("testCompareTo") {
      expect(0).toEqual(buf.compareTo(buf))
      if (!buf.isReadOnly) {
        expect(buf.capacity > ByteBufferTest.SMALL_TEST_LENGTH).toBe(true)
        buf.clear
        val other: ByteBuffer = ByteBuffer.allocate(buf.capacity)
        loadTestData1(buf)
        loadTestData1(other)
        expect(0).toEqual(buf.compareTo(other))
        expect(0).toEqual(other.compareTo(buf))
        buf.position(1)
        expect(buf.compareTo(other) > 0).toBe(true)
        expect(other.compareTo(buf) < 0).toBe(true)
        other.position(2)
        expect(buf.compareTo(other) < 0).toBe(true)
        expect(other.compareTo(buf) > 0).toBe(true)
        buf.position(2)
        other.limit(ByteBufferTest.SMALL_TEST_LENGTH)
        expect(buf.compareTo(other) > 0).toBe(true)
        expect(other.compareTo(buf) < 0).toBe(true)
      }
      // expect(ByteBuffer.wrap(new Array[Byte](21)).compareTo(ByteBuffer.allocateDirect(21)) == 0).toBe(true)
    }

    it("testDuplicate") {
      buf.clear
      buf.mark
      buf.position(buf.limit)
      val duplicate: ByteBuffer = buf.duplicate
      expect(buf eq duplicate).toBe(false)
      expect(buf.position).toEqual(duplicate.position)
      expect(buf.limit).toEqual(duplicate.limit)
      expect(buf.isReadOnly).toEqual(duplicate.isReadOnly)
      // expect(buf.isDirect).toEqual(duplicate.isDirect)
      expect(buf.order == duplicate.order).toBe(true)
      assertContentEquals(buf, duplicate)
      duplicate.reset
      expect(duplicate.position).toEqual(0)
      duplicate.clear
      expect(buf.position).toEqual(buf.limit)
      buf.reset
      expect(buf.position).toEqual(0)
      if (!duplicate.isReadOnly) {
        loadTestData1(buf)
        assertContentEquals(buf, duplicate)
        loadTestData2(duplicate)
        assertContentEquals(buf, duplicate)
      }
    }

    it("testEquals") {
      expect(buf == buf).toBe(true)
      val readonly: ByteBuffer = buf.asReadOnlyBuffer
      expect(buf == readonly).toBe(true)
      val duplicate: ByteBuffer = buf.duplicate
      expect(buf == duplicate).toBe(true)
      expect(buf == java.lang.Boolean.TRUE).toBe(false)
      expect(buf.capacity > ByteBufferTest.SMALL_TEST_LENGTH).toBe(true)
      buf.limit(buf.capacity).position(0)
      readonly.limit(readonly.capacity).position(1)
      expect(buf == readonly).toBe(false)
      buf.limit(buf.capacity - 1).position(0)
      duplicate.limit(duplicate.capacity).position(0)
      expect(buf == duplicate).toBe(false)
    }

    it("testGet") {
      buf.clear
      var i: Int = 0
      while (i < buf.capacity) {
        expect(buf.position).toEqual(i)
        expect(buf.get).toEqual(buf.get(i))
        i += 1
      }
      expect(() => buf.get).toThrow()
    }

    it("testGetbyteArray") {
      val array: Array[Byte] = new Array[Byte](1)
      buf.clear
      var i: Int = 0
      while (i < buf.capacity) {
        expect(buf.position).toEqual(i)
        val ret: ByteBuffer = buf.get(array)
        expect(array(0)).toEqual(buf.get(i))
        expect(ret eq buf).toBe(true)
        i += 1
      }
      expect(() => buf.get(array)).toThrow()
      expect(() => buf.get(null.asInstanceOf[Array[Byte]])).toThrow()
    }

    it("testGetbyteArrayintint") {
      buf.clear
      val array: Array[Byte] = new Array[Byte](buf.capacity)
      expect(() => buf.get(new Array[Byte](buf.capacity + 1), 0, buf.capacity + 1)).toThrow()
      expect(buf.position).toEqual(0)
      expect(() => buf.get(array, -1, array.length)).toThrow()
      buf.get(array, array.length, 0)
      expect(() => buf.get(array, array.length + 1, 1)).toThrow()
      expect(buf.position).toEqual(0)
      expect(() => buf.get(array, 2, -1)).toThrow()
      expect(() => buf.get(array, 2, array.length)).toThrow()
      expect(() => buf.get(null.asInstanceOf[Array[Byte]], -1, 0)).toThrow()
      expect(() => buf.get(array, 1, Integer.MAX_VALUE)).toThrow()
      expect(() => buf.get(array, Integer.MAX_VALUE, 1)).toThrow()
      expect(buf.position).toEqual(0)
      buf.clear
      val ret: ByteBuffer = buf.get(array, 0, array.length)
      expect(buf.position).toEqual(buf.capacity)
      assertContentEquals(buf, array, 0, array.length)
      expect(ret eq buf).toBe(true)
    }

    it("testGetint") {
      buf.clear
      var i: Int = 0
      while (i < buf.capacity) {
        expect(buf.position).toEqual(i)
        expect(buf.get).toEqual(buf.get(i))
        i += 1
      }
      expect(() => buf.get(-1)).toThrow()
      expect(() => buf.get(buf.limit)).toThrow()
    }

    it("testHasArray") {
      if (buf.hasArray)
        expect(buf.array == null).toBe(false)
      else
        expect(() => buf.array).toThrow()
    }

    it("testHashCode") {
      buf.clear
      loadTestData1(buf)
      val readonly: ByteBuffer = buf.asReadOnlyBuffer
      val duplicate: ByteBuffer = buf.duplicate
      expect(buf.hashCode == readonly.hashCode).toBe(true)
      expect(buf.capacity > ByteBufferTest.SMALL_TEST_LENGTH).toBe(true)
      duplicate.position(buf.capacity / 2)
      expect(buf.hashCode != duplicate.hashCode).toBe(true)
    }

    it("readOnlyHashCode") {
      var buf: ByteBuffer = ByteBuffer.allocate(ByteBufferTest.BUFFER_LENGTH)
      loadTestData1(buf)
      buf = buf.asReadOnlyBuffer
      buf.clear
      val readonly: ByteBuffer = buf.asReadOnlyBuffer
      val duplicate: ByteBuffer = buf.duplicate
      expect(buf.hashCode).toEqual(readonly.hashCode)
      duplicate.position(buf.capacity / 2)
      expect(buf.hashCode != duplicate.hashCode).toBe(true)
    }

    // it("testIsDirect") {
      // buf.isDirect
    // }

    it("testOrder") {
      buf.order(ByteOrder.LITTLE_ENDIAN)
      expect(ByteOrder.LITTLE_ENDIAN == buf.order).toBe(true)
      buf.order(ByteOrder.BIG_ENDIAN)
      expect(ByteOrder.BIG_ENDIAN == buf.order).toBe(true)
    }

    it("testPutbyte") {
      if (buf.isReadOnly) {
        expect(() => buf.clear).toThrow()
        return
      }
      buf.clear
      var i: Int = 0
      while (i < buf.capacity) {
        expect(buf.position).toEqual(i)
        val ret: ByteBuffer = buf.put(i.toByte)
        expect(buf.get(i)).toEqual(i.toByte)
        expect(ret eq buf).toBe(true)
        i += 1
      }
      expect(() => buf.put(0.toByte)).toThrow()
    }

    it("testPutbyteArray") {
      val array: Array[Byte] = new Array[Byte](1)
      if (buf.isReadOnly) {
        expect(() => buf.put(array)).toThrow()
        return
      }
      buf.clear
      var i: Int = 0
      while (i < buf.capacity) {
        expect(buf.position).toEqual(i)
        array(0) = i.toByte
        val ret: ByteBuffer = buf.put(array)
        expect(buf.get(i)).toEqual(i.toByte)
        expect(ret eq buf).toBe(true)
        i += 1
      }
      expect(() => buf.put(array)).toThrow()
      expect(() => buf.put(null.asInstanceOf[Array[Byte]])).toThrow()
    }

    it("testPutbyteArrayintint") {
      buf.clear
      val array: Array[Byte] = new Array[Byte](buf.capacity)
      if (buf.isReadOnly) {
        expect(() => buf.put(array, 0, array.length)).toThrow()
        return
      }
      expect(() => buf.put(new Array[Byte](buf.capacity + 1), 0, buf.capacity + 1)).toThrow()
      expect(buf.position).toEqual(0)
      expect(() => buf.put(array, -1, array.length)).toThrow()
      expect(() => buf.put(array, array.length + 1, 0)).toThrow()
      buf.put(array, array.length, 0)
      expect(buf.position).toEqual(0)
      expect(() => buf.put(array, 0, -1)).toThrow()
      expect(() => buf.put(array, 2, array.length)).toThrow()
      expect(() => buf.put(array, 2, Integer.MAX_VALUE)).toThrow()
      expect(() => buf.put(array, Integer.MAX_VALUE, 1)).toThrow()
      expect(() => buf.put(null.asInstanceOf[Array[Byte]], 2, Integer.MAX_VALUE)).toThrow()
      expect(buf.position).toEqual(0)
      loadTestData2(array, 0, array.length)
      val ret: ByteBuffer = buf.put(array, 0, array.length)
      expect(buf.position).toEqual(buf.capacity)
      assertContentEquals(buf, array, 0, array.length)
      expect(ret eq buf).toBe(true)
    }

    it("testPutByteBuffer") {
      val other: ByteBuffer = ByteBuffer.allocate(buf.capacity)
      if (buf.isReadOnly) {
        expect(() => buf.clear).toThrow()
        expect(() => buf.clear).toThrow()
        return
      }
      expect(() => buf.put(buf)).toThrow()
      expect(() => buf.put(ByteBuffer.allocate(buf.capacity + 1))).toThrow()
      expect(() => buf.put(null.asInstanceOf[ByteBuffer])).toThrow()
      loadTestData2(other)
      other.clear
      buf.clear
      val ret: ByteBuffer = buf.put(other)
      expect(other.position).toEqual(other.capacity)
      expect(buf.position).toEqual(buf.capacity)
      assertContentEquals(other, buf)
      expect(ret eq buf).toBe(true)
    }

    it("testPutintbyte") {
      if (buf.isReadOnly) {
        expect(() => buf.put(0, 0.toByte)).toThrow()
        return
      }
      buf.clear
      var i: Int = 0
      while (i < buf.capacity) {
        expect(buf.position).toEqual(0)
        val ret: ByteBuffer = buf.put(i, i.toByte)
        expect(buf.get(i)).toEqual(i.toByte)
        expect(ret eq buf).toBe(true)
        i += 1
      }
      expect(() => buf.put(-1, 0.toByte)).toThrow()
      expect(() => buf.put(buf.limit, 0.toByte)).toThrow()
    }

    it("testSlice") {
      expect(buf.capacity > ByteBufferTest.SMALL_TEST_LENGTH).toBe(true)
      buf.position(1)
      buf.limit(buf.capacity - 1)
      val slice: ByteBuffer = buf.slice
      expect(buf.isReadOnly).toEqual(slice.isReadOnly)
      // expect(buf.isDirect).toEqual(slice.isDirect)
      expect(buf.order == slice.order).toBe(true)
      expect(slice.position).toEqual(0)
      expect(slice.limit).toEqual(buf.remaining)
      expect(slice.capacity).toEqual(buf.remaining)
      expect(() => slice.reset).toThrow()
      if (!slice.isReadOnly) {
        loadTestData1(slice)
        assertContentLikeTestData1(buf, 1, 0.toByte, slice.capacity)
        buf.put(2, 100.toByte)
        expect(slice.get(1)).toEqual(100)
      }
    }

    it("testToString") {
      val str: String = buf.toString
      expect(str.indexOf("Byte") >= 0 || str.indexOf("byte") >= 0).toBe(true)
      expect(str.indexOf("" + buf.position) >= 0).toBe(true)
      expect(str.indexOf("" + buf.limit) >= 0).toBe(true)
      expect(str.indexOf("" + buf.capacity) >= 0).toBe(true)
    }

    it("testAsCharBuffer") {
      var charBuffer: CharBuffer = null
      val bytes = new Array[Byte](2)
      var value: Char = 0
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
      charBuffer = buf.asCharBuffer
      expect(ByteOrder.BIG_ENDIAN eq charBuffer.order).toBe(true)
      while (charBuffer.remaining > 0) {
        buf.get(bytes)
        value = charBuffer.get
        expect(bytes2char(bytes, buf.order)).toEqual(value)
      }
      buf.clear
      buf.order(ByteOrder.LITTLE_ENDIAN)
      charBuffer = buf.asCharBuffer
      expect(ByteOrder.LITTLE_ENDIAN eq charBuffer.order).toBe(true)
      while (charBuffer.remaining > 0) {
        buf.get(bytes)
        value = charBuffer.get
        expect(bytes2char(bytes, buf.order)).toEqual(value)
      }
      if (!buf.isReadOnly) {
        buf.clear
        buf.order(ByteOrder.BIG_ENDIAN)
        charBuffer = buf.asCharBuffer
        expect(ByteOrder.BIG_ENDIAN eq charBuffer.order).toBe(true)
        while (charBuffer.remaining > 0) {
          value = charBuffer.remaining.toChar
          charBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, char2bytes(value, buf.order))).toBe(true)
        }
        buf.clear
        buf.order(ByteOrder.LITTLE_ENDIAN)
        charBuffer = buf.asCharBuffer
        expect(ByteOrder.LITTLE_ENDIAN eq charBuffer.order).toBe(true)
        while (charBuffer.remaining > 0) {
          value = charBuffer.remaining.toChar
          charBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, char2bytes(value, buf.order))).toBe(true)
        }
      }
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testAsDoubleBuffer") {
      var doubleBuffer: DoubleBuffer = null
      val bytes = new Array[Byte](8)
      var value: Double = 0
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
      doubleBuffer = buf.asDoubleBuffer
      expect(ByteOrder.BIG_ENDIAN eq doubleBuffer.order).toBe(true)
      while (doubleBuffer.remaining > 0) {
        buf.get(bytes)
        value = doubleBuffer.get
        if (!(JDouble.isNaN(bytes2double(bytes, buf.order)) && JDouble.isNaN(value))) {
          expect(bytes2double(bytes, buf.order)).toBeCloseTo(value, 2)
        }
      }
      buf.clear
      buf.order(ByteOrder.LITTLE_ENDIAN)
      doubleBuffer = buf.asDoubleBuffer
      expect(ByteOrder.LITTLE_ENDIAN eq doubleBuffer.order).toBe(true)
      while (doubleBuffer.remaining > 0) {
        buf.get(bytes)
        value = doubleBuffer.get
        if (!(JDouble.isNaN(bytes2double(bytes, buf.order)) && JDouble.isNaN(value))) {
          expect(bytes2double(bytes, buf.order)).toBeCloseTo(value, 2)
        }
      }
      if (!buf.isReadOnly) {
        buf.clear
        buf.order(ByteOrder.BIG_ENDIAN)
        doubleBuffer = buf.asDoubleBuffer
        expect(ByteOrder.BIG_ENDIAN eq doubleBuffer.order).toBe(true)
        while (doubleBuffer.remaining > 0) {
          value = doubleBuffer.remaining.toDouble
          doubleBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, double2bytes(value, buf.order))).toBe(true)
        }
        buf.clear
        buf.order(ByteOrder.LITTLE_ENDIAN)
        doubleBuffer = buf.asDoubleBuffer
        expect(ByteOrder.LITTLE_ENDIAN eq doubleBuffer.order).toBe(true)
        while (doubleBuffer.remaining > 0) {
          value = doubleBuffer.remaining.toDouble
          doubleBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, double2bytes(value, buf.order))).toBe(true)
        }
      }
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testAsFloatBuffer") {
      var floatBuffer: FloatBuffer = null
      val bytes = new Array[Byte](4)
      var value: Float = 0
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
      floatBuffer = buf.asFloatBuffer
      expect(ByteOrder.BIG_ENDIAN eq floatBuffer.order).toBe(true)
      while (floatBuffer.remaining > 0) {
        buf.get(bytes)
        value = floatBuffer.get
        if (!(JFloat.isNaN(bytes2float(bytes, buf.order)) && JFloat.isNaN(value))) {
          expect(bytes2float(bytes, buf.order)).toBeCloseTo(value, 2)
        }
      }
      buf.clear
      buf.order(ByteOrder.LITTLE_ENDIAN)
      floatBuffer = buf.asFloatBuffer
      expect(ByteOrder.LITTLE_ENDIAN eq floatBuffer.order).toBe(true)
      while (floatBuffer.remaining > 0) {
        buf.get(bytes)
        value = floatBuffer.get
        if (!(JFloat.isNaN(bytes2float(bytes, buf.order)) && JFloat.isNaN(value))) {
          expect(bytes2float(bytes, buf.order)).toBeCloseTo(value, 2)
        }
      }
      if (!buf.isReadOnly) {
        buf.clear
        buf.order(ByteOrder.BIG_ENDIAN)
        floatBuffer = buf.asFloatBuffer
        expect(ByteOrder.BIG_ENDIAN eq floatBuffer.order).toBe(true)
        while (floatBuffer.remaining > 0) {
          value = floatBuffer.remaining.toFloat
          floatBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, float2bytes(value, buf.order))).toBe(true)
        }
        buf.clear
        buf.order(ByteOrder.LITTLE_ENDIAN)
        floatBuffer = buf.asFloatBuffer
        expect(ByteOrder.LITTLE_ENDIAN eq floatBuffer.order).toBe(true)
        while (floatBuffer.remaining > 0) {
          value = floatBuffer.remaining.toFloat
          floatBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, float2bytes(value, buf.order))).toBe(true)
        }
      }
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testAsIntBuffer") {
      var intBuffer: IntBuffer = null
      val bytes = new Array[Byte](4)
      var value: Int = 0
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
      intBuffer = buf.asIntBuffer
      expect(ByteOrder.BIG_ENDIAN eq intBuffer.order).toBe(true)
      while (intBuffer.remaining > 0) {
        buf.get(bytes)
        value = intBuffer.get
        expect(bytes2int(bytes, buf.order)).toEqual(value)
      }
      buf.clear
      buf.order(ByteOrder.LITTLE_ENDIAN)
      intBuffer = buf.asIntBuffer
      expect(ByteOrder.LITTLE_ENDIAN eq intBuffer.order).toBe(true)
      while (intBuffer.remaining > 0) {
        buf.get(bytes)
        value = intBuffer.get
        expect(bytes2int(bytes, buf.order)).toEqual(value)
      }
      if (!buf.isReadOnly) {
        buf.clear
        buf.order(ByteOrder.BIG_ENDIAN)
        intBuffer = buf.asIntBuffer
        expect(ByteOrder.BIG_ENDIAN eq intBuffer.order).toBe(true)
        while (intBuffer.remaining > 0) {
          value = intBuffer.remaining.toInt
          intBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, int2bytes(value, buf.order))).toBe(true)
        }
        buf.clear
        buf.order(ByteOrder.LITTLE_ENDIAN)
        intBuffer = buf.asIntBuffer
        expect(ByteOrder.LITTLE_ENDIAN eq intBuffer.order).toBe(true)
        while (intBuffer.remaining > 0) {
          value = intBuffer.remaining.toInt
          intBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, int2bytes(value, buf.order))).toBe(true)
        }
      }
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testAsLongBuffer") {
      var longBuffer: LongBuffer = null
      val bytes = new Array[Byte](8)
      var value: Long = 0L
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
      longBuffer = buf.asLongBuffer
      expect(ByteOrder.BIG_ENDIAN eq longBuffer.order).toBe(true)
      while (longBuffer.remaining > 0) {
        buf.get(bytes)
        value = longBuffer.get
        expect(bytes2long(bytes, buf.order)).toEqual(value)
      }
      buf.clear
      buf.order(ByteOrder.LITTLE_ENDIAN)
      longBuffer = buf.asLongBuffer
      expect(ByteOrder.LITTLE_ENDIAN eq longBuffer.order).toBe(true)
      while (longBuffer.remaining > 0) {
        buf.get(bytes)
        value = longBuffer.get
        expect(bytes2long(bytes, buf.order)).toEqual(value)
      }
      if (!buf.isReadOnly) {
        buf.clear
        buf.order(ByteOrder.BIG_ENDIAN)
        longBuffer = buf.asLongBuffer
        expect(ByteOrder.BIG_ENDIAN eq longBuffer.order).toBe(true)
        while (longBuffer.remaining > 0) {
          value = longBuffer.remaining.toLong
          longBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, long2bytes(value, buf.order))).toBe(true)
        }
        buf.clear
        buf.order(ByteOrder.LITTLE_ENDIAN)
        longBuffer = buf.asLongBuffer
        expect(ByteOrder.LITTLE_ENDIAN eq longBuffer.order).toBe(true)
        while (longBuffer.remaining > 0) {
          value = longBuffer.remaining.toLong
          longBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, long2bytes(value, buf.order))).toBe(true)
        }
      }
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testAsShortBuffer") {
      var shortBuffer: ShortBuffer = null
      val bytes = new Array[Byte](2)
      var value: Short = 0
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
      shortBuffer = buf.asShortBuffer
      expect(ByteOrder.BIG_ENDIAN eq shortBuffer.order).toBe(true)
      while (shortBuffer.remaining > 0) {
        buf.get(bytes)
        value = shortBuffer.get
        expect(bytes2short(bytes, buf.order)).toEqual(value)
      }
      buf.clear
      buf.order(ByteOrder.LITTLE_ENDIAN)
      shortBuffer = buf.asShortBuffer
      expect(ByteOrder.LITTLE_ENDIAN eq shortBuffer.order).toBe(true)
      while (shortBuffer.remaining > 0) {
        buf.get(bytes)
        value = shortBuffer.get
        expect(bytes2short(bytes, buf.order)).toEqual(value)
      }
      if (!buf.isReadOnly) {
        buf.clear
        buf.order(ByteOrder.BIG_ENDIAN)
        shortBuffer = buf.asShortBuffer
        expect(ByteOrder.BIG_ENDIAN eq shortBuffer.order).toBe(true)
        while (shortBuffer.remaining > 0) {
          value = shortBuffer.remaining.toShort
          shortBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, short2bytes(value, buf.order))).toBe(true)
        }
        buf.clear
        buf.order(ByteOrder.LITTLE_ENDIAN)
        shortBuffer = buf.asShortBuffer
        expect(ByteOrder.LITTLE_ENDIAN eq shortBuffer.order).toBe(true)
        while (shortBuffer.remaining > 0) {
          value = shortBuffer.remaining.toShort
          shortBuffer.put(value)
          buf.get(bytes)
          expect(Arrays.equals(bytes, short2bytes(value, buf.order))).toBe(true)
        }
      }
      buf.clear
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testGetChar") {
      val nbytes: Int = 2
      val bytes = new Array[Byte](nbytes)
      var value: Char = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        expect(i * nbytes).toEqual(buf.position)
        buf.mark
        buf.get(bytes)
        buf.reset
        value = buf.getChar
        expect(bytes2char(bytes, buf.order)).toEqual(value)
        i += 1
      }
      expect(() => buf.getChar).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testGetCharint") {
      val nbytes: Int = 2
      val bytes = new Array[Byte](nbytes)
      var value: Char = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        buf.position(i)
        value = buf.getChar(i)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(bytes2char(bytes, buf.order)).toEqual(value)
        i += 1
      }
      expect(() => buf.getChar(-1)).toThrow()
      expect(() => buf.getChar(buf.limit - nbytes + 1)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testPutChar") {
      if (buf.isReadOnly) {
        expect(() => buf.clear).toThrow()
        return
      }
      val nbytes: Int = 2
      val bytes = new Array[Byte](nbytes)
      var value: Char = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toChar
        buf.mark
        buf.putChar(value)
        expect((i + 1) * nbytes).toEqual(buf.position)
        buf.reset
        buf.get(bytes)
        expect(Arrays.equals(char2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putChar(value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testPutCharint") {
      if (buf.isReadOnly) {
        expect(() => buf.putChar(0, 1.toChar)).toThrow()
        return
      }
      val nbytes: Int = 2
      val bytes = new Array[Byte](nbytes)
      var value: Char = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toChar
        buf.position(i)
        buf.putChar(i, value)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(Arrays.equals(char2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putChar(-1, value)).toThrow()
      expect(() => buf.putChar(buf.limit - nbytes + 1, value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
      // expect(() => ByteBuffer.allocateDirect(16).putChar(Integer.MAX_VALUE, 'h')).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    /*
    when("doubleToRawLongBits").it("testGetDoubleint") {
      val nbytes: Int = 8
      val bytes = new Array[Byte](nbytes)
      var value: Double = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        buf.position(i)
        value = buf.getDouble(i)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        if (!(JDouble.isNaN(bytes2double(bytes, buf.order)) && JDouble.isNaN(value))) {
          expect(bytes2double(bytes, buf.order)).toBeCloseTo(value, 2)
        }
        i += 1
      }
      expect(() => buf.getDouble(-1)).toThrow()
      expect(() => buf.getDouble(buf.limit - nbytes + 1)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
      // expect(() => ByteBuffer.allocateDirect(16).getDouble(Integer.MAX_VALUE)).toThrow()
    }

    when("doubleToRawLongBits").it("testPutDouble") {
      if (buf.isReadOnly) {
        expect(() => {buf.clear; buf.putDouble(1.toDouble)}).toThrow()
        return
      }
      val nbytes: Int = 8
      val bytes = new Array[Byte](nbytes)
      var value: Double = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toDouble
        buf.mark
        buf.putDouble(value)
        expect((i + 1) * nbytes).toEqual(buf.position)
        buf.reset
        buf.get(bytes)
        expect(Arrays.equals(double2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putDouble(value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    when("doubleToRawLongBits").it("testPutDoubleint") {
      if (buf.isReadOnly) {
        expect(() => buf.putDouble(0, 1.toDouble)).toThrow()
        return
      }
      val nbytes: Int = 8
      val bytes = new Array[Byte](nbytes)
      var value: Double = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toDouble
        buf.position(i)
        buf.putDouble(i, value)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(Arrays.equals(double2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putDouble(-1, value)).toThrow()
      expect(() => buf.putDouble(buf.limit - nbytes + 1, value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    when("floatToRawIntBits").it("testGetFloat") {
      val nbytes: Int = 4
      val bytes = new Array[Byte](nbytes)
      var value: Float = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        expect(i * nbytes).toEqual(buf.position)
        buf.mark
        buf.get(bytes)
        buf.reset
        value = buf.getFloat
        if (!(JFloat.isNaN(bytes2float(bytes, buf.order)) && JFloat.isNaN(value))) {
          expect(bytes2float(bytes, buf.order)).toBeCloseTo(value, 2)
        }
        i += 1
      }
      expect(() => buf.getFloat).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    when("floatToRawIntBits").it("testGetFloatint") {
      val nbytes: Int = 4
      val bytes = new Array[Byte](nbytes)
      var value: Float = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        buf.position(i)
        value = buf.getFloat(i)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        if (!(JFloat.isNaN(bytes2float(bytes, buf.order)) && JFloat.isNaN(value))) {
          expect(bytes2float(bytes, buf.order)).toBeCloseTo(value, 2)
        }
        i += 1
      }
      expect(() => buf.getFloat(-1)).toThrow()
      expect(() => buf.getFloat(buf.limit - nbytes + 1)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    when("floatToRawIntBits").it("testPutFloat") {
      if (buf.isReadOnly) {
        expect(() => buf.clear).toThrow()
        return
      }
      val nbytes: Int = 4
      val bytes = new Array[Byte](nbytes)
      var value: Float = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toFloat
        buf.mark
        buf.putFloat(value)
        expect((i + 1) * nbytes).toEqual(buf.position)
        buf.reset
        buf.get(bytes)
        expect(Arrays.equals(float2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putFloat(value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    when("floatToRawIntBits").it("testPutFloatint") {
      if (buf.isReadOnly) {
        expect(() => buf.putFloat(0, 1.toFloat)).toThrow()
        return
      }
      val nbytes: Int = 4
      val bytes = new Array[Byte](nbytes)
      var value: Float = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toFloat
        buf.position(i)
        buf.putFloat(i, value)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(Arrays.equals(float2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putFloat(-1, value)).toThrow()
      expect(() => buf.putFloat(buf.limit - nbytes + 1, value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }
    */

    it("testGetInt") {
      val nbytes: Int = 4
      val bytes = new Array[Byte](nbytes)
      var value: Int = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        expect(i * nbytes).toEqual(buf.position)
        buf.mark
        buf.get(bytes)
        buf.reset
        value = buf.getInt
        expect(bytes2int(bytes, buf.order)).toEqual(value)
        i += 1
      }
      expect(() => buf.getInt).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testGetIntint") {
      val nbytes: Int = 4
      val bytes = new Array[Byte](nbytes)
      var value: Int = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        buf.position(i)
        value = buf.getInt(i)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(bytes2int(bytes, buf.order)).toEqual(value)
        i += 1
      }
      expect(() => buf.getInt(-1)).toThrow()
      expect(() => buf.getInt(buf.limit - nbytes + 1)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
      // expect(() => ByteBuffer.allocateDirect(16).getInt(Integer.MAX_VALUE)).toThrow()
    }

    it("testPutInt") {
      if (buf.isReadOnly) {
        expect(() => {buf.clear; buf.putInt(1.toInt)}).toThrow()
        return
      }
      val nbytes: Int = 4
      val bytes = new Array[Byte](nbytes)
      var value: Int = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toInt
        buf.mark
        buf.putInt(value)
        expect((i + 1) * nbytes).toEqual(buf.position)
        buf.reset
        buf.get(bytes)
        expect(Arrays.equals(int2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putInt(value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testPutIntint") {
      if (buf.isReadOnly) {
        expect(() => buf.putInt(0, 1.toInt)).toThrow()
        return
      }
      val nbytes: Int = 4
      val bytes = new Array[Byte](nbytes)
      var value: Int = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toInt
        buf.position(i)
        buf.putInt(i, value)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(Arrays.equals(int2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putInt(-1, value)).toThrow()
      expect(() => buf.putInt(buf.limit - nbytes + 1, value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testGetLong") {
      val nbytes: Int = 8
      val bytes = new Array[Byte](nbytes)
      var value: Long = 0L
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        expect(i * nbytes).toEqual(buf.position)
        buf.mark
        buf.get(bytes)
        buf.reset
        value = buf.getLong
        expect(bytes2long(bytes, buf.order)).toEqual(value)
        i += 1
      }
      expect(() => buf.getLong).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testGetLongint") {
      val nbytes: Int = 8
      val bytes = new Array[Byte](nbytes)
      var value: Long = 0L
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        buf.position(i)
        value = buf.getLong(i)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(bytes2long(bytes, buf.order)).toEqual(value)
        i += 1
      }
      expect(() => buf.getLong(-1)).toThrow()
      expect(() => buf.getLong(buf.limit - nbytes + 1)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testPutLong") {
      if (buf.isReadOnly) {
        expect(() => buf.clear).toThrow()
        return
      }
      val nbytes: Int = 8
      val bytes = new Array[Byte](nbytes)
      var value: Long = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toLong
        buf.mark
        buf.putLong(value)
        expect((i + 1) * nbytes).toEqual(buf.position)
        buf.reset
        buf.get(bytes)
        expect(Arrays.equals(long2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putLong(value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testPutLongint") {
      if (buf.isReadOnly) {
        expect(() => buf.putLong(0, 1.toLong)).toThrow()
        return
      }
      val nbytes: Int = 8
      val bytes = new Array[Byte](nbytes)
      var value: Long = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toLong
        buf.position(i)
        buf.putLong(i, value)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(Arrays.equals(long2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putLong(-1, value)).toThrow()
      expect(() => buf.putLong(buf.limit - nbytes + 1, value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testGetShort") {
      val nbytes: Int = 2
      val bytes = new Array[Byte](nbytes)
      var value: Short = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        expect(i * nbytes).toEqual(buf.position)
        buf.mark
        buf.get(bytes)
        buf.reset
        value = buf.getShort
        expect(bytes2short(bytes, buf.order)).toEqual(value)
        i += 1
      }
      expect(() => buf.getShort).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testGetShortint") {
      val nbytes: Int = 2
      val bytes = new Array[Byte](nbytes)
      var value: Short = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        buf.position(i)
        value = buf.getShort(i)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(bytes2short(bytes, buf.order)).toEqual(value)
        i += 1
      }
      expect(() => buf.getShort(-1)).toThrow()
      expect(() => buf.getShort(buf.limit - nbytes + 1)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testPutShort") {
      if (buf.isReadOnly) {
        expect(() => buf.clear).toThrow()
        return
      }
      val nbytes: Int = 2
      val bytes = new Array[Byte](nbytes)
      var value: Short = 0
      buf.clear
      var i: Int = 0
      while (buf.remaining >= nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toShort
        buf.mark
        buf.putShort(value)
        expect((i + 1) * nbytes).toEqual(buf.position)
        buf.reset
        buf.get(bytes)
        expect(Arrays.equals(short2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putShort(value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testPutShortint") {
      if (buf.isReadOnly) {
        expect(() => buf.putShort(0, 1.toShort)).toThrow()
        return
      }
      val nbytes: Int = 2
      val bytes = new Array[Byte](nbytes)
      var value: Short = 0
      buf.clear
      var i: Int = 0
      while (i <= buf.limit - nbytes) {
        buf.order(if (i % 2 == 0) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN)
        value = i.toShort
        buf.position(i)
        buf.putShort(i, value)
        expect(i).toEqual(buf.position)
        buf.get(bytes)
        expect(Arrays.equals(short2bytes(value, buf.order), bytes)).toBe(true)
        i += 1
      }
      expect(() => buf.putShort(-1, value)).toThrow()
      expect(() => buf.putShort(buf.limit - nbytes + 1, value)).toThrow()
      buf.order(ByteOrder.BIG_ENDIAN)
    }

    it("testWrappedByteBuffer_null_array") {
      val array: Array[Byte] = null
      expect(() => ByteBuffer.wrap(array, -1, 0)).toThrow()
      expect(() => ByteBuffer.wrap(new Array[Byte](10), Integer.MAX_VALUE, 2)).toThrow()
    }

  }

  private def loadTestData1(array: Array[Byte], offset: Int, length: Int): Unit = {
    var i: Int = 0
    while (i < length) {
      array(offset + i) = i.toByte
      i += 1
    }
  }

  private def loadTestData2(array: Array[Byte], offset: Int, length: Int) {
    var i: Int = 0
    while (i < length) {
      array(offset + i) = (length - i).toByte
      i += 1
    }
  }

  private def loadTestData1(buf: ByteBuffer) {
    buf.clear
    var i: Int = 0
    while (i < buf.capacity) {
      buf.put(i, i.toByte)
      i += 1
    }
  }

  private def loadTestData2(buf: ByteBuffer) {
    buf.clear
    var i: Int = 0
    while (i < buf.capacity) {
      buf.put(i, (buf.capacity - i).toByte)
      i += 1
    }
  }

  private def assertContentEquals(buf: ByteBuffer, array: Array[Byte], offset: Int, length: Int) {
    var i: Int = 0
    while (i < length) {
      expect(buf.get(i)).toEqual(array(offset + i))
      i += 1
    }
  }

  private def assertContentEquals(buf: ByteBuffer, other: ByteBuffer) {
    expect(buf.capacity).toEqual(other.capacity)
    var i: Int = 0
    while (i < buf.capacity) {
      expect(buf.get(i)).toEqual(other.get(i))
      i += 1
    }
  }

  private def assertContentLikeTestData1(buf: ByteBuffer, startIndex: Int, startValue: Byte, length: Int) {
    var value: Byte = startValue
    var i: Int = 0
    while (i < length) {
      expect(buf.get(startIndex + i)).toEqual(value)
      value = (value + 1).toByte
      i += 1
    }
  }

  private def bytes2int(bytes: Array[Byte], order: ByteOrder): Int = {
    val nbytes: Int = 4
    var bigHead: Int = 0
    var step: Int = 0
    if (order eq ByteOrder.BIG_ENDIAN) {
      bigHead = 0
      step = 1
    } else {
      bigHead = nbytes - 1
      step = -1
    }
    var result: Int = 0
    var p: Int = bigHead
    var i: Int = 0
    while (i < nbytes) {
      result = result << 8
      result = result | (bytes(p) & 0xff)
      p += step
      i += 1
    }
    result
  }

  private def bytes2long(bytes: Array[Byte], order: ByteOrder): Long = {
    val nbytes: Int = 8
    var bigHead: Int = 0
    var step: Int = 0
    if (order eq ByteOrder.BIG_ENDIAN) {
      bigHead = 0
      step = 1
    } else {
      bigHead = nbytes - 1
      step = -1
    }
    var result: Long = 0
    var p: Int = bigHead
    var i: Int = 0
    while (i < nbytes) {
      result = result << 8
      result = result | (bytes(p) & 0xff)
      p += step
      i += 1
    }
    result
  }

  private def bytes2short(bytes: Array[Byte], order: ByteOrder): Short = {
    val nbytes: Int = 2
    var bigHead: Int = 0
    var step: Int = 0
    if (order eq ByteOrder.BIG_ENDIAN) {
      bigHead = 0
      step = 1
    } else {
      bigHead = nbytes - 1
      step = -1
    }
    var result: Short = 0
    var p: Int = bigHead
    var i: Int = 0
    while (i < nbytes) {
      result = (result << 8).toShort
      result = (result | (bytes(p) & 0xff)).toShort
      p += step
      i += 1
    }
    result
  }

  private def bytes2char(bytes: Array[Byte], order: ByteOrder): Char =
    bytes2short(bytes, order).toChar

  private def bytes2float(bytes: Array[Byte], order: ByteOrder): Float =
    intBitsToFloat(bytes2int(bytes, order))

  private def bytes2double(bytes: Array[Byte], order: ByteOrder): Double =
    longBitsToDouble(bytes2long(bytes, order))

  private def int2bytes(_value: Int, order: ByteOrder): Array[Byte] = {
    var value = _value
    val nbytes: Int = 4
    var smallHead: Int = 0
    var step: Int = 0
    if (order eq ByteOrder.BIG_ENDIAN) {
      smallHead = nbytes - 1
      step = -1
    } else {
      smallHead = 0
      step = 1
    }
    val bytes = new Array[Byte](nbytes)
    var p: Int = smallHead
    var i: Int = 0
    while (i < nbytes) {
      bytes(p) = (value & 0xff).toByte
      value = value >> 8
      p += step
      i += 1
    }
    bytes
  }

  private def long2bytes(_value: Long, order: ByteOrder): Array[Byte] = {
    var value = _value
    val nbytes: Int = 8
    var smallHead: Int = 0
    var step: Int = 0
    if (order eq ByteOrder.BIG_ENDIAN) {
      smallHead = nbytes - 1
      step = -1
    } else {
      smallHead = 0
      step = 1
    }
    val bytes = new Array[Byte](nbytes)
    var p: Int = smallHead
    var i: Int = 0
    while (i < nbytes) {
      bytes(p) = (value & 0xff).toByte
      value = value >> 8
      p += step
      i += 1
    }
    bytes
  }

  private def short2bytes(_value: Short, order: ByteOrder): Array[Byte] = {
    var value = _value
    val nbytes: Int = 2
    var smallHead: Int = 0
    var step: Int = 0
    if (order eq ByteOrder.BIG_ENDIAN) {
      smallHead = nbytes - 1
      step = -1
    } else {
      smallHead = 0
      step = 1
    }
    val bytes = new Array[Byte](nbytes)
    var p: Int = smallHead
    var i: Int = 0
    while (i < nbytes) {
      bytes(p) = (value & 0xff).toByte
      value = (value >> 8).toShort
      p += step
      i += 1
    }
    bytes
  }

  private def char2bytes(value: Char, order: ByteOrder): Array[Byte] =
    short2bytes(value.toShort, order)

  private def float2bytes(value: Float, order: ByteOrder): Array[Byte] =
    int2bytes(floatToRawIntBits(value), order)

  private def double2bytes(value: Double, order: ByteOrder): Array[Byte] =
    long2bytes(doubleToRawLongBits(value), order)

  // ----------------------------------------------------------------------------------------------
  // Copy-n-paste from java.nio package object.
  // Necessity for both goes away when Double.doubleToLongBits and co are implemented.
  private[this] val pkgarraybuf = new ArrayBuffer(8)
  private[this] val pkgdbl      = new Float64Array(pkgarraybuf)
  private[this] val pkgfloat    = new Float32Array(pkgarraybuf)
  private[this] val pkgint      = new Int8Array(pkgarraybuf)

  def doubleToRawLongBits(s: Double): Long = doubleToLongBits(s)
  def doubleToLongBits(s: Double): Long = {
    pkgdbl(0) = s
    val b0 = pkgint(0).toLong
    val b1 = pkgint(1).toLong
    val b2 = pkgint(2).toLong
    val b3 = pkgint(3).toLong
    val b4 = pkgint(4).toLong
    val b5 = pkgint(5).toLong
    val b6 = pkgint(6).toLong
    val b7 = pkgint(7).toLong
    if (ByteOrder.nativeOrder eq ByteOrder.LITTLE_ENDIAN)
      b7 | (b6<<0x8) | (b5<<0x10) | (b4<<0x18) | (b3<<0x20) | (b2<<0x28) | (b1<<0x30) | (b0<<0x38)
    else
      b0 | (b1<<0x8) | (b2<<0x10) | (b3<<0x18) | (b4<<0x20) | (b5<<0x28) | (b6<<0x30) | (b7<<0x38)
  }

  def longBitsToDouble(s: Long): Double = {
    if (ByteOrder.nativeOrder eq ByteOrder.LITTLE_ENDIAN) {
      pkgint(7) = s.toByte
      pkgint(6) = (s >> 0x8).toByte
      pkgint(5) = (s >> 0x10).toByte
      pkgint(4) = (s >> 0x18).toByte
      pkgint(3) = (s >> 0x20).toByte
      pkgint(2) = (s >> 0x28).toByte
      pkgint(1) = (s >> 0x30).toByte
      pkgint(0) = (s >> 0x38).toByte
    } else {
      pkgint(0) = s.toByte
      pkgint(1) = (s >> 0x8).toByte
      pkgint(2) = (s >> 0x10).toByte
      pkgint(3) = (s >> 0x18).toByte
      pkgint(4) = (s >> 0x20).toByte
      pkgint(5) = (s >> 0x28).toByte
      pkgint(6) = (s >> 0x30).toByte
      pkgint(7) = (s >> 0x38).toByte
    }
    pkgdbl(0)
  }

  private[nio] def floatToRawIntBits(s: Float): Int = floatToIntBits(s)
  private[nio] def floatToIntBits(s: Float): Int = {
    pkgfloat(0) = s
    val b0 = pkgint(0).toInt
    val b1 = pkgint(1).toInt
    val b2 = pkgint(2).toInt
    val b3 = pkgint(3).toInt
    if (ByteOrder.nativeOrder eq ByteOrder.LITTLE_ENDIAN)
      b3 | (b2<<0x8) | (b1<<0x10) | (b0<<0x18)
    else
      b0 | (b1<<0x8) | (b2<<0x10) | (b3<<0x18)
  }

  private[nio] def intBitsToFloat(s: Int): Float = {
    if (ByteOrder.nativeOrder eq ByteOrder.LITTLE_ENDIAN) {
      pkgint(3) = s.toByte
      pkgint(2) = (s >> 0x8).toByte
      pkgint(1) = (s >> 0x10).toByte
      pkgint(0) = (s >> 0x18).toByte
    } else {
      pkgint(0) = s.toByte
      pkgint(1) = (s >> 0x8).toByte
      pkgint(2) = (s >> 0x10).toByte
      pkgint(3) = (s >> 0x18).toByte
    }
    pkgfloat(0)
  }
}
