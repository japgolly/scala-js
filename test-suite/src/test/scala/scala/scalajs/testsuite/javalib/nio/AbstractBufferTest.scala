package scala.scalajs.testsuite.javalib.nio

import org.scalajs.jasminetest.JasmineTest
import java.nio.Buffer
import java.nio.ByteBuffer
import java.nio.InvalidMarkException

object AbstractBufferTest extends AbstractBufferTest {

  def testBody(suite: => Unit): Unit = describe("java.nio.Buffer") {
    beforeEach { baseSetup() }
  }

}

trait AbstractBufferTest extends JasmineTest {

  def testBody(suite: => Unit): Unit

  var baseBuf: Buffer = null
  def baseSetup(): Unit = baseBuf = ByteBuffer.allocate(10)

  testBody {

    it("testCapacity") {
      expect(0 <= baseBuf.position && baseBuf.position <= baseBuf.limit && baseBuf.limit <= baseBuf.capacity).toBe(true)
    }

    it("testClear") {
      val oldPosition = baseBuf.position
      val oldLimit = baseBuf.limit
      val ret = baseBuf.clear
      expect(ret eq baseBuf).toBe(true)
      expect(baseBuf.position).toEqual(0)
      expect(baseBuf.limit).toEqual(baseBuf.capacity)
      expect(() => baseBuf.reset).toThrow()
      baseBuf.limit(oldLimit)
      baseBuf.position(oldPosition)
    }

    it("testFlip") {
      val oldPosition: Int = baseBuf.position
      val oldLimit: Int = baseBuf.limit
      val ret: Buffer = baseBuf.flip
      expect(ret eq baseBuf).toBe(true)
      expect(baseBuf.position).toEqual(0)
      expect(baseBuf.limit).toEqual(oldPosition)
      expect(() => baseBuf.reset).toThrow()
      baseBuf.limit(oldLimit)
      baseBuf.position(oldPosition)
    }

    it("testHasRemaining") {
      val oldPosition: Int = baseBuf.position
      val oldLimit: Int = baseBuf.limit
      expect(baseBuf.hasRemaining).toEqual(baseBuf.position < baseBuf.limit)
      baseBuf.position(baseBuf.limit)
      expect(baseBuf.hasRemaining).toBe(false)
      baseBuf.limit(oldLimit)
      baseBuf.position(oldPosition)
    }

    it("testIsReadOnly") {
      baseBuf.isReadOnly
    }

    it("testLimit") {
      expect(0 <= baseBuf.position && baseBuf.position <= baseBuf.limit && baseBuf.limit <= baseBuf.capacity).toBe(true)
    }

    it("testLimitint") {
      val oldPosition: Int = baseBuf.position
      val oldLimit: Int = baseBuf.limit
      val ret: Buffer = baseBuf.limit(baseBuf.limit)
      expect(ret eq baseBuf).toBe(true)
      baseBuf.mark
      baseBuf.limit(baseBuf.capacity)
      expect(baseBuf.limit).toEqual(baseBuf.capacity)
      expect(baseBuf.position).toEqual(oldPosition)
      baseBuf.reset
      if (baseBuf.capacity > 0) {
        baseBuf.limit(baseBuf.capacity)
        baseBuf.position(baseBuf.capacity)
        baseBuf.mark
        baseBuf.limit(baseBuf.capacity - 1)
        expect(baseBuf.position).toEqual(baseBuf.limit)
        expect(() => baseBuf.reset).toThrow()
      }
      expect(() => baseBuf.limit(-1)).toThrow()
      expect(() => baseBuf.limit(baseBuf.capacity + 1)).toThrow()
      baseBuf.limit(oldLimit)
      baseBuf.position(oldPosition)
    }

    it("testMark") {
      val oldPosition: Int = baseBuf.position
      val oldLimit: Int = baseBuf.limit
      val ret: Buffer = baseBuf.mark
      expect(ret eq baseBuf).toBe(true)
      baseBuf.mark
      baseBuf.position(baseBuf.limit)
      baseBuf.reset
      expect(baseBuf.position).toEqual(oldPosition)
      baseBuf.mark
      baseBuf.position(baseBuf.limit)
      baseBuf.reset
      expect(baseBuf.position).toEqual(oldPosition)
      baseBuf.limit(oldLimit)
      baseBuf.position(oldPosition)
    }

    it("testPosition") {
      expect(0 <= baseBuf.position && baseBuf.position <= baseBuf.limit && baseBuf.limit <= baseBuf.capacity).toBe(true)
    }

    it("testPositionint") {
      val oldPosition: Int = baseBuf.position
      val oldLimit: Int = baseBuf.limit
      expect(() => baseBuf.position(-1)).toThrow()
      expect(() => baseBuf.position(baseBuf.limit + 1)).toThrow()
      baseBuf.mark
      baseBuf.position(baseBuf.position)
      baseBuf.reset
      expect(baseBuf.position).toEqual(oldPosition)
      baseBuf.position(0)
      expect(baseBuf.position).toEqual(0)
      baseBuf.position(baseBuf.limit)
      expect(baseBuf.position).toEqual(baseBuf.limit)
      if (baseBuf.capacity > 0) {
        baseBuf.limit(baseBuf.capacity)
        baseBuf.position(baseBuf.limit)
        baseBuf.mark
        baseBuf.position(baseBuf.limit - 1)
        expect(baseBuf.position).toEqual(baseBuf.limit - 1)
        expect(() => baseBuf.reset).toThrow()
      }
      val ret: Buffer = baseBuf.position(0)
      expect(ret eq baseBuf).toBe(true)
      baseBuf.limit(oldLimit)
      baseBuf.position(oldPosition)
    }

    it("testRemaining") {
      expect(baseBuf.remaining).toEqual(baseBuf.limit - baseBuf.position)
    }

    it("testReset") {
      val oldPosition: Int = baseBuf.position
      val oldLimit: Int = baseBuf.limit
      baseBuf.mark
      baseBuf.position(baseBuf.limit)
      baseBuf.reset
      expect(baseBuf.position).toEqual(oldPosition)
      baseBuf.mark
      baseBuf.position(baseBuf.limit)
      baseBuf.reset
      expect(baseBuf.position).toEqual(oldPosition)
      val ret: Buffer = baseBuf.reset
      expect(ret eq baseBuf).toBe(true)
      baseBuf.clear
      expect(() => baseBuf.reset).toThrow()
      baseBuf.limit(oldLimit)
      baseBuf.position(oldPosition)
    }

    it("testRewind") {
      val oldPosition: Int = baseBuf.position
      val oldLimit: Int = baseBuf.limit
      val ret: Buffer = baseBuf.rewind
      expect(baseBuf.position).toEqual(0)
      expect(ret eq baseBuf).toBe(true)
      expect(() => baseBuf.reset).toThrow()
      baseBuf.limit(oldLimit)
      baseBuf.position(oldPosition)
    }
  }
}
