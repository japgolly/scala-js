package scala.scalajs.testsuite.javalib.nio

import org.scalajs.jasminetest.JasmineTest
import java.nio.ByteOrder._

object ByteOrderTest extends JasmineTest {

  describe("java.nio.ByteOrder") {

    it("Should provide a native order") {
      expect(nativeOrder == BIG_ENDIAN || nativeOrder == LITTLE_ENDIAN).toBe(true)
    }
  }
}
