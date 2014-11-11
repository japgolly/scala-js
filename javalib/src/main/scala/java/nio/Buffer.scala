package java.nio

import Buffer.UNSET_MARK

object Buffer {
    final val UNSET_MARK: Int = -1
}

abstract class Buffer private[nio] (_capacity: Int) {
  if (_capacity < 0)
    throw new IllegalArgumentException()

  private[nio] var _limit = capacity

  private[nio] var _mark = UNSET_MARK

  private[nio] var _position: Int = 0

  final def clear(): Buffer = {
    _position = 0
    _mark = UNSET_MARK
    _limit = capacity
    this
  }

  final def flip(): Buffer = {
    _limit = position
    _position = 0
    _mark = UNSET_MARK
    this
  }

  final def hasRemaining: Boolean =
    _position < _limit

  def isReadOnly: Boolean

  final def capacity(): Int = _capacity

  final def limit(): Int = _limit

  final def limit(newLimit: Int): Buffer = {
    if (newLimit < 0 || newLimit > capacity)
      throw new IllegalArgumentException()

    _limit = newLimit
    if (_position > newLimit)
      _position = newLimit
    if ((_mark != UNSET_MARK) && (_mark > newLimit))
      _mark = UNSET_MARK
    this
  }

  final def mark(): Buffer = {
    _mark = _position
    this
  }

  final def position(): Int = _position

  final def position(newPosition: Int): Buffer = {
    if (newPosition < 0 || newPosition > limit)
      throw new IllegalArgumentException()

    _position = newPosition
    if ((_mark != UNSET_MARK) && (_mark > _position))
      _mark = UNSET_MARK
    this
  }

  final def remaining(): Int =
    limit - position

  final def reset(): Buffer = {
    if (_mark == UNSET_MARK)
      throw new InvalidMarkException()
    _position = _mark
    this
  }

    final def rewind(): Buffer = {
    _position = 0
    _mark = UNSET_MARK
    this
  }
}
