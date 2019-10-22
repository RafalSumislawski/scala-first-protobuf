package pl.shumikowo.s1pb

sealed class WireType(val asInt: Int)

object WireType {
  case object Varint extends WireType(0)
  case object Fixed64 extends WireType(1)
  case object LengthDelimited extends WireType(2)
  case object Fixed32 extends WireType(5)
}
