package pl.shumikowo.s1pb

object ProtobufTypes {

  sealed trait ProtobufModel

  case class ProtobufProduct(typ: ProtobufType, fields: Vector[ProtobufField]) extends ProtobufModel

  case class ProtobufSum(typ: ProtobufType, alternatives: Vector[ProtobufField]) extends ProtobufModel

  case class ProtobufField(typ: ProtobufType, name: String, id: Int, repeated: Boolean = false)

  type ProtobufType = String

}
