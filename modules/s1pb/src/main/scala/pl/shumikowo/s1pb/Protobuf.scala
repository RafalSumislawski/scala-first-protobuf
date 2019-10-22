package pl.shumikowo.s1pb

import java.nio.charset.{CharsetEncoder, StandardCharsets}
import java.time.Instant

import com.google.protobuf.{CodedInputStream, WireFormat}
import magnolia.{CaseClass, Magnolia, Param, TypeName}
import pl.shumikowo.s1pb.Protobuf._

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.language.experimental.macros

trait Protobuf[T] {

  def repeated: Boolean = false

  def wireType: WireType

  def protobufType: ProtobufType

  def messageModel: Option[ProtobufModel] = None

  final def requiredModels: Set[ProtobufModel] = requiredModelsGen(Set.empty)

  def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] = messageModel.toSet

  final def canBeRoot: Boolean = messageModel.isDefined && !repeated

  def write(out: Output, value: T): Unit

  def read(in: CodedInputStream): T

}

object Protobuf {

  sealed trait ProtobufModel

  case class ProtobufProduct(typ: ProtobufType, fields: Vector[ProtobufField]) extends ProtobufModel

  case class ProtobufSum(typ: ProtobufType, alternatives: Vector[ProtobufField]) extends ProtobufModel

  case class ProtobufField(typ: ProtobufType, name: String, id: Int, repeated: Boolean = false)

  type ProtobufType = String

  sealed class WireType(val asInt: Int)
  case object Varint extends WireType(0)
  case object Fixed64 extends WireType(1)
  case object LengthDelimited extends WireType(2)
  case object Fixed32 extends WireType(5)

  @inline def apply[T: Protobuf]: Protobuf[T] = implicitly[Protobuf[T]]

  type Typeclass[T] = Protobuf[T]

  // @formatter:off
  implicit val protobufferifyBoolean: Protobuf[Boolean] = new Protobuf[Boolean] {
    override def wireType: WireType = Varint
    override def protobufType: ProtobufType = "bool"
    override def write(out:  Output, value:  Boolean): Unit = out.writeBoolNoTag(value)
    override def read(in:  CodedInputStream): Boolean = in.readBool()
}

  implicit val protobufferifyInt: Protobuf[Int] = new Protobuf[Int] {
    override def wireType: WireType = Varint
    override def protobufType: ProtobufType = "int32"
    override def write(out:  Output, value: Int): Unit = out.writeInt32NoTag(value)
    override def read(in:  CodedInputStream): Int = in.readInt32()
}

  implicit val protobufferifyLong: Protobuf[Long] = new Protobuf[Long] {
    override def wireType: WireType = Varint
    override def protobufType: ProtobufType = "int64"
    override def write(out:  Output, value: Long): Unit = out.writeInt64NoTag(value)
    override def read(in:  CodedInputStream): Long = in.readInt64()
  }

  implicit val protobufferifyString: Protobuf[String] = new Protobuf[String] {
    override def wireType: WireType = LengthDelimited
    override def protobufType: ProtobufType = "string"
    override def write(out:  Output, value: String): Unit = out.write(value.getBytes(StandardCharsets.UTF_8)) // TODO reduce allocations
    override def read(in:  CodedInputStream): String = new String(in.readRawBytes(in.getBytesUntilLimit), StandardCharsets.UTF_8) // TODO reduce allocations
  }

  implicit val protobufferifyInstant: Protobuf[Instant] = new Protobuf[Instant] {
    override def wireType: WireType = Fixed64
    override def protobufType: ProtobufType = "int64"
    override def write(out:  Output, value: Instant): Unit = out.writeInt64NoTag(value.toEpochMilli)
    override def read(in:  CodedInputStream): Instant = Instant.ofEpochMilli(in.readInt64())
  }

  implicit def protobufferifyOption[T](implicit T: Protobuf[T]): Protobuf[Option[T]] = T.asInstanceOf[Protobuf[Option[T]]]

  implicit def protobufferifyVector[T](implicit T: Protobuf[T]): Protobuf[Vector[T]] = new ProtobufSeq(T, () => Vector.newBuilder[T])

  implicit def protobufferifyList[T](implicit T: Protobuf[T]): Protobuf[List[T]] = new ProtobufSeq(T, () => List.newBuilder[T])

  implicit def protobufferifySeq[T](implicit T: Protobuf[T]): Protobuf[Seq[T]] = new ProtobufSeq(T, () => Seq.newBuilder[T])

  class ProtobufSeq[S <: Seq[T], T](val protobufElement: Protobuf[T], val builder: () => scala.collection.mutable.Builder[T, S]) extends Protobuf[S] {
    override def repeated: Boolean = true
    override def wireType: WireType = LengthDelimited // ???
    override def protobufType: ProtobufType = protobufElement.protobufType
    override def messageModel: Option[ProtobufModel] = protobufElement.messageModel
    override def requiredModelsGen(alreadyKnown:  Set[TypeName]): Set[ProtobufModel] = protobufElement.requiredModelsGen(alreadyKnown)
    override def write(out: Output, value: S): Unit = ???
    override def read(in: CodedInputStream): S = ???
  }

  class ProtobufValueType[T, T2](protobufNested: Protobuf[T2], extract: T => T2, box: T2 => T) extends Protobuf[T]{
    override def repeated: Boolean = protobufNested.repeated
    override def wireType: WireType = protobufNested.wireType
    override def protobufType: ProtobufType = protobufNested.protobufType
    override def messageModel: Option[ProtobufModel] = protobufNested.messageModel
    override def requiredModelsGen(alreadyKnown:  Set[TypeName]): Set[ProtobufModel] = protobufNested.requiredModelsGen(alreadyKnown)
    override def write(out: Output, value: T): Unit = protobufNested.write(out, extract(value))
    override def read(in: CodedInputStream): T = box(protobufNested.read(in))
  }

  private def protobufValueType[T, T2](ctx: CaseClass[Protobuf, T], p: Param[Protobuf, T]): Protobuf[T] =
    new ProtobufValueType[T, p.PType](p.typeclass, (x: T) => p.dereference(x), (x: p.PType) => ctx.rawConstruct(Seq(x)))

  def combine[T](ctx: CaseClass[Protobuf, T]): Protobuf[T] =
    if (ctx.isValueClass && ctx.parameters.nonEmpty) protobufValueType(ctx, ctx.parameters.head)
    else if (ctx.isObject) ???
    else new Protobuf[T] {

      private val params = ctx.parameters.toArray
      private val positionToIndex = (1 to params.length).toArray
      private val indexToPosition = positionToIndex.zipWithIndex.toMap

      override def wireType: WireType = LengthDelimited

      override def protobufType: ProtobufType = ctx.typeName.short

      override def messageModel: Option[ProtobufModel] =
        Some(ProtobufProduct(
          ctx.typeName.short,
          params.zip(positionToIndex).map { case (p, idx) => ProtobufField(p.typeclass.protobufType, p.label, idx, p.typeclass.repeated) }.toVector
        ))

      override def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] =
        if (alreadyKnown.contains(ctx.typeName)) {
          Set.empty
        } else {
          val ak = alreadyKnown + ctx.typeName
          ctx.parameters
            .collect { case param => param.typeclass.requiredModelsGen(ak) }
            .toSet.flatten ++ messageModel
        }

      override def write(out: Output, value: T): Unit = {
        var i = 0
        while(i < params.length){
          val param = params(i)
          val index = positionToIndex(i)
          param.typeclass match {
            case seq if seq.isInstanceOf[ProtobufSeq[_, _]] =>
              // TODO compressed arrays
              val protobufElement = seq.asInstanceOf[ProtobufSeq[_, Any]].protobufElement
              param.dereference(value).asInstanceOf[Seq[_]].foreach{ element =>
                writeField(out, index, protobufElement, element)
              }
            case other =>
              writeField(out, index, other.asInstanceOf[Protobuf[Any]], param.dereference(value))
          }
          i += 1
        }
      }

      private def writeField[T2](out: Output, index: Int, pb: Protobuf[T2], value: T2): Unit = {
        out.writeTag(index, pb.wireType.asInt)
        if(pb.wireType == LengthDelimited){
          val lengthField = out.sub
          val content = out.sub
          pb.write(content, value)
          lengthField.writeInt32NoTag(content.size)
        } else {
          pb.write(out, value)
        }
      }

      override def read(in: CodedInputStream): T = {
        val fields = new Array[Any](params.length)
        var tag = in.readTag()
        while(tag != 0) {
          val index = WireFormat.getTagFieldNumber(tag)
          val wireTypeAsInt = WireFormat.getTagWireType(tag)
          indexToPosition.get(index) match {
            case Some(position) =>
              val param = params(position)
              if(wireTypeAsInt != param.typeclass.wireType.asInt){
                throw new RuntimeException(s"Something's wrong with the schema. Expected wireType=[${param.typeclass.wireType.asInt}] at index [$index], instead got [$wireType]")
              }
              param.typeclass match {
                case seq if seq.isInstanceOf[ProtobufSeq[_, _]] =>
                  // TODO compressed arrays
                  val seq2 = seq.asInstanceOf[ProtobufSeq[_, Any]]
                  val elem = readField(in, seq2.protobufElement)
                  val currentValue = fields(position)
                  if(currentValue == null) {
                    fields(position) = (seq2.builder() += elem)
                  } else {
                    currentValue.asInstanceOf[mutable.Builder[Any,_]] += elem
                  }
                case other =>
                  fields(position) = readField(in, other)
              }
            case None => in.skipField(tag)
          }
          tag = in.readTag()
        }
        var i = 0
        while(i < fields.length){
          val current = fields(i)
          current match {
            case b: mutable.Builder[_, _] => fields(i) = b.result()
            case _ =>
          }
          i += 1
        }
        ctx.rawConstruct(fields)
      }
    }

  private def readField[T2](in: CodedInputStream, pb: Protobuf[T2]): T2  = {
    if(pb.wireType == LengthDelimited) {
      val length = in.readInt32()
      val oldLimit = in.pushLimit(length)
      val result = pb.read(in)
      in.popLimit(oldLimit)
      result
    } else {
      pb.read(in)
    }
  }

//  def dispatch[T](ctx: SealedTrait[Protobuf, T]): Protobuf[T] = {
//    new Protobuf[T] {
//      override def protobufType: ProtobufType = ctx.typeName.short
//
//      override def messageModel: Option[ProtobufModel] =
//        Some(ProtobufSum(
//          ctx.typeName.short,
//          ctx.subtypes.zipWithIndex.map { case (st, idx) => ProtobufField(st.typeName.short, st.typeName.short + "Field", idx + 1) }.toVector
//        ))
//
//      override def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] =
//        if (alreadyKnown.contains(ctx.typeName)) {
//          Set.empty
//        } else {
//          val ak = alreadyKnown + ctx.typeName
//          ctx.subtypes
//            .collect { case param => param.typeclass.requiredModelsGen(ak) }
//            .toSet.flatten ++ messageModel
//        }
//    }
//  }

  implicit def gen[T]: Protobuf[T] = macro Magnolia.gen[T]

}
