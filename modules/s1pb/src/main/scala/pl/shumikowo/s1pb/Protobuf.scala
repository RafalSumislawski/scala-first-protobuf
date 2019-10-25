package pl.shumikowo.s1pb

import java.nio.charset.StandardCharsets
import java.time.Instant

import com.google.protobuf.{CodedInputStream, WireFormat}
import magnolia._
import pl.shumikowo.s1pb.Protobuf.{ImapedProtobuf, ImapedRepeatedProtobuf}
import pl.shumikowo.s1pb.ProtobufTypes._
import pl.shumikowo.s1pb.WireType._

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.ClassTag

trait Protobuf[T] {

  def repeated: Boolean = false

  // TODO try to bind together wireType, protobufType, and, messageModel

  def wireType: WireType

  def protobufType: ProtobufType

  def messageModel: Option[ProtobufModel] = None

  final def requiredModels: Set[ProtobufModel] = requiredModelsGen(Set.empty)

  def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] = messageModel.toSet

  final def canBeRoot: Boolean = messageModel.isDefined && !repeated // TODO use it (and write tests)

  def write(out: Output, value: T): Unit

  def read(in: CodedInputStream): T

  def imap[T2](f: T => T2, g: T2 => T): Protobuf[T2] =
    new ImapedProtobuf[T, T2](this, f, g)

}

trait RepeatedProtobuf[T, E] extends Protobuf[T] {

  def protobufElement: Protobuf[E]

  def builder: () => mutable.Builder[E, T]

  def forEach(t: T, f: E => Unit): Unit

  override def repeated: Boolean = true

  override def wireType: WireType = LengthDelimited

  override def messageModel: Option[ProtobufModel] = None

  override def write(out: Output, value: T): Unit = ??? // TODO nice error

  override def read(in: CodedInputStream): T = ??? // TODO nice error

  override def imap[T2](transform: T => T2, reverse: T2 => T): RepeatedProtobuf[T2, E] =
    new ImapedRepeatedProtobuf[T, T2, E](this, transform, reverse)
}

object Protobuf {

  @inline def apply[T: Protobuf]: Protobuf[T] = implicitly[Protobuf[T]]

  type Typeclass[T] = Protobuf[T]

  implicit object ProtobufBoolean extends Protobuf[Boolean] {
    override def wireType: WireType = Varint
    override def protobufType: ProtobufType = "bool"
    override def write(out: Output, value: Boolean): Unit = out.writeBoolNoTag(value)
    override def read(in: CodedInputStream): Boolean = in.readBool()
  }

  implicit object ProtobufByte extends Protobuf[Byte] {
    override def wireType: WireType = Varint
    override def protobufType: ProtobufType = "int32"
    override def write(out: Output, value: Byte): Unit = out.writeInt32NoTag(value)
    override def read(in: CodedInputStream): Byte = in.readInt32().toByte
  }

  implicit object ProtobufInt extends Protobuf[Int] {
    override def wireType: WireType = Varint
    override def protobufType: ProtobufType = "int32"
    override def write(out: Output, value: Int): Unit = out.writeInt32NoTag(value)
    override def read(in: CodedInputStream): Int = in.readInt32()
  }

  implicit object ProtobufLong extends Protobuf[Long] {
    override def wireType: WireType = Varint
    override def protobufType: ProtobufType = "int64"
    override def write(out: Output, value: Long): Unit = out.writeInt64NoTag(value)
    override def read(in: CodedInputStream): Long = in.readInt64()
  }

  implicit object ProtobufString extends Protobuf[String] {
    override def wireType: WireType = LengthDelimited
    override def protobufType: ProtobufType = "string"
    override def write(out: Output, value: String): Unit = out.write(value.getBytes(StandardCharsets.UTF_8)) // TODO reduce allocations
    override def read(in: CodedInputStream): String = new String(in.readRawBytes(in.getBytesUntilLimit), StandardCharsets.UTF_8) // TODO reduce allocations
  }

  implicit object ProtobufFloat extends Protobuf[Float] {
    override def wireType: WireType = Fixed32
    override def protobufType: ProtobufType = "float"
    override def write(out: Output, value: Float): Unit = out.writeFloatNoTag(value)
    override def read(in: CodedInputStream): Float = in.readFloat()
  }

  implicit object ProtobufDouble extends Protobuf[Double] {
    override def wireType: WireType = Fixed64
    override def protobufType: ProtobufType = "double"
    override def write(out: Output, value: Double): Unit = out.writeDoubleNoTag(value)
    override def read(in: CodedInputStream): Double = in.readDouble()
  }

  implicit object ProtobufShort extends Protobuf[Short] {
    override def wireType: WireType = Varint
    override def protobufType: ProtobufType = "int32"
    override def write(out: Output, value: Short): Unit = out.writeInt32NoTag(value)
    override def read(in: CodedInputStream): Short = in.readInt32().toShort
  }

  implicit object ProtobufChar extends Protobuf[Char] {
    override def wireType: WireType = Varint
    override def protobufType: ProtobufType = "int32"
    override def write(out: Output, value: Char): Unit = out.writeInt32NoTag(value)
    override def read(in: CodedInputStream): Char = in.readInt32().toChar
  }

  implicit object ProtobufInstant extends Protobuf[Instant] {
    override def wireType: WireType = Fixed64
    override def protobufType: ProtobufType = "int64"
    override def write(out: Output, value: Instant): Unit = out.writeInt64NoTag(value.toEpochMilli)
    override def read(in: CodedInputStream): Instant = Instant.ofEpochMilli(in.readInt64())
  }

  implicit def protobufOption[T](implicit T: Protobuf[T]): Protobuf[Option[T]] = new ProtobufOption(T)

  class ProtobufOption[T](val protobufNested: Protobuf[T]) extends Protobuf[Option[T]] {
    override def repeated: Boolean = protobufNested.repeated
    override def wireType: WireType = protobufNested.wireType
    override def protobufType: ProtobufType = protobufNested.protobufType
    override def messageModel: Option[ProtobufModel] = protobufNested.messageModel
    override def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] = protobufNested.requiredModelsGen(alreadyKnown)
    override def write(out: Output, value: Option[T]): Unit = ???
    override def read(in: CodedInputStream): Option[T] = ???
  }

  implicit def protobufVector[T](implicit T: Protobuf[T]): Protobuf[Vector[T]] = new ProtobufIterable(T, () => Vector.newBuilder[T])

  implicit def protobufList[T](implicit T: Protobuf[T]): Protobuf[List[T]] = new ProtobufIterable(T, () => List.newBuilder[T])

  implicit def protobufSeq[T](implicit T: Protobuf[T]): Protobuf[Seq[T]] = new ProtobufIterable(T, () => Seq.newBuilder[T])

  case class MapEntry[K, V](k: K, v: V)

  implicit def protobufMap[K: Protobuf, V: Protobuf]: Protobuf[Map[K, V]] =
    protobufSeq[MapEntry[K, V]].imap(
      _.map { case MapEntry(k, v) => (k -> v) }(scala.collection.breakOut),
      _.map { case (k, v) => MapEntry(k, v) }(scala.collection.breakOut)
    )

  class ProtobufIterable[T <: scala.collection.Iterable[E], E](val protobufElement: Protobuf[E], val builder: () => mutable.Builder[E, T])
    extends RepeatedProtobuf[T, E] {
    override def protobufType: ProtobufType = protobufElement.protobufType
    override def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] = protobufElement.requiredModelsGen(alreadyKnown)
    override def forEach(t: T, f: E => Unit): Unit = t.foreach(f)
  }

  implicit object protobufArrayOfBytes extends Protobuf[Array[Byte]] {
    override def wireType: WireType = LengthDelimited
    override def protobufType: ProtobufType = "bytes"
    override def write(out: Output, value: Array[Byte]): Unit = out.write(value)
    override def read(in: CodedInputStream): Array[Byte] = in.readRawBytes(in.getBytesUntilLimit)
  }

  implicit def protobufArray[T: ClassTag](implicit T: Protobuf[T]): Protobuf[Array[T]] = new ProtobufArray(T, () => Array.newBuilder[T])

  class ProtobufArray[T <: Array[E], E](val protobufElement: Protobuf[E], val builder: () => mutable.Builder[E, T])
    extends RepeatedProtobuf[T, E] {
    override def protobufType: ProtobufType = protobufElement.protobufType
    override def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] = protobufElement.requiredModelsGen(alreadyKnown)
    override def forEach(t: T, f: E => Unit): Unit = t.foreach(f)
  }

  // TODO enums

  def combine[T](ctx: CaseClass[Protobuf, T]): Protobuf[T] =
    if (ctx.isValueClass && ctx.parameters.nonEmpty) protobufValueType(ctx, ctx.parameters.head)
    else if (ctx.isObject) ???
    else new ProtobufProductType(ctx)

  class ProtobufProductType[T](ctx: CaseClass[Protobuf, T]) extends Protobuf[T] {

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
          .map(_.typeclass.requiredModelsGen(ak))
          .toSet.flatten ++ messageModel
      }

    override def write(out: Output, value: T): Unit = {
      var i = 0
      while (i < params.length) {
        val param = params(i)
        val index = positionToIndex(i)
        val paramValue = param.dereference(value)
        param.typeclass match {
          case pb if pb.isInstanceOf[RepeatedProtobuf[_, _]] =>
            val pb2 = pb.asInstanceOf[RepeatedProtobuf[Any, Any]]

            if (pb2.protobufElement.wireType == LengthDelimited) {
              pb2.forEach(paramValue.asInstanceOf[Any], element => writeField(out, index, pb2.protobufElement, element))
            } else { // packed encoding
              writePacked(out, index, pb2, paramValue)
            }

          case option if option.isInstanceOf[ProtobufOption[_]] =>
            paramValue.asInstanceOf[Option[Any]] match {
              case Some(e) => writeField(out, index, option.asInstanceOf[ProtobufOption[Any]].protobufNested, e)
              case None =>
            }

          case other =>
            writeField(out, index, other.asInstanceOf[Protobuf[Any]], paramValue)
        }
        i += 1
      }
    }

    override def read(in: CodedInputStream): T = {
      val fields = new Array[Any](params.length)
      var tag = in.readTag()
      while (tag != 0) {
        val index = WireFormat.getTagFieldNumber(tag)
        val wireTypeAsInt = WireFormat.getTagWireType(tag)
        indexToPosition.get(index) match {
          case Some(position) =>
            val param = params(position)

            param.typeclass match {
              case seq if seq.isInstanceOf[RepeatedProtobuf[_, _]] =>
                val seq2 = seq.asInstanceOf[RepeatedProtobuf[_, Any]]
                val currentValue = fields(position)
                val builder =
                  if (currentValue == null) {
                    val b = seq2.builder()
                    fields(position) = b
                    b
                  } else {
                    currentValue.asInstanceOf[mutable.Builder[Any, _]]
                  }
                if(seq2.protobufElement.wireType != LengthDelimited || wireTypeAsInt == LengthDelimited.asInt){ // packed encoding
                  val length = in.readInt32()
                  val oldLimit = in.pushLimit(length)
                  while(!in.isAtEnd){
                    builder += seq2.protobufElement.read(in)
                  }
                  in.popLimit(oldLimit)
                } else {
                  val elem = readField(in, seq2.protobufElement)
                  builder += elem
                }
              case seq if seq.isInstanceOf[ProtobufOption[_]] =>
                if (wireTypeAsInt != param.typeclass.wireType.asInt)
                  throw new RuntimeException(s"Something's wrong with the schema. Expected wireType=[${param.typeclass.wireType.asInt}] at index [$index], instead got [$wireType]")
                fields(position) = Some(readField(in, seq.asInstanceOf[ProtobufOption[Any]].protobufNested))
              case other =>
                if (wireTypeAsInt != param.typeclass.wireType.asInt)
                  throw new RuntimeException(s"Something's wrong with the schema. Expected wireType=[${param.typeclass.wireType.asInt}] at index [$index], instead got [$wireType]")
                fields(position) = readField(in, other)
            }
          case None => in.skipField(tag)
        }
        tag = in.readTag()
      }
      var i = 0
      while (i < fields.length) {
        val current = fields(i)
        val param = params(i)
        current match {
          case b: mutable.Builder[_, _] if param.typeclass.isInstanceOf[RepeatedProtobuf[_, _]] => fields(i) = b.result()
          case null if param.typeclass.isInstanceOf[ProtobufOption[_]] => fields(i) = None
          case _ =>
        }
        i += 1
      }
      ctx.rawConstruct(fields)
    }
  }

  private def protobufValueType[T, T2](ctx: CaseClass[Protobuf, T], p: Param[Protobuf, T]): Protobuf[T] =
    new ProtobufValueType[T, p.PType](p.typeclass, (x: T) => p.dereference(x), (x: p.PType) => ctx.rawConstruct(Seq(x)))

  class ProtobufValueType[T, T2](protobufNested: Protobuf[T2], extract: T => T2, box: T2 => T) extends Protobuf[T] {
    override def repeated: Boolean = protobufNested.repeated
    override def wireType: WireType = protobufNested.wireType
    override def protobufType: ProtobufType = protobufNested.protobufType
    override def messageModel: Option[ProtobufModel] = protobufNested.messageModel
    override def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] = protobufNested.requiredModelsGen(alreadyKnown)
    override def write(out: Output, value: T): Unit = protobufNested.write(out, extract(value))
    override def read(in: CodedInputStream): T = box(protobufNested.read(in))
  }

  def dispatch[T](ctx: SealedTrait[Protobuf, T]): Protobuf[T] = new ProtobufSumType[T](ctx)

  class ProtobufSumType[T](ctx: SealedTrait[Protobuf, T]) extends Protobuf[T] {
    private val subtypes: Array[Subtype[Typeclass, T]] = ctx.subtypes.toArray.sortBy(_.typeName.full)
    private val subtypeToIndex: Map[Subtype[Typeclass, T], Int] = subtypes.zip(1 to subtypes.length).toMap
    private val indexToSubtype: Map[Int, Subtype[Typeclass, T]] = (1 to subtypes.length).zip(subtypes).toMap

    override def protobufType: ProtobufType = ctx.typeName.short

    override def wireType: WireType = LengthDelimited

    override def messageModel: Option[ProtobufModel] =
      Some(ProtobufSum(
        ctx.typeName.short,
        subtypes.zipWithIndex.map { case (st, idx) => ProtobufField(st.typeName.short, st.typeName.short + "Field", idx + 1) }.toVector
      ))

    override def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] =
      if (alreadyKnown.contains(ctx.typeName)) {
        Set.empty
      } else {
        val ak = alreadyKnown + ctx.typeName
        ctx.subtypes
          .map(_.typeclass.requiredModelsGen(ak))
          .toSet.flatten ++ messageModel
      }

    override def write(out: Output, value: T): Unit = {
      val index = ctx.dispatch(value)(subtypeToIndex)
      val pb = ctx.dispatch(value)(_.typeclass).asInstanceOf[Protobuf[T]]
      writeField(out, index, pb, value)
    }

    override def read(in: CodedInputStream): T = {
      val tag = in.readTag()
      val index = WireFormat.getTagFieldNumber(tag)
      val wireTypeAsInt = WireFormat.getTagWireType(tag)
      val pb = indexToSubtype(index).typeclass.asInstanceOf[Protobuf[T]]
      if (wireTypeAsInt != pb.wireType.asInt) {
        throw new RuntimeException(s"Something's wrong with the schema. Expected wireType=[${pb.wireType.asInt}] at index [$index], instead got [$wireType]")
      }
      Protobuf.readField(in, pb)
    }
  }

  private def writeField[T2](out: Output, index: Int, pb: Protobuf[T2], value: T2): Unit = {
    out.writeTag(index, pb.wireType.asInt)
    if (pb.wireType == LengthDelimited) {
      val lengthField = out.sub
      val content = out.sub
      pb.write(content, value)
      lengthField.writeInt32NoTag(content.size)
    } else {
      pb.write(out, value)
    }
  }

  private def writePacked[T2, E](out: Output, index: Int, pb: RepeatedProtobuf[T2, E], value: T2): Unit = {
    out.writeTag(index, LengthDelimited.asInt)
    val lengthField = out.sub
    val content = out.sub
    pb.forEach(value, e => pb.protobufElement.write(content, e))
    lengthField.writeInt32NoTag(content.size)
  }

  private def readField[T2](in: CodedInputStream, pb: Protobuf[T2]): T2 = {
    if (pb.wireType == LengthDelimited) {
      val length = in.readInt32()
      val oldLimit = in.pushLimit(length)
      val result = pb.read(in)
      in.popLimit(oldLimit)
      result
    } else {
      pb.read(in)
    }
  }

  implicit def gen[T]: Protobuf[T] = macro Magnolia.gen[T]

  class ImapedProtobuf[T, T2](base: Protobuf[T], f: T => T2, g: T2 => T) extends Protobuf[T2] {
    override def repeated: Boolean = base.repeated
    override def wireType: WireType = base.wireType
    override def protobufType: ProtobufType = base.protobufType
    override def messageModel: Option[ProtobufModel] = base.messageModel
    override def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] = base.requiredModelsGen(alreadyKnown)
    override def write(out: Output, value: T2): Unit = base.write(out, g(value))
    override def read(in: CodedInputStream): T2 = f(base.read(in))
  }

  class ImapedRepeatedProtobuf[S, S2, T](base: RepeatedProtobuf[S, T], f: S => S2, g: S2 => S)
    extends RepeatedProtobuf[S2, T] {
    override def protobufType: ProtobufType = base.protobufType
    override def requiredModelsGen(alreadyKnown: Set[TypeName]): Set[ProtobufModel] = base.requiredModelsGen(alreadyKnown)
    override def protobufElement: Typeclass[T] = base.protobufElement
    override def builder: () => mutable.Builder[T, S2] = () => base.builder().mapResult(f)
    override def forEach(t: S2, f: T => Unit): Unit = base.forEach(g(t), f)
  }
}
