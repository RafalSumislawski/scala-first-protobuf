import java.nio.ByteBuffer

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment
import org.specs2.specification.{AfterAll, BeforeEach}
import pl.shumikowo.s1pb.Models.SealedCase1
import pl.shumikowo.s1pb.{ProtoCodec, Protobuf, Models => m, generated => g}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

class CompatibilityTest extends Specification with BeforeEach with AfterAll {
  sequential

  val s1pb = new ProtoCodec()

  "Products of simple types" should {
    test(
      m.SimpleTypes(42, 1234567, "test"),
      g.SimpleTypes(42, 1234567, "test")
    )
  }

  "Nested products" should {
    test(
      m.NestedProducts(m.InnerNestedProducts(m.BottomNestedProduct(1, 2), m.BottomNestedProduct(3, 4)), m.InnerNestedProducts(m.BottomNestedProduct(5, 6), m.BottomNestedProduct(7, 8))),
      g.NestedProducts(Some(g.InnerNestedProducts(Some(g.BottomNestedProduct(1, 2)), Some(g.BottomNestedProduct(3, 4)))), Some(g.InnerNestedProducts(Some(g.BottomNestedProduct(5, 6)), Some(g.BottomNestedProduct(7, 8))))),
    )
  }

  "Value types" should {
    test(
      m.WithValueType(m.ValueType(42)),
      g.WithValueType(42)
    )
  }

  "Sequence types" should {
    test(
      m.WithSequence(Seq("a", "b"), Vector("e", "f"), List("c", "d")),
      g.WithSequence(Seq("a", "b"), Vector("e", "f"), List("c", "d"))
    )
  }

  "Options" should {
    test(
      m.WithOptions(None, Some(7), None, Some(m.SimpleObject(8))),
      g.WithOptions(0, 7, None, Some(g.SimpleObject(8)))
    )
  }

  "Top level sealed traits 1" should {
    test(
      m.SealedCase1(42, "42").asInstanceOf[m.Sealed],
      g.Sealed(g.Sealed.Alternatives.SealedCase1Field(g.SealedCase1(42, "42")))
    )
  }

  "Top level sealed traits 2" should {
    test(
      m.SealedCase2(42, "42").asInstanceOf[m.Sealed],
      g.Sealed(g.Sealed.Alternatives.SealedCase2Field(g.SealedCase2(42, "42")))
    )
  }

  "Sealed traits inside case classes" should {
    test(
      m.WithSealed(m.Sealed2Case2(42, "42")),
      g.WithSealed(Some(g.Sealed2(g.Sealed2.Alternatives.Sealed2Case2Field(g.Sealed2Case2(42, "42")))))
    )
  }

  private def test[M: Protobuf, G <: GeneratedMessage with Message[G] : GeneratedMessageCompanion](m1: M, g1: G): Fragment = {
    s"model --protobuf--> model " in {
      log(s"starting with model $m1")
      val e = s1pb.encode(m1)
      e.flip()
      log(s"it's been encoded to ${pretty(e)}")
      val m2 = s1pb.decode[M](e)
      log(s"and decoded to $m2")

      m2 must_== m1
    }

    "model --protobuf--> scalapb " in {
      log(s"starting with model $m1")
      val e = s1pb.encode(m1)
      e.flip()
      log(s"it's been encoded to ${pretty(e)}")
      val g2 = implicitly[GeneratedMessageCompanion[G]].parseFrom(CodedInputStream.newInstance(e))
      log(s"and decoded to $g2")

      g2 must_== g1
    }

    "scalapb --protobuf--> model" in {
      log(s"starting with model $g1")
      val e = ByteBuffer.allocate(1024)
      val cos = CodedOutputStream.newInstance(e)
      g1.writeTo(cos)
      cos.flush()
      e.flip()
      log(s"it's been encoded to ${pretty(e)}")
      val m2 = s1pb.decode[M](e)
      log(s"and decoded to $m2")

      m2 must_== m1
    }
  }

  private def pretty(bb: ByteBuffer): String = {
    val bb2 = bb.duplicate()
    val arr = new Array[Byte](bb2.remaining())
    bb2.get(arr)
    s"pos=${bb.position()} lim=${bb.limit()} cap=${bb.capacity()} " + arr.map(b => f"$b%02x").mkString("[", " ", "]")
  }

  private var logs: Vector[String] = Vector.empty[String]

  private def log(s: String): Unit = logs :+= s

  override def before = {
    logs.foreach(println)
    logs = Vector.empty[String]
  }

  override def afterAll = {
    logs.foreach(println)
    logs = Vector.empty[String]
  }
}
