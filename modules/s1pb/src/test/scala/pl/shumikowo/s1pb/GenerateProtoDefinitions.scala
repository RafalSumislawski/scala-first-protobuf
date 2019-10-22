package pl.shumikowo.s1pb

import pl.shumikowo.s1pb.Models.{NestedProducts, SimpleTypes, WithOptions, WithSequence, WithValueType}

class GenerateProtoDefinitions extends Spec {

  "ProtoRenderer" should {

    "SimpleTypes" in {
      saveProtoDefinition[SimpleTypes]("SimpleTypes")
      ok
    }

    "NestedProducts" in {
      saveProtoDefinition[NestedProducts]("NestedProducts")
      ok
    }

    "WithValueType" in {
      saveProtoDefinition[WithValueType]("WithValueType")
      ok
    }

    "WithSequence" in {
      saveProtoDefinition[WithSequence]("WithSequence")
      ok
    }

    "WithOptions" in {
      saveProtoDefinition[WithOptions]("WithOptions")
      ok
    }
  }

  private def saveProtoDefinition[T: Protobuf](name: String): Unit =
    save(name, new ProtoRenderer("test", "pl.shumikowo.s1pb.generated", name).render(Vector(implicitly[Protobuf[T]])))
}