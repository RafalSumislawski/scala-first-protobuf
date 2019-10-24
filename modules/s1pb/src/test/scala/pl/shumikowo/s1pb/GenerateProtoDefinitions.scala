package pl.shumikowo.s1pb

import org.specs2.specification.core.Fragment
import pl.shumikowo.s1pb.Models._

class GenerateProtoDefinitions extends Spec {

  "ProtoRenderer" should {
    saveProtoDefinition[SimpleTypes]("SimpleTypes")
    saveProtoDefinition[NestedProducts]("NestedProducts")
    saveProtoDefinition[WithValueType]("WithValueType")
    saveProtoDefinition[WithSequence]("WithSequence")
    saveProtoDefinition[WithOptions]("WithOptions")
    saveProtoDefinition[Sealed]("Sealed")
    saveProtoDefinition[WithSealed]("WithSealed")
    saveProtoDefinition[WithMap]("WithMap")
    saveProtoDefinition[WithArrays]("WithArrays")
  }

  private def saveProtoDefinition[T: Protobuf](name: String): Fragment = {
    name in {
      save(name, new ProtoRenderer("test", "pl.shumikowo.s1pb.generated", name).render(Vector(implicitly[Protobuf[T]])))
      ok
    }
  }
}