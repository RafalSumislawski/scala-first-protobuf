package pl.shumikowo.s1pb

import pl.shumikowo.s1pb.ProtobufTypes._

class ProtoRenderer(protoPackage: String, javaPackage: String, javaClassName: String, additional: String = "") {

  def render(messages: Vector[Protobuf[_]]): String = {

    val header =
      s"""
        |syntax = "proto3";
        |
        |option java_multiple_files = true;
        |option java_package = "$javaPackage";
        |option java_outer_classname = "$javaClassName";
        |
        |package $protoPackage;
        |
        |""".stripMargin
    val models = messages.flatMap(pb => pb.messageModel ++ pb.requiredModels).toSet

    header ++ models.map(render).mkString("\n\n")
  }

  private def render(m: ProtobufModel): String = m match {
    case ProtobufProduct(typ, fields) =>
      s"""message $typ {
         |${fields.map(f => s"  ${render(f, f.id)};").mkString("\n")}
         |}""".stripMargin
    case ProtobufSum(typ, alternatives) =>
      s"""message $typ {
         |  oneof alternatives {
         |${alternatives.map(f => s"    ${render(f, f.id)};").mkString("\n")}
         |  }
         |}""".stripMargin
  }

  private def render(f: ProtobufField, id: Int): String =
    s"${if (f.repeated) "repeated " else ""}${f.typ} ${f.name} = $id"
}
