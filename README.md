# Scala-first protobuf (S1PB)
A library for deriving protobuf serialisers/deserializes and .proto files from Scala code.

# Encoding / decoding
```scala
import Protobuf._
case class Foo(a: Int, b: String)
val s1pb = new ProtoCodec()

val foo = Foo(7, "foos")
val bytes: ByteBuffer = s1pb.encode(foo)
bytes.flip()
val decodedFoo = s1pb.decode[Foo](bytes)
```

# Generating .proto definitions
```scala
val renderer = new ProtoRenderer("protoPackageNAme", "java.package.name", "JavaClassName")
val protoDefinition: String = renderer.render(Vector(implicitly[Protobuf[Foo]]))
```