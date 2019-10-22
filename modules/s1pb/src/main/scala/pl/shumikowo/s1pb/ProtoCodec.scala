package pl.shumikowo.s1pb

import java.nio.ByteBuffer

import com.google.protobuf.CodedInputStream

class ProtoCodec() {

  def encode[T: Protobuf](t: T): ByteBuffer = {
    val out = new Output
    implicitly[Protobuf[T]].write(out, t)
    val bb = ByteBuffer.allocate(out.size)
    out.copyTo(bb)
    bb
  }

  def decode[T: Protobuf](data: ByteBuffer): T = {
    val cis = CodedInputStream.newInstance(data)
    implicitly[Protobuf[T]].read(cis)
  }

}
