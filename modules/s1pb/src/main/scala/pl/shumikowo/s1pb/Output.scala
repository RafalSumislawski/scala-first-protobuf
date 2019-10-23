package pl.shumikowo.s1pb

import java.nio.ByteBuffer

import com.google.protobuf.CodedOutputStream

class Output() {

  private var bufs: List[Either[Output, ByteBuffer]] = List.empty

  def sub: Output = {
    val newOutput = new Output()
    bufs = Left(newOutput) +: bufs
    newOutput
  }

  def onTail: CodedOutputStream = {
    // TODO reuse the buffers and cos
    val newBuf = ByteBuffer.allocate(4*1024)
    bufs = Right(newBuf) +: bufs
    CodedOutputStream.newInstance(newBuf)
  }

  def size: Int = bufs.foldLeft(0)((acc, e) => acc + e.fold(_.size, _.position()))

  def copyTo(bb: ByteBuffer): Unit = {
    bufs.reverse.foreach {
      case Left(out) =>
        out.copyTo(bb)
      case Right(bb2) =>
        val bb3 = bb2.duplicate()
        bb3.flip()
        bb.put(bb3)
    }
  }

  def writeBoolNoTag(boolean: Boolean): Unit = {
    val t = onTail
    t.writeBoolNoTag(boolean)
    t.flush()
  }

  def writeInt32NoTag(int: Int): Unit = {
    val t = onTail
    t.writeInt32NoTag(int)
    t.flush()
  }

  def writeInt64NoTag(long: Long): Unit = {
    val t = onTail
    t.writeInt64NoTag(long)
    t.flush()
  }

  def writeStringNoTag(string: String): Unit = {
    val t = onTail
    t.writeStringNoTag(string)
    t.flush()
  }

  def write(b: Array[Byte]): Unit = {
    val t = onTail
    t.write(b, 0, b.length)
    t.flush()
  }

  def writeTag(fieldNumber: Int, wireType: Int): Unit = {
    val t = onTail
    t.writeTag(fieldNumber, wireType)
    t.flush()
  }



}
