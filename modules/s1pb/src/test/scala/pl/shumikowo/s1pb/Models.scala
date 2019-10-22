package pl.shumikowo.s1pb

object Models {

  case class SimpleTypes(i: Int, l: Long, s: String)

  case class NestedProducts(p1: InnerNestedProducts, p2: InnerNestedProducts)
  case class InnerNestedProducts(p1: BottomNestedProduct, p2: BottomNestedProduct)
  case class BottomNestedProduct(i1: Int, i2: Int)

  case class WithSequence(s: Seq[String], v: Vector[String], l: List[String])

  case class WithValueType(v: ValueType)
  case class ValueType(i: Int) extends AnyVal

  case class WithOptions(o1: Option[Int], o2: Option[Int], o3: Option[SimpleObject], o4: Option[SimpleObject])
  case class SimpleObject(i: Int)
}
