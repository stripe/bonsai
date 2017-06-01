package com.stripe.bonsai

trait FullBinaryTreeOps[T, BL, LL] extends TreeOps[T, Either[BL, LL]] {

  override def reduce[A](node: Node)(f: (Either[BL, LL], Iterable[A]) => A): A =
    foldNode(node)({ (lbl, lc, rc) =>
      f(Left(lbl), reduce(lc)(f) :: reduce(rc)(f) :: Nil)
    }, lbl => f(Right(lbl), Nil))

  def foldNode[A](node: Node)(f: (BL, Node, Node) => A, g: LL => A): A

  def reduceNode[A](node: Node)(f: (BL, A, A) => A, g: LL => A): A =
    foldNode(node)((lbl, rc, lc) => f(lbl, reduceNode(lc)(f, g), reduceNode(rc)(f, g)), g)

  def label(node: Node): Either[BL, LL] =
    foldNode(node)((bl, _, _) => Left(bl), ll => Right(ll))

  def children(node: Node): Iterable[Node] =
    foldNode(node)((_, lc, rc) => lc :: rc :: Nil, _ => Nil)

  def collectLeafLabelsF[A](node: Node)(f: LL => A): Set[A] =
    reduceNode[Set[A]](node)((_, lc, rc) => lc ++ rc, ll => Set(f(ll)))

  def collectLeafLabels(node: Node): Set[LL] = collectLeafLabelsF(node)(identity)
}

object FullBinaryTreeOps {
  final def apply[T, BL, LL](implicit ops: FullBinaryTreeOps[T, BL, LL]): FullBinaryTreeOps[T, BL, LL] = ops
}
