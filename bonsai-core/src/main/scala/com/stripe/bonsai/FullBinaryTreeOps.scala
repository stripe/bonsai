package com.stripe.bonsai

trait FullBinaryTreeOps[T, BL, LL] extends TreeOps[T, Either[BL, LL]] {

  type Label = Either[BL, LL]

  override def reduce[A](node: Node)(f: (Label, Iterable[A]) => A): A =
    foldNode(node)({ (lc, rc, lbl) =>
      f(Left(lbl), reduce(lc)(f) :: reduce(rc)(f) :: Nil)
    }, lbl => f(Right(lbl), Nil))

  def foldNode[A](node: Node)(f: (Node, Node, BL) => A, g: LL => A): A

  def reduceNode[A](node: Node)(f: (BL, Iterable[A]) => A)(g: LL => A): A =
    foldNode(node)({ (lc, rc, lbl) =>
      f(lbl, reduceNode(lc)(f)(g) :: reduceNode(rc)(f)(g) :: Nil)
    }, g)

  def label(node: Node): Either[BL, LL] =
    foldNode(node)({ case (_, _, bl) => Left(bl) }, ll => Right(ll))

  def children(node: Node): Iterable[Node] =
    foldNode(node)({ case (lc, rc, _) => lc :: rc :: Nil }, _ => Nil)
}

object FullBinaryTreeOps {
  final def apply[T, BL, LL](implicit ops: FullBinaryTreeOps[T, BL, LL]): FullBinaryTreeOps[T, BL, LL] = ops
}
