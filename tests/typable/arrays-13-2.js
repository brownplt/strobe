function foo(a, e) /*: forall a <: Int . Array<Array<'a>> * 'a -> Array<Array<'a>> */ {
    return a.concat([[e]]);
}
