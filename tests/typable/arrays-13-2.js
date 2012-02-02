function foo(a, e) /*: forall a <: Num . Array<Array<'a>> * 'a -> Array<Array<'a>> */ {
    return a.concat([[e]]);
}
