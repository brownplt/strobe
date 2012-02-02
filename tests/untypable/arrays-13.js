function foo(a) /*: forall a <: Num . Array<Array<'a>> -> Array<Array<'a>> */ {
    return a.concat([[5]]);
}
