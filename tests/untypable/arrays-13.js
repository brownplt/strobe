function foo(a) /*: forall a <: Int . Array<Array<'a>> -> Array<Array<'a>> */ {
    return a.concat([[5]]);
}
