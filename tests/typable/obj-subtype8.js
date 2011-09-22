function foo(o) /*: #{x : Int, y : _, __proto__ : { y: _, __proto__: { y: _, __proto__: {y : Bool} } } } -> #{y : ^Bool} */ {
    return o;
}
