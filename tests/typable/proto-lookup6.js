function foo(o, s)
/*:
    (rec o . { __proto__: Object, *: Int + 'o, /___(.*)_/: Any})
  * /([^_])(.*)([^_])/
 -> Undef + Int + (rec o . { __proto__: Object, *: Int + 'o, /___(.*)_/: Any})
*/
{
    return o[s];
}
