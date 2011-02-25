function foo(o, s)
/*:
    (rec o . {proto: Object, *: Int + 'o; /_(.*)_/: Any})
  * /([^_])(.*)([^_])/
 -> Undef + Int + (rec o . {proto: Object, *: Int + 'o; /_(.*)_/: Any})
*/
{
    return o[s];
}
