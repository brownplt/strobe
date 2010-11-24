/* We want to still allow not including arguments that are <: Undef,
 * but we don't want to mess up and *not* provide restargs that are
 * expected.  This should be caught with a well-formedness error */
function bar(f) /*: (Str * Str + Undef * Int ... -> Bool) -> Bool */ {
    return f("string");
}

