function parse_query(text, id) 
/*: 
Str * Str -> 
Array<{op: Str + Undef, toString: -> Str}> + Undef */
{

    // Convert a query string into an array of op/name/value selectors.
    // A query string is a sequence of triples wrapped in brackets; or names,
    // possibly prefixed by # . & > _, or :option, or * or /. A triple is a name,
    // and operator (one of [=, [!=, [*=, [~=, [|=, [$=, or [^=) and a value.

    // If the id parameter is supplied, then the name following # must have the
    // id as a prefix and must match the ADsafe rule for id: being all uppercase
    // letters and digits with one underbar.

    // A name must be all lower case and may contain digits, -, or _.

    var match /*: upcast Undef + Array<Str + Undef> */,          // A match array
    query = /*: {op: Str + Undef, toString: -> Str} */ [], // The resulting query array
    selector /*: upcast Undef + 
                {op: Str + Undef, toString: -> Str} */,
    qx = (/*: cheat Bool */ id) ?
        /^\s*(?:([\*\/])|\[\s*([a-z][0-9a-z_\-]*)\s*(?:([!*~|$\^]?\=)\s*([0-9A-Za-z_\-*%&;.\/:!]+)\s*)?\]|#\s*([A-Z]+_[A-Z0-9]+)|:\s*([a-z]+)|([.&_>\+]?)\s*([a-z][0-9a-z\-]*))\s*/ :
        /^\s*(?:([\*\/])|\[\s*([a-z][0-9a-z_\-]*)\s*(?:([!*~|$\^]?\=)\s*([0-9A-Za-z_\-*%&;.\/:!]+)\s*)?\]|#\s*([\-A-Za-z0-9_]+)|:\s*([a-z]+)|([.&_>\+]?)\s*([a-z][0-9a-z\-]*))\s*/;

    // Loop over all of the selectors in the text.

    do {

        // The qx teases the components of one selector out of the text, ignoring
        // whitespace.

        //          match[0]  the whole selector
        //          match[1]  * /
        //          match[2]  attribute name
        //          match[3]  = != *= ~= |= $= ^=
        //          match[4]  attribute value
        //          match[5]  # id
        //          match[6]  : option
        //          match[7]  . & _ > +
        //          match[8]      name

        match = qx.exec(text);
        if (!(/*: cheat Bool */ match)) {
            return error("ADsafe: Bad query:" + text);
        }

        // Make a selector object and stuff it in the query.

        if (match[1]) {

            // The selector is * or /

            selector = {
                op: match[1]
            };
        } else if (match[2]) {

            // The selector is in brackets.

            selector = match[3] ? {
                op: /*: upcast Str + Undef */ ('[' + match[3]),
                name: match[2],
                value: match[4]
            } : {
                op: /*: upcast Str + Undef */ '[',
                name: match[2]
            };
        } else if (match[5]) {

            // The selector is an id.

            if (query.length > 0 || match[5].length <= id.length ||
                match[5].slice(0, id.length) !== id) {
                return error("ADsafe: Bad query: " + text);
            }
            selector = {
                op: /*: upcast Str + Undef */ '#',
                name: match[5]
            };

            // The selector is a colon.

        } else if (match[6]) {
            selector = {
                op: /*: upcast Str + Undef */ (':' + match[6])
            };

            // The selector is one of > + . & _ or a naked tag name

        } else {
            selector = {
                op: match[7],
                name: match[8]
            };
        }

        // Add the selector to the query.

        query.push(selector);

        // Remove the selector from the text. If there is more text, have another go.

        text = text.slice(match[0].length);
    } while (/*: cheat Bool */ text);
    return query;
}

