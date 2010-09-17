function quest(query, nodes)
/*:  Array<{op: Str + Undef, name: Str + Undef, value: Str + Undef}> *
     Undef + Array<HTMLElement>
  -> Undef + Array<HTMLElement> */
{
    var selector /*: upcast Undef + {op: Str + Undef, name: Str + Undef, value: Str + Undef} */, 
    func /*: upcast Undef + (HTMLElement + Undef -> Any) */, 
    i /*: upcast Undef + Int */, 
    j /*: upcast Undef + Int */;

    // Step through each selector.

    for (i = 0; i < query.length; /*: cheat Undef */ (i += 1)) {
        selector = query[i];
        name = /*: cheat Str */ (selector.name);
        func = hunter[selector.op];

        // There are two kinds of selectors: hunters and peckers. If this is a hunter,
        // loop through the the nodes, passing each node to the hunter function.
        // Accumulate all the nodes it finds.

        if (typeof func === 'function') {
            if (star) {
                return error("ADsafe: Query violation: *" +
                             selector.op + (selector.name || ''));
            }
            result = /*: HTMLElement */ [];
            for (j = 0; j < nodes.length; /*: cheat Undef */ (j += 1)) {
                func(nodes[j]);
            }
        } else {

            // If this is a pecker, get its function. There is a special case for
            // the :first and :rest selectors because they are so simple.

            value = selector.value;
            flipflop = false;
            func = pecker[selector.op];
            if (typeof func !== 'function') {
                switch (selector.op) {
                case ':first':
                    result = nodes.slice(0, 1);
                    break;
                case ':rest':
                    result = /*: cheat Array<HTMLElement> */ (nodes.slice(1));
                    break;
                default:
                    return error('ADsafe: Query violation: :' + selector.op);
                }
            } else {

                // For the other selectors, make an array of nodes that are filtered by
                // the pecker function.

                result = /*: HTMLElement */ [];
                for (j = 0; j < nodes.length; /*:cheat Undef */ (j += 1)) {
                    if (func(nodes[j])) {
                        result.push(nodes[/*: cheat Int */ j]);
                    }
                }
            }
        }
        nodes = result;
    }
    return result;
}