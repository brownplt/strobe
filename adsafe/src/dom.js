var root /*: upcast Undef + HTMLElement */;

var dom = {
    q: function (text) /*: ['Ad] 'Ad -> 'Ad */ {
        star = false;
        var query = parse_query(text, id);
        if (typeof hunter[query[0].op] !== 'function') {
            return error('ADsafe: Bad query: ' + query[0]);
        }
        return /*: obj* 'AdObj */ (new Bunch(/*: cheat Array<HTMLElement> */ (quest(/*: cheat Array<{op: Str + Undef, name: Str + Undef, value: Str + Undef}> */ query, [/*: cheat HTMLElement */ root]))));
    }
};
