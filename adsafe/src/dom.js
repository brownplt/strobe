var root /*: upcast Undef + Null + HTMLElement */;

var dom = {
    q: function (text) /*: ['Ad] 'Ad -> 'Ad */ {
        star = false;
        var query = parse_query(text, id);
        if (typeof hunter[query[0].op] !== 'function') {
            return error('ADsafe: Bad query: ' + query[0]);
        }
        return /*: obj* 'AdObj */ (new Bunch(/*: cheat Array<HTMLElement> */ (quest(/*: cheat Array<{op: Str + Undef, name: Str + Undef, value: Str + Undef}> */ query, [/*: cheat HTMLElement */ root]))));
    },
    combine: function (array) /*: ['Ad] 'Ad -> 'Ad */ {
        if (!array || !array.length) {
            return error('ADsafe: Bad combination.');
        }
        var b = array[0].___nodes___, i /*: upcast Undef + Int */;
        for (i = i; /*: cheat Bool */ (i < array.length); /*: cheat Int */ (i += 1)) {
            b = /*: cheat Array<HTMLElement> */ (b.concat(array[i].___nodes___));
        }
        return /*: obj* 'AdObj */ (new Bunch(/*: cheat Array<HTMLElement> */b));
    },
    count: function () /*: ['Ad] -> 'Ad */ {
        return 1;
    },
    ephemeral: function (bunch) /*: ['Ad] 'Ad -> 'Ad */ {
        if (ephemeral) {
            ephemeral.remove();
        }
        ephemeral = bunch;
        return /*: cheat 'Ad */ dom;
    },
    fragment: function () /*: ['Ad] -> 'Ad */ {
        return /*: obj* 'AdObj */ (new Bunch([document.createDocumentFragment()]));
    },
    remove: function () /*: ['Ad] -> 'Ad */ {
        purge_event_handlers(/*: cheat HTMLElement */ root);
        root.parent.removeElement(/*: cheat HTMLElement */ root);
        root = null;
    },
    tag: function (tag, type, name) /*: ['Ad] 'Ad * 'Ad * 'Ad -> 'Ad */ {
        var node /*: upcast Undef + HTMLElement */;
        if (typeof tag !== 'string') {
            return error();
        }
        if (makeableTagName[tag] !== true) {
            return error('ADsafe: Bad tag: ' + tag);
        }
        node = document.createElement(tag);
        if (name) {
            node.autocomplete = 'off';
            /*: cheat 'Ad */ (node.name = name);
        }
        if (type) {
            /*: cheat 'Ad */ (node.type = type);
        }
        return /*: obj* 'AdObj */ (new Bunch([/*: cheat HTMLElement */node]));
    },
    text: function (text) /*: ['Ad] 'Ad -> 'Ad */ {
        var a /*: upcast Undef + Array<HTMLElement> */, i /*: upcast Undef + Int */;
        if (text instanceof Array) {
            a = /*: HTMLElement */ [];
            for (i = 0; /*: cheat Bool */ (i < text.length); /*: cheat Int */ (i += 1)) {
                a[i] = document.createTextNode(String(text[i]));
            }
            return /*: obj* 'AdObj */ (new Bunch(/*: cheat Array<HTMLElement> */ a));
        }
        return /*: obj* 'AdObj */ (new Bunch([document.createTextNode(String(text))]));
    },
    append: function (bunch) /*: ['Ad] 'Ad -> 'Ad */ {
        var b = bunch.___nodes___, i /*: upcast Undef + Int */, n /*: upcast Undef + HTMLElement */;
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            n = b[i];
// Is this even possible?
//            if (typeof n === 'string' || typeof n === 'number') {
//                n = document.createTextNode(String(n));
//            }
            root.appendChild(n);
        }
        return /*: cheat 'Ad */ dom;
    },
    prepend: function (bunch) /*: ['Ad] 'Ad -> 'Ad */ {
        var b = bunch.___nodes___, i /*: upcast Undef + Int */;
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            root.insertBefore(b[i], /*: cheat HTMLElement */ (root.firstChild));
        }
        return /*: cheat 'Ad */ dom;
    },
    row: function (values) /*: ['Ad] 'Ad -> 'Ad */ {
        var tr = document.createElement('tr'),
        td /*: upcast Undef + HTMLElement */,
        i /*: upcast Undef + Int */;
        for (i = 0; /*: cheat Bool */ (i < values.length); /*: cheat Int */ (i += 1)) {
            td = document.createElement('td');
            td.appendChild(document.createTextNode(String(values[/*: cheat Int */ i])));
            tr.appendChild(td);
        }
        return /*: obj* 'AdObj */ (new Bunch([tr]));
    }
};
