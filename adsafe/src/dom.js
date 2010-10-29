var root /*: upcast HTMLElement + Undef */;
var dom /*: upcast 'Ad */;
var dom_outer = {
    q: function (text) /*: ['Ad] 'Ad -> 'Ad */ {
        star = false;
        var query = parse_query(text, id);
        if (typeof hunter[query[0].op] !== 'function') {
            return error('ADsafe: Bad query: ' + query[0]);
        }
        return /*: obj* 'AdObj */ (new Bunch(quest(query, [root])));
    },
    combine: function (array) /*: ['Ad] 'Ad -> 'Ad */ {
        if (!array || !array.length) {
            return error('ADsafe: Bad combination.');
        }
        var b = array[0].___nodes___, i = 0;
        for (i = 0; /*: cheat Bool */ (i < array.length); i += 1) {
            b = /*: cheat Array<HTMLElement + Undef> */ (b.concat(array[i].___nodes___));
        }
        return /*: obj* 'AdObj */ (new Bunch(b));
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
        return /*: obj* 'AdObj */ (new Bunch([/*: upcast HTMLElement + Undef */ (document.createDocumentFragment())]));
    },
    remove: function () /*: ['Ad] -> 'Ad */ {
        purge_event_handlers(/*: cheat HTMLElement */ root);
        root.parent.removeElement(/*: cheat HTMLElement */ root);
        /*: cheat Undef */ (root = null); // TODO fix this
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
            node.name = string_check(name);
        }
        if (type) {
            node.type = string_check(type);
        }
        return /*: obj* 'AdObj */ (new Bunch([/*: upcast HTMLElement + Undef */ node]));
    },
    text: function (text) /*: ['Ad] 'Ad -> 'Ad */ {
        var a /*: upcast Undef + Array<HTMLElement + Undef> */, i = 0;
        if (text instanceof Array) {
            a = /*: HTMLElement + Undef */ [];
            for (i = 0; /*: cheat Bool */ (i < text.length); i += 1) {
                a[i] = document.createTextNode(String(text[i]));
            }
            return /*: obj* 'AdObj */ (new Bunch(a));
        }
        return /*: obj* 'AdObj */ (new Bunch([/*: upcast HTMLElement + Undef */ (document.createTextNode(String(text)))]));
    },
    append: function (bunch) /*: ['Ad] 'Ad -> 'Ad */ {
        var b = bunch.___nodes___, i = 0, n /*: upcast Undef + HTMLElement */;
        for (i = 0; i < b.length; i += 1) {
            n = b[i];
// Is this even possible?
            if (typeof n === 'string' || typeof n === 'number') {
                n = document.createTextNode(String(n));
            }
            root.appendChild(n);
        }
        return /*: cheat 'Ad */ dom;
    },
    prepend: function (bunch) /*: ['Ad] 'Ad -> 'Ad */ {
        var b = bunch.___nodes___, i = 0;
        for (i = 0; i < b.length; i += 1) {
            root.insertBefore(b[i], root.firstChild);
        }
        return /*: cheat 'Ad */ dom;
    },
    row: function (values) /*: ['Ad] 'Ad -> 'Ad */ {
        var tr = document.createElement('tr'),
        td /*: upcast Undef + HTMLElement */,
        i = 0;
        for (i = 0; /*: cheat Bool */ (i < values.length); i += 1) {
            td = document.createElement('td');
            td.appendChild(document.createTextNode(String(values[i])));
            tr.appendChild(td);
        }
        return /*: obj* 'AdObj */ (new Bunch([/*: upcast HTMLElement + Undef */ tr]));
    }
};
