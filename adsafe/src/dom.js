var root /*: upcast HTMLElement + Undef */;
var dom /*: upcast 'Ad */;
var dom_outer = /*: obj* 'AdObj */ {
    q: /*:upcast 'Ad */ (/*: obj* 'AdObj */ (function (text) /*: ['Ad + HTMLWindow] 'Ad * 'Ad ... -> 'Ad */ {
        star = false;
        var query = parse_query(text, id);
        if (typeof hunter[query[0].op] !== 'function') {
            return error('ADsafe: Bad query: ' + query[0]);
        }
        return /*: obj* 'AdObj */ (new Bunch(quest(query, [root])));
    })),
    combine: /*:upcast 'Ad */ (/*: obj* 'AdObj */ (function (array) /*: ['Ad + HTMLWindow] 'Ad * 'Ad ... -> 'Ad */ {
        if (!array || !array.length) {
            return error('ADsafe: Bad combination.');
        }
        var b = array[0].___nodes___, i = 0;
        for (i = 0; i < array.length; i += 1) {
            b = b.concat(array[i].___nodes___);
        }
        return /*: obj* 'AdObj */ (new Bunch(b));
    })),
    count: /*: upcast 'Ad */ (/*: obj* 'AdObj */ (function () /*: ['Ad + HTMLWindow] 'Ad ... -> 'Ad */ {
        return 1;
    })),
    ephemeral: /*: upcast 'Ad */ (/*: obj* 'AdObj */ (function (bunch) /*: ['Ad + HTMLWindow] 'Ad * 'Ad ... -> 'Ad */ {
        if (ephemeral) {
            ephemeral.remove();
        }
        ephemeral = bunch;
        return dom;
    })),
    fragment: /*:upcast 'Ad */ (/*: obj* 'AdObj */ (function () /*: ['Ad + HTMLWindow] 'Ad ... -> 'Ad */ {
        return /*: obj* 'AdObj */ (new Bunch([/*: upcast HTMLElement + Undef */ (document.createDocumentFragment())]));
    })),
    remove: /*: upcast 'Ad */ (/*: obj* 'AdObj */ (function () /*: ['Ad + HTMLWindow] 'Ad ... -> 'Ad */ {
        purge_event_handlers(root);
        root.parent.removeElement(root);
        root = undefined;
    })),
    tag: /*: upcast 'Ad */ 
    (/*: obj* 'AdObj */ 
        (function (tag, type, name) 
         /*: ['Ad + HTMLWindow] 'Ad * 'Ad * 'Ad * 'Ad ... -> 'Ad */ 
         {
             var node /*: upcast Undef + HTMLElement */;
             if (typeof tag !== 'string') {
                 return error();
             }
             if (makeableTagName[tag] !== true) {
                 return error('ADsafe: Bad tag: ' + tag);
             }
             node = document.createElement(makeableTagName[safe_name(tag)]);
             if (name) {
                 node.autocomplete = 'off';
                 node.name = string_check(name);
             }
             if (type) {
                 node.type = string_check(type);
             }
             return /*: obj* 'AdObj */ (new Bunch([/*: upcast HTMLElement + Undef */ node]));
         })),
    text: /*: upcast 'Ad */ 
    (/*: obj* 'AdObj */ 
        (function (text) 
         /*: ['Ad + HTMLWindow] 'Ad * 'Ad ... -> 'Ad */ {
             var a /*: upcast Undef + Array<HTMLElement + Undef> */, i = 0;
             if (text instanceof Array) {
                 a = /*: HTMLElement + Undef */ [];
                 for (i = 0; i < text.length; i += 1) {
                     a[i] = document.createTextNode(String(text[i]));
                 }
                 return /*: obj* 'AdObj */ (new Bunch(a));
             }
             return /*: obj* 'AdObj */ (new Bunch(
                 [/*: upcast HTMLElement + Undef */ 
                     (document.createTextNode(String(text)))]));
         })),
    append: /*: upcast 'Ad */ 
    (/*: obj* 'AdObj */ 
        (function (bunch) /*: ['Ad + HTMLWindow] 'Ad * 'Ad ... -> 'Ad */ {
            var b = bunch.___nodes___, i = 0, n /*: upcast Undef + HTMLElement */;
            for (i = 0; i < b.length; i += 1) {
                n = b[i];
                // Is this even possible?
                if (typeof n === 'string' || typeof n === 'number') {
                    n = document.createTextNode(String(n));
                }
                root.appendChild(n);
            }
            return dom;
        })),
    prepend: /*: upcast 'Ad */ 
    (/*: obj* 'AdObj */ 
        (function (bunch) 
         /*: ['Ad + HTMLWindow] 'Ad * 'Ad ... -> 'Ad */ {
             var b = bunch.___nodes___, i = 0;
             for (i = 0; i < b.length; i += 1) {
                 root.insertBefore(b[i], root.firstChild);
             }
             return dom;
         })),
    row: /*: upcast 'Ad */ 
    (/*: obj* 'AdObj */ 
        (function (values) 
         /*: ['Ad + HTMLWindow] 'Ad * 'Ad ... -> 'Ad */ {
             var tr = document.createElement('tr'),
             td /*: upcast Undef + HTMLElement */,
             i = 0;
             for (i = 0; i < values.length; i += 1) {
                 td = document.createElement('td');
                 td.appendChild(document.createTextNode(String(values[i])));
                 tr.appendChild(td);
             }
             return /*: obj* 'AdObj */ (new Bunch([/*: upcast HTMLElement + Undef */ tr]));
         }))
};
