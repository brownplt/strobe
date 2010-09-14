var star = false;

function Bunch(nodes) 
/*: constructor (Array<HTMLElement> -> {___nodes___: Array<HTMLElement>, ___star___: Bool}) */ 
{
    this.___nodes___ = nodes;
    this.___star___ = star && nodes.length > 1;
    star = false;
}

function Bunch_getValue() /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, 
    i /*: upcast Undef + Int */, 
    node /*: upcast Undef + HTMLElement */;
    for (i = 0; i < b.length; i += 1) {
        node = b[i];
        if (node.nodeName === '#text') {
            a[i] = node.nodeValue;
        } else if ((/*: cheat Bool */ node.tagName) && node.type !== 'password') {
            a[i] = node.value;
            if (a[i] === undefined && node.firstChild &&
                node.firstChild.nodeName === '#text') {
                a[i] = node.firstChild.nodeValue;
            }
        }
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a; // Need Array<'Ad> <: 'Ad
}

function Bunch_value(value) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window || value === undefined) {
        return error();
    }
    var b = this.___nodes___, 
    i /*: upcast Undef + Int */,
    node /*: upcast Undef + HTMLElement */;

    if (value instanceof Array && b.length === /*: cheat Int */ value.length) {
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            node = b[i];
            if (/*: cheat Bool */ node.tagName) {
                if (node.type !== 'password') {
                    if (typeof node.value === 'string') {
                        node.value = /*: cheat Undef */ (value[i]);
                    } else {
                        while (node.firstChild) {
                            // thinks node is undef + HTMLElement, should just be HTMLElement
                            purge_event_handlers(/*: cheat HTMLElement */ node); 
                            node.removeChild(node.firstChild);
                        }
                        node.appendChild(document.createTextNode(
                            String(/*: cheat 'Ad */ (value[i]))));
                    }
                }
            } else if (node.nodeName === '#text') {
                node.nodeValue = String(/*: cheat 'Ad */ (value[i]));
            }
        }
    } else {
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            node = b[i];
            if (/*: cheat Bool */ node.tagName) {
                if (typeof node.value === 'string') {
                    node.value = /*: cheat Undef */ value;
                } else {
                    while (node.firstChild) {
                        purge_event_handlers(/*: cheat HTMLElement */ node);
                        node.removeChild(node.firstChild);
                    }
                    node.appendChild(document.createTextNode(
                        String(value)));
                }
            } else if (node.nodeName === '#text') {
                node.nodeValue = String(value);
            }
        }
    }
    return this;
}

function Bunch_title(value) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window) {
        return error('ADsafe error.');
    }
    var b = this.___nodes___, 
    i /*: upcast Undef + Int */,
    node /*: upcast Undef + HTMLElement */;
    if (value instanceof Array) {
        if (/*: cheat Int */ (value.length) !== b.length) {
            return error('ADsafe: Array length: ' + b.length +
                         '-' /*+ value.length*/);
        }
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            node = b[i];
            if (/*: cheat Bool */ (node.tagName)) {
                node.title = String(/*: cheat 'Ad */ (value[i]));
            }
        }
    } else {
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            node = b[i];
            if (/*: cheat Bool */ (node.tagName)) {
                node.title = String(value);
            }
        }
    }
    return this;
}

function Bunch_getTitle() /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], 
    b = this.___nodes___, 
    i /*: upcast Undef + Int */;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].title;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}

function Bunch_each(func) /*: ['Ad] 'Ad -> 'Ad */ {
    var b = this.___nodes___, i /*: upcast Undef + Int */;
    if (this !== this.window && typeof func === 'function') {
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            /*: cheat 'Ad -> 'Ad */ func(/*: cheat 'Ad */ (new Bunch([b[i]])));
        }
        return this;
    }
    return error();
}

function Bunch_append (appendage) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window) {
        return error();
    }
    var b = this.___nodes___,
    flag = false,
    i /*: upcast Undef + Int */,
    j /*: upcast Undef + Int */,
    node /*: upcast Undef + HTMLElement */,
    rep /*: upcast Undef + Array<HTMLElement> */;
    if (b.length === 0 || /*: cheat Bool */ (!appendage)) {
        return this;
    }
    if (appendage instanceof Array) {
        if (/*: cheat Int */ (appendage.length) !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' /*+ value.length*/);
        }
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            rep = /*: cheat 'Ad */ (appendage[i]).___nodes___;
            for (j = 0; j < rep.length; /*: cheat Int */ (j += 1)) {
                b[/*: cheat Int */i].appendChild(rep[j]);
            }
        }
    } else {
        rep = appendage.___nodes___;
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            node = b[i];
            for (j = 0; j < rep.length; /*: cheat Int */ (j += 1)) {
                node.appendChild(flag ?
                                 rep[j].cloneNode(true) : rep[j]);
            }
            flag = true;
        }
    }
    return this;
}

function Bunch_blur () /*: ['Ad] -> 'Ad */ {
    if (this === this.window) {
        return error('ADsafe error.');
    }
    var b = this.___nodes___, i /*: upcast Int + Undef */, node /*: upcast Undef + HTMLElement */;
    has_focus = null;
    for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
        node = b[i];
        if (node.blur) {
            /*:cheat -> 'Ad */ (node.blur)();
        }
    }
    return this;
}

function Bunch_check (value) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window) {
        return error();
    }
    var b = this.___nodes___, i /*: upcast Int + Undef */, node /*: upcast Undef + HTMLElement */;
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + /*: cheat Int */ (value.length));
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.checked = !!/*: cheat 'Ad */(value[i]);
            }
        }
    } else {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.checked = !!value;
            }
        }
    }
    return this;
}

function Bunch_count () /*: ['Ad] -> Int */ {
    return this.___nodes___.length;
}

