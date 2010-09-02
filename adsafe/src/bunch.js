var star = false;

function Bunch(nodes) 
/*: constructor (Array<ASNode> -> {___nodes___: Array<ASNode>, ___star___: Bool}) */ 
{
    this.___nodes___ = nodes;
    this.___star___ = star && nodes.length > 1;
    star = false;
}

function Bunch_getValue() /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, 
    i /*: upcast Undef + Int */, 
    node /*: upcast Undef + ASNode */;
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
    node /*: upcast Undef + ASNode */;

    if (value instanceof Array && b.length === /*: cheat Int */ value.length) {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                if (node.type !== 'password') {
                    if (typeof node.value === 'string') {
                        node.value = value[i];
                    } else {
                        while (node.firstChild) {
                            // thinks node is undef + ASNode, should just be ASNode
                            purge_event_handlers(/*: cheat ASNode */ node); 
                            node.removeChild(node.firstChild);
                        }
                        node.appendChild(document.createTextNode(
                            String(value[i])));
                    }
                }
            } else if (node.nodeName === '#text') {
                node.nodeValue = String(value[i]);
            }
        }
    } else {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (/*: cheat Bool */ node.tagName) {
                if (typeof node.value === 'string') {
                    node.value = value;
                } else {
                    while (node.firstChild) {
                        purge_event_handlers(/*: cheat ASNode */ node);
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
    node /*: upcast Undef + ASNode */;
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' + b.length +
                         '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.title = String(value[i]);
            }
        }
    } else {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
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
    return a.length === 1 ? a[0] : a;
}

function Bunch_each(func) /*: ['Ad] 'Ad -> 'Ad */ {
    var b = this.___nodes___, i /*: upcast Undef + Int */;
    if (this !== this.window && typeof func === 'function') {
        for (i = 0; i < b.length; i += 1) {
            func(new Bunch([b[i]]));
        }
        return this;
    }
    return error();
}
