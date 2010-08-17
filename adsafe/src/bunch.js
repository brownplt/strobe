function Bunch_getValue() /*: [Ad] -> Ad */ {
    var a = /*: Ad */ [], b = this.___nodes___, 
    i = 0, 
    node = /*: upcast Undef + ASNode */ undefined;
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
    return a.length === 1 ? a[0] : /*:cheat Ad */ a; // Need Array<Ad> <: Ad
}

function Bunch_value(value) /*: [Ad] Ad -> Ad */ {
    if (this === this.window || value === undefined) {
        return error();
    }
    var b = this.___nodes___, 
    i = 0, // Joe set this to 0 --- flow analysis doesn't like upcast Undef + Int
    node = /*: upcast Undef + ASNode */ undefined; // added undefined and upcast

    if (value instanceof Array && b.length === value.length) {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (/*: cheat Bool */ node.tagName) {
                if (node.type !== 'password') {
                    if (typeof node.value === 'string') {
                        node.value = (/*: cheat Array<Ad> */ value)[i];
                    } else {
                        while (node.firstChild) {
                            // thinks node is undef + ASNode, should just be ASNode
                            purge_event_handlers(/*: cheat ASNode */ node); 
                            node.removeChild(node.firstChild);
                        }
                        node.appendChild(document.createTextNode(
                            String((/*: cheat Array<Ad> */value)[i])));
                    }
                }
            } else if (node.nodeName === '#text') {
                node.nodeValue = String((/*: cheat Array<Ad> */ value)[i]);
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

function Bunch_title(value) /*: [Ad] Ad -> Ad */ {
    if (this === this.window) {
        return error('ADsafe error.');
    }
    var b = this.___nodes___, 
    i = 0, // added by Joe
    node = /*: upcast Undef + ASNode */ undefined;
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' + b.length +
                         '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (/*: cheat Bool */ node.tagName) {
                node.title = String((/*: cheat Array<Ad> */ value)[i]);
            }
        }
    } else {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (/*: cheat Bool */ node.tagName) {
                node.title = String(value);
            }
        }
    }
    return this;
}

function Bunch_getTitle() /*: [Ad] -> Ad */ {
    var a = /*: Ad */ [], 
    b = this.___nodes___, 
    i = 0; // Added by Joe
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].title;
    }
    return a.length === 1 ? a[0] : /*: cheat Ad */ a;
}

function Bunch_on(type, func) /*: [Ad] Ad * Ad -> Ad */ {
    if (this === this.window || typeof type !== 'string' ||
        typeof func !== 'function') {
        return error();
    }

    var b = this.___nodes___, 
    i = 0,// Added by Joe
    node /*: upcast Undef + ASNode */, 
    on /*: upcast Undef + */, ontype;
    for (i = 0; i < b.length; i += 1) {
        node = b[i];

        // The change event does not propogate, so we must put the handler on the
        // instance.

        if (type === 'change') {
            ontype = 'on' + type;  // WTF?!  I edit this to not be foolish
            if (node['onchange'] !== dom_event) {
                node['onchange'] = dom_event;
            }
        }

        // Register an event. Put the function in a handler array, making one if it
        // doesn't yet exist for this type on this node.

        on = node['___ on ___'];
        if (!on) {
            on = {};
            node['___ on ___'] = on;
        }
        if (on.hasOwnProperty(type)) {
            on[type].push(func);
        } else {
            on[type] = [func];
        }
    }
    return this;
}