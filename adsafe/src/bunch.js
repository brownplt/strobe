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

