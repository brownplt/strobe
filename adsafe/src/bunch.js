var star = false;

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
            node.blur();
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
        if ((/*: cheat Int */ value.length) !== b.length) {
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

function Bunch_empty () /*: ['Ad] -> 'Ad */ {
    if (this === this.window) {
        return error('ADsafe error.');
    }
    var b = this.___nodes___, i /*: upcast Undef + Int */, node /*: upcast Undef + HTMLElement */;
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + value.length);
        }
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            node = b[i];
            while (node.firstChild) {
                purge_event_handlers(/*: cheat HTMLElement */ node);
                node.removeChild(node.firstChild);
            }
        }
    } else {
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            node = b[i];
            while (node.firstChild) {
                purge_event_handlers(/*: cheat HTMLElement */ node);
                node.removeChild(node.firstChild);
            }
        }
    }
    return this;
}

function Bunch_enable (enable) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window) {
        return error('ADsafe error.');
    }
    var b = this.___nodes___, i /*: upcast Undef + Int */, node /*: upcast Undef + HTMLElement */;
    if (enable instanceof Array) {
        if (/*:cheat 'Ad */ (enable.length) !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + /*: cheat Int */ (enable.length));
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.disabled = !/*: cheat 'Ad */ (enable[i]);
            }
        }
    } else {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.disabled = !enable;
            }
        }
    }
    return this;
}

function Bunch_ephemeral () /*: ['Ad] -> 'Ad */ {
    if (this === this.window) {
        return error('ADsafe error.');
    }
    if (ephemeral) {
        ephemeral.remove();
    }
    ephemeral = this;
    return this;
}

function Bunch_explode () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i /*: upcast Undef + Int */;
    for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
        a[i] = /*: obj* 'AdObj */ (new Bunch([b[i]]));
    }
    return /*: cheat 'Ad */ a;
}

function Bunch_focus () /*: ['Ad] -> 'Ad */ {
    var b = this.___nodes___;
    if (this !== this.window) {
        if (b.length === 1 && allow_focus) {
            has_focus = b[0].focus();
            return this;
        }
    }
    return error();
}

function Bunch_fragment () /*: ['Ad] -> 'Ad */ {
    return /*: obj* 'AdObj */ (new Bunch([document.createDocumentFragment()]));
}

function Bunch_getCheck () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i /*: upcast Undef + Int */;
    for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
        a[i] = b[i].checked;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getClass () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i /*: upcast Undef + Int */;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].className;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getMark () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i /*: upcast Undef + Int */;
    for (i = 0; i < b.length; i += 1) {
        a[i] = /*: cheat 'Ad */ (b[i]['_adsafe mark_']);
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getName () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i /*: upcast Undef + Int */;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].name;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}

function Bunch_getOffsetHeight () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i /*: upcast Undef + Int */;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].offsetHeight;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getOffsetWidth () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i /*: upcast Undef + Int */;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].offsetWidth;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getParent () /*: ['Ad] -> 'Ad */ {
    var a = /*: HTMLElement */ [], b = this.___nodes___, i /*: upcast Undef + Int */, n /*: upcast Undef + HTMLElement */;
    for (i = 0; i < b.length; i += 1) {
        n = b[i].parentNode;
        if (/*: cheat Bool */ (n['___adsafe root___'])) {
            return error('ADsafe parent violation.');
        }
        a[i] = /*: cheat HTMLElement */ n;
    }
    return /*: obj* 'AdObj */ (new Bunch(a));
}

function Bunch_getSelection () /*: ['Ad] -> 'Ad */ {
    if (this === this.window) {
        return error();
    }
    var b = this.___nodes___, end /*: upcast Undef + Int */, node /*: upcast Undef + HTMLElement */, start /*: upcast Undef + Int */, range /*: upcast Undef + Range */;
    if (b.length === 1 && allow_focus) {
        node = b[0];
        if (typeof node.selectionStart === 'number') {
            start = node.selectionStart;
            end = node.selectionEnd;
            return node.value.slice(/*: cheat Int */ start, /*: cheat Int */ end);
        } else {
            range = node.createTextRange();
            range.expand('textedit');
            if (range.inRange(/*: cheat Range */ the_range)) {
                return the_range.text;
            }
        }
    }
    return null;
}


function Bunch_selection (string) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window) {
        return error();
    }
    var b = this.___nodes___, 
    end /*: upcast Undef + Int */, 
    node /*: upcast Undef + HTMLElement */, 
    old /*: upcast Undef + Str */, 
    start /*: upcast Undef + Int */, 
    range /*: upcast Undef + Range */;
    if (b.length === 1 && allow_focus) {
        node = b[0];
        if (typeof node.selectionStart === 'number') {
            start = node.selectionStart;
            end = node.selectionEnd;
            old = node.value;
            node.value = old.slice(0, start) + string + /*: cheat Str */ (old.slice(end));
            /*: cheat 'Ad */ (node.selectionStart = node.selectionEnd = start +
                              string.length);
            node.focus();
        } else {
            range = node.createTextRange();
            range.expand('textedit');
            if (range.inRange(/*: cheat Range */ the_range)) {
                the_range.select();
                /*: cheat 'Ad */ (the_range.text = string);
                the_range.select();
            }
        }
    }
    return this;
}

function Bunch_style (name, value) /*: ['Ad] 'Ad * 'Ad -> 'Ad */ {
    if (this === this.window ||
        value === undefined || /*: cheat Bool */ (/url/i.test(value))) {
        return error();
    }
    var b = this.___nodes___,
    i /*: upcast Undef + Int */,
    node /*: upcast Undef + HTMLElement */,
    v /*: upcast Undef + Str */;
    if (value instanceof Array) {
        if (/*: cheat Bool */ (value.length !== b.length)) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + /*: cheat Str */ (value.length));
        }
        for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
            node = b[i];
	    v = String(/*: cheat 'Ad */ (value[i]));
            if (node.tagName) {
                if (name !== 'float') {
                    /*: cheat Str */ (node.style[name] = v);
                } else {
                    node.style.cssFloat = node.style.styleFloat = /*: cheat Str */ v;
                }
            }
        }
    } else {
	v = String(value);
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                if (name !== 'float') {
                    /*: cheat Str */ (node.style[name] = v);
                } else {
                    node.style.cssFloat = node.style.styleFloat = /*: cheat Str */v;
                }
            }
        }
    }
    return this;
}

function Bunch_tag (tag, type, name) /*: ['Ad] 'Ad * 'Ad * 'Ad-> 'Ad */ {
    var node /*: upcast Undef + HTMLElement */ ;
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
    return /*: obj* 'AdObj */ (new Bunch([/*: cheat HTMLElement */ node]));
}

function Bunch_text (text) /*: ['Ad] 'Ad -> 'Ad */ {
    var a /*: upcast Undef + Array<HTMLElement> */, i /*: upcast Undef + Int */;
    if (text instanceof Array) {
        a = /*: HTMLElement */ [];
        for (i = 0; /*: cheat Bool */ (i < text.length); /*: cheat Int */ (i += 1)) {
            a[i] = document.createTextNode(String(/*: cheat 'Ad */ (text[i])));
        }
        return /*: obj* 'AdObj */ (new Bunch(/*: cheat Array<HTMLElement> */ a));
    }
    return /*: obj* 'AdObj */ (new Bunch([document.createTextNode(String(text))]));
}

function Bunch_fire (event) /*: ['Ad] 'Ad -> 'Ad */ {

    // Fire an event on an object. The event can be either
    // a string containing the name of the event, or an
    // object containing a type property containing the
    // name of the event. Handlers registered by the 'on'
    // method that match the event name will be invoked.

    var array /*: upcast Undef + Array<'Ad> */,
    b /*: upcast Undef + Array<HTMLElement> */,
    i /*: upcast Undef + Int */,
    j /*: upcast Undef + Int */,
    n /*: upcast Undef + Int */,
    node /*: upcast Undef + HTMLElement */,
    on /*: upcast Undef + {#proto: Object, *: Array<'Ad>, #code: Bot} */,
    type /*: upcast 'Ad */;

    if (this === this.window) {
        return error();
    }
    if (typeof event === 'string') {
        type = event;
        event = /*: obj* 'AdObj */ {type: /*: upcast 'Ad */ type};
    } else if (typeof event === 'object') {
        type = event.type;
    } else {
        return error();
    }
    b = this.___nodes___;
    n = b.length;
    for (i = 0; /*: cheat Bool */ (i < n); /*: cheat Int */ (i += 1)) {
        node = b[i];
        on = /*: cheat {#proto: Object, *: Array<'Ad>, #code: Bot} */ (node['___ on ___']);

        // If an array of handlers exist for this event, then
        // loop through it and execute the handlers in order.

        if (on && on.hasOwnProperty(type)) {
            array = /*: cheat Array<'Ad> */ (on[type]);
            for (j = 0; j < array.length; /*: cheat Int */ (j += 1)) {

                // Invoke a handler. Pass the event object.

                /*: cheat 'Ad */ (array[j].call(this, event));
            }
        }
    }
    return this;
}

function Bunch_getStyle (name) /*: ['Ad] 'Ad -> 'Ad */ {
    var a = /*: Str */ [], b = this.___nodes___, i /*: upcast Undef + Int */, node /*: upcast Undef + HTMLElement */, s /*: upcast Any */;
    for (i = 0; i < b.length; /*: cheat Int */ (i += 1)) {
        node = b[i];
        if (node.tagName) {
            s = name !== 'float' ? /*: cheat Any */ (getStyleObject(node)[name]) :
		getStyleObject(/*: cheat HTMLElement */ node).cssFloat ||
                getStyleObject(/*: cheat HTMLElement */ node).styleFloat;
	    if (typeof s === 'string') {
		a[/*: cheat Int */i] = s;
	    }
        }
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
