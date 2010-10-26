function Bunch_getValue() /*: ['Ad] -> 'Ad */ {
    reject_global(this);
    var a = /*: 'Ad */ [], b = this.___nodes___, 
    i = 0, 
    node /*: upcast Undef + HTMLElement */;
    for (i = 0; i < b.length; i += 1) {
        node = b[i];
        if (node.nodeName === '#text') {
            a[i] = node.nodeValue;
        } else if (node.tagName && node.type !== 'password') {
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
    reject_global(this);
    if (value === undefined) {
        return error();
    }
    var b = this.___nodes___, 
    i = 0,
    node /*: upcast Undef + HTMLElement */;

    if (value instanceof Array && b.length === value.length) {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                if (node.type !== 'password') {
                    if (typeof node.value === 'string') {
                        node.value = string_check(value[i]);
                    } else {
                        while (node.firstChild) {
                            purge_event_handlers(node); 
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
        value = String(value);
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                if (typeof node.value === 'string') {
                    node.value = string_check(value); // modified from adsafe
                } else {
                    while (node.firstChild) {
                        purge_event_handlers(node);
                        node.removeChild(node.firstChild);
                    }
                    node.appendChild(document.createTextNode(string_check(value))); // modified from adsafe
                }
            } else if (node.nodeName === '#text') {
                node.nodeValue = String(value);
            }
        }
    }
    return this;
}

function Bunch_title(value) /*: ['Ad] 'Ad -> 'Ad */ {
    reject_global(this);
    var b = this.___nodes___, 
    i = 0,
    node /*: upcast Undef + HTMLElement */;
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' + b.length +
                         '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.title = string_check(value[i]);
            }
        }
    } else {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.title = string_check(value);
            }
        }
    }
    return this;
}

function Bunch_getTitle() /*: ['Ad] -> 'Ad */ {
    reject_global(this);
    var a = /*: 'Ad */ [], 
    b = this.___nodes___, 
    i = 0;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].title;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}

function Bunch_each(func) /*: ['Ad] 'Ad -> 'Ad */ {
    reject_global(this);
    var b = this.___nodes___, i = 0;
    if (typeof func === 'function') {
        for (i = 0; i < b.length; i += 1) {
            func(/*: obj* 'AdObj */ (new Bunch([b[i]])));
        }
        return this;
    }
    return error();
}

function Bunch_append (appendage) /*: ['Ad] 'Ad -> 'Ad */ {
    reject_global(this);
    var b = this.___nodes___,
    flag = false,
    i = 0,
    j = 0,
    node /*: upcast Undef + HTMLElement */,
    rep /*: upcast Undef + Array<HTMLElement> */;
    if (b.length === 0 || !appendage) {
        return this;
    }
    if (appendage instanceof Array) {
        if (appendage.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            rep = appendage[i].___nodes___;
            for (j = 0; j < rep.length; j += 1) {
                b[i].appendChild(rep[j]);
            }
        }
    } else {
        rep = appendage.___nodes___;
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            for (j = 0; j < rep.length; j += 1) {
                node.appendChild(flag ?
                                 rep[j].cloneNode(true) : rep[j]);
            }
            flag = true;
        }
    }
    return this;
}

function Bunch_blur () /*: ['Ad] -> 'Ad */ {
    reject_global(this);
    var b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */;
    has_focus = null;
    for (i = 0; i < b.length; i += 1) {
        node = b[i];
        if (node.blur) {
            node.blur();
        }
    }
    return this;
}

function Bunch_check (value) /*: ['Ad] 'Ad -> 'Ad */ {
    reject_global(this);
    var b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */;
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.checked = !!value[i];
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
    reject_global(this);
    return this.___nodes___.length;
}

function Bunch_empty () /*: ['Ad] -> 'Ad */ {
    reject_global(this);
    var b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */;
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            while (node.firstChild) {
                purge_event_handlers(node);
                node.removeChild(node.firstChild);
            }
        }
    } else {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            while (node.firstChild) {
                purge_event_handlers(node);
                node.removeChild(node.firstChild);
            }
        }
    }
    return this;
}

function Bunch_enable (enable) /*: ['Ad] 'Ad -> 'Ad */ {
    reject_global(this);
    var b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */;
    if (enable instanceof Array) {
        if (enable.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + enable.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.disabled = !enable[i];
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
    reject_global(this);
    if (ephemeral) {
        ephemeral.remove();
    }
    ephemeral = this;
    return this;
}

function Bunch_explode () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i = 0;
    for (i = 0; i < b.length; i += 1) {
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
    var a = /*: 'Ad */ [], b = this.___nodes___, i = 0;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].checked;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getClass () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i = 0;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].className;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getMark () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i = 0;
    for (i = 0; i < b.length; i += 1) {
        a[i] = /*: cheat 'Ad */ (b[i]['_adsafe mark_']);
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getName () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i = 0;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].name;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}

function Bunch_getOffsetHeight () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i = 0;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].offsetHeight;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getOffsetWidth () /*: ['Ad] -> 'Ad */ {
    var a = /*: 'Ad */ [], b = this.___nodes___, i = 0;
    for (i = 0; i < b.length; i += 1) {
        a[i] = b[i].offsetWidth;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}
function Bunch_getParent () /*: ['Ad] -> 'Ad */ {
    var a = /*: HTMLElement */ [], b = this.___nodes___, i = 0, n /*: upcast Undef + HTMLElement */;
    for (i = 0; i < b.length; i += 1) {
        n = b[i].parentNode;
        if (/*: cheat Str */ (n['___adsafe root___'])) {
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
    var b = this.___nodes___, end = 0, node /*: upcast Undef + HTMLElement */, start = 0, range /*: upcast Undef + Range */;
    if (b.length === 1 && allow_focus) {
        node = b[0];
        if (typeof node.selectionStart === 'number') {
            start = node.selectionStart;
            end = node.selectionEnd;
            return node.value.slice(start, end);
        } else {
            range = node.createTextRange();
            range.expand('textedit');
            if (range.inRange(the_range)) {
                return the_range.text;
            }
        }
    }
    return null;
}


function Bunch_selection (string_in) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window) {
        return error();
    }
    var b = this.___nodes___, 
    end = 0, 
    node /*: upcast Undef + HTMLElement */, 
    old /*: upcast Undef + Str */, 
    start = 0, 
    range /*: upcast Undef + Range */,
    string = string_check(string_in);
    if (b.length === 1 && allow_focus) {
        node = b[0];
        if (typeof node.selectionStart === 'number') {
            start = node.selectionStart;
            end = node.selectionEnd;
            old = node.value;
            node.value = old.slice(0, start) + string + old.slice(end);
            node.selectionStart = node.selectionEnd = start +
                              string.length;
            node.focus();
        } else {
            range = node.createTextRange();
            range.expand('textedit');
            if (range.inRange(the_range)) {
                the_range.select();
                the_range.text = string;
                the_range.select();
            }
        }
    }
    return this;
}

function Bunch_style (name, value) /*: ['Ad] 'Ad * 'Ad -> 'Ad */ {
    reject_global(this);
    reject_name(name);
    if (value === undefined || /url/i.test(string_check(value))) {
        error();
    }
    var b = this.___nodes___,
    i = 0,
    node /*: upcast Undef + HTMLElement */,
    v = "";
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
	    v = String(value[i]);
            if (node.tagName) {
                if (name !== 'float') {
                    /*: cheat Str */ (node.style[name] = v);
                } else {
                    node.style.cssFloat = node.style.styleFloat = v;
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
                    node.style.cssFloat = node.style.styleFloat = v;
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
        node.name = String(name);
    }
    if (type) {
        node.type = String(type);
    }
    return /*: obj* 'AdObj */ (new Bunch([/*: cheat HTMLElement */node]));
}

function Bunch_text (text) /*: ['Ad] 'Ad -> 'Ad */ {
    var a /*: upcast Undef + Array<HTMLElement> */, i = 0;
    if (text instanceof Array) {
        a = /*: HTMLElement */ [];
        for (i = 0; i < Number(text.length); i += 1) {
            a[i] = document.createTextNode(String(text[i]));
        }
        return /*: obj* 'AdObj */ (new Bunch(a));
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
    i = 0,
    j = 0,
    n = 0,
    node /*: upcast Undef + HTMLElement */,
    on /*: upcast Undef + {#proto: Object, *: Array<'Ad>, #code: Bot} */,
    type /*: upcast 'Ad */,
    check_typ /*: upcast Any */;

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
    for (i = 0; i < n; i += 1) {
        node = b[i];
        on = /*: cheat {#proto: Object, *: Array<'Ad>, #code: Bot} */ (node['___ on ___']);

        // If an array of handlers exist for this event, then
        // loop through it and execute the handlers in order.

        if (on && on.hasOwnProperty(/*: cheat Str */ type)) {  // broken because of event.type being an object
            array = /*: cheat Array<'Ad> */ (on[type]);
            for (j = 0; j < array.length; j += 1) {

                // Invoke a handler. Pass the event object.

                /*: cheat 'Ad */ (array[j].call(this, event));
            }
        }
    }
    return this;
}

function Bunch_getStyle (name) /*: ['Ad] 'Ad -> 'Ad */ {
    reject_global(this);
    reject_name(name);
    var a = /*: Str */ [], b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */, s /*: upcast Any */;
    for (i = 0; i < b.length; i += 1) {
        node = b[i];
        if (node.tagName) {
            s = name !== 'float' ? /*: cheat Any */ (getStyleObject(node)[name]) :
		getStyleObject(node).cssFloat ||
                getStyleObject(node).styleFloat;
	    if (typeof s === 'string') {
		a[i] = s;
	    }
        }
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}

function Bunch_getTagName () /*: ['Ad] -> 'Ad */ {
    var a = /*: Undef + Str */ [], b = this.___nodes___, i = 0, name /*: upcast Undef + Str */;
    for (i = 0; i < b.length; i += 1) {
        name = b[i].tagName;
        a[i] = typeof name === 'string' ? name.toLowerCase() : name;
    }
    return a.length === 1 ? a[0] : /*: cheat 'Ad */ a;
}


function Bunch_klass (value) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window || /url/i.test(string_check(value))) {
        return error('ADsafe error.');
    }
    var b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */;
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.className = String(value[i]);
            }
        }
    } else {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                node.className = String(value);
            }
        }
    }
    return this;
}

function Bunch_mark (value) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window || /url/i.test(string_check(value))) {
        return error('ADsafe error.');
    }
    var b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */;
    if (value instanceof Array) {
        if (value.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                /*: cheat 'Ad */ (node['_adsafe mark_'] = value[i]);
            }
        }
    } else {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            if (node.tagName) {
                /*: cheat 'Ad */ (node['_adsafe mark_'] = value);
            }
        }
    }
    return this;
}

function Bunch_off (type) /*: ['Ad] 'Ad -> 'Ad */ {
    reject_global(this);
    var b = this.___nodes___, i=0, node/*: upcast Undef + HTMLElement */;
    for (i = 0; i < b.length; i += 1) {
        node = b[i];
        if (typeof type === 'string') {
            if (/*: cheat Bool */ (typeof node['___ on ___'])) {
                /*: cheat Null */ (node['___ on ___'][type] = null);
            }
        } else {
            /*: cheat Null */ (node['___ on ___'] = null);
        }
    }
    return this;
}

function Bunch_on (type_in, func) /*: ['Ad] 'Ad * 'Ad -> 'Ad */ {
    reject_global(this);
    if (typeof type_in !== 'string' || typeof func !== 'function') {
        error();
    }

    var b = this.___nodes___, i = 0, node /*: upcast Undef + HTMLElement */, on /*: upcast Undef + {#proto: Object, *: Array<'Ad>, #code: Bot} */, ontype /*: upcast Undef + Str */, type = String(type_in);
    for (i = 0; i < b.length; i += 1) {
        node = b[i];

        // The change event does not propogate, so we must put the handler on the
        // instance.

        if (type === 'change') {
            if (node.onchange !== dom_event) {
                node.onchange = dom_event;
            }
        }

        // Register an event. Put the function in a handler array, making one if it
        // doesn't yet exist for this type on this node.

        on = /*: cheat {#proto: Object, *: Array<'Ad>, #code: Bot} */ (node['___ on ___']);
        if (!on) {
            on = /*: obj* {#proto: Object, *: Array<'Ad>, #code: Bot} */ {};
            /*: cheat {#proto: Object, *: Array<'Ad>, #code: Bot} */ (node['___ on ___'] = on);
        }
        if (owns(on, type)) {
            /*: cheat Array<'Ad> */ (on[type].push(func));
        } else {
            /*: cheat Array<'Ad> */ (on[type] = [func]);
        }
    }
    return this;
}

function Bunch_protect () /*: ['Ad] -> 'Ad */ {
    if (this === this.window) {
        return error('ADsafe error.');
    }
    var b = this.___nodes___, i = 0;
    for (i = 0; i < b.length; i += 1) {
        /*: cheat Str */ (b[i]['___adsafe root___'] = '___adsafe root___');
    }
    return this;
}

function Bunch_q (text) /*: ['Ad] 'Ad -> 'Ad */ {
    star = this.___star___;
    return /*: obj* 'AdObj */ (new Bunch(
        quest(/*: cheat Array<{op: Str + Undef, name: Str + Undef, value: Str + Undef}> */ (
            parse_query(text, id)), 
            this.___nodes___)));
}
function Bunch_remove () /*: ['Ad] -> 'Ad */ {
    reject_global(this);
    this.replace();
}

function Bunch_replace (replacement) /*: ['Ad] 'Ad -> 'Ad */ {
    if (this === this.window) {
        return error();
    }
    var b = this.___nodes___,
    flag = false,
    i = 0,
    j = 0,
    newnode /*: upcast Undef + HTMLElement */,
    node /*: upcast Undef + HTMLElement */,
    parent /*: upcast Undef + HTMLElement */,
    rep /*: upcast Undef + Array<HTMLElement> */;
    if (b.length === 0) {
        return;
    }
    for (i = 0; i < b.length; i += 1) {
        purge_event_handlers(b[i]);
    }
    if (!replacement ||
        replacement.length === 0 ||
        (replacement.___nodes___ &&
	 replacement.___nodes___.length === 0)) {
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            purge_event_handlers(node);
            if (node.parentNode) {
                node.parentNode.removeChild(node);
            }
        }
    } else if (replacement instanceof Array) {
        if (replacement.length !== b.length) {
            return error('ADsafe: Array length: ' +
                         b.length + '-' + value.length);
        }
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            parent = node.parentNode;
            purge_event_handlers(node);
            if (parent) {
                rep = replacement[i].___nodes___;
                if (rep.length > 0) {
                    newnode = rep[0];
                    parent.replaceNode(newnode);
                    for (j = 1; j < rep.length; j += 1) {
                        node = newnode;
                        newnode = rep[j];
                        parent.insertBefore(/*: cheat HTMLElement */ newnode, /*: cheat HTMLElement */ (node.nextSibling));
                    }
                } else {
                    parent.removeChild(node);
                }
            }
        }
    } else {
        rep = replacement.___nodes___;
        for (i = 0; i < b.length; i += 1) {
            node = b[i];
            purge_event_handlers(node);
            if (node.parentNode) {
                newnode = flag ? rep[0].cloneNode(true) : rep[0];
                parent.replaceNode(newnode);
                for (j = 1; j < rep.length; j += 1) {
                    node = newnode;
                    newnode = flag ? rep[j].clone(true) : rep[j];
                    parent.insertBefore(/*: cheat HTMLElement */ newnode, /*: cheat HTMLElement */ (node.nextSibling));
                }
                flag = true;
            }
        }
    }
    return this;
}
function Bunch_select () /*: ['Ad] -> 'Ad */ {
    if (this === this.window) {
        return error();
    }
    var b = this.___nodes___;
    if (b.length !== 1 || !allow_focus) {
        return error();
    }
    b[0].focus();
    b[0].select();
    return this;
}
