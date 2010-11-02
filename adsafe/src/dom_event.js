function Bunch(nodes) 
/*: constructor (Undef + Array<HTMLElement + Undef> -> {___nodes___: Array<HTMLElement + Undef> + Undef, ___star___: Bool + Undef}) */ 
{
    this.___nodes___ = nodes;
    this.___star___ = star && nodes.length > 1;
    star = false;
}

var allow_focus = true,
ephemeral /*: upcast 'Ad */,
the_range /*: upcast Undef + Range */,
id /*: upcast Undef + Str */;

function dom_event (e) /*: Event -> Undef */ {
    var key /*: upcast Undef + Str */,
    target /*: upcast 'Ad */,
    that /*: upcast 'Ad */,
    the_event /*: upcast 'Ad */,
    the_target /*: upcast HTMLElement + Undef */,
    the_actual_event = e || event,
    type = the_actual_event.type;

    // Get the target node and wrap it in a bunch.

    the_target = the_actual_event.target ||
        the_actual_event.srcElement;
    target = /*: obj* 'AdObj */ (new Bunch([the_target]));
    that = target;

    // Use the PPK hack to make focus bubbly on IE.
    // When a widget has focus, it can use the focus method.

    switch (type) {
    case 'mousedown':
        allow_focus = true;
        if (document.selection) {
            the_range = document.selection.createRange();
        }
        break;
    case 'focus':
    case 'focusin':
        allow_focus = true;
        has_focus = the_target;
        the_actual_event.cancelBubble = false;
        type = 'focus';
        break;
    case 'blur':
    case 'focusout':
        allow_focus = false;
        has_focus = null;
        type = 'blur';
        break;
    case 'keypress':
        allow_focus = true;
        has_focus = the_target;
        key = String.fromCharCode(the_actual_event.charCode ||
                                  the_actual_event.keyCode);
        switch (key) {
        case '\u000d':
        case '\u000a':
            type = 'enterkey';
            break;
        case '\u001b':
            type = 'escapekey';
            break;
        }
        break;

        // This is a workaround for Safari.

    case 'click':
        allow_focus = true;
    }
    if (the_actual_event.cancelBubble &&
        the_actual_event.stopPropagation) {
        the_actual_event.stopPropagation();
    }

    // Make the event object.

    the_event = 
        /*: obj* 'AdObj */
    {
        altKey: /*: upcast 'Ad */ (the_actual_event.altKey),
        ctrlKey: /*: upcast 'Ad */ (the_actual_event.ctrlKey),
        // TODO, gave bubble a blank argument
        bubble: /*: upcast 'Ad */ (/*: obj* 'AdObj */ (function () /*: ['Ad + HTMLWindow] 'Ad ... -> 'Ad */ {
            
            // Bubble up. Get the parent of that node. It becomes the new that.
            // the getParent throws when bubbling is not possible.
            
            try {
                var parent = that.getParent(),
                b = parent.___nodes___[0];
                that = parent;
                // the_event.that = that; // FIXME

                // If that node has an event handler, fire it. Otherwise, bubble up.

                if (/*: cheat Bool */ (b['___ on ___'] &&
                                       b['___ on ___'][type])) {
                    that.fire(the_event);
                } else {
                    the_event.bubble();
                }
            } catch (e) {
                return error(/*: cheat Undef */ e);
            }
        })),
        key: /*: upcast 'Ad */ key,
        preventDefault: /*: cheat 'Ad */ (function () /*: -> Undef */ {
            if (the_actual_event.preventDefault) {
                the_actual_event.preventDefault();
            }
            the_actual_event.returnValue = false;
        }),
        shiftKey: /*: upcast 'Ad */ (the_actual_event.shiftKey),
        target: /*: upcast 'Ad */ target,
        that: /*: upcast 'Ad */ that,
        type: /*: upcast 'Ad */ type,
        x: /*: upcast 'Ad */ (the_actual_event.clientX),
        y: /*: upcast 'Ad */ (the_actual_event.clientY)
    };
    that.fire(the_event);
    // If the target has event handlers, then fire them. Otherwise, bubble up.

    if (/*: cheat Bool */ (the_target['___ on ___'] &&
                           the_target['___ on ___'][the_event.type])) {
        target.fire(the_event);
    } else {
        for (;;) {
            the_target = the_target.parentNode;
            if (!the_target) {
                break;
            }
            if (/*: cheat Bool */ (the_target['___ on ___'] &&
                                   the_target['___ on ___'][the_event.type])) {
                that = /*: obj* 'AdObj */ (new Bunch([the_target]));
                /*: cheat 'Ad */ (the_event.that = that);
                that.fire(the_event);
                break;
            }
            if (/*: cheat Bool */ (the_target['___adsafe root___'])) {
                break;
            }
        }
    }
    if (the_event.type === 'escapekey') {
        if (ephemeral) {
            ephemeral.remove();
        }
        ephemeral = null;
    }
    // This line is annoying --- why add Null to all these types?
    /*: cheat Undef */ (that = the_target = the_event = the_actual_event = null);
    return;
}
