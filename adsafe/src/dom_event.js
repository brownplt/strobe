var allow_focus = true,
ephemeral = true,
the_range /*: upcast Undef + Range */;

function Bunch(nodes) 
/*: constructor (Array<HTMLElement> -> {___nodes___: Array<HTMLElement>, ___star___: Bool}) */ 
{
    this.___nodes___ = nodes;
    this.___star___ = star && nodes.length > 1;
    star = false;
}

function dom_event (e) /*: Event -> Undef */ {
    var key /*: upcast Undef + Str */,
    target /*: upcast Undef + Bunch */,
    that /*: upcast Undef + Bunch */,
//    the_event /*: upcast Undef + Event */,
    the_target /*: upcast Undef + HTMLElement */,
    the_actual_event = /*: cheat Bool */ e || event,
    type = the_actual_event.type;

    // Get the target node and wrap it in a bunch.

    the_target = the_actual_event.target ||
        the_actual_event.srcElement;
    target = new Bunch([/*: cheat HTMLElement */ the_target]);
    that = target;

    // Use the PPK hack to make focus bubbly on IE.
    // When a widget has focus, it can use the focus method.

    switch (type) {
    case 'mousedown':
        allow_focus = true;
        if (/*: cheat Bool */ (document.selection)) {
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
        key = String.fromCharCode(/*: cheat Int */ (the_actual_event.charCode ||
                                                    the_actual_event.keyCode));
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
    if (/*: cheat Bool */ (the_actual_event.cancelBubble &&
                           the_actual_event.stopPropagation)) {
        the_actual_event.stopPropagation();
    }

    // Make the event object.

    var the_event = 
        /*: cheat 
            {
              altKey: Bool,
              ctrlKey: Bool,
              bubble: (-> Undef),
              key: Undef + Str,
              preventDefault: (-> Undef),
              shiftKey: Bool,
              target: Undef + Bunch,
              that: Undef + Bunch,
              "type": Str,
              x: Int,
              y: Int,
              #proto: Object,
              *: Undef,
              #code: Undef
           }
       */
    {
        altKey: the_actual_event.altKey,
        ctrlKey: the_actual_event.ctrlKey,
        bubble: function () /*: -> Undef */ {

            // Bubble up. Get the parent of that node. It becomes the new that.
            // the getParent throws when bubbling is not possible.

            try {
                var parent = /*: cheat Bunch */ (that.getParent()),
                b = parent.___nodes___[0];
                that = parent;
//                the_event.that = /*: cheat Bunch */ that;

                // If that node has an event handler, fire it. Otherwise, bubble up.

                if (/*: cheat Bool */ (b['___ on ___'] &&
                                       b['___ on ___'][type])) {
                    //that.fire(the_event);
                } else {
                    //the_event.bubble();
                }
            } catch (e) {
                return error(/*: cheat Undef */ e);
            }
        },
        key: key,
        preventDefault: function () /*: -> Undef */ {
            if (/*: cheat Bool */ (the_actual_event.preventDefault)) {
                the_actual_event.preventDefault();
            }
            the_actual_event.returnValue = false;
        },
        shiftKey: the_actual_event.shiftKey,
        target: target,
        that: that,
        type: type,
        x: the_actual_event.clientX,
        y: the_actual_event.clientY
    };
    /*: cheat 'Ad -> 'Ad */ (that.fire)(/*: upcast 'Ad */ the_event);
    // If the target has event handlers, then fire them. Otherwise, bubble up.

    if (/*: cheat Bool */ (the_target['___ on ___'] &&
                           the_target['___ on ___'][the_event.type])) {
        target.fire(the_event);
    } else {
        for (;;) {
            the_target = the_target.parentNode;
            if (/*: cheat Bool */ (!the_target)) {
                break;
            }
            if (/*: cheat Bool */ (the_target['___ on ___'] &&
                                   the_target['___ on ___'][the_event.type])) {
                that = new Bunch([/*: cheat HTMLElement */ the_target]);
                the_event.that = that;
                /*: cheat 'Ad -> 'Ad */ (that.fire)(/*: upcast 'Ad */ the_event);
                break;
            }
            if (the_target['___adsafe root___']) {
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
    that = the_target = the_event = the_actual_event = null;
    return;
}