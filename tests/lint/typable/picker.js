// picker.js
// 2008-10-23

"use strict";


// The picker is a lightweight data entry device. It is activated by a
// mousedown event. A menu is displayed, centered around the cursor. The menu
// is removed when an action is triggered, or when the cursor leaves the menu.

// The picker is called by a mousedown function. It pops up a dialog made
// up of simple tiles. The one you mouseup on produces a value. The picker
// maker function takes these parameters:
//      box         A box that will be the container of the picker.
//      x           The x of the click point.
//      y           The y of the click point.
//      func        A function that will be called when a pick is made.

ADSAFE.lib("picker", function () {
    return function (box, x, y, func) {

        var i;


// Make span elements and put them in the box.

        box.append(box.tag('span')
            .value('clear')
            .style('backgroundColor', 'white')
            .style('color', 'black'));
        for (i = 1; i < 10; i += 1) {
            box.append(box.tag('span')
                .value(i)
                .style('backgroundColor', 'white')
                .style('color', 'black'));
        }
        box
            .ephemeral()
            .style('cursor', 'default')
            .style('position', 'absolute')
            .style('left', Math.max(x - (box.getOffsetWidth() / 2), 0) + 'px')
            .style('top', (y - (box.getOffsetHeight() / 2)) + 'px')
            .style('visibility', 'visible')
            .on('mouseup', function (e) {
                if (e.target.getTagName() === 'span') {
                    func(e.target.getValue());
                }
                box.ephemeral();
                e.preventDefault();
            })
            .on('mouseout', function (e) {
                if (e.target.getTagName() === 'span') {
                    e.target
                        .style('backgroundColor', 'white')
                        .style('color', 'black');
                } else {
                    box.ephemeral();
                }
                e.preventDefault();
            })
            .on('mouseover', function (e) {
                if (e.target.getTagName() === 'span') {
                    e.target
                        .style('backgroundColor', 'green')
                        .style('color', 'yellow');
                }
                e.preventDefault();
            });
    };
});

