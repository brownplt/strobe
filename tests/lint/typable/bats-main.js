<div id="BATS_">
<script>
ADSAFE.go('BATS_', function (dom, lib) {
    "use strict";
    var input  = dom.q('#BATS_INPUT'),
    button = dom.q('#BATS_BUTTON'),
    screen = dom.q('pre'),
    bats   = dom.q('#BATS_BATS'),
    fences = dom.q('#BATS_FENCES'),
    width  = dom.q('#BATS_WIDTH'),
    height = dom.q('#BATS_HEIGHT'),
    now_serving = 0,
    race,
    key_to_move = {
        q: 'nw',
        w: 'n',
        e: 'ne',
        a: 'w',
        s: 'stand',
        d: 'e',
        z: 'sw',
        y: 'sw',
        x: 's',
        c: 'se',
        ' ': 'jump',
        '0': 'jump',
        '7': 'nw',
        '8': 'n',
        '9': 'ne',
        '4': 'w',
        '5': 'stand',
        '6': 'e',
        '1': 'sw',
        '2': 's',
        '3': 'se'
    };

    function reset() {
        button.value('Play');
    }

    function schedule_bats() {
        now_serving += 1;
        var serving = now_serving;
        ADSAFE.later(function () {
            var a;
            if (serving === now_serving) {
                a = lib.bats.move_bats();
                screen.value(a[1]);
                if (a[0] !== true) {
                    button.value('Play');
                } else {
                    schedule_bats();
                }
            }
        }, 2000);
    }

    bats.on('change', reset);
    fences.on('change', reset);
    width.on('change', reset);
    height.on('change', reset);

// The button changes the state of the game.
// Wait -> Play -> Pause -> Resume

    button
        .on('click', function (e) {
            var a;
            now_serving += 1;
            switch (button.getValue()) {
            case 'Play':
                a = lib.bats.play({
                    width:  +width.getValue(),
                    height: +height.getValue(),
                    bats:   +bats.getValue(),
                    fences: +fences.getValue()
                });
                screen.value(a[1]);
                if (a[0] === true) {
                    button.value('Pause');
                    screen.value(a[1]);
                    input.focus();
                } else {
                    screen.value('Please modify the game parameters.');
                }
                break;
            case 'Pause':
                button.value('Resume');
                break;
            case 'Resume':
                if (!race) {
                    button.value('Pause');
                    input.focus();
                }
                break;
            }
            e.preventDefault();
        })
        .value('Play')
        .focus();



// The input is an invisible input_text that receives the keystrokes. It must
    // have focus while the game is being played. When it loses focus, we
// automatically pause the game.

    input
        .focus()
        .on('keypress', function (e) {
            var a, direction;
            if (button.getValue() === 'Pause') {
                direction = ADSAFE.get(key_to_move, e.key.toLowerCase());
                if (typeof direction === 'string') {
                    now_serving += 1;
                    e.preventDefault();
                    a = lib.bats.move(direction);
                    screen.value(a[1]);
                    if (a[0] === true) {
                        ADSAFE.later(function () {
                            a = lib.bats.move_bats();
                            screen.value(a[1]);
                            if (a[0] !== true) {
                                button.value('Play');
                            } else {
                                schedule_bats();
                            }
                        }, 100);
                    } else {
                        button.value('Play');
                    }
                }
            }
        })
        .on('blur', function (e) {
            if (button.getValue() === 'Pause') {
                button.value('Resume');
            }

            // If the input loses focus, then we want to pause the game. We do that by
            // setting the button value to 'Resume'. Clicking on the 'Resume' button sets
            // its value to 'Pause'. Confusion results from clicking on 'Pause' because
            // that creates a 'blur' event followed by a click event. We cannot easily
            // detect if the blur and click are in response to one user action or two.
            // We fake it by keeping a race variable that is set by the blur and cleared
            // a second later. If the race bit is set, we can ignore the click.

            race = true;
            ADSAFE.later(function () {
                race = false;
            }, 1000);
        });
});
</script>
</div>