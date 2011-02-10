// sudokuwidget.js

// The sudoku game master.

// 2008-10-10



ADSAFE.lib("widget", function () {
    "use strict";
        return function (dom, picker, game, sudokusolver) {
            var clear,              // Reference to the clear button dom node.
            div,                // Reference to the button box dom node.
            i,                  // Outer loop variable.
            j,                  // Inner loop variable.
            nodes = [],         // Array of 81 places on the game board.
            play,               // Reference to the play button dom node.
            setup,              // Reference to the setup button dom node.
            solve,              // Reference to the solve button dom node.
            solver,
            table,              // Reference to the table dom node.
            tbody,              // Reference to the tbody dom node.
            td,                 // Reference to the td dom node.
            tr;                 // Reference to the tr dom node.

            // Show the piece that is currently on one of the 81 places.

            function update(place) {
                var value = game.get(place);
                if (value) {
                    ADSAFE.get(nodes, +place)
                        .value(value)
                        .style('fontStyle',
                               game.getProtect(place) === 'setup' ? 'italic' : 'normal');
                } else {
                    ADSAFE.get(nodes, +place)
                        .value('')
                        .style('fontStyle', 'normal');
                }
            }

            // The doPlay function starts play mode.

            function doPlay() {
                game.play();
                play.klass('sudoku-mode');
                setup.klass('');
            }

            // Make the game board. It is a table that contains 81 places. Each place will
            // hold a piece, which is a number between 0 and 9.

            // The table handles all of the mousedown events for the places.
            // It pops up a picker.

            table = dom.tag('table')
                .klass('sudoku')
                .on('mousedown', function (e) {
                    var box, place, target = e.target;
                    if (target.getTagName() === 'td') {
                        place = target.getMark();
                        box = dom.tag('div').protect().klass('sudoku-picker');

                        dom.append(box);
                        picker(box, e.x, e.y, function (value) {
                            value = +value || 0;
                            game.set(place, value);
                            if (!game.check(place)) {
                                game.set(place, 0);
                            }
                            update(place);
                        });
                    }
                    e.preventDefault();
                });

            // Populate a tbody with a 9x9 array of cells.
            tbody = dom.tag('tbody');

            for (i = 0; i < 9; i += 1) {
                tr = dom.tag('tr');
                for (j = 0; j < 9; j += 1) {
                    td = dom.tag('td');
                    if ((i < 3 || i > 5) !== (j < 3 || j > 5)) {
                        td.klass('suduko-region');
                    }
                    td.mark(nodes.length);
                    nodes.push(td);
                    tr.append(td);
                }
                tbody.append(tr);
            }

            // Make the button box.

            div = dom.tag('div').klass('sudoku-control');

            // Make the setup button. It puts the game in setup mode. In play mode,
            // pieces that were entered in setup mode are protected.

            setup = dom.tag('input', 'button')
                .value('Set Up')
                .klass('sudoku-mode')
                .on('click', function (e) {
                    if (solver) {
                        solver.halt();
                    }
                    game.setup();
                    setup.klass('sudoku-mode');
                    play.klass('');
                    solve.klass('');
                });

            // Make the play mode button.

            play = dom.tag('input', 'button')
                .value('Play')
                .on('click', doPlay);

            // Make the clear button. It cancels the solver and clears the board.

            clear = dom.tag('input', 'button')
                .value('Clear')
                .on('click', function (e) {
                    var place;
                    for (place = 0; place < 81; place += 1) {
                        game.set(place, 0);
                        update(place);
                    }
                    if (solver) {
                        solver.halt();
                        game.play();
                        solve.klass('');
                        solver = '';
                    }
                });

            // Make the solve button. It calls the brute force solver.

            solve = dom.tag('input', 'button')
                .value('Solve')
                .on('click', function (e) {
                    doPlay();
                    solve.klass('sudoku-mode');
                    solver = sudokusolver(game, update, function () {
                        solve.klass('');
                    });
                });

            div
                .append(setup)
                .append(play)
                .append(clear)
                .append(solve);

            dom
                .append(table.append(tbody))
                .append(div);

        };

});