// sudokusolver.js

// 2008-10-08

// The sudokusolver demonstrates a technique called eteration. It loops
// without using a loop. It allows a long-duration computation to go on
// without blocking the interactive aspects of the application.

// It works by defining a unit of work, which is encapsulated in the solver
// function. Each time solver is called, one unit of work is accomplished,
// computing a possible solution for one square. If solver determines that
// there is more work to do, then it uses ADSAFE.later to schedule the next unit
// of work.



ADSAFE.lib("solver", function () {

    "use strict";
    return function() {
        // TypedJS doesn't support named function expressions, so this small change
        // is made for now.
        return function /*sudokusolver*/(game, update, finish) {


            //  sudokusolver begins the process of solving a puzzle. It takes three
            //  parameters:
            //      game
            //              A sudoku object.
            //      update(piece)
            //              A function which will update the display as each cell is given
            //              a new value. This is usually a bad guess, but is sometimes the
            //              final correct value.
            //      finish(success)
            //              When the process converges on a solution, it calls this function
            //              with a true parameter. If it is unable to find a solution, it is
            //              called with a false parameter.


            var place;

            function start() {
                game.solve();
                var free = -1, i;
                for (i = 0; i < 81; i += 1) {
                    if (!game.getProtect(i)) {
                        free = i;
                        if (!game.get(i)) {
                            break;
                        }
                    }
                }
                return free;
            }

            function solver() {
                if (place < 0) {
                    return false;
                }
                var guess = game.get(place);
                for (;;) {
                    if (guess) {
                        guess += 1;
                    } else {
                        guess = 1;
                    }
                    if (guess > 9) {
                        game.set(place, 0);
                        update(place);
                        for (;;) {
                            place -= 1;
                            if (place < 0) {
                                if (finish) {
                                    finish(false);
                                }
                                game.play();
                                return;
                            }
                            if (!game.getProtect(place)) {
                                break;
                            }
                        }
                        ADSAFE.later(solver, 0);
                        return;
                    }
                    game.set(place, guess);
                    if (game.check(place)) {
                        update(place);
                        for (;;) {
                            place += 1;
                            if (place >= 81) {
                                if (finish) {
                                    finish(true);
                                }
                                game.play();
                                return;
                            }
                            if (!game.getProtect(place)) {
                                break;
                            }
                        }
                        ADSAFE.later(solver, 0);
                        return;
                    }
                }
            }

            // Start the solver.

            place = start();
            solver();

            // Return an object containing one method: halt.

            return {
                halt: function () {

                    // The halt method forces the process to immediately fail. It causes the
                    // invocation of finish(false);

                    place = -1;
                }
            };
        };
    };
});