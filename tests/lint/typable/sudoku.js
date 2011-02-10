// sudoku.js

// 2008-10-08


// The sudoku function makes an object which manages the gamestate of a sudoku
// game.



ADSAFE.lib("sudoku", function () {
    "use strict";
    return function() {
        var mode = 'setup', board = [], protect = [],

        // We make a static data structure called neighbors. It is an array of
        // arrays [81][20]. For each place in the board, it contains the indexes of the
        // 20 cells that directly constrain its value.

        a, b, i, j, k, m, n, neighbors = [], x, y, yj9x;

        // Compute the neighbors for each of the 81 cells.

        for (i = 0; i < 81; i += 1) {

            // Put the 8 row neighbors and 8 column neighbors in a.

            a = [];
            x = i % 9;
            y = Math.floor(i / 9);
            for (j = 0; j < 9; j += 1) {
                n = y * 9 + j;
                if (n !== i) {
                    a.push(n);
                }
                n = j * 9 + x;
                if (n !== i) {
                    a.push(n);
                }
            }

            // Put the 4 region neighbors that are not already in a in a.

            x = Math.floor(x / 3) * 3;
            y = Math.floor(y / 3) * 3;

            for (j = 0; j < 3; j += 1) {
                yj9x = ((y + j) * 9) + x;
                for (k = 0; k < 3; k += 1) {
                    n = yj9x + k;
                    if (n !== i) {
                        b = true;
                        for (m = 0; m < a.length; m += 1) {
                            if (ADSAFE.get(a, +m) === n) {
                                b = false;
                                break;
                            }
                        }
                        if (b) {
                            a.push(n);
                        }
                    }
                }
            }
            ADSAFE.set(neighbors, +i, a);
        }

        // Return an object containing six methods:
        //      check, get, getProtect, play, set, setup.

        return {

            // Check that the piece is legal. It does not determine if it is right.

            check: function (place) {
                var piece = ADSAFE.get(board, +place),
                neighbor = ADSAFE.get(neighbors, +place),
                n = neighbor.length,
                i;
                if (!piece) {
                    return false;
                }
                for (i = 0; i < n; i += 1) {
                    if (ADSAFE.get(board, +(ADSAFE.get(neighbor, +i))) === piece) {
                        return false;
                    }
                }
                return true;
            },

            // Get the piece.

            get: function (place) {
                return ADSAFE.get(board, +place);
            },

            // Get the piece from the problem.

            getProtect: function (place) {
                return ADSAFE.get(protect, +place);
            },

            // Go into play mode. In play mode, the problem cannot be changed.

            play: function () {
                mode = 'play';
            },

            // Place a piece on the board. In setup mode, a piece cannot be placed if there
            // a related piece in the problem.

            set: function (place, piece) {
                if (mode === 'setup' ||
                    (mode === 'play' && ADSAFE.get(protect, +place) !== 'setup')) {
                    ADSAFE.set(board, +place, piece);
                    ADSAFE.set(protect, +place, piece && mode);
                } else if (!(ADSAFE.get(protect, +place))) {
                    ADSAFE.set(board, +place, piece);
                }
            },

            // Go into setup mode. In setup mode, pieces are placed on the board and also on
            // an alternate board called the problem.

            setup: function () {
                mode = 'setup';
            },

            // Go into solve mode.

            solve: function () {
                mode = '';
            }
        };
    };
});

