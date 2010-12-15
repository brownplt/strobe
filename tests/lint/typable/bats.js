// bats.js
// 2008-10-29

// This is an ADsafe library that implements the game logic for B@TS.

"use strict";

ADSAFE.lib("bats", function () {

    var seed = 0xAD5AFE,    // Seed for the random number generator
        now_playing,        // The game state: true, 'DIE', 'WIN'
        width = 26,
        height = 23,
        nr_fences = 10,
        nr_bats = 10,
        you_column,         // Location of the player
        you_row,
        bats,               // Array of bats
        board,              // The game board
        hor = {
            nw: -1,
            w: -1,
            sw: -1,
            n: 0,
            stand: 0,
            s: 0,
            ne: 1,
            e: 1,
            se: 1
        },
        ver = {
            nw: -1,
            w: 0,
            sw: 1,
            n: -1,
            stand: 0,
            s: 1,
            ne: -1,
            e: 0,
            se: 1
        };

    function random() {

// ADsafe does not provide a random number generator, so we make our own.
// It returns a non-negative number that is less than 1. It uses a 32-bit
// feedback shift register. It is adequate for playing a game.

        seed ^= seed << 1;
        seed ^= seed >> 3;
        seed ^= seed << 10;
//        return (seed + 2147483648) / 4294967296;
        return seed;
    }


    function rnd(n) {

// rnd returns a non-negative integer that is less than n.

        return Math.floor(n * random());
    }


    function clear()  {

// Clear the board, placing fences around the perimeter. The board is
// represented as an array of arrays.

        var i,
            j,
            a = [],
            b;

        board = [];
        for (j = 0; j < width; j += 1) {
            a.push('###');
        }
        b = a.slice();
        board[0] = a;
        for (i = 2; i < height; i += 1) {
            a = ['###'];
            for (j = 2; j < width; j += 1) {
                a.push('   ');
            }
            a.push('###');
            board.push(a);
        }
        board.push(b);
    }


    function place(piece, n) {

// Place n pieces randomly on the board, each into an empty spot.
// Return an array containing the locations of all of the pieces.

        var a = [], rank, row, column;
        while (n) {
            row = rnd(height - 2) + 1;
            column = rnd(width - 2) + 1;
            rank = ADSAFE.get(board, row);
            if (ADSAFE.get(rank, column) === '   ') {
                ADSAFE.set(rank, column, piece);
                a.push({
                    row: row,
                    column: column
                });
                n -= 1;
            }
        }
        return a;
    }


    function display() {

// Produce a big string from the board.

        var a = [], row;
        for (row = 0; row < height; row += 1) {
            a.push(ADSAFE.get(board, row).join(''));
        }
        return a.join('\r\n');
    }


// Return the game model object.

    return {
        play: function (spec) {
            width = spec.width || width;
            height = spec.height || height;
            nr_fences = spec.fences || nr_fences;
            nr_bats = spec.bats || nr_bats;
            now_playing = width > 8 && height > 8 &&
                nr_fences < width * height / 4 &&
                nr_bats < width * height / 5;
            clear();
            if (now_playing) {

// Set up the board. Put the player in the middle. Surround him with fences
// and bats.

                you_column = Math.floor(width / 2);
                you_row = Math.floor(height / 2);
                ADSAFE.set(ADSAFE.get(board, you_row), you_column, 'YOU');
                place('###', nr_fences);
                bats = place('^@^', nr_bats);
            }
            return [now_playing, display()];
        },
        move: function (direction) {
            var a, horizontal, vertical;
            if (now_playing === true) {
                horizontal = ADSAFE.get(hor, direction);
                vertical = ADSAFE.get(ver, direction);
                if (typeof horizontal === 'number') {
                    ADSAFE.set(ADSAFE.get(board, +you_row), +you_column, '   ');
                    you_row += vertical;
                    you_column += horizontal;
                    if (ADSAFE.get(ADSAFE.get(board, +you_row), +you_column) === '   ') {
                        ADSAFE.set(ADSAFE.get(board, +you_row), +you_column, 'YOU');
                    } else {
                        ADSAFE.set(ADSAFE.get(board, +you_row), +you_column, 'DIE');
                        now_playing = 'DIE';
                    }
                } else if (direction === 'jump') {
                    ADSAFE.set(ADSAFE.get(board, +you_row), +you_column, '   ');
                    a = place('YOU', 1);
                    you_row = a[0].row;
                    you_column = a[0].column;
                    ADSAFE.set(ADSAFE.get(board, +you_row), +you_column, 'YOU');
                }
            }
            return [now_playing, display()];
        },
        move_bats: function () {

// Move all of the bats toward the player. If a bat lands on a fences or
// another bat, it is removed from the game. If a bat lands on the player,
// the game is over.

            var i, n = bats.length, b = [], a, piece,
                row, column, inc_row, inc_column,
                abs_row, abs_column, delta_row, delta_column;
            if (now_playing === true) {
                for (i = 0; i < n; i += 1) {
                    a = ADSAFE.get(bats, +i);
                    row = a.row;
                    column = a.column;
                    ADSAFE.set(ADSAFE.get(board, +row), +column, '   ');
                    delta_row = you_row - row;
                    if (delta_row < 0) {
                        inc_row = -1;
                        abs_row = -delta_row;
                    } else {
                        inc_row = 1;
                        abs_row = delta_row;
                    }
                    delta_column = you_column - column;
                    if (delta_column < 0) {
                        inc_column = -1;
                        abs_column = -delta_column;
                    } else {
                        inc_column = 1;
                        abs_column = delta_column;
                    }
                    if (abs_row > abs_column) {
                        row += inc_row;
                        if (random() <= abs_column / abs_row) {
                            column += inc_column;
                        }
                    } else {
                        column += inc_column;
                        if (random() <= abs_row / abs_column) {
                            row += inc_row;
                        }
                    }
                    piece = ADSAFE.get(ADSAFE.get(board, +row), +column);;
                    if (piece === 'YOU') {
                        ADSAFE.set(ADSAFE.get(board, +you_row), +you_column, 'DIE');
                        now_playing = 'DIE';
                        return [now_playing, display()];
                    } else if (piece === '   ') {
                        ADSAFE.set(ADSAFE.get(board, +row), +column, '^@^');
                        b.push({row: row, column: column});
                    }
                }
                bats = b;
                if (bats.length === 0) {
                    ADSAFE.set(ADSAFE.get(board, +you_row), +you_column, 'WIN');
                    now_playing = 'WIN';
                }
            }
            return [now_playing, display()];
        }
    };
});
