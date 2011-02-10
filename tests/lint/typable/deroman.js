<div id="DEROMAN_">
<script>
"use strict";
ADSAFE.go("DEROMAN_", function (dom, lib) {
    var deroman = (function () {
        var value = {
            I: 1,
            V: 5,
            X: 10,
            L: 50,
            C: 100,
            D: 500,
            M: 1000
        };

        return function (string) {
            var i = 1, letter, next, result = 0;

            string = string.toUpperCase();
            letter = ADSAFE.get(value, string.charAt(0));
            while (typeof letter === 'number') {
                next = ADSAFE.get(value, string.charAt(i));
                if (typeof next !== 'number' || letter >= next) {
                    result += letter;
                } else {
                    result -= letter;
                }
                letter = next;
                i += 1;
            }
            return result;
        };

    }()),
    input = dom.q("input_text");
    input
        .on('enterkey', function (e) {
            dom.q('#DEROMAN_RESULT').value(deroman(input.getValue()));
            input.select();
        })
        .focus();
});
</script>
</div>