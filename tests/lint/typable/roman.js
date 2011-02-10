<div id="ROMAN_">
<script>
    ADSAFE.go("ROMAN_", function(dom, lib) {
        "use strict";
        var roman = (function () {
            var table = [
                ['', 'I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX'],
                ['', 'X', 'XX', 'XXX', 'XL', 'L', 'LX', 'LXX', 'LXXX', 'XC'],
                ['', 'C', 'CC', 'CCC', 'CD', 'D', 'DC', 'DCC', 'DCCC', 'CM']
            ];
            
            return function (n) {
                var result = '', i;
                
                n = +n;
                for (i = 0; i < table.length; i += 1) {
                    result = ADSAFE.get(ADSAFE.get(table, +i), +(n % 10)) + result;
                    n = Math.floor(n / 10);
                }
                for (i = 0; i < n; i += 1) {
                    result = 'M' + result;
                }
                return result;
            };
        }()),
        input = dom.q("input_text");
        input
            .on('enterkey', function (e) {
                dom.q('#ROMAN_RESULT').value(roman(input.getValue()));
            input.select();
            })
            .focus();
    });
</script>
</div>
