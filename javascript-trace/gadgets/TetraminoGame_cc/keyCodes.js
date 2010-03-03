function keyCodes() {
  this.keys = new Array();
  this.keys.push( [8, "Backspace"] );
  this.keys.push( [9, "Tab"] );

  this.keys.push( [13, "Return"] );

  this.keys.push( [16, "Shift"] );
  this.keys.push( [17, "Ctrl"] );
  this.keys.push( [19, "Pause"] );
  this.keys.push( [20, "Caps"] );
  this.keys.push( [27, "ESC"] );

  this.keys.push( [32, "Space"] );
  this.keys.push( [33, "PageUp"] );
  this.keys.push( [34, "PageDown"] );
  this.keys.push( [35, "End"] );
  this.keys.push( [36, "Home"] );

  this.keys.push( [37, "Left"] );
  this.keys.push( [38, "Up"] );
  this.keys.push( [39, "Right"] );
  this.keys.push( [40, "Down"] );

  this.keys.push( [45, "Ins"] );
  this.keys.push( [46, "Del"] );

  this.keys.push( [48, "D0"] );
  this.keys.push( [49, "D1"] );
  this.keys.push( [50, "D2"] );
  this.keys.push( [51, "D3"] );
  this.keys.push( [52, "D4"] );
  this.keys.push( [53, "D5"] );
  this.keys.push( [54, "D6"] );
  this.keys.push( [55, "D7"] );
  this.keys.push( [56, "D8"] );
  this.keys.push( [57, "D9"] );

  this.keys.push( [65, "A"] );
  this.keys.push( [66, "B"] );
  this.keys.push( [67, "C"] );
  this.keys.push( [68, "D"] );
  this.keys.push( [69, "E"] );
  this.keys.push( [70, "F"] );
  this.keys.push( [71, "G"] );
  this.keys.push( [72, "H"] );
  this.keys.push( [73, "I"] );
  this.keys.push( [74, "J"] );
  this.keys.push( [75, "K"] );
  this.keys.push( [76, "L"] );
  this.keys.push( [77, "M"] );
  this.keys.push( [78, "N"] );
  this.keys.push( [79, "O"] );
  this.keys.push( [80, "P"] );
  this.keys.push( [81, "Q"] );
  this.keys.push( [82, "R"] );
  this.keys.push( [83, "S"] );
  this.keys.push( [84, "T"] );
  this.keys.push( [85, "U"] );
  this.keys.push( [86, "V"] );
  this.keys.push( [87, "W"] );
  this.keys.push( [88, "X"] );
  this.keys.push( [89, "Y"] );
  this.keys.push( [90, "Z"] );

  this.keys.push( [93, "Context"] );

  this.keys.push( [96, "Num0"] );
  this.keys.push( [97, "Num1"] );
  this.keys.push( [98, "Num2"] );
  this.keys.push( [99, "Num3"] );
  this.keys.push( [100, "Num4"] );
  this.keys.push( [101, "Num5"] );
  this.keys.push( [102, "Num6"] );
  this.keys.push( [103, "Num7"] );
  this.keys.push( [104, "Num8"] );
  this.keys.push( [105, "Num9"] );

  this.keys.push( [106, "Multiply"] );
  this.keys.push( [107, "Plus"] );
  this.keys.push( [108, "Enter"] );
  this.keys.push( [109, "Plus"] );
  this.keys.push( [110, "NumDel"] );
  this.keys.push( [111, "Plus"] );



  this.keys.push( [112, "F1"] );
  this.keys.push( [113, "F2"] );
  this.keys.push( [114, "F3"] );
  this.keys.push( [115, "F4"] );
  this.keys.push( [116, "F5"] );
  this.keys.push( [117, "F6"] );
  this.keys.push( [118, "F7"] );
  this.keys.push( [119, "F8"] );
  this.keys.push( [120, "F9"] );
  this.keys.push( [121, "F10"] );
  this.keys.push( [122, "F11"] );
  this.keys.push( [123, "F12"] );

}

keyCodes.prototype.getNameForKey = function (keyCode) {
  var key = null;
  for (var i=0; i<this.keys.length; i++) {
    key = this.keys[i];
    if (key[0] == keyCode) {
      key = key[1];
      return key;
    }
  }
  return "Key"+keyCode;
}