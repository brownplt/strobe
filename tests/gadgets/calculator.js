// *** TYPES FOR gadgets/Calculator_cc/scientific.js *** 
//took a lot of clicking to get these
//allRight, checkValid, and convertMath were impossible
//to be called, since they would onyl come frmo an
//"express" mode. i filled those in myself.
/*::
  function getValue : ((Int + String + Double) -> (Int + Double))
  function menuAddItems : (Dom -> Void)
  function menuItemClicked : (String -> Void)
  function redrawAll : ( -> Void)
  function getNumber : ((Int + Double) -> (Int + Double + String))
  function getExpressionString : ( -> String)
  function braces : (Int -> Void)
  function iRefresh : ( -> Void)
  function iClear : ( -> Void)
  function viewOpen : ( -> Void)
  function handleDigit : (String -> Void)
  function handleOperator : (String -> Void)
  function reverse : (String -> String)
  function changeBase : ((String + Int) -> Void)
  function insertPi : ( -> Void)
  function convertInt : ( -> Void)
  function iFlags : ( -> Void)
  function toRadians : ( -> Void)
  function toReference : (Int -> Void)
  function changeAngleRef : (Int -> Void)

  function allRight : (String -> Bool)
  function checkValid : (String -> Bool)
  function convertMath : (String -> String)

  function transistState : (String -> Void)
  function keyTrap : ( -> Void)
  function backSpace : ( -> Void)
  function changeSign : ( -> Void)
  function dmsFunction : ( -> Void)
  function unaryExp : ( -> Void)
  function unaryLog : ( -> Void)
  function addE : ( -> Void)
  function mSinh : ((Int + Double) -> (Int + Double))
  function mCosh : (Double -> Double)
  function mTanh : (Double -> Int)
  function iSinh : ((Int + Double) -> Double)
  function iCosh : (Double -> Double)
  function iTanh : (Double -> Double)
  function unarySin : ( -> Void)
  function unaryCos : ( -> Void)
  function unaryTan : ( -> Void)
  function unaryCube : ( -> Void)
  function unaryFact : ( -> Void)
  function unarySquare : ( -> Void)
  function unaryInverse : ( -> Void)
  function baseButtons : ( -> Void)
  function percentOf : ( -> Void)
  function calculate : ((Int + Double) * String * (Int + Double) -> (Int + Double))
  function unaryNot : ( -> Void)
*/


var expression;
var display;
var base;
var feFlag = 0;
var dEditable;
var dExists;
var dValue;
var memory = 0;
var scripting = false;
var angleRef;
var beginFlag;
var sci_flag = 0;
var ans;
var opr;
var oprFlag;
var lastOpr;
var lastVal; 
var lastOprFlag;
var cheatCode = 'Express';
var state = 0;

var expression = undefined;

for (var k = 0; k <= 25; ++k) {
  var a = String_fromCharCode("A".charCodeAt(0) + k);
  eval(a + '=' + 0);
}

function getValue(a) {
  if (a == '.')
    a += '0';

  if (base != 10) {
    a = parseInt(a, base);
  } else {
    a = parseFloat(a);
  }

  return a;
}

function menuAddItems(menu) {
  menu.AddItem("Simple", (sci_flag == 0) ? gddMenuItemFlagChecked : 0, menuItemClicked);
  menu.AddItem("Scientific", (sci_flag == 1) ? gddMenuItemFlagChecked : 0, menuItemClicked);
  //	menu.AddItem("Express",sci_flag == 2?gddMenuItemFlagChecked:0,menuItemClicked);
}

function menuItemClicked(item) {
  var id = (item == "Simple") ? 0 : ((item == "Scientific") ? 1 : 2);
  if (id ^ sci_flag) {
    sci_flag = id;
    redrawAll();
    iClear();
  }
}

function redrawAll() {
  if (sci_flag != 1) {
    basePanel.visible = false;
    degdispPanel.visible = false;
    advPanel.visible = false;
    flagPanel.visible = false;
    atofPanel.visible = false;
    dframe.src = "frame.png";
    stdPanel.x = 10;
    stdPanel.y = 50;
    face.width = 160;
    displayPanel.width = 150;
    expressionPanel.width = 150;
    simX.visible = true;
    sciX.visible = false;
    stdPanel.visible = true;
    view.width = 190;
    view.height = 190;
  } else {
    basePanel.visible = true;
    degdispPanel.visible = true;
    advPanel.visible = true;
    flagPanel.visible = true;
    atofPanel.visible = true;
    dframe.src = "frame_sci.png";
    stdPanel.x = 147;
    stdPanel.y = 78;
    face.width = 366;
    displayPanel.width = 357;
    stdPanel.width = 357;
    simX.visible = false;
    sciX.visible = true;
    stdPanel.visible = true;
    view.width = 396;
    view.height = 238;
  }

  if (sci_flag == 2) {
    simX.visible = false;
    stdPanel.visible = false;
  }

  base = 10;

  if (!expression)
    expression = [];

  iRefresh();
}

function getNumber(a) {
  var dp = "0123456789ABCDEF";
  var ans = '' ;
  var i = 0;
  var ef = 0;
  var cnt = 0;
  var nflag = 0;
  var old = a;

  for (i = 0; i < ans.length; ++i)
      ef |= (ans.substr(i, 1) == 'E');

  if (base != 10) {
    if (a < 0) {
      nflag = 1;
      a = -a;
    }
    a += .5;

    while (a >= 1) {
      ans += dp.charAt(parseInt(a%base));
      a /= base;
      if (cnt++ > 30) {
		    alert(E_NLARGE);
		    changeBase('10');
		    return getNumber(old);
      }
    }

    a = reverse(ans);
    if (a == '')
      a = '0';

    if (nflag) {
      a = '-' + a;
      if (a == '-0') {
        a = '0';
      }
    }
  } else if (feFlag && !ef) {
    ans += a;
    var i = 0;
    for (i = 0; i < ans.length; ++i) {
      if (ans.substr(i, 1) == '.') {
        break;
      }
    }
    a = ans.substr(0, i) + ans.substr(i + 1);
    a = a.substr(0, 1) + "." + a.substr(1);
    a += "E" + (i - 1);
  }

  return a;
}

function getExpressionString() {
  var ret = '';
  var i = 0;
  var e = "";

  for (i = 0; i < expression.length; ++i) {
    e = expression[i];
    if (typeof(e) == 'string') {
      ret += e;
    } else {
      ret += getNumber(e);
    }
    ret += ' ';
  }
  // ret += '['  +  oprFlag  +  ','  +  opr  +  ','  +  ans  +  ']';

  return ret;
}

function braces(a) {
	if (!a) {
	  if (beginFlag || !dExists && !oprFlag) {
	    expression.push('(');
	    return;
	  } else {
	    if (oprFlag && dExists)
	      handleOperator('=');
	    if (!oprFlag || dExists)
	      opr = '*';
	    if (!dExists)
	      display = ans;
	    if (!dValue)
	      display = getValue(display);

	    expression.push(display);
	    expression.push(opr);
	    expression.push('(');
	    dExists = dEditable = display = ans = oprFlag = 0;
	    beginFlag = 1;
	  }
	} else {
	  if (oprFlag && dExists)
	    handleOperator('=');

	  if (expression.length) {
	    var e = expression.pop();
	    if (!expression.length) {
	      oprFlag = 0;
	    } else if ((e = expression.pop()) == '(') {
	      expression.push(e);
	      oprFlag = 0;
	    } else {
	      opr = e;
	      ans = expression.pop();
	      oprFlag = 1;
	    }
	  }
	}

	lastOprFlag = 0;
	iRefresh();
}

function iRefresh() {
  expressionPanel.innerText = getExpressionString();
  displayPanel.innerText = (dValue && sci_flag != 2) ? getNumber(display) : display;
  if (displayPanel.innerText == '')
    displayPanel.innerText = '0';

  if (dValue && display == Infinity || displayPanel.innerText == '1.#INF') {
    display = 'Infinity';
    dExists = dEditable = dValue = 0;
    iRefresh();
  }
}

function iClear() {
  beginFlag = 1;
  expression = [];
  display = 0;
  dExists = 1;
  dEditable = 1;
  lastOprFlag = oprFlag = ans = 0;

  var bb = base;
  changeBase('10');
  changeBase(""+bb);

  if (sci_flag == 2) {
    display = '';
  }
}

function viewOpen() {
  base = 10;
  iClear();
  angleRef = 1;
  invFlag.value = 0;
  hypFlag.value = 0;
  changeAngleRef(0);
  iRefresh();
}

function handleDigit(d) {
  var cc = d.charCodeAt(0) - "0".charCodeAt(0);
  var end = 0;

  beginFlag = 0;

  if (cc  >=  0 && cc  <=  9) {
  } else if (d != '.') {
    cc = d.charCodeAt(0) - "a".charCodeAt(0)  +  10;
  }

  if (base <= cc)
    end = 1;

  if (d == '.' && !end) {
    var flag = 0;

    for (var i = 0; i < display.length; ++i)
      if (display.substr(i, 1) == '.')
        flag = 1;

    if (flag || base != 10)
      end = 1;
  }

  if (!dEditable && !end) {
    dExists = dEditable = 1;
    dValue = 0;
    display = '';
  }

  if (d >= 'a' && d <= 'f' && !end)
    d = String_fromCharCode(d.charCodeAt(0) + "A".charCodeAt(0) - "a".charCodeAt(0));

  if (dValue) {
    var ff = feFlag;
    feFlag = 0;
    display = getNumber(display);
    feFlag = ff;
    dValue = 0;
  }

  for (var i = 0; i < display.length; ++i)
	  if (display.substr(i, 1) == 'E' && d == 'E')
		  end = 1;

  if (display.length == undefined)
    display = '';

  if (!end) {
    if (display.length < 20)
      display += d;
  }

  while (display.length > 1 && display.substr(0, 1) == '0') {
    display = display.substr(1);
  }

  lastOprFlag = 0;
  iRefresh();
}

function handleOperator(o) {
  var eq = 0;
  beginFlag = 0;
  iFlags();

  if (!dExists && o == '=') {
    display = ans;
    dExists = 1;
  }

  if (dExists && oprFlag) {
    if (!dValue) {
      dValue = 1;
      display = getValue(display);
    }

    lastOprFlag = 1;
    lastVal = display;
    lastOpr = opr;
    display = calculate(ans, lastOpr, lastVal);
    oprFlag = 0;
    eq = 1;
  }

  dEditable = 0;

  if (o == '=') {
    if (!oprFlag && lastOprFlag && !eq) {
      display = ans = calculate(ans, lastOpr, lastVal);
    }
    oprFlag = 0;
  } else {
    if (!dValue) {
      dValue = 1;
      display = getValue(display);
    }
    opr = o;
    ans = display;
    dExists = 0;
    oprFlag = 1;
  }
  iRefresh();
}

function reverse(a) {
  var r = '';
  for (var i = a.length - 1 ; i >= 0; i-=1 ) {
    r += a.charAt(i);
  }
  return r;
}

function changeBase(b) {
  if (scripting)
    return;

  scripting = true;
  if (!dValue) {
    dValue = 1;
    display = getValue(display);
  }

  base = b;
  var zero = "0".charCodeAt(0);
  var aaa = "a".charCodeAt(0);

  for (var i = 0; i < 16; ++i) {
    var id = "b_" + i;
    var img = "\"b_" + String_fromCharCode(i >= 10 ? aaa + i-10 : zero + i) + "_";
    var post = ".png\"";
    var dis = img + "dis" + post;
    if (base > i) {
      eval(id + ".image = " + img + "u" + post);
      eval(id + ".overImage = " + img + "h" + post);
      eval(id + ".downImage = " + img + "d" + post);
    } else {
	    eval(id + ".image = " + dis);
      eval(id + ".overImage = " + dis);
      eval(id + ".downImage = " + dis);
    }
  }
  hexPanel.value = b == 16;
  decPanel.value = b == 10;
  octPanel.value = b == 8;
  binPanel.value = b == 2;
  baseButtons();
  scripting = false;
  iRefresh();
}

function insertPi() {
  display = Math.PI;
  dValue = 1;
  dExists = 1;
  dEditable = 0;
  iRefresh();
}

function convertInt() {
  if (!dValue)
    display = getValue(display);

  dValue = 1;

  if (display > 0) {
    display = Math.floor(display);
  } else {
    display = Math.ceil(display);
  }

  dEditable = 0;
}

function iFlags() {
  invFlag.value = hypFlag.value = 0;
}

function toRadians() {
  if (!dValue)
    display = getValue(display);

  dValue = 1;

  if (angleRef == 0) {
    display *= Math.PI/180;
  } else if (angleRef == 2) {
    display *= Math.PI/200;
  }

  angleRef = 1;
}

function toReference(a) {
  toRadians();

  if (a == 0) {
    display *= 180/Math.PI;
  } else if (a == 2) {
    display *= 200/Math.PI;
  }

  angleRef = a;
}

function changeAngleRef(a) {
  if (angleRef == a || scripting)
    return;

  scripting = true;
  toReference(a);
  degPanel.value = a == 0;
  radPanel.value = a == 1;
  gradPanel.value = a == 2;
  iRefresh();
  scripting = false;
}

function allRight(s) {
  if (s.length == 1) {
    return s >= 'A' && s <= 'Z';
  } else {
    return s == ''||s == 'sin' || s == 'cos' || s == 'tan' || s == 'atan' || s == 'pow'
      || s == 'log' || s == 'asin' || s == 'acos' || s == 'atan' || s == 'floor'
      || s == 'ceil' || s == 'random';
  }
}

function checkValid(a) {
  var s = '';
  a += ' ';

  for (var i = 0; i < a.length; ++i) {
    var p = String_fromCharCode(a.charCodeAt(i));
    if (p >= 'A' && p <= 'Z' || p >= 'a' && p <= 'z') {
      s += p;
    } else {
      if (!allRight(s)) {
        return false;
      }
      s = '';
    }
  }
  return true;
}

function convertMath(a) {
  var s = ''; var r = '';
  a += ' ';

  for (var i = 0; i < a.length; ++i) {
    var p = String_fromCharCode(a.charCodeAt(i));
    if (p >= 'A' && p <= 'Z' || p >= 'a' && p <= 'z') {
      s += p;
    } else {
      if (s.length == 1) {
        r += s;
      } else if (s.length) {
        r += 'Math.' + s;
      }
      s = '';
      r += p;
    }
  }

  return r;
}

function transistState(c) {
  var prefix = cheatCode.substr(0, state)  +  c;
  for (var k = state + 1; k >= 0; k-=1) {
    if (cheatCode.substr(0, k) == prefix.substr(prefix.length - k, k)) {
      state = k;
      break;
    }
  }

  if (state == cheatCode.length) {
    sci_flag = 2;
    redrawAll();
    iClear();
    iRefresh();
  }
}

function keyTrap() {
  var pc = event.keyCode;
  var c = String_fromCharCode(pc);

  transistState(c);

  if (sci_flag == 2) {
    if (c == '\r' || c == ' ') {
      if (checkValid(display)) {
        var p = convertMath(display);
        expression = [];
        expression.push(eval(p));
      } else {
        alert(EXPR_RULES);
      }
    } else if (pc == 27) {
      iClear();
    } else if (pc == 8) {
      if (display.length)
        display = display.substr(0, display.length - 1);
    } else {
      display += c;
    }
  } else {
    if (c == '\r' || c == ' ')
      c = '=';
    if (c >= '0' && c <= '9' || c == '.')
      handleDigit(c);

    if (c == '+' || c == '-' || c == '=' || c == '*' || c == '/' || c == '^') {
      handleOperator(c);
    } else if (c == '%') {
      percentOf();
    } else if (c == '(' || c == ')') {
      braces(c == ')');
    } else if (c == '!') {
      unaryFact();
    } else if (pc == 27) {
      iClear();
    } else if (pc == 8) {
      backSpace();
    }
  }

  iRefresh();
}

function backSpace() {
  if (!dEditable)
    return;

  if (dValue)
    display = getNumber(display);
  dValue = 0;

  if (display.length)
    display = display.substr(0, display.length - 1);

  if (!display.length) {
	  display = '0';
	  dExists = dEditable = 0;
  }

  iRefresh();
}

function changeSign() {
  if (!dValue)
    display = getValue(display);
  dValue = 1;
  display = -display;
  dEditable = 0;
  iRefresh();
}

function dmsFunction() {
  if (base == 10) {
    var a; var d;
    if (!dValue)
      display = getValue(display);
    d = display;
    a = parseInt(d);
    d -= a;

    if (invFlag.value) {
      d *= 10000;
      var brak = parseInt(d / 100);
      a += brak / 60.0;
      a += parseInt(d % 100) / 3600.0;
      display = a;
    } else {
      d *= 3600;
      a += parseInt(d / 60) * .01;
      a += (d % 60) * .0001;
      display = a;
    }
    display = getNumber(display);
    dValue = 0;
  }
  invFlag.value = hypFlag.value = 0;
}

function unaryExp() {
  if (dExists) {
    if (!dValue)
      display = getValue(display);
    dValue = 1;

    if (!invFlag.value) {
      display = Math.exp(display);
    } else if (display>0) {
      display = Math.log(display);
    } else {
      alert(E_EXP_DOMAIN);
      iClear();
    }

    dEditable = 0;
  }

  invFlag.value = hypFlag.value = 0;
}

function unaryLog() {
  if (!dValue)
    display = getValue(display);
  dValue = 1;

  if (!invFlag.value) {
    invFlag.value = 1;
    unaryExp();
    display /= Math.log(10);
  } else {
    display = Math.pow(10, display);
  }

  invFlag.value = hypFlag.value = 0;
}

function addE() {
  if (dEditable && dExists&&base == 10) {
    if (dValue)
      display = getNumber(display);
    dValue = 0;

    var flag = 0;
    for (var i = 0; i < display.length; ++i)
      if (display.substr(i, 1) == 'E')
        flag = 1;

    if (!flag)
      display += 'E';
  }

  iRefresh();
}

function mSinh(x) {
  return (Math.exp(x) - Math.exp(-x)) / 2;
}

function mCosh(x) {
  return (Math.exp(x) - Math.exp(-x)) / 2;
}

function mTanh(x) {
  return mSinh(x)/mCosh(x);
}

function iSinh(x) {
  return Math.log(x + Math.sqrt(x * x + 1));
}

function iCosh(x) {
  return Math.log(x + Math.sqrt(x * x - 1));
}

function iTanh(x) {
  return .5 * Math.log((1 + x) / (1 - x));
}

function unarySin() {
  if (!dExists)
    return;
  if (!dValue)
    display = getValue(display);
  dValue = 1;

  var aref = angleRef;
  var inv = invFlag.value;
  var hyp = hypFlag.value;
  if (!inv)
    toRadians();

  if (inv && hyp) {
    display = iSinh(display);
  } else if (inv && !hyp) {
    if (Math.abs(display) > 1) {
      alert(E_SINCOS_DOMAIN);
    } else {
      display = Math.asin(display);
    }
  } else if (hyp) {
    display = mSinh(display);
  } else {
    display = Math.sin(display);
  }

  if (inv) {
     angleRef = 1;
     toReference(aref);
  } else {
    angleRef = aref;
  }

  invFlag.value = hypFlag.value = 0;
  dEditable = 0;
}

function unaryCos() {
  if (!dExists)
    return;

  if (!dValue)
    display = getValue(display);
  dValue = 1;

  var aref = angleRef; var inv = invFlag.value; var hyp = hypFlag.value;
  if (!inv)
    toRadians();

  if (inv && hyp) {
    display = iCosh(display);
  } else if (inv && !hyp) {
    if (Math.abs(display) > 1) {
      alert(E_SINCOS_DOMAIN);
    } else {
      display = Math.acos(display);
    }
  } else if (hyp) {
    display = mCosh(display);
  } else {
    display = Math.cos(display);
  }

  if (inv) {
    angleRef = 1;
    toReference(aref);
  } else {
    angleRef = aref;
  }

  invFlag.value = hypFlag.value = 0;
  dEditable = 0;
}

function unaryTan() {
  if (!dExists)
    return;

  if (!dValue)
    display = getValue(display);
  dValue = 1;

  var aref = angleRef; var inv = invFlag.value; var hyp = hypFlag.value;
  if (!inv)
    toRadians();

  if (inv && hyp) {
    display = iTanh(display);
  } else if (inv && !hyp) {
    display = Math.atan(display);
  } else if (hyp) {
    display = mTanh(display);
  } else {
    display = Math.tan(display);
  }

  if (inv) {
    angleRef = 1;
    toReference(aref);
  } else {
    angleRef = aref;
  }

  invFlag.value = hypFlag.value = 0;

  if (display > 1e15) {
	  alert(E_RORANGE);
	  iClear();
  }

  dEditable = 0;
}

function unaryCube() {
  if (!dExists)
    return;

  if (!dValue)
    display = getValue(display);
  dValue = 1;

  if (!invFlag.value) {
    display = display * display * display;
  } else {
    display = Math.pow(display, 1.0 / 3);
  }

  invFlag.value = hypFlag.value = 0;
  dEditable = 0;
}

function unaryFact() {
  if (!dExists)
    return;

  if (!dValue)
    display = getValue(display);
  dValue = 1;

  if (!invFlag.value) {
    if (display < 0) {
      alert(E_FACT_DOMAIN);
    } else if (display > 100) {
      alert(E_RLARGE);
    } else {
      var b = 1; var i = 1;
      for (;i <= display; ++i)
        b *= i;
      display = b;
    }
  } else if (display > 0){
    var b = 1; var i = 1;
    while (b < display) {
      b *= i++;
    }
    display = i;
  } else {
    alert(E_DOMAIN);
  }
  invFlag.value = hypFlag.value = 0;
  dEditable = 0;
}

function unarySquare() {
  if (!dExists)
    return;

  if (!dValue)
    display = getValue(display);
  dValue = 1;

  if (!invFlag.value) {
    display*=display;
  } else if (display >= 0) {
    display = Math.sqrt(display);
  } else {
    alert(E_SQRT_DOMAIN);
  }

  invFlag.value = hypFlag.value = 0;
  dEditable = 0;
}

function unaryInverse() {
  if (!dExists)
    return;

  if (!dValue)
    display = getValue(display);
  dValue = 1;

  //alert(display);
  if (display) {
    display = 1 / display;
  } else {
    alert(E_INV_DOMAIN);
  }

  invFlag.value = hypFlag.value = 0;
  dEditable = 0;
}

function baseButtons() {
  if (base == 10) {
    b_dms.image = "b_dms_u.png";
    b_dms.overImage = "b_dms_h.png";
    b_dms.downImage = "b_dms_d.png";
    b_exp.image = "b_exp_u.png";
    b_exp.overImage = "b_exp_h.png";
    b_exp.downImage = "b_exp_d.png";
  } else {
    b_dms.image = "b_dms_dis.png";
    b_dms.overImage = "b_dms_dis.png";
    b_dms.downImage = "b_dms_dis.png";
    b_exp.image = "b_exp_dis.png";
    b_exp.overImage = "b_exp_dis.png";
    b_exp.downImage = "b_exp_dis.png";
  }
}

function percentOf() {
  if (oprFlag && dExists) {
    display = display * ans * .01;
    dEditable = 0;
    dExists = 1;
    iRefresh();
  }
}

function calculate(a,opr,b) {
  if (opr == '%' || opr == '/') {
    if (b == 0) {
      alert(E_DZERO);
      return 0;
    }
  }

  var r = 0;
  if (opr == '+' || opr == '-' || opr == '/' || opr == '*' || opr == '%')  {
    r =  eval("(" + a + ")" + opr + "(" + b + ")");
  } else if (opr == '^') {
    r =  Math.pow(a,b);
  } else if (opr == 'N') {
    r =  parseInt(a + .5) & parseInt(b + .5);
  } else if (opr == '|') {
    r =  parseInt(a + .5) | parseInt(b + .5);
  } else if (opr == 'X') {
    r =  parseInt(a + .5) ^ parseInt(b + .5);
  } else if (opr == 'L') {
    r =  parseInt(a + .5) << parseInt(b + .5);
  } else if (opr == '!') {
    r =  parseInt(a + .5)  !=  parseInt(b + .5);
  } else {
    alert(E_SYSTEM);
  }

  return r;
}

function unaryNot() {
  if (!dValue) {
    dValue = 1;
    display = getValue(display);
  }

  if (!display) {
    display = 1;
  } else {
    var bb = base;
    base = 2;
    display = getNumber(display);
    dValue = 0;

    for (var i = 0; i < display.length; ++i) {
      var c = display.charAt(i);
      var cc =  (c < 10) ? display.charCodeAt(i) - "0".charCodeAt(0) :
        display.charCodeAt(i) - "A".charCodeAt(0) + 10;
      cc = base - 1 - cc;
      cc = (cc < 10) ? cc + "0".charCodeAt(0) : cc + "A".chatCodeAt(0) - 10;
      display = display.substr(0, i) + String_fromCharCode(cc) + display.substr(i + 1);
    }

    display = getValue(display);
    base = bb;
  }

  dValue = 1;
  iRefresh();
}

//moved down to here cause of strange function stmt things.
redrawAll();
plugin.onAddCustomMenuItems = menuAddItems;

