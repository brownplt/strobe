//Currency converter, from google gadgets.
//formatted so i can look at it beter.
function Currency() /*: constructor -> {rates : {AED : Double, ARS : Double, AUD : Double, BDT : Double, BGN : Double, BHD : Double, BND : Double, BRL : Double, BWP : Double, CAD : Double, CHF : Double, CLP : Double, CNY : Double, COP : Double, CYP : Double, CZK : Double, DKK : Double, EEK : Double, EGP : Double, EUR : Double, GBP : Double, HKD : Double, HRK : Double, HUF : Double, IDR : Double, ILS : Double, INR : Double, IRR : Double, ISK : Double, JMD : Double, JPY : Double, KRW : Double, KWD : Double, LKR : Double, LTL : Double, LVL : Double, LYD : Double, MTL : Double, MUR : Double, MVR : Double, MXN : Double, MYR : Double, NOK : Double, NPR : Double, NZD : Double, OMR : Double, PEN : Double, PHP : Double, PKR : Double, PLN : Double, QAR : Double, RON : Double, RUB : Double, SAR : Double, SEK : Double, SGD : Double, SIT : Double, SKK : Double, THB : Double, TND : Double, TRY : Double, TTD : Double, TWD : Double, UAH : Double, USD : Double, 
                                                 VEB : Double, XAG : Double, XAL : Double, XAU : Double, XCD : Double, XPT : Double, ZAR : Double, FOO : Int}, konq : Bool} */ {
    this.rates={'AED':5.046500,'ARS':5.265700,'AUD':1.576500,'BDT':94.768100,'BGN':1.955300,'BHD':0.518100,'BND':1.944100,'BRL':2.566700,'BWP':9.482000,'CAD':1.463300,'CHF':1.470800,'CLP':744.337000,'CNY':9.380000,'COP':2697.749000,'CYP':0.000000,'CZK':26.105000,'DKK':7.446100,'EEK':15.696400,'EGP':7.518900,'EUR':1.000000,'GBP':0.873600,'HKD':10.676600,'HRK':7.324500,'HUF':274.762300,'IDR':12894.512700,'ILS':5.141200,'INR':64.149700,'IRR':13602.096700,'ISK':176.937200,'JMD':122.597500,'JPY':123.085200,'KRW':1574.683100,'KWD':0.396400,'LKR':157.454600,'LTL':3.446600,'LVL':0.711200,'LYD':1.723300,'MTL':0.000000,'MUR':41.630700,'MVR':17.586500,'MXN':18.013800,'MYR':4.749800,'NOK':8.194300,'NPR':102.908800,'NZD':1.984000,'OMR':0.528800,'PEN':3.963800,'PHP':64.129100,'PKR':116.785700,'PLN':4.063500,'QAR':5.000800,'RON':4.139400,'RUB':41.702100,'SAR':5.152700,'SEK':10.202300,'SGD':1.950200,'SIT':239.640000,'SKK':29.640200,'THB':45.642600,'TND':1.884400,'TRY':2.081500,'TTD':8.765800,'TWD':44.111200,'UAH':11.025900,'USD':1.373900,'VEB':0.000000,'XAG':0.103000,'XAL':0.000000,'XAU':0.001300,'XCD':3.592900,'XPT':0.001200,'ZAR':10.566600,'FOO':1};
    this.konq=(navigator.userAgent.indexOf('Konqueror')>=0);
}

Currency.prototype.convert=function() /*: -> Void */ {
    var from=document.getElementById('from').value;
    var to=document.getElementById('to').value;
    //var rate=this.rates[to]/this.rates[from];
    var rate=1.0;
    var re=document.getElementById('result');
    var val=document.getElementById('value').value;
    if(val.length===0){
      re.value='';}
    else{
      re.value=this.format(val*rate);}
};
			
Currency.prototype.format = function(num) /*: Double -> String */ { 
			  num = (num.toString()).replace(/\'/g,'');
			  if(isNaN(num)){num="0";}
			  var sign=(num==(num=Math.abs(num)));
			  num=Math.floor(num*100+0.50000000001);
			  var cents=num%100;
			  num=Math.floor(num/100).toString();
			  if(cents<10){cents="0"+cents;}
			  for(var i=0;i<Math.floor((num.length-(1+i))/3);i++)
			  {num=num.substring(0,num.length-(4*i+3))+","+num.substring(num.length-(4*i+3));}
			  return((sign?'':'-')+num+'.'+cents);
};
			
Currency.prototype.label=function(el,sl) /*: String * Dom -> Void */ {
var txt=document.createTextNode(sl.value);var e=document.getElementById(el);e.replaceChild(txt,e.firstChild);
};

var cnvtr=new Currency();

function swapFromTo() /*: -> Void */ {
var tmp=document.getElementById('from').value;document.getElementById('from').value=document.getElementById('to').value;document.getElementById('to').value=tmp;cnvtr.label('from_s',document.getElementById('from'));cnvtr.label('to_s',document.getElementById('to'));}
