/*::
rec type jQueryInst = {(Str -> jQueryInst) & (jQueryInst -> jQueryInst) & (Str * jQueryInst -> jQueryInst) with
  fn : jQueryInst,
  prototype : jQueryInst,
  init : (Str * jQueryInst -> jQueryInst),
  parent : -> jQueryInst,
  not : Str -> jQueryInst,
  find : Str -> jQueryInst,
  html : (Undef -> Str) & (Str -> jQueryInst),
  css : (Ext -> jQueryInst) & (Str * Str -> jQueryInst),
  noConflict : -> Undef,
  each : (Ext * Str -> Undef) -> Undef,
  get : Str * Ext * (Str -> Undef) * Str -> Undef,
  children : -> jQueryInst,
  clone : -> jQueryInst,
  append : jQueryInst -> jQueryInst,
  eq : Num -> jQueryInst,
  after : (jQueryInst + Str) -> jQueryInst,
  length : Num,
  /(([0-9])*|("+Infinity"|("-Infinity"|"NaN")))/ :! jQueryInst,
};

*/
var jQuery = /*: jQueryInst */null;

/*:: type GBrowser = { nsIDOMElement with
  selectedTab : nsIDOMNode
} ; */

var gBrowser = /*: GBrowser */null;

( /*: -> Undef */ function() {
  var jsLoader = Components.classes["@mozilla.org/moz/jssubscript-loader;1"].getService(Components.interfaces.mozIJSSubScriptLoader);
  jsLoader.loadSubScript("chrome://firetracktor/content/jquery-1.2.6.min.js");

  // Init jQuery
  jQuery.noConflict();

  var $ = /*: cheat jQueryInst */ function(selector,context) {
    return new jQuery.fn.init(selector,context||launchpad.doc);
  };

  $.fn = jQuery.fn;
  $.prototype = jQuery.fn;

  /*:: type LaunchPad = rec l . {AnObject with
  host : Str,
  asin : Str,
  doc : nsIDOMDocument,
  log : [this('l)] Str -> Undef,
  priceAsFloat : [this('l)] Str -> Num,
  setItem : [this('l)] Str * Str -> Undef,
  getItem : [this('l)] Str -> Str,
  run : [this('l)] nsIDOMDocument * nsIDOMEvent -> Undef,
  requestAvailable : [this('l)] Str * Ext -> Undef,
  responseAvailable : [this('l)] Str -> Undef,
  } ; */
  var launchpad = /*: LaunchPad */null;

  launchpad.host = "http://thetracktor.com";
	//launchpad.host = "http://127.0.0.1:8000";

  launchpad.log = function(text) {
    dump(text + "\n");
  };
  launchpad.priceAsFloat = function (price) {  
     return parseFloat(price.replace(/,/g,'').replace(/[^\d\.]/g,''));
  };
  launchpad.setItem = function( key, value){
    var ss = Components.classes["@mozilla.org/browser/sessionstore;1"]
                        .getService(Components.interfaces.nsISessionStore);
     var currentTab = gBrowser.selectedTab;
     ss.setTabValue(currentTab, key, value);
  };
  launchpad.getItem = function(key){

    var ss = Components.classes["@mozilla.org/browser/sessionstore;1"]
                       .getService(Components.interfaces.nsISessionStore);
    var currentTab = gBrowser.selectedTab;
    var retrievedData = ss.getTabValue(currentTab, key);
    return retrievedData;

  };
  launchpad.run = function(doc, ev) {
    // is this amazon?  get asin if so
  	var loc = doc.location.href;
  	var matches = loc.match(/(?:dp|product)\/([^\/?]+).*$/);
  	
  	var m = loc.match(/^http:\/\/www\.amazon\.com\/.*\/dp\/([^\/?]+).*$/);
  	if(!m) {
  	    // try the product method
  	    m = loc.match(/^http:\/\/www\.amazon\.com\/.*\/product\/([^\/?]+).*$/);
  	}

  	if(!m) {
  	    return;
  	}

  
  	var asin = m[1];
    launchpad.asin = asin;
    
  	// setup
  	//this.win = aEvent.target.defaultView.wrappedJSObject;
  	this.doc = doc;
    
    // get amazon, new and used prices.
  	var price_dict = /*: Ext */{};
    // launchpad.log("Getting prices")
  	// Find all price boxes
  	$(".olpCondLink .price, b.priceLarge").not('#buyboxusedDivId b.priceLarge').each( /*: Ext * Str -> Undef */function(i, o){
        var price = launchpad.priceAsFloat( $(o).html());    
      
      // launchpad.log(price);
      
    	var surround_text = $(o).parent().html();
    	var type = null;
    	var is_amazon = /Amazon/gi;
      var is_new = /New/gi;
      var is_used = /Used/gi;

      if(is_amazon.exec(surround_text) != null)
    	  price_dict["Amazon"] = {"price": price};
      else if( is_new.exec(surround_text) != null)
    	  price_dict["New"] = {"price": price};
      else if( is_used.exec(surround_text) != null)
    	  price_dict["Used"] = {"price": price};      

  	});

    // If no amazon price, this is a third party item.  Get new price + shipping
  	if( !price_dict["Amazon"]){
    	$("#pricePlusShippingQty .price").each( function(i, o){

      	var price = launchpad.priceAsFloat( $(o).html() );    

        var shipping_span = $(o).parent().find( "span:contains(shipping)");    
      	var shipping = launchpad.priceAsFloat( shipping_span.html() );

    	  price_dict["New"] = {"price": price, "shipping": shipping};

    	});
    }
    // launchpad.log("Ready to ajax");
    
    // check cache
    var cached_message = launchpad.getItem(asin);
    if(cached_message)
      this.responseAvailable( cached_message );
    else
      this.requestAvailable(asin, price_dict);
  };
  
  launchpad.requestAvailable = function( asin, price_dict) {
    // This has to be jQuery not "$"
	  // launchpad.log("req");
    jQuery.get(launchpad.host + "/ajax/available/" + asin + "/",  {"prices": JSON.stringify(price_dict) }, launchpad.responseAvailable, "json");
  };
  
  launchpad.responseAvailable = function( data ) {
    // launchpad.log("resp");
  	// launchpad.log(data);
    
    launchpad.setItem( launchpad.asin, data);
		

    // expand the options box if it exists.  If not there is some ugly overlap
  	$("#price-and-olp-condition-link-outer").css("height", "100%");
    // launchpad.log(1);

  	// now try to find a good place to splice into the page
  	//var idealSpot = $("div.buying > table.product");
  	//var idealSpot = $("div.buying ~ table > tbody > tr");
    var idealSpot = $("form#handleBuy > table:last > tbody > tr");

    if(idealSpot.length < 3)
      idealSpot = $( idealSpot[ idealSpot.length-1 ] );
    else
      idealSpot = $( idealSpot[2] );

  	var iframe = "<iframe src='" + launchpad.host + "/plot/" + launchpad.asin +
  	   "/' style='width:100%;height:280px' frameborder='0' scrolling='no'/>";
    if( data )
    	var tracktor_plot_row = "<tr><td colspan='2'>" + iframe + "</td></tr>";
  	else
  		var tracktor_plot_row = ("<tr><td colspan='2' style='padding:1px 0px 4px 0px'>"
  			+ "No price history available from <a href='http://thetracktor.com'>The Tracktor</a>.<br/>When possible, we will add this product to our listing and track its price history."
  			+ "</td></tr>");

  	idealSpot.after( tracktor_plot_row );

  	// Side bar track box
  	if( data ){
  		// yank the buy box and reinsert
  		// yank the buy box and reinsert
  		var buybox = $("<div></div>").append($("#buyboxDivId").children().clone()).css("margin-bottom", "5px");

  		// inject our content
  		var sidebar_iframe = "<iframe class='tracktor' src='" + launchpad.host + "/track_form/" + launchpad.asin +
  		   "/' style='width:100%; height:190px' frameborder='0' scrolling='no'/>";

  		// Side bar
    $("div.buyTopBox > .cBoxInner, .topLeft #buyBoxContent", buybox).html("<center onClick='window.open(\"http://thetracktor.com/detail/"+launchpad.asin+"/\")'>Price Notification<br/>powered by <span style=\"cursor:pointer; text-decoration:underline;\">The Tracktor</span></center>").css({"font-size": "11px", "font-weight": "bold", "padding":"4px 0px 5px"});

  		// Set buy box under wishlist box
  		$("div.buyBottomBox", buybox).css({"z-index": 0});

  		// Enter content
  		$("div.buyBottomBox > .cBoxInner, .bottomLeft #buyBoxContent", buybox).html( sidebar_iframe );
      $('.buyBox table', buybox).css({"padding-left":"10px", "padding-right":"5px"});
  		// build up the table row
  		var newcontent = $("<td></tr>").append(buybox);
  		var row = $("<tr></tr>").append(newcontent);

  		// put it after their buy box
  		var rows = $("table.buyingDetailsGrid > tbody > tr").eq(2).after(row);

  	}
  };

  var delay = /*: [nsIDOMEventTarget] nsIDOMEvent -> Undef */ function (aEvent) {
	  var doc = /*: cheat nsIDOMDocument */(aEvent.originalTarget);
          setTimeout(function() { launchpad.run(doc, aEvent); }, 1);
  };

  var load = /*: [Window] -> Undef */function() {
    gBrowser.addEventListener("DOMContentLoaded", delay, true);
  };

  window.addEventListener("pageshow", load, false);

})();
