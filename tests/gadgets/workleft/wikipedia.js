var assert = function(cond) /*: Bool -> Void */ {
  if (!cond) {
    throw 0;
  }
};

// ****************************** simple_http_query.js ***********************

// A general function to simplify downloading data from a website and also
// handle errors gracefully.
function SimpleHTTPRequest() /*: constructor (-> {request : (String * (Null + String -> Void) * (Boolean + Void) -> Void), stop : ( -> Any)}) */ {
  this.request = function(url, receivedResultCallback, opt_getStream) /*: String * (String -> Void) * (Boolean + Void) -> Void */ {};
  this.stop = function() /*:  -> Any */ {};
}

// ****************************** throbber.js ***********************

// Creates a throbber with elements. A throbber is a 'spinning thing' that
// normally symbolizes waiting. Firefox has one in the top right animated when
// loading a webpage.
function Throbber(ownerDiv, imagePrefix, imageSuffix, totalFrames, frameDelay) /*: constructor (Div * String * String * Int * Int -> {show : ( -> Void), hide : ( -> Void)}) */ {
  this.show = function() /*: -> Void */ {};
  this.hide = function() /*: -> Void */ {};
  var imagePrefix_ = imagePrefix;
  var imageSuffix_ = imageSuffix;
  var totalFrames_ = totalFrames;
  var frameDelay_ = frameDelay;

  var element_ = /*:downcast Img*/(ownerDiv.appendElement("<img visible=\"false\" />"));
  var curFrame_ = 0;
  var timer_ = /*:upcast Null + Int*/null;

  // Returns a frame's filename
  function getImageFilename(frame) /*: Int -> String */ {
    return imagePrefix_ + frame + imageSuffix_;
  }

  // Returns the next frame number in the animation
  function getNextFrame() /*:  -> Int */ {
    curFrame_++;
    if (curFrame_ == totalFrames_ + 1)
      curFrame_ = 1;

    return curFrame_;
  }

  // Show the throbber and animate it
  function show() /*:  -> Void */ {
    curFrame_ = 0;
    element_.visible = true;

    // Start the animation if it is not already running
    if (timer_ == null) {
      timer_ = setInterval(onAnimate, frameDelay_);
      onAnimate();
    }
  }

  // Hide the throbber
  function hide() /*:  -> Void */ {
    element_.visible = false;

    // Stop the animation only if it is running
    if (typeof timer_ === "number") { //timer_ != null) {
      clearInterval(timer_);
      timer_ = null;
    }
  }

  // Called whenever a frame needs to be drawn
  function onAnimate() /*:  -> Void */ {
    element_.src = getImageFilename(getNextFrame());
  }

  this.show = show;
  this.hide = hide;
}



// ****************************** wikipedia_query.js ************

// This class will query Google for Wikipedia pages. It uses the Google cached
// pages to scrape from since the Wikipedia official servers are known to be
// very slow. Also, this makes it easier for us to control the server load.
function WikipediaQuery() /*: constructor ( -> {query : (String * (String * Null * String -> Void) -> (String + Void)), stop : ( -> Any), getDisambiguationArray : ( -> Any)}) */ {

  this.query = function(name, receivedResultCallback) /*: String * (String * Null * String -> Void) -> (String + Void) */ { return "tmp"; };
  this.stop = function() /*: -> Any */ {};
  this.getDisambiguationArray = function() /*: -> Any */ {};

  var QUERY_URL =
    //"http://www.google.com/search?q=cache%3Aen.wikipedia.org%2Fwiki%2F";
    //"http://en.wikipedia.org/wiki/";
    "http://en.wikipedia.org/wiki/Special:Search/";
  var FIRST_PARAGRAPH_REGEX = /<p>(.+)\s*<\/p>/g;
  var IMAGE_URL_REGEX = /img src=\"((?:http\:\/\/upload\.wikimedia\.org)(?:.*?))(\")/g;
  var ARTICLE_NOT_FOUND_REGEX = /^Wikipedia does not have an article with this exact name./;
  var DISAMBIGUATION_PAGE_REGEX = /href=\"\/wiki\/Wikipedia\:Disambiguation/;
  var DISAMBIGUATION_TITLES_REGEX = /<a href=\".*?\" title=\"(.*?)\"./g;
  var ACTION_EDIT_SEARCH = "ACTION=EDIT";
  var MULTIPLE_ARTICLES = "It has been suggested that this article be split into multiple articles accessible";
  var TEMPLATE = "/wiki/Template:";
  var MAIN_PAGE_TITLE = "<title>Main Page - Wikipedia, the free encyclopedia</title>";
  var OPEN_TABLE_TEXT = "<table class=\"messagebox";
  var CLOSE_TABLE_TEXT = "</table";
  var BAD_IMAGES = ["WIKI_LETTER_W.PNG", "LINKFA-STAR.PNG"];

  var MINIMUM_PARAGRAPH_LENGTH = 15;

  var receivedResultCallback_ = /*:upcast Null + (String * Null * String -> Void)*/null; // (paragraph, imageURL, articleURL)
  var request_ = null;
  var articleURL_ = null;

  var pageText_ = null;
  var originalQuery_ = /*:upcast Null + String*/null;

  function query(name, receivedResultCallback) /*: String * (String * Null * String -> Void) -> (String + Void) */ {
    assert(request_ == null);
    if (request_ != null)
      return;
    assert(receivedResultCallback != null);
    if (receivedResultCallback == null)
      return;

    receivedResultCallback_ = receivedResultCallback;
    originalQuery_ = name;

    // Create a clean and safe query name
    var safeName = originalQuery_.replace(/ /g, "_");
    safeName = encodeURI(safeName);

    request_ = new SimpleHTTPRequest();
    request_.request(QUERY_URL + safeName, onReceivedWebpage);
    articleURL_ = QUERY_URL + safeName;

    return articleURL_;
  }

  function onReceivedWebpage(text) /*: String -> Void */ {
    request_ = null;
    pageText_ = text;

    // Verify that the request was successful
    if (pageText_ == null) {
      receivedResultCallback_(null, null, null);
      return;
    }

    // Verify that the page is valid and not a return to the main page
    if (pageText_.indexOf(MAIN_PAGE_TITLE) != -1) {
      receivedResultCallback_(null, null, null);
      return;
    }

    // Check if the text "It has been suggested that this article be split into
    // multiple articles accessible" exists. This means that there is an
    // ambiguation link on the page which will improperly trigger the
    // the disambiguation mode.
    var notDisambiguation = false;
    if (pageText_.indexOf(MULTIPLE_ARTICLES) != -1)
      notDisambiguation = true;

    // Check if this is a disambiguation page. Disambiguation pages have a line
    // at the bottom of the page which mentions that the page is intended for
    // disambiguation.
    if (!notDisambiguation) {
      var disambiguationRegex = DISAMBIGUATION_PAGE_REGEX.exec(pageText_);
      if ((disambiguationRegex != null) && (disambiguationRegex.length == 1)) {
        receivedResultCallback_("", null, null);
        return;
      }
    }

    // Get the first paragraph's text
    var firstParagraph = getArticleFirstParagraph();

    // Check if no suitable first paragraph was found
    if (firstParagraph == "") {
      receivedResultCallback_(null, null, null);
      return;
    }

    // Check if this is a real article of if it's a 'not found' page
    var notFoundRegex = ARTICLE_NOT_FOUND_REGEX.exec(firstParagraph);
    if ((notFoundRegex != null) && (notFoundRegex.length == 1)) {
      receivedResultCallback_(null, null, null);
      return;
    }

    // Get the image URL
    var imageURL = getFirstImageURL();

    receivedResultCallback_(firstParagraph, imageURL, articleURL_);
  }

  // Parses the page text that should already be set in pageText_ and retrieves
  // the text of the first paragraph.
  function getArticleFirstParagraph() /*:  -> String */ {
    // Loop through all paragraph tags in the article until one is found that is
    // suitable for the description
    var result = null;
    var firstParagraph = "";

    while ((result = FIRST_PARAGRAPH_REGEX.exec(pageText_)) != null) {
      if (result.length >= 2) {
        // Check if there is a <td> tag before this <p> tag. This strategic
        // search is very often successful in finding improper <p> tags put in
        // tables which is never the actual first paragraph.
        var openTagLoc = pageText_.substr(0, result.index).lastIndexOf("<");

        if (openTagLoc >= -1) {
          // Check if there is a template link in the potential paragraph. If
          // there is, this is likely not the proper first paragraph.
          if (result[1].indexOf(TEMPLATE) == -1) {
            var tdCheckText = pageText_.substr(openTagLoc,
              result.index - openTagLoc).toUpperCase();

            // Only continue if the TD tag was not found
            if (tdCheckText.indexOf("<TD") == -1) {
              // Take the current text between <p> and </p> tags
              var plainText = htmlToPlainText(result[1]);
              plainText = plainText.replace(/^\s+/g, "");  // Strip leading spaces
              plainText = plainText.replace(/\s+$/g, "");  // Strip trailing spaces

              // Check for minimum length
              if (plainText.length >= MINIMUM_PARAGRAPH_LENGTH) {
                firstParagraph = plainText;
                return firstParagraph;
              }
            }
          }
        }
      }
    }

    return "";
  }

  // Parses the page text to find the first image's URL
  function getFirstImageURL() /*:  -> Null */ {
    var result = null;
    var imageURL = "";

    while ((result = IMAGE_URL_REGEX.exec(pageText_)) != null) {
      if (result.length >= 2) {
        // We have a location for a picture but one problem can happen. If the
        // picture is within a messagebox table we're not interested in it since
        // this usually just says "This article is locked to prevent inaccurate
        // data" and just has a picture of a lock (which is not the actual image
        // that we want). Therefore, we check to make sure we're not in a table.
        var lastCloseTableLoc = pageText_.lastIndexOf(CLOSE_TABLE_TEXT, result.index);
        var lastOpenTableLoc = pageText_.lastIndexOf(OPEN_TABLE_TEXT, result.index);

        if (lastCloseTableLoc >= lastOpenTableLoc) {
          // The closest tag is a </table>, which means this image is not
          // already inside a table -- therefore take the URL.
          var URL = result[1];

          // Do not take the URL if it has been blacklisted
          var bad = false;
          for (var i = 0; ((i <= BAD_IMAGES.length - 1) && (!bad)); i++) {
            if (URL.toUpperCase().indexOf(BAD_IMAGES[i]) > -1)
              bad = true;
          }

          if (!bad)
            return result[1];
        }
      }
    }

    return null;
  }

  // Stop a query, if it exists
  function stop() /*:  -> Any */ {
    if (request_ != null)
      request_.stop();
  }

  // Get an array of article titles of articles similar to the last query name
  function getDisambiguationArray() /*:  -> Any */ {
    var articleTitles = [];

    var result = null;
    var upperQuery = originalQuery_.toUpperCase();

    while ((result = DISAMBIGUATION_TITLES_REGEX.exec(pageText_)) != null) {
      if (result.length >= 2) {
        // If there is a colon in the title, this means it is a special page,
        // therefore do not parse
        if (result[1].indexOf(":") == -1) {
          // Do not accept the item if it has "action=edit" in the url. This
          // means the link is a placeholder and does not actually contain any
          // useful information
          if (result[0].toUpperCase().indexOf(ACTION_EDIT_SEARCH) == -1) {
            // Look for the original query in the title of this article
            if (result[1].toUpperCase().indexOf(upperQuery) != -1) {
              // Since found, add this to the list of valid articles if not
              // already on the list
              var found = false;
              var upperResult = htmlToPlainText(result[1].toUpperCase());

              for (var i = 0; i <= articleTitles.length - 1; i++) {
                if (articleTitles[i].toUpperCase() == upperResult) {
                  found = true;
                  break;
                }
              }

              if (!found)
                articleTitles.push(htmlToPlainText(result[1]));
            }
          }
        }
      }
    }

    return articleTitles.sort();
  }

  // A simple function to get only the text from a webpage. Some small
  // conversions are done to convert some html elements to symbols.
  function htmlToPlainText(item) /*: String -> String */ {
    if (item) {
      // Remove html tags
      item = item.replace(/<([^>]|\n)*>/g, '');

      // Convert symbols
      item = item.replace(/&nbsp;/g, ' ');
      item = item.replace(/&quot;/g, '"');
      item = item.replace(/&amp;/g, '&');
      item = item.replace(/&lt;/g, '<');
      item = item.replace(/&gt;/g, '>');
      item = item.replace(/&#160;/g, ' ');
      return item;
    } else {
      return "";
    }
  }

  this.query = query;
  this.stop = stop;
  this.getDisambiguationArray = getDisambiguationArray;
}


// ****************************** listbox.js ****************************

var AREA_IMAGE = "images/results_default.png";
var AREA_LEFT_BORDER = 3;
var AREA_TOP_BORDER = 3;
var AREA_BOTTOM_BORDER = 3;
var AREA_RIGHT_BORDER = 3;
var ITEM_HEIGHT = 15;
var ITEM_TEXT_SIZE = 9;
var ITEM_TEXT_FONT = "Geneva";
var ITEM_SELECTOR_COLOR = "#000000";
var ITEM_SELECTOR_OPACITY = "50";
var ITEM_HOVER_SELECTOR_COLOR = "#000000";
var ITEM_HOVER_SELECTOR_OPACITY = "20";

// This class creates and shows a ListBox which can be used to display multiple
// items. Scrolling is implemented too so feel free to overpopulate the list.
function ListBox(parentDivElement_, acceptedCallback_, cancelledCallback_) /*: Any * Any * Any -> Any */ {
  var shown_ = false;
  var divElement_ = null;
  var backgroundElement_ = null;
  var selectorElement_ = null;
  var hoverElement_ = null;
  var items_ = [];
  var selectedItem_ = null;
  var hoveredItem_ = null;
  var firstVisibleItem_ = null;
  var totalVisibleItems_ = null;

  // Add an item to the end of the listbox
  function addItem(displayValue) /*: Any -> Any */ {
    var newItem = {}; ///XXX: new Object();
    newItem.displayValue = displayValue;
    newItem.labelElement = null;

    items_.push(newItem);
  }

  function show(selectItem) /*: Any -> Any */ {
    shown_ = true;

    // If a background image was specified, draw it at the top left
    backgroundElement_ = parentDivElement_.appendElement("<img " +
      "src=\"" + AREA_IMAGE + "\" " +
      "width=\"" + parentDivElement_.width + "\" height=\"" +
      parentDivElement_.height + "\" x=\"0\" y=\"0\" />");

    // Create the main item which will contain the items
    divElement_ = parentDivElement_.appendElement("<div " +
      "x=\"" + AREA_LEFT_BORDER + "\" y=\"" + AREA_TOP_BORDER + "\" " +
      "width=\"" + (parentDivElement_.width - AREA_RIGHT_BORDER - AREA_LEFT_BORDER - 1) + "\" " +
      "height=\"" + (parentDivElement_.height - AREA_BOTTOM_BORDER - AREA_TOP_BORDER - 1) + "\" />");

    // Create the selector picture
    selectorElement_ = divElement_.appendElement("<div " +
      "width=\"" + divElement_.width + "\" " +
      "height=\"" + ITEM_HEIGHT + "\" background=\"" + ITEM_SELECTOR_COLOR + "\" " +
      "opacity=\"" + ITEM_SELECTOR_OPACITY + "\" visible=\"false\" />");

    // Create the hovered selector picture
    hoverElement_ = divElement_.appendElement("<div " +
      "width=\"" + divElement_.width + "\" " +
      "height=\"" + ITEM_HEIGHT + "\" background=\"" + ITEM_HOVER_SELECTOR_COLOR + "\" " +
      "opacity=\"" + ITEM_HOVER_SELECTOR_OPACITY + "\" visible=\"false\" />");

    // Draw the current selection and items
    if (selectItem != null)
      selectedItem_ = selectItem;

    hoveredItem_ = null;
    firstVisibleItem_ = 0;
    totalVisibleItems_ = parseInt(divElement_.height / ITEM_HEIGHT);

    showSelectedItem();
    drawAllVisibleItems();
  }

  // Removes all elements in the main div and resets the state
  function hide() /*:  -> Any */ {
    if (!shown_)
      return;
    shown_ = false;

    parentDivElement_.removeElement(backgroundElement_);
    parentDivElement_.removeElement(divElement_);

    items_ = [];
  }

  function drawAllVisibleItems() /*:  -> Any */ {
    for (var i = 0; i <= items_.length - 1; i++) {
      // If the item exists, remove it
      if (items_[i].labelElement != null)
        divElement_.removeElement(items_[i].labelElement);

      // Draw the item in the proper position
      var drawY = getYPositionOfItem(i);
      var cleanText = items_[i].displayValue.replace(/\&/g, " ");

      if (drawY != null){
        items_[i].labelElement = divElement_.appendElement("<label " +
          "x=\"" + getXPositionOfItem(i) + "\" " +
          "y=\"" + drawY + "\" " +
          "width=\"" + divElement_.width + "\" " +
          "height=\"" + ITEM_HEIGHT + "\" " +
          "size=\"" + ITEM_TEXT_SIZE + "\" font=\"" + ITEM_TEXT_FONT + "\" " +
          "trimming=\"character-ellipsis\" enabled=\"true\">" +
          cleanText + "</label>");
        setMouseEventsForLabel(items_[i].labelElement, i);
      }
    }
  }

  // Used to set the mouse events for a certain item element
  function setMouseEventsForLabel(element, index) /*: Any * Any -> Any */ {
    element.onmouseover = function () /*:  -> Any */ { onMouseOverItem(index); };
    element.onmouseout = function () /*:  -> Any */ { onMouseOffItem(index); };
    element.onclick = function () /*:  -> Any */ { onMouseClickItem(index); };
  }

  // Called when the mouse goes over an item
  function onMouseOverItem(index) /*: Any -> Any */ {
    hoveredItem_ = index;
    showHoveredItem();
  }

  // Called when the mouse leaves an item
  function onMouseOffItem(index) /*: Any -> Any */ {
    hoveredItem_ = null;
    hideHoveredItem();
  }

  // Called when the user clicks on an item
  function onMouseClickItem(index) /*: Any -> Any */ {
    acceptedCallback_(items_[index].displayValue);
  }

  // Returns the y position of an item (if visible)
  function getYPositionOfItem(index) /*: Any -> Any */ {
    var indexPosition = index - firstVisibleItem_;

    if ((indexPosition < 0) || (indexPosition >= totalVisibleItems_))
      return null;

    return indexPosition * ITEM_HEIGHT;
  }

  // Returns the x position of an item
  function getXPositionOfItem(index) /*: Any -> Any */ {
    return 0;
  }

  function showSelectedItem() /*:  -> Any */ {
    if (selectedItem_ == null)
      return;

    // Verify bounds
    if (selectedItem_ < 0)
      selectedItem_ = 0;
    if (selectedItem_ > items_.length - 1)
      selectedItem_ = items_.length - 1;

    // Check if scroll up is needed
    if (selectedItem_ < firstVisibleItem_) {
      firstVisibleItem_ = selectedItem_;
      drawAllVisibleItems();
    }

    // Check if scroll down is needed
    if (selectedItem_ > firstVisibleItem_ + totalVisibleItems_ - 1) {
      firstVisibleItem_ = selectedItem_ - totalVisibleItems_ + 1;
      drawAllVisibleItems();
    }

    // Select the item
    selectorElement_.x = 0;
    selectorElement_.y = getYPositionOfItem(selectedItem_);
    selectorElement_.visible = true;
  }

  function showHoveredItem() /*:  -> Any */ {
    // Place the hovered selector
    hoverElement_.x = 0;
    hoverElement_.y = getYPositionOfItem(hoveredItem_);
    hoverElement_.visible = true;
  }

  function hideHoveredItem() /*:  -> Any */ {
    // Place the hovered selector
    hoverElement_.visible = false;
  }

  // Does the action associated with a certain key
  function notifyKeyPress(keyCode) /*: Any -> Any */ {
    if (!shown_)
      return false;

    if (keyCode == 38) {            // Up arrow
      selectedItem_--;
      showSelectedItem();

    } else if (keyCode == 40) {     // Down arrow
      selectedItem_++;
      showSelectedItem();

    } else if (keyCode == 33) {     // Page up
      selectedItem_ -= totalVisibleItems_ - 1;
      showSelectedItem();

    } else if (keyCode == 34) {     // Page down
      selectedItem_ += totalVisibleItems_ - 1;
      showSelectedItem();

    } else if (keyCode == 13) {     // Enter
      acceptedCallback_(items_[selectedItem_].displayValue);

    } else if (keyCode == 27) {     // Escape
      cancelledCallback_();

    } else if (keyCode == 36) {     // Home
      selectedItem_ = 0;
      showSelectedItem();

    } else if (keyCode == 35) {     // End
      selectedItem_ = items_.length - 1;
      showSelectedItem();

    } else {
      return false;
    }

    return true;
  }

  this.addItem = addItem;
  this.notifyKeyPress = notifyKeyPress;
  this.show = show;
  this.hide = hide;
}



var DEFAULT_HTML_STYLE = "<html>" +
  // Set the style (fonts, sizes, etc)
  "<style type=\"text/css\">" +
  "<!--" +
  "body,td,th {" +
  "	 font-family: Geneva, Arial, Helvetica, sans-serif;" +
  "	 font-size: 12px;" +
  "	 color: #333333;" +
  "}" +
  "body {" +
  "	 margin-left: 0px;" +
  "	 margin-top: 0px;" +
  "	 margin-right: 0px;" +
  "	 margin-bottom: 0px;" +

  // Remove scrollbar from side
  "  overflow: auto;" +
  "}" +
  "-->" +
  "</style>" +

  // Disable right click
  "<script language=\"Javascript\">" +
  "  document.oncontextmenu=new Function(\"return false\")" +
  "</script>";

// ****************************** wikipedia_history.js ************

var HISTORY_OPTIONS_NAME = "history";
var MAX_HISTORY_ENTRIES = 5;
var HISTORY_DETAILS_HTML = DEFAULT_HTML_STYLE +
  // Set the style for the list
  "<style type=\"text/css\">" +
  "#vertmenu {" +
  "  font-family: Verdana, Arial, Helvetica, sans-serif;" +
  "  font-size: 11px;" +
  "  color: #666666;" +
  "  width: 100%;" +
  "  padding: 0px;" +
  "  margin: 0px;" +
  "}" +
  "#vertmenu ul {" +
  "  list-style: none;" +
  "  margin: 0px;" +
  "  padding: 0px;" +
  "  border: none;" +
  "}" +
  "#vertmenu ul li {" +
  "  margin: 0px;" +
  "  padding: 0px;" +
  "}" +
  "#vertmenu ul li a {" +
  "  font-size: 80%;" +
  "  display: block;" +
  "  padding: 2px 0px 2px 7px;" +
  "  text-decoration: none;" +
  "  color: #008800;" +
  "  width:100%;" +
  "}" +
  "#vertmenu ul li a:hover, #vertmenu ul li a:focus {" +
  "  color: #000000;" +
  "  background-color: #eeeeee;" +
  "}" +
  "</style>" +
  "<body>" +

  // Draw the menu
  "<div id=\"vertmenu\">" +
  RECENT_SEARCHES +
  "<ul>" +
  "[MENUITEMS]";
  "</ul>" +
  "</div>" +
  "</body>";
var HISTORY_DETAILS_HTML_ITEM =
  "<li><a href=\"\" onclick=\"window.external.selectedItem('[TEXT]'); return false;\">&raquo; [TEXT]</a></li>";
var MAX_HISTORY_ITEMS = 5;

// This class takes care of tracking the wikipedia history. This includes
// storing/loading keywords and showing the history in sidebar or not
function WikipediaHistory(historyDiv_, selectedHistoryItemCallback_) /*: constructor (null * (Any -> Any) -> {show : ( -> Any), hide : ( -> Any), addHistoryItem : (String -> Void)}) */ {
  var historyListbox_ = null;
  var items_ = [];
  var inSidebar_ = (historyDiv_ == null);
  var ieDetailsView_ = null;

  // Load the history from the options
  if (options.exists(HISTORY_OPTIONS_NAME)) {
    items_ = options(HISTORY_OPTIONS_NAME).split('|');
    if ((items_.length == 1) && (items_[0] == ""))
      items_.splice(0, 1);
  }

  // Show the history box with all history items
  function show() /*:  -> Any */ {
    // If there are no items, do not show
    if (items_.length == 0)
      return;

    if (!inSidebar_) {
      // Verify that the history is not already up
      if (historyListbox_ != null)
        return;

      // Add the items to the listbox in reverse order
      historyListbox_ = new ListBox(historyDiv_, onAcceptedEntry, onCancelledEntry);
      for (var i = items_.length - 1; i >= 0; i--)
        historyListbox_.addItem(items_[i]);

      historyListbox_.show(null);

    } else {
      // Generate the list of items
      var itemsHtml = "";
      for (var i = items_.length - 1; i >= 0; i--) {
        itemsHtml += HISTORY_DETAILS_HTML_ITEM.replace(/\[TEXT\]/g, items_[i]);
      }

      // Generate the total html
      var html = HISTORY_DETAILS_HTML.replace("[MENUITEMS]", itemsHtml);

      // Show the details view
      ieDetailsView_ = new DetailsView();
      ieDetailsView_.html_content = true;
      ieDetailsView_.setContent("", undefined, html, false, 0);

      var externalObject = {}; //XXX: new Object();
      externalObject.selectedItem = onSelectedItem;
      ieDetailsView_.external = externalObject;

      pluginHelper.showDetailsView(ieDetailsView_, WIKIPEDIA_SEARCH, gddDetailsViewFlagNone,
        onHistoryDetailsViewFeedback);
    }
  }

  function onSelectedItem(itemText) /*: Any -> Any */ {
    // Callback to the parent. Note that if the parent does a hide() on this
    // object nothing will happen since the details view windows cannot be
    // closed.
    selectedHistoryItemCallback_(itemText);
  }

  // Called when the history details view of the IE gets clicked on
  function onHistoryDetailsViewFeedback(detailsViewFlags) /*: Any -> Any */ {
  }

  // Hide the history box if it's open
  function hide() /*:  -> Any */ {
    if (!inSidebar_) {
      if (historyListbox_ != null) {
        historyListbox_.hide();
        historyListbox_ = null;
      }
    }
  }

  // Called when the user accepts a selection in the listbox
  function onAcceptedEntry(text) /*: Any -> Any */ {
    hide();
    selectedHistoryItemCallback_(text);
  }

  // Called when the user cancels a selection in the listbox
  function onCancelledEntry() /*:  -> Any */ {
    hide();
  }

  // Add an item to the history options object
  function addHistoryItem(text) /*: String -> Void */ {
    text = text.replace(/\|/g, " ");

    // If the item is already the most recent, do not add it again
    if (items_.length >= 1) {
      if (items_[items_.length - 1] == text)
        return;
    }

    // If there are already the maximum amount of items, remove the oldest
    if (items_.length == MAX_HISTORY_ENTRIES)
      items_.splice(0, 1);

    items_.push(text);
    saveHistoryItems();
  }

  // Save the current items list to the history options object
  function saveHistoryItems() /*:  -> Void */ {
    //options(HISTORY_OPTIONS_NAME) = items_.join("|");
    options.putValue(HISTORY_OPTIONS_NAME, items_.join("|"));
  }

  this.show = show;
  this.hide = hide;
  this.addHistoryItem = addHistoryItem;
}


// ****************************** simple_animation.js ************************

// This class simply wraps around the framework's animation methods, but adds
// a callback for when the animation is completed
function SimpleAnimation(animateCallback_, startValue_, endValue_, duration_, completedCallback_) /*: Any * Any * Any * Any * Any -> Any */ {
  var animation_ = null;
  var timer_ = null;

  function start() /*:  -> Any */ {
    animation_ = beginAnimation(onAnimate, startValue_, endValue_, duration_);
    timer_ = setTimeout(onAnimationComplete, duration_);
  }

  function stop() /*:  -> Any */ {
    if (animation_ != null)
      view.cancelAnimation(animation_);
    if (timer_ != null)
      view.clearInterval(timer_);
  }

  function onAnimate() /*:  -> Any */ {
    // Do not animate if stopped
    if (timer_ == null)
      return;

    animateCallback_(event.value);
  }

  function onAnimationComplete() /*:  -> Any */ {
    // Kill the timer/animation
    stop();

    completedCallback_();
  }

  this.start = start;
  this.stop = stop;
}



var IMAGE_SRC = "<img src=\"[IMAGEURL]\" class=\"articleimg\">";
var ANIMATION_SLIDE_TIME = 200;
var DETAILS_IMAGE_WIDTH_MAX = 82;
var DETAILS_IMAGE_HEIGHT_MAX = 150;
var DETAILS_IMAGE_X = 260;
var DETAILS_IMAGE_Y = 11;
var DETAILS_HTML = DEFAULT_HTML_STYLE +
  "<style type=\"text/css\">" +
  "<!--" +
  ".articleimg {" +
  "  max-width: 82px;" +
  "  width:expression(this.width > 82 ? \"82px\" : this.width);" +
  "}" +
  "-->" +
  "</style>" +
  "<body>" +
  "<table border=0>" +
  "<tr><td>[ARTICLETEXT]</td><td valign=top>" +
  "[IMAGESRC]" +
  "</td></tr></table>" +
  "</body>" +
  "</html>";


var DISAMBIGUATION_DETAILS_HTML = DEFAULT_HTML_STYLE +
  // Set the style for the list
  "<style type=\"text/css\">" +
  "#vertmenu {" +
  "  font-family: Verdana, Arial, Helvetica, sans-serif;" +
  "  font-size: 11px;" +
  "  color: #666666;" +
  "  width: 100%;" +
  "  padding: 0px;" +
  "  margin: 0px;" +
  "}" +
  "#vertmenu ul {" +
  "  list-style: none;" +
  "  margin: 0px;" +
  "  padding: 0px;" +
  "  border: none;" +
  "}" +
  "#vertmenu ul li {" +
  "  margin: 0px;" +
  "  padding: 0px;" +
  "}" +
  "#vertmenu ul li a {" +
  "  font-size: 80%;" +
  "  display: block;" +
  "  padding: 2px 0px 2px 7px;" +
  "  text-decoration: none;" +
  "  color: #008800;" +
  "  width:100%;" +
  "}" +
  "#vertmenu ul li a:hover, #vertmenu ul li a:focus {" +
  "  color: #000000;" +
  "  background-color: #eeeeee;" +
  "}" +
  "</style>" +
  "<body>" +

  // Center the menu vertically and horizontally
  //"<div class=\"greenBorder\" style=\"display: table; height: 100%; _position: relative; overflow: hidden;\">" +
  //"<div style=\" _position: absolute; _top: 50%; _left: 50%; display: table-cell; vertical-align: middle;\">" +
  //"<div class=\"greenBorder\" style=\" _position: relative; _top: -50%; _left: -50%\">" +

  // Draw the menu
  "<div align=\"center\">" +
  "<div id=\"vertmenu\" align=\"left\">" +
  DISAMBIGUATION +
  "<ul>" +
  "[MENUITEMS]";
  "</ul>" +
  "</div>" +
  "</div>" +
  "</body>";
var DISAMBIGUATION_DETAILS_HTML_ITEM =
  "<li><a href=\"\" onclick=\"window.external.selectedItem('[TEXT]'); return false;\">&raquo; [TEXT]</a></li>";



// ****************************** wikipedia_details_controller.js ************

// This class controls the article details panel. This class controls showing
// and hiding of the panel with animations and also displays the text inside.
function DetailsController(mainDivElement_, textElement_, imageElement_, popoutElement_, detailsLoadedCallback_, showArticleCallback_) /*: constructor (null * null * null * null * ( -> Void) * ( -> Void) -> {show : (String * null * String -> Void), hide : ( -> Void)}) */ {
  var curArticleText_ = null;
  var curImageURL_ = null;
  var curKeywords_ = null;
  var imageRequest_ = null;
  var animation_ = null;
  var inSidebar_ = (mainDivElement_ == null);
  var ieDetailsView_ = null;

  // XXX: moved this to top of function so it would be defined
  // Called when the popout button is clicked on
  function onPopoutClick() /*:  -> Any */ {
    showArticleCallback_();
  }

  if (!inSidebar_)
    popoutElement_.onclick = onPopoutClick;

  // Show the panel (details of the article)
  function show(articleText, imageURL, keywords) /*: String * null * String -> Void */ {
    curArticleText_ = articleText;
    curImageURL_ = imageURL;
    curKeywords_ = keywords;

    if (!inSidebar_) {
      textElement_.innerText = curArticleText_;

      // If there is an image, start downloading it
      if (imageURL != null) {
        imageRequest_ = new SimpleHTTPRequest();
        imageRequest_.request(curImageURL_, onImageReceived, true);
      } else {
        // Since no images need to be downloaded, finish.
        if (detailsLoadedCallback_ != null)
          detailsLoadedCallback_();
      }

      // Show details opening animation
      mainDivElement_.y = -mainDivElement_.height;
      mainDivElement_.visible = true;

      if (animation_ != null)
        animation_.stop();
      animation_ = new SimpleAnimation(onAnimateSlide, -mainDivElement_.height, 0,
        ANIMATION_SLIDE_TIME, onOpenAnimationCompleted);
      animation_.start();
    } else {
      // Open a details view panel instead of our own panel.
      var html = DETAILS_HTML;
      html = html.replace("[ARTICLETEXT]", curArticleText_);

      // Only show an image if one exists
      if ((curImageURL_ != null) && (curImageURL_ != "")) {
        html = html.replace("[IMAGESRC]", IMAGE_SRC);
      } else {
        html = html.replace("[IMAGESRC]", "");
      }
      html = html.replace("[IMAGEURL]", curImageURL_);

      ieDetailsView_ = new DetailsView();
      ieDetailsView_.html_content = true;
      ieDetailsView_.setContent("", undefined, html, false, 0);
      pluginHelper.showDetailsView(ieDetailsView_, VIEW_ON_WIKIPEDIA,
        gddDetailsViewFlagToolbarOpen, onDetailsViewFeedback);

      // End the loading now since IE will take care of the rest
      if (detailsLoadedCallback_ != null)
        detailsLoadedCallback_();
    }
  }

  // Called when the user clicks on the wikipedia details view
  function onDetailsViewFeedback(detailsViewFlags) /*: Int -> Void */ {
    if (detailsViewFlags == gddDetailsViewFlagToolbarOpen)
      showArticleCallback_();
  }

  // Remove the details panel
  function hide() /*:  -> Void */ {
    curArticleText_ = null;
    curImageURL_ = null;

    if (!inSidebar_) {
      // Stop the animation
      if (animation_ != null)
        animation_.stop();
      animation_ = new SimpleAnimation(onAnimateSlide, 0, -mainDivElement_.height,
        ANIMATION_SLIDE_TIME, onCloseAnimationCompleted);
      animation_.start();

      // Check if an image download is happening, if so, cancel it.
      if (imageRequest_ != null) {
        imageRequest_.stop();
        imageRequest_ = null;
      }
    } else {
    }
  }

  // Called when the next animation frame is to be drawn
  function onAnimateSlide(value) /*: Any -> Any */ {
    mainDivElement_.y = value;
  }

  // Resizes the image element to use the correct aspect ratio when displaying
  // the image
  function resizeImageElement() /*:  -> Any */ {
    // Calculate sizes such that the image can occupy as much space as possible
    // and also keep the same aspect ratio
    var heightRatio = imageElement_.srcHeight / DETAILS_IMAGE_HEIGHT_MAX;
    var widthRatio = imageElement_.srcWidth / DETAILS_IMAGE_WIDTH_MAX;

    var resizeRatio = 1;
    if (heightRatio > widthRatio) {
      // Since the height ratio is larger than the width ratio, this means that
      // the height of the image will be its maximum and the width will be resized
      // according to this ratio.
      resizeRatio = 1 / heightRatio;
    } else {
      // The width ratio is larger than the height ratio (or they're the same)
      resizeRatio = 1 / widthRatio;
    }

    // Calculate the new size such that at least one border (width or height) is maxed
    var newWidth = imageElement_.srcWidth * resizeRatio;
    var newHeight = imageElement_.srcHeight * resizeRatio;

    // Calculate the new positions such that the image is aligned to top right
    var newX = DETAILS_IMAGE_X + (DETAILS_IMAGE_WIDTH_MAX - newWidth);
    var newY = DETAILS_IMAGE_Y;

    imageElement_.x = newX;
    imageElement_.y = newY;

    // If the image is smaller than the box size, keep the image at the same size
    if ((imageElement_.srcWidth < DETAILS_IMAGE_WIDTH_MAX) &&
        (imageElement_.srcHeight < DETAILS_IMAGE_HEIGHT_MAX)) {
      imageElement_.width = imageElement_.srcWidth;
      imageElement_.height = imageElement_.srcHeight;
    } else {
      imageElement_.width = newWidth;
      imageElement_.height = newHeight;
    }
  }

  // Called when the animation of the details opening is completed
  function onOpenAnimationCompleted() /*:  -> Any */ {
    animation_ = null;
  }

  // Called when the animation of the details closing is completed
  function onCloseAnimationCompleted() /*:  -> Any */ {
    animation_ = null;
    imageElement_.src = "";
    textElement_.innerText = "";
    mainDivElement_.visible = false;
  }

  // Called when the current article's image finishes downloading (or errors)
  function onImageReceived(data) /*: Any -> Any */ {
    imageRequest_ = null;

    if (detailsLoadedCallback_ != null)
      detailsLoadedCallback_();

    if (data == null)
      return;

    // Update the UI
    imageElement_.visible = false;
    imageElement_.src = data;
    resizeImageElement();
    imageElement_.visible = true;
  }

  // Called when the user click the title of the details view popup
  function onDetailsViewTitleClick() /*:  -> Any */ {
    showArticleCallback_();
  }

  this.show = show;
  this.hide = hide;
}



// ****************************** expiring_timer.js ****************************

// ExpiringTimer is a timer that acts like a regular timer and triggers an
// event at a certain interval. The difference, however, is that an
// ExpiringTimer will stop triggering events after a certain amount of time if
// not pinged. When both interval and expireAfter are the same, this can be
// used as a method of preventing "too-fast" situations. This forces the
// callback to happen at most once every interval.
function ExpiringTimer(interval_, expireAfter_, callback_) /*: constructor (Int * Int * ( -> Void) -> {ping : (Any -> Any)}) */ {
  var expireTimer_ = null;
  var timer_ = null;

  // Resets all the expiry timer and ensure the main timer is running.
  // opt_callbackNow, if true, will callback immediately if the expiry timer is
  // not already running.
  function ping(opt_callbackNow) /*: Any -> Any */ {
    var callbackNow = false;

    // Restart the main timer if it is not already running
    if (timer_ == null)
      timer_ = setInterval(onTimer, interval_);

    // Stop the expire timer if it exists because we will be resetting it
    if (expireTimer_ != null) {
      clearInterval(expireTimer_);
      expireTimer_ = null;
    } else {
      if ((opt_callbackNow) && (opt_callbackNow === true))
        callbackNow = true;
    }
    expireTimer_ = setTimeout(onExpireTimer, expireAfter_);

    if (callbackNow)
      callback_();
  }

  function onTimer() /*:  -> Any */ {
    callback_();
  }

  function onExpireTimer() /*:  -> Any */ {
    expireTimer_ = null;

    // Kill the main timer since we have expired
    if (timer_ != null) {
      clearInterval(timer_);
      timer_ = null;
    }
  }

  this.ping = ping;
}

// ****************************** wikipedia_disambiguation.js ************
// This class takes care of managing disambiguation using a listbox
function WikipediaDisambiguation(disambiguationDiv_, textboxElement_, textboxOnKeyDown_, textboxOnKeyPress_, selectedDisambiguationItemCallback_) /*: Any * Any * Any * Any * Any -> Any */ {
  var listbox_ = null;
  var ieDetailsView_ = null;

  // It is assumed that if there is no onKeyPress event set, this won't be
  // reacting to search-as-you-type, therefore must be in the sidebar
  var inSidebar_ = (textboxOnKeyPress_ == null);

  // Verify that valid callbacks are given
  if (textboxOnKeyDown_ == null) { textboxOnKeyDown_ = function () /*:  -> Any */ {}; };
  if (textboxOnKeyPress_ == null) { textboxOnKeyPress_ = function () /*:  -> Any */ {}; };

  // Show the disambiguation dropdown
  function show(arrayOfItems) /*: Any -> Any */ {
    // We are creating a timer here to avoid a bug that does a slow redraw when
    // show() is called from an onData callback from XMLHttpRequest.
    setTimeout(function () /*:  -> Any */ {
      if (!inSidebar_) {
        // Create the listbox and show all the items
        listbox_ = new ListBox(historydiv, onAcceptedDisambiguation,
          onCancelledDisambiguation);
        for (var i = 0; i <= arrayOfItems.length - 1; i++)
          listbox_.addItem(arrayOfItems[i]);
        listbox_.show(0);

        // Bind to the textbox control to be able to use the up and down arrow keys
        textboxElement_.onKeyDown = onTextBoxKeyDown;
        textboxElement_.onKeyPress = function () /*:  -> Any */ {};
      } else {
         // Generate the list of items
        var itemsHtml = "";
        for (var i = 0; i <= arrayOfItems.length - 1; i++) {
          itemsHtml += DISAMBIGUATION_DETAILS_HTML_ITEM.replace(/\[TEXT\]/g, arrayOfItems[i]);
        }

        // Generate the total html
        var html = DISAMBIGUATION_DETAILS_HTML.replace("[MENUITEMS]", itemsHtml);

        // Show the details view
        ieDetailsView_ = new DetailsView();
        ieDetailsView_.html_content = true;
        ieDetailsView_.setContent("", undefined, html, false, 0);

        var externalObject = {}; //XXX: new Object();
        externalObject.selectedItem = onSelectedItem;
        ieDetailsView_.external = externalObject;

        pluginHelper.showDetailsView(ieDetailsView_, WIKIPEDIA_SEARCH, gddDetailsViewFlagNone,
          onDisambiguationDetailsViewFeedback);
      }
    }, 1);
  }

  // Hide the disambiguation dropdown
  function hide() /*:  -> Any */ {
    // Only destroy everything once the current event is completed (this usually
    // is because hide() is called on a keyboard event and not all keyboard
    // events are completed, so if we were to reset callbacks, the remaining
    // callbacks would be wrongly called!)
    setTimeout(function () /*:  -> Any */ {
      // Remove the listbox
      if (listbox_ != null) {
        listbox_.hide();
        listbox_ = null;
      }

      // Restore proper onKeyPress function
      textboxElement_.onKeyDown = textboxOnKeyDown_;
      textboxElement_.onKeyPress = textboxOnKeyPress_;
    }, 1);
  }

  function onSelectedItem(itemText) /*: Any -> Any */ {
    // Callback to the parent. Note that if the parent does a hide() on this
    // object nothing will happen since the details view windows cannot be
    // closed.
    selectedDisambiguationItemCallback_(itemText);
  }

  // Called when the disambiguation details view of the IE gets clicked on
  function onDisambiguationDetailsViewFeedback(detailsViewFlags) /*: Any -> Any */ {
    if (detailsViewFlags == 0)
      selectedDisambiguationItemCallback_(null);
  }


  // Called when the user types anything into the textbox
  function onTextBoxKeyDown() /*:  -> Any */ {
    if (listbox_.notifyKeyPress(event.keyCode))
      event.returnValue = false;
  }

  // Called when the user accepts a certain disambiguation entry
  function onAcceptedDisambiguation(value) /*: Any -> Any */ {
    hide();
    selectedDisambiguationItemCallback_(value);
  }

  // Called when the user cancels disambiguation entry
  function onCancelledDisambiguation() /*:  -> Any */ {
    hide();
    selectedDisambiguationItemCallback_(null);
  }

  this.show = show;
  this.hide = hide;
}


// ****************************** main.js ****************************
var FIELD_COLOR_FOCUS = "#F7F7F7";
var FIELD_COLOR_NOFOCUS = "#F2F2F2";
var FIELD_IMAGE_FOCUS = "images/field_focus.png";
var FIELD_IMAGE_NOFOCUS = "images/field_default.png";
var FIELD_COLOR_FOCUS_SIDEBAR = "#FFFFFF";
var FIELD_COLOR_NOFOCUS_SIDEBAR = "#FFFFFF";
var FIELD_IMAGE_FOCUS_SIDEBAR = "images/field_focus_sidebar.png";
var FIELD_IMAGE_NOFOCUS_SIDEBAR = "images/field_default_sidebar.png";
var THROBBER_IMAGES_PREFIX = "images/throbber";
var THROBBER_IMAGES_SUFFIX = ".png";
var THROBBER_IMAGES_TOTAL = 8;
var THROBBER_IMAGES_DELAY = 70;
var SAYT_INTERVAL_THROTTLE = 1000;

var throbber_ = null;
var searchAsYouTypeTimer_ = null;
var curQuery_ = null;
var lastQueryText_ = "";
var articleDetails_ = null;
var inSidebar_ = true;
var articleURL_ = null;
var history_ = null;
var historyShown_ = false;
var disambiguation_ = null;

// Called when the text changed in the textbox. This call is throttled by using
// an expiring timer.
function onNewQuery() /*:  -> Void */ {
  // Only take this as a new query if the query text has actually changed
  var searchText = inSidebar_ ? textbox_sidebar.value : textbox.value;

  // Clean the text such that same queries can be detected
  searchText = searchText.replace(/^\s+/g, "");  // Strip leading spaces
  searchText = searchText.replace(/\s+$/g, "");  // Strip trailing spaces

  // If there is Talk: or Special: in front of the query, ignore it since
  // access special pages is not supported.
  searchText = searchText.replace(/^Talk\:/gi, "");
  searchText = searchText.replace(/^Special\:/gi, "");
  searchText = searchText.replace(/^Wikipedia\:/gi, "");
  searchText = searchText.replace(/^Template\:/gi, "");

  if ((!inSidebar_) && (searchText == lastQueryText_))
    return;

  // Hide any disambiguation results if some are shown
  if ((disambiguation_ != null) && (!inSidebar_)) {
    disambiguation_.hide();
    onSelectedDisambiguation(null);
  }

  lastQueryText_ = searchText;

  // Check if a query is already happening, if so, cancel it.
  if (curQuery_ != null) {
    curQuery_.stop();
    curQuery_ = null;
  }

  setNoResults();

  // If the textbox is empty, stop since no results are needed
  articleURL_ = null;
  if (lastQueryText_ == "")
    return;

  // Show the throbber since we're downloading something. An important thing to
  // note here is that the throbber could already be going from a previous
  // query that was cancelled. This is fine since show() handles this
  // case properly.
  throbber_.show();

  // Make the query
  curQuery_ = new WikipediaQuery();
  articleURL_ = curQuery_.query(lastQueryText_, onQueryReceived);
}

// Called when the gadget enters/leaves the sidebar
function onDisplayTargetChange(displayTarget) /*: Int -> Void */ {
  // Find out the new display mode
  if (displayTarget == gddTargetSidebar) {
    inSidebar_ = true;
  } else if (displayTarget == gddTargetFloatingView) {
    inSidebar_ = false;
  }

  // Change UI modes
  bardiv_sidebar.visible = inSidebar_;
  bardiv.visible = !inSidebar_;
  resultsdivparent.visible = !inSidebar_;

  // Resize the view depending on the display mode. We use a timer here because
  // the gadget cannot be resized when calling onDisplayTargetChange.
  view.setTimeout(function () /*:  -> Void */ {
    if (inSidebar_) {
      view.width = bardiv_sidebar.width;
      view.height = bardiv_sidebar.height;
    } else {
      view.width = ((bardiv.width > resultsdivparent.width) ?
        bardiv.width : resultsdivparent.width);
      view.height = resultsdivparent.y + resultsdivparent.height;
    } }, 1);

  // Create a throbber to show when an internet request is being processed
  if (throbber_ != null) {
    throbber_.hide();
  }
  throbber_ = new Throbber(inSidebar_ ? throbber_sidebar : throbber,
    THROBBER_IMAGES_PREFIX, THROBBER_IMAGES_SUFFIX, THROBBER_IMAGES_TOTAL,
    THROBBER_IMAGES_DELAY);

  // Create a details view that is for the proper view
  if (articleDetails_ != null)
    articleDetails_.hide();
  if (inSidebar_) {
    articleDetails_ = new DetailsController(null, null, null, null,
      onDetailsLoaded, onShowArticle);
  } else {
    articleDetails_ = new DetailsController(resultsdiv, articletext,
      articleimage, popout, onDetailsLoaded, onShowArticle);
  }

  // Make both textboxes contain the same text
  if (inSidebar_) {
    textbox_sidebar.value = textbox.value;
  } else {
    textbox.value = textbox_sidebar.value;
  }

  // Create the wikipedia history object
  if (history_ != null) {
    historyShown_ = false;
    history_.hide();
  }
  history_ = new WikipediaHistory(inSidebar_ ? null : historydiv, onSelectedHistory);

  // Reset the last query and requery. This is important if the user searched
  // for something in sidebar mode and then needs the result in floating mode.
  if (!inSidebar_) {
    lastQueryText_ = "";
    onNewQuery();
  }
}

// Called when the gadget first starts up
function onOpen() /*:  -> Void */ {
  onDisplayTargetChange(gddTargetSidebar);

  // Show the search box as not having focus since it will not on by default
  updateFieldFocus(false);

  // Create the expiring timer for search-as-you-type
  searchAsYouTypeTimer_ = new ExpiringTimer(SAYT_INTERVAL_THROTTLE,
    SAYT_INTERVAL_THROTTLE, onNewQuery);
}

// Set the search field's focus picture and also textbox background color
function updateFieldFocus(hasFocus) /*: Bool -> Void */ {
  if (hasFocus) {
    field.src = FIELD_IMAGE_FOCUS;
    textbox.background = FIELD_COLOR_FOCUS;
    field_sidebar.src = FIELD_IMAGE_FOCUS_SIDEBAR;
    textbox_sidebar.background = FIELD_COLOR_FOCUS_SIDEBAR;
  } else {
    field.src = FIELD_IMAGE_NOFOCUS;
    textbox.background = FIELD_COLOR_NOFOCUS;
    field_sidebar.src = FIELD_IMAGE_NOFOCUS_SIDEBAR;
    textbox_sidebar.background = FIELD_COLOR_NOFOCUS_SIDEBAR;
  }
}

// Goes to the current article's URL in the webbrowser
function showArticleDetails() /*:  -> Void */ {
  if (articleURL_ == null)
    return;

  openURL(articleURL_);
}

// Called when a user clicks the search history drop down or details view
// depending on if the gadget is in the sidebar or not
function onSearchDropClick() /*:  -> Any */ {
  if (!historyShown_) {
    historyShown_ = true;
    history_.show();
  } else {
    history_.hide();
    historyShown_ = false;
  }
}

// Called when a user selects an item in the history
function onSelectedHistory(text) /*: Any -> Any */ {
  historyShown_ = false;

  if (inSidebar_) {
    // Send the new query since search-as-you-type doesn't automatically
    // already search for us.
    textbox_sidebar.value = text;
    onNewQuery();
  } else {
    textbox.value = text;
  }
}

// Called when a user types something in the textbox
function onTextBoxChange() /*:  -> Void */ {
  if (!inSidebar_)
    searchAsYouTypeTimer_.ping(false);
}

// Called when a user types text into the textbox
function onTextBoxKeyPress() /*:  -> Void */ {
  // Clear the search box if the escape key is pressed
  if (event.keyCode == 27) {
    textbox.value = "";
    return;
  }

  // When the enter key is pressed, the action depends on if this gadget is
  // currently in the sidebar or floating.
  if (event.keyCode == 13) {
    if (inSidebar_) {
      onNewQuery();
    } else {
      showArticleDetails();
    }
  }
}

// Called when the textbox is clicked on. This is a good time to hide the
// history if it is displayed
function onTextBoxClick() /*:  -> Any */ {
  if (historyShown_) {
    history_.hide();
    historyShown_ = false;
  }
}

// Called when the textbox received keyboard focus
function onTextBoxFocusIn() /*:  -> Void */ {
  updateFieldFocus(true);
}

// Called when the textbox loses keyboard focus
function onTextBoxFocusOut() /*:  -> Void */ {
  updateFieldFocus(false);
}

// Hide the results panel if it is shown.
function setNoResults() /*:  -> Void */ {
  articleDetails_.hide();
  throbber_.hide();
}

// Called when the Wikipedia search completes (success or failure)
function onQueryReceived(text, imageURL, articleURL) /*: String * null * String -> Void */ {
  // No results found
  if (text == null) {
    curQuery_ = null;
    throbber_.hide();
    return;
  }

  // Disambiguation page
  if (text == "") {
    var disambiguationResults = curQuery_.getDisambiguationArray();
    curQuery_ = null;
    throbber_.hide();

    // If no disambiguation was found (maybe the term didn't appear in any links)
    if (disambiguationResults.length == 0)
      return;

    // Create a separate disambiguation if in sidebar as opposed to not
    if (inSidebar_) {
      // Do disambiguation in details view
      disambiguation_ = new WikipediaDisambiguation(historydiv, textbox_sidebar,
        null, null, onSelectedDisambiguation);
    } else {
      // Do disambiguation as a drop down
      disambiguation_ = new WikipediaDisambiguation(historydiv, textbox, null,
        onTextBoxKeyPress, onSelectedDisambiguation);
    }

    // Show the disambiguation drop down
    disambiguation_.show(disambiguationResults);
    return;
  }

  curQuery_ = null;

  // Since the query was found, add it to the history
  history_.addHistoryItem(lastQueryText_);

  // Show the details with the required information
  articleDetails_.show(text, imageURL, lastQueryText_);
}

// Called when the user has selected an item in the disambiguation dropdown
function onSelectedDisambiguation(value) /*: Any -> Any */ {
  if (value != null) {
    if (!inSidebar_) {
      textbox.value = value;
    } else {
      textbox_sidebar.value = value;
      onNewQuery();
    }
  }

  disambiguation_ = null;
}

// Called when the details view is open and all images/data are downloaded
function onDetailsLoaded() /*:  -> Void */ {
  throbber_.hide();
}

// Called when the user clicks the popout button in the details view
function onShowArticle() /*:  -> Void */ {
  showArticleDetails();
}

// Opens the requested URL using the user's default web browser
function openURL(url) /*: String -> Void */ {
  try {
    var shell = /*:downcast ShellApplication*/(new ActiveXObject("Shell.Application"));
    shell.Open(url);
  } catch (e) {
    return;
  }
}

// Monitor when the gadget enters/leaves the sidebar
pluginHelper.onDisplayTargetChange = onDisplayTargetChange;


