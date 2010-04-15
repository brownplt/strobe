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

// Monitor when the gadget enters/leaves the sidebar
pluginHelper.onDisplayTargetChange = onDisplayTargetChange;

// Called when the gadget first starts up
function onOpen() {
  onDisplayTargetChange(gddTargetSidebar);

  // Show the search box as not having focus since it will not on by default
  updateFieldFocus(false);

  // Create the expiring timer for search-as-you-type
  searchAsYouTypeTimer_ = new ExpiringTimer(SAYT_INTERVAL_THROTTLE,
    SAYT_INTERVAL_THROTTLE, onNewQuery);
}

// Called when the gadget enters/leaves the sidebar
function onDisplayTargetChange(displayTarget) {
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
  view.setTimeout(function () {
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

// Set the search field's focus picture and also textbox background color
function updateFieldFocus(hasFocus) {
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
function showArticleDetails() {
  if (articleURL_ == null)
    return;

  openURL(articleURL_);
}

// Called when a user clicks the search history drop down or details view
// depending on if the gadget is in the sidebar or not
function onSearchDropClick() {
  if (!historyShown_) {
    historyShown_ = true;
    history_.show();
  } else {
    history_.hide();
    historyShown_ = false;
  }
}

// Called when a user selects an item in the history
function onSelectedHistory(text) {
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
function onTextBoxChange() {
  if (!inSidebar_)
    searchAsYouTypeTimer_.ping(false);
}

// Called when a user types text into the textbox
function onTextBoxKeyPress() {
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
function onTextBoxClick() {
  if (historyShown_) {
    history_.hide();
    historyShown_ = false;
  }
}

// Called when the textbox received keyboard focus
function onTextBoxFocusIn() {
  updateFieldFocus(true);
}

// Called when the textbox loses keyboard focus
function onTextBoxFocusOut() {
  updateFieldFocus(false);
}

// Called when the text changed in the textbox. This call is throttled by using
// an expiring timer.
function onNewQuery() {
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

// Hide the results panel if it is shown.
function setNoResults() {
  articleDetails_.hide();
  throbber_.hide();
}

// Called when the Wikipedia search completes (success or failure)
function onQueryReceived(text, imageURL, articleURL) {
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
function onSelectedDisambiguation(value) {
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
function onDetailsLoaded() {
  throbber_.hide();
}

// Called when the user clicks the popout button in the details view
function onShowArticle() {
  showArticleDetails();
}

// Opens the requested URL using the user's default web browser
function openURL(url) {
  try {
    var shell = new ActiveXObject("Shell.Application");
    shell.Open(url);
  } catch (e) {
    return;
  }
}
