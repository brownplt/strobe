var ExpiringTimer = __typedjs(function  (interval_,
                                         expireAfter_,
                                         callback_)
                              {
                                var expireTimer_ = null;
                                var timer_ = null;
                                var ping = __typedjs(function  (opt_callbackNow)
                                                     {
                                                       var callbackNow = false;
                                                       if (timer_ == null)
                                                       timer_ = setInterval(onTimer,interval_);
                                                       if (expireTimer_ != null)
                                                       {
                                                         clearInterval(expireTimer_);
                                                         expireTimer_ = null;
                                                       }
                                                       else {
                                                              if ((opt_callbackNow) && (opt_callbackNow === true))
                                                              callbackNow = true;
                                                            };
                                                       expireTimer_ = setTimeout(onExpireTimer,
                                                                                 expireAfter_);
                                                       if (callbackNow)
                                                       callback_();
                                                     },
                                                     arguments.callee,
                                                     "ping",
                                                     "gadgets/WikipediaSearch/wikipedia_all.js",
                                                     0);
                                var onTimer = __typedjs(function  ()
                                                        {
                                                          callback_();
                                                        },
                                                        arguments.callee,
                                                        "onTimer",
                                                        "gadgets/WikipediaSearch/wikipedia_all.js",
                                                        1);
                                var onExpireTimer = __typedjs(function  ()
                                                              {
                                                                expireTimer_ = null;
                                                                if (timer_ != null)
                                                                {
                                                                  clearInterval(timer_);
                                                                  timer_ = null;
                                                                };
                                                              },
                                                              arguments.callee,
                                                              "onExpireTimer",
                                                              "gadgets/WikipediaSearch/wikipedia_all.js",
                                                              2);
                                __thisref(this,arguments.callee).ping = ping;
                              },
                              undefined,
                              "ExpiringTimer",
                              "gadgets/WikipediaSearch/wikipedia_all.js",
                              0);
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
var ListBox = __typedjs(function  (parentDivElement_,
                                   acceptedCallback_,
                                   cancelledCallback_)
                        {
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
                          var addItem = __typedjs(function  (displayValue)
                                                  {
                                                    var newItem = __new(Object,[]);
                                                    newItem.displayValue = displayValue;
                                                    newItem.labelElement = null;
                                                    items_.push(newItem);
                                                  },
                                                  arguments.callee,
                                                  "addItem",
                                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                                  0);
                          var show = __typedjs(function  (selectItem)
                                               {
                                                 shown_ = true;
                                                 backgroundElement_ = parentDivElement_.appendElement("<img " + "src=\"" + AREA_IMAGE + "\" " + "width=\"" + parentDivElement_.width + "\" height=\"" + parentDivElement_.height + "\" x=\"0\" y=\"0\" />");
                                                 divElement_ = parentDivElement_.appendElement("<div " + "x=\"" + AREA_LEFT_BORDER + "\" y=\"" + AREA_TOP_BORDER + "\" " + "width=\"" + (parentDivElement_.width - AREA_RIGHT_BORDER - AREA_LEFT_BORDER - 1) + "\" " + "height=\"" + (parentDivElement_.height - AREA_BOTTOM_BORDER - AREA_TOP_BORDER - 1) + "\" />");
                                                 selectorElement_ = divElement_.appendElement("<div " + "width=\"" + divElement_.width + "\" " + "height=\"" + ITEM_HEIGHT + "\" background=\"" + ITEM_SELECTOR_COLOR + "\" " + "opacity=\"" + ITEM_SELECTOR_OPACITY + "\" visible=\"false\" />");
                                                 hoverElement_ = divElement_.appendElement("<div " + "width=\"" + divElement_.width + "\" " + "height=\"" + ITEM_HEIGHT + "\" background=\"" + ITEM_HOVER_SELECTOR_COLOR + "\" " + "opacity=\"" + ITEM_HOVER_SELECTOR_OPACITY + "\" visible=\"false\" />");
                                                 if (selectItem != null)
                                                 selectedItem_ = selectItem;
                                                 hoveredItem_ = null;
                                                 firstVisibleItem_ = 0;
                                                 totalVisibleItems_ = parseInt(divElement_.height / ITEM_HEIGHT);
                                                 showSelectedItem();
                                                 drawAllVisibleItems();
                                               },
                                               arguments.callee,
                                               "show",
                                               "gadgets/WikipediaSearch/wikipedia_all.js",
                                               1);
                          var hide = __typedjs(function  ()
                                               {
                                                 if (! shown_)
                                                 return;
                                                 shown_ = false;
                                                 parentDivElement_.removeElement(backgroundElement_);
                                                 parentDivElement_.removeElement(divElement_);
                                                 items_ = [];
                                               },
                                               arguments.callee,
                                               "hide",
                                               "gadgets/WikipediaSearch/wikipedia_all.js",
                                               2);
                          var drawAllVisibleItems = __typedjs(function  ()
                                                              {
                                                                for (var i = 0; i <= items_.length - 1; i++)
                                                                {
                                                                  if (items_[i].labelElement != null)
                                                                  divElement_.removeElement(items_[i].labelElement);
                                                                  var drawY = getYPositionOfItem(i);
                                                                  var cleanText = items_[i].displayValue.replace(/\&/g,
                                                                                                                 " ");
                                                                  if (drawY != null)
                                                                  {
                                                                    items_[i].labelElement = divElement_.appendElement("<label " + "x=\"" + getXPositionOfItem(i) + "\" " + "y=\"" + drawY + "\" " + "width=\"" + divElement_.width + "\" " + "height=\"" + ITEM_HEIGHT + "\" " + "size=\"" + ITEM_TEXT_SIZE + "\" font=\"" + ITEM_TEXT_FONT + "\" " + "trimming=\"character-ellipsis\" enabled=\"true\">" + cleanText + "</label>");
                                                                    setMouseEventsForLabel(items_[i].labelElement,
                                                                                           i);
                                                                  };
                                                                };
                                                              },
                                                              arguments.callee,
                                                              "drawAllVisibleItems",
                                                              "gadgets/WikipediaSearch/wikipedia_all.js",
                                                              3);
                          var setMouseEventsForLabel = __typedjs(function  (element,index)
                                                                 {
                                                                   element.onmouseover = __typedjs(function  ()
                                                                                                   {
                                                                                                     onMouseOverItem(index);
                                                                                                   },
                                                                                                   arguments.callee,
                                                                                                   "element.onmouseover",
                                                                                                   "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                                   0);
                                                                   element.onmouseout = __typedjs(function  ()
                                                                                                  {
                                                                                                    onMouseOffItem(index);
                                                                                                  },
                                                                                                  arguments.callee,
                                                                                                  "element.onmouseout",
                                                                                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                                  1);
                                                                   element.onclick = __typedjs(function  ()
                                                                                               {
                                                                                                 onMouseClickItem(index);
                                                                                               },
                                                                                               arguments.callee,
                                                                                               "element.onclick",
                                                                                               "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                               2);
                                                                 },
                                                                 arguments.callee,
                                                                 "setMouseEventsForLabel",
                                                                 "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                 4);
                          var onMouseOverItem = __typedjs(function  (index)
                                                          {
                                                            hoveredItem_ = index;
                                                            showHoveredItem();
                                                          },
                                                          arguments.callee,
                                                          "onMouseOverItem",
                                                          "gadgets/WikipediaSearch/wikipedia_all.js",
                                                          5);
                          var onMouseOffItem = __typedjs(function  (index)
                                                         {
                                                           hoveredItem_ = null;
                                                           hideHoveredItem();
                                                         },
                                                         arguments.callee,
                                                         "onMouseOffItem",
                                                         "gadgets/WikipediaSearch/wikipedia_all.js",
                                                         6);
                          var onMouseClickItem = __typedjs(function  (index)
                                                           {
                                                             acceptedCallback_(items_[index].displayValue);
                                                           },
                                                           arguments.callee,
                                                           "onMouseClickItem",
                                                           "gadgets/WikipediaSearch/wikipedia_all.js",
                                                           7);
                          var getYPositionOfItem = __typedjs(function  (index)
                                                             {
                                                               var indexPosition = index - firstVisibleItem_;
                                                               if ((indexPosition < 0) || (indexPosition >= totalVisibleItems_))
                                                               return null;
                                                               return indexPosition * ITEM_HEIGHT;
                                                             },
                                                             arguments.callee,
                                                             "getYPositionOfItem",
                                                             "gadgets/WikipediaSearch/wikipedia_all.js",
                                                             8);
                          var getXPositionOfItem = __typedjs(function  (index)
                                                             {
                                                               return 0;
                                                             },
                                                             arguments.callee,
                                                             "getXPositionOfItem",
                                                             "gadgets/WikipediaSearch/wikipedia_all.js",
                                                             9);
                          var showSelectedItem = __typedjs(function  ()
                                                           {
                                                             if (selectedItem_ == null)
                                                             return;
                                                             if (selectedItem_ < 0)
                                                             selectedItem_ = 0;
                                                             if (selectedItem_ > items_.length - 1)
                                                             selectedItem_ = items_.length - 1;
                                                             if (selectedItem_ < firstVisibleItem_)
                                                             {
                                                               firstVisibleItem_ = selectedItem_;
                                                               drawAllVisibleItems();
                                                             };
                                                             if (selectedItem_ > firstVisibleItem_ + totalVisibleItems_ - 1)
                                                             {
                                                               firstVisibleItem_ = selectedItem_ - totalVisibleItems_ + 1;
                                                               drawAllVisibleItems();
                                                             };
                                                             selectorElement_.x = 0;
                                                             selectorElement_.y = getYPositionOfItem(selectedItem_);
                                                             selectorElement_.visible = true;
                                                           },
                                                           arguments.callee,
                                                           "showSelectedItem",
                                                           "gadgets/WikipediaSearch/wikipedia_all.js",
                                                           10);
                          var showHoveredItem = __typedjs(function  ()
                                                          {
                                                            hoverElement_.x = 0;
                                                            hoverElement_.y = getYPositionOfItem(hoveredItem_);
                                                            hoverElement_.visible = true;
                                                          },
                                                          arguments.callee,
                                                          "showHoveredItem",
                                                          "gadgets/WikipediaSearch/wikipedia_all.js",
                                                          11);
                          var hideHoveredItem = __typedjs(function  ()
                                                          {
                                                            hoverElement_.visible = false;
                                                          },
                                                          arguments.callee,
                                                          "hideHoveredItem",
                                                          "gadgets/WikipediaSearch/wikipedia_all.js",
                                                          12);
                          var notifyKeyPress = __typedjs(function  (keyCode)
                                                         {
                                                           if (! shown_)
                                                           return false;
                                                           if (keyCode == 38)
                                                           {
                                                             selectedItem_--;
                                                             showSelectedItem();
                                                           }
                                                           else if (keyCode == 40)
                                                                {
                                                                  selectedItem_++;
                                                                  showSelectedItem();
                                                                }
                                                                else if (keyCode == 33)
                                                                     {
                                                                       selectedItem_ -= totalVisibleItems_ - 1;
                                                                       showSelectedItem();
                                                                     }
                                                                     else if (keyCode == 34)
                                                                          {
                                                                            selectedItem_ += totalVisibleItems_ - 1;
                                                                            showSelectedItem();
                                                                          }
                                                                          else if (keyCode == 13)
                                                                               {
                                                                                 acceptedCallback_(items_[selectedItem_].displayValue);
                                                                               }
                                                                               else if (keyCode == 27)
                                                                                    {
                                                                                      cancelledCallback_();
                                                                                    }
                                                                                    else if (keyCode == 36)
                                                                                         {
                                                                                           selectedItem_ = 0;
                                                                                           showSelectedItem();
                                                                                         }
                                                                                         else if (keyCode == 35)
                                                                                              {
                                                                                                selectedItem_ = items_.length - 1;
                                                                                                showSelectedItem();
                                                                                              }
                                                                                              else {
                                                                                                     return false;
                                                                                                   };
                                                           return true;
                                                         },
                                                         arguments.callee,
                                                         "notifyKeyPress",
                                                         "gadgets/WikipediaSearch/wikipedia_all.js",
                                                         13);
                          __thisref(this,arguments.callee).addItem = addItem;
                          __thisref(this,arguments.callee).notifyKeyPress = notifyKeyPress;
                          __thisref(this,arguments.callee).show = show;
                          __thisref(this,arguments.callee).hide = hide;
                        },
                        undefined,
                        "ListBox",
                        "gadgets/WikipediaSearch/wikipedia_all.js",
                        1);
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
pluginHelper.onDisplayTargetChange = onDisplayTargetChange;
var onOpen = __typedjs(function  ()
                       {
                         onDisplayTargetChange(gddTargetSidebar);
                         updateFieldFocus(false);
                         searchAsYouTypeTimer_ = __new(ExpiringTimer,
                                                       [SAYT_INTERVAL_THROTTLE,
                                                        SAYT_INTERVAL_THROTTLE,
                                                        onNewQuery]);
                       },
                       undefined,
                       "onOpen",
                       "gadgets/WikipediaSearch/wikipedia_all.js",
                       2);
var onDisplayTargetChange = __typedjs(function  (displayTarget)
                                      {
                                        if (displayTarget == gddTargetSidebar)
                                        {
                                          inSidebar_ = true;
                                        }
                                        else if (displayTarget == gddTargetFloatingView)
                                             {
                                               inSidebar_ = false;
                                             };
                                        bardiv_sidebar.visible = inSidebar_;
                                        bardiv.visible = ! inSidebar_;
                                        resultsdivparent.visible = ! inSidebar_;
                                        view.setTimeout(__typedjs(function  ()
                                                                  {
                                                                    if (inSidebar_)
                                                                    {
                                                                      view.width = bardiv_sidebar.width;
                                                                      view.height = bardiv_sidebar.height;
                                                                    }
                                                                    else {
                                                                           view.width = ((bardiv.width > resultsdivparent.width) ? bardiv.width : resultsdivparent.width);
                                                                           view.height = resultsdivparent.y + resultsdivparent.height;
                                                                         };
                                                                  },
                                                                  arguments.callee,
                                                                  "",
                                                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                  0),
                                                        1);
                                        if (throbber_ != null)
                                        {
                                          throbber_.hide();
                                        };
                                        throbber_ = __new(Throbber,
                                                          [inSidebar_ ? throbber_sidebar : throbber,
                                                           THROBBER_IMAGES_PREFIX,
                                                           THROBBER_IMAGES_SUFFIX,
                                                           THROBBER_IMAGES_TOTAL,
                                                           THROBBER_IMAGES_DELAY]);
                                        if (articleDetails_ != null)
                                        articleDetails_.hide();
                                        if (inSidebar_)
                                        {
                                          articleDetails_ = __new(DetailsController,
                                                                  [null,
                                                                   null,
                                                                   null,
                                                                   null,
                                                                   onDetailsLoaded,
                                                                   onShowArticle]);
                                        }
                                        else {
                                               articleDetails_ = __new(DetailsController,
                                                                       [resultsdiv,
                                                                        articletext,
                                                                        articleimage,
                                                                        popout,
                                                                        onDetailsLoaded,
                                                                        onShowArticle]);
                                             };
                                        if (inSidebar_)
                                        {
                                          textbox_sidebar.value = textbox.value;
                                        }
                                        else {
                                               textbox.value = textbox_sidebar.value;
                                             };
                                        if (history_ != null)
                                        {
                                          historyShown_ = false;
                                          history_.hide();
                                        };
                                        history_ = __new(WikipediaHistory,
                                                         [inSidebar_ ? null : historydiv,
                                                          onSelectedHistory]);
                                        if (! inSidebar_)
                                        {
                                          lastQueryText_ = "";
                                          onNewQuery();
                                        };
                                      },
                                      undefined,
                                      "onDisplayTargetChange",
                                      "gadgets/WikipediaSearch/wikipedia_all.js",
                                      3);
var updateFieldFocus = __typedjs(function  (hasFocus)
                                 {
                                   if (hasFocus)
                                   {
                                     field.src = FIELD_IMAGE_FOCUS;
                                     textbox.background = FIELD_COLOR_FOCUS;
                                     field_sidebar.src = FIELD_IMAGE_FOCUS_SIDEBAR;
                                     textbox_sidebar.background = FIELD_COLOR_FOCUS_SIDEBAR;
                                   }
                                   else {
                                          field.src = FIELD_IMAGE_NOFOCUS;
                                          textbox.background = FIELD_COLOR_NOFOCUS;
                                          field_sidebar.src = FIELD_IMAGE_NOFOCUS_SIDEBAR;
                                          textbox_sidebar.background = FIELD_COLOR_NOFOCUS_SIDEBAR;
                                        };
                                 },
                                 undefined,
                                 "updateFieldFocus",
                                 "gadgets/WikipediaSearch/wikipedia_all.js",
                                 4);
var showArticleDetails = __typedjs(function  ()
                                   {
                                     if (articleURL_ == null)
                                     return;
                                     openURL(articleURL_);
                                   },
                                   undefined,
                                   "showArticleDetails",
                                   "gadgets/WikipediaSearch/wikipedia_all.js",
                                   5);
var onSearchDropClick = __typedjs(function  ()
                                  {
                                    if (! historyShown_)
                                    {
                                      historyShown_ = true;
                                      history_.show();
                                    }
                                    else {
                                           history_.hide();
                                           historyShown_ = false;
                                         };
                                  },
                                  undefined,
                                  "onSearchDropClick",
                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                  6);
var onSelectedHistory = __typedjs(function  (text)
                                  {
                                    historyShown_ = false;
                                    if (inSidebar_)
                                    {
                                      textbox_sidebar.value = text;
                                      onNewQuery();
                                    }
                                    else {
                                           textbox.value = text;
                                         };
                                  },
                                  undefined,
                                  "onSelectedHistory",
                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                  7);
var onTextBoxChange = __typedjs(function  ()
                                {
                                  if (! inSidebar_)
                                  searchAsYouTypeTimer_.ping(false);
                                },
                                undefined,
                                "onTextBoxChange",
                                "gadgets/WikipediaSearch/wikipedia_all.js",
                                8);
var onTextBoxKeyPress = __typedjs(function  ()
                                  {
                                    if (event.keyCode == 27)
                                    {
                                      textbox.value = "";
                                      return;
                                    };
                                    if (event.keyCode == 13)
                                    {
                                      if (inSidebar_)
                                      {
                                        onNewQuery();
                                      }
                                      else {
                                             showArticleDetails();
                                           };
                                    };
                                  },
                                  undefined,
                                  "onTextBoxKeyPress",
                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                  9);
var onTextBoxClick = __typedjs(function  ()
                               {
                                 if (historyShown_)
                                 {
                                   history_.hide();
                                   historyShown_ = false;
                                 };
                               },
                               undefined,
                               "onTextBoxClick",
                               "gadgets/WikipediaSearch/wikipedia_all.js",
                               10);
var onTextBoxFocusIn = __typedjs(function  ()
                                 {
                                   updateFieldFocus(true);
                                 },
                                 undefined,
                                 "onTextBoxFocusIn",
                                 "gadgets/WikipediaSearch/wikipedia_all.js",
                                 11);
var onTextBoxFocusOut = __typedjs(function  ()
                                  {
                                    updateFieldFocus(false);
                                  },
                                  undefined,
                                  "onTextBoxFocusOut",
                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                  12);
var onNewQuery = __typedjs(function  ()
                           {
                             var searchText = inSidebar_ ? textbox_sidebar.value : textbox.value;
                             searchText = searchText.replace(/^\s+/g,"");
                             searchText = searchText.replace(/\s+$/g,"");
                             searchText = searchText.replace(/^Talk\:/gi,"");
                             searchText = searchText.replace(/^Special\:/gi,"");
                             searchText = searchText.replace(/^Wikipedia\:/gi,"");
                             searchText = searchText.replace(/^Template\:/gi,"");
                             if ((! inSidebar_) && (searchText == lastQueryText_))
                             return;
                             if ((disambiguation_ != null) && (! inSidebar_))
                             {
                               disambiguation_.hide();
                               onSelectedDisambiguation(null);
                             };
                             lastQueryText_ = searchText;
                             if (curQuery_ != null)
                             {
                               curQuery_.stop();
                               curQuery_ = null;
                             };
                             setNoResults();
                             articleURL_ = null;
                             if (lastQueryText_ == "")
                             return;
                             throbber_.show();
                             curQuery_ = __new(WikipediaQuery,[]);
                             articleURL_ = curQuery_.query(lastQueryText_,onQueryReceived);
                           },
                           undefined,
                           "onNewQuery",
                           "gadgets/WikipediaSearch/wikipedia_all.js",
                           13);
var setNoResults = __typedjs(function  ()
                             {
                               articleDetails_.hide();
                               throbber_.hide();
                             },
                             undefined,
                             "setNoResults",
                             "gadgets/WikipediaSearch/wikipedia_all.js",
                             14);
var onQueryReceived = __typedjs(function  (text,
                                           imageURL,
                                           articleURL)
                                {
                                  if (text == null)
                                  {
                                    curQuery_ = null;
                                    throbber_.hide();
                                    return;
                                  };
                                  if (text == "")
                                  {
                                    var disambiguationResults = curQuery_.getDisambiguationArray();
                                    curQuery_ = null;
                                    throbber_.hide();
                                    if (disambiguationResults.length == 0)
                                    return;
                                    if (inSidebar_)
                                    {
                                      disambiguation_ = __new(WikipediaDisambiguation,
                                                              [historydiv,
                                                               textbox_sidebar,
                                                               null,
                                                               null,
                                                               onSelectedDisambiguation]);
                                    }
                                    else {
                                           disambiguation_ = __new(WikipediaDisambiguation,
                                                                   [historydiv,
                                                                    textbox,
                                                                    null,
                                                                    onTextBoxKeyPress,
                                                                    onSelectedDisambiguation]);
                                         };
                                    disambiguation_.show(disambiguationResults);
                                    return;
                                  };
                                  curQuery_ = null;
                                  history_.addHistoryItem(lastQueryText_);
                                  articleDetails_.show(text,imageURL,lastQueryText_);
                                },
                                undefined,
                                "onQueryReceived",
                                "gadgets/WikipediaSearch/wikipedia_all.js",
                                15);
var onSelectedDisambiguation = __typedjs(function  (value)
                                         {
                                           if (value != null)
                                           {
                                             if (! inSidebar_)
                                             {
                                               textbox.value = value;
                                             }
                                             else {
                                                    textbox_sidebar.value = value;
                                                    onNewQuery();
                                                  };
                                           };
                                           disambiguation_ = null;
                                         },
                                         undefined,
                                         "onSelectedDisambiguation",
                                         "gadgets/WikipediaSearch/wikipedia_all.js",
                                         16);
var onDetailsLoaded = __typedjs(function  ()
                                {
                                  throbber_.hide();
                                },
                                undefined,
                                "onDetailsLoaded",
                                "gadgets/WikipediaSearch/wikipedia_all.js",
                                17);
var onShowArticle = __typedjs(function  ()
                              {
                                showArticleDetails();
                              },
                              undefined,
                              "onShowArticle",
                              "gadgets/WikipediaSearch/wikipedia_all.js",
                              18);
var openURL = __typedjs(function  (url)
                        {
                          try
                          {
                            var shell = __new(ActiveXObject,["Shell.Application"]);
                            shell.Open(url);
                          }
                          catch (e) {
                                      return;
                                    };
                        },
                        undefined,
                        "openURL",
                        "gadgets/WikipediaSearch/wikipedia_all.js",
                        19);
var SimpleAnimation = __typedjs(function  (animateCallback_,
                                           startValue_,
                                           endValue_,
                                           duration_,
                                           completedCallback_)
                                {
                                  var animation_ = null;
                                  var timer_ = null;
                                  var start = __typedjs(function  ()
                                                        {
                                                          animation_ = beginAnimation(onAnimate,
                                                                                      startValue_,
                                                                                      endValue_,
                                                                                      duration_);
                                                          timer_ = setTimeout(onAnimationComplete,
                                                                              duration_);
                                                        },
                                                        arguments.callee,
                                                        "start",
                                                        "gadgets/WikipediaSearch/wikipedia_all.js",
                                                        0);
                                  var stop = __typedjs(function  ()
                                                       {
                                                         if (animation_ != null)
                                                         view.cancelAnimation(animation_);
                                                         if (timer_ != null)
                                                         view.clearInterval(timer_);
                                                       },
                                                       arguments.callee,
                                                       "stop",
                                                       "gadgets/WikipediaSearch/wikipedia_all.js",
                                                       1);
                                  var onAnimate = __typedjs(function  ()
                                                            {
                                                              if (timer_ == null)
                                                              return;
                                                              animateCallback_(event.value);
                                                            },
                                                            arguments.callee,
                                                            "onAnimate",
                                                            "gadgets/WikipediaSearch/wikipedia_all.js",
                                                            2);
                                  var onAnimationComplete = __typedjs(function  ()
                                                                      {
                                                                        stop();
                                                                        completedCallback_();
                                                                      },
                                                                      arguments.callee,
                                                                      "onAnimationComplete",
                                                                      "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                      3);
                                  __thisref(this,arguments.callee).start = start;
                                  __thisref(this,arguments.callee).stop = stop;
                                },
                                undefined,
                                "SimpleAnimation",
                                "gadgets/WikipediaSearch/wikipedia_all.js",
                                20);
var SimpleHTTPRequest = __typedjs(function  ()
                                  {
                                    var getStream_ = false;
                                    var request_ = null;
                                    var stop_ = false;
                                    var receivedResultCallback_ = null;
                                    var request = __typedjs(function  (url,
                                                                       receivedResultCallback,
                                                                       opt_getStream)
                                                            {
                                                              assert(receivedResultCallback != null);
                                                              if (receivedResultCallback == null)
                                                              return;
                                                              receivedResultCallback_ = receivedResultCallback;
                                                              assert(request_ == null);
                                                              if (request_ != null)
                                                              {
                                                                receivedResultCallback_(null);
                                                                return;
                                                              };
                                                              request_ = __new(XMLHttpRequest,[]);
                                                              try
                                                              {
                                                                request_.open("GET",url,true);
                                                              }
                                                              catch (e) {
                                                                          request_ = null;
                                                                          receivedResultCallback_(null);
                                                                          return;
                                                                        };
                                                              request_.onreadystatechange = onData;
                                                              stop_ = false;
                                                              var getStream = false;
                                                              if ((opt_getStream !== undefined) && (opt_getStream === true))
                                                              getStream = true;
                                                              getStream_ = getStream;
                                                              try
                                                              {
                                                                request_.send();
                                                              }
                                                              catch (e) {
                                                                          request_ = null;
                                                                          receivedResultCallback_(null);
                                                                          return;
                                                                        };
                                                            },
                                                            arguments.callee,
                                                            "request",
                                                            "gadgets/WikipediaSearch/wikipedia_all.js",
                                                            0);
                                    var stop = __typedjs(function  ()
                                                         {
                                                           if (request_ == null)
                                                           return;
                                                           stop_ = true;
                                                           request_.abort();
                                                           request_ = null;
                                                         },
                                                         arguments.callee,
                                                         "stop",
                                                         "gadgets/WikipediaSearch/wikipedia_all.js",
                                                         1);
                                    var onData = __typedjs(function  ()
                                                           {
                                                             if (stop_)
                                                             return;
                                                             assert(request_ != null);
                                                             if (request_.readyState != 4)
                                                             return;
                                                             if (request_.status != 200)
                                                             {
                                                               receivedResultCallback_(null);
                                                               request_ = null;
                                                               return;
                                                             };
                                                             if (getStream_)
                                                             {
                                                               receivedResultCallback_(request_.responseStream);
                                                             }
                                                             else {
                                                                    receivedResultCallback_(request_.responseText);
                                                                  };
                                                             request_ = null;
                                                           },
                                                           arguments.callee,
                                                           "onData",
                                                           "gadgets/WikipediaSearch/wikipedia_all.js",
                                                           2);
                                    __thisref(this,arguments.callee).request = request;
                                    __thisref(this,arguments.callee).stop = stop;
                                  },
                                  undefined,
                                  "SimpleHTTPRequest",
                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                  21);
var Throbber = __typedjs(function  (ownerDiv,
                                    imagePrefix,
                                    imageSuffix,
                                    totalFrames,
                                    frameDelay)
                         {
                           var imagePrefix_ = imagePrefix;
                           var imageSuffix_ = imageSuffix;
                           var totalFrames_ = totalFrames;
                           var frameDelay_ = frameDelay;
                           var element_ = ownerDiv.appendElement("<img visible=\"false\" />");
                           var curFrame_ = 0;
                           var timer_ = null;
                           var getImageFilename = __typedjs(function  (frame)
                                                            {
                                                              return imagePrefix_ + frame + imageSuffix_;
                                                            },
                                                            arguments.callee,
                                                            "getImageFilename",
                                                            "gadgets/WikipediaSearch/wikipedia_all.js",
                                                            0);
                           var getNextFrame = __typedjs(function  ()
                                                        {
                                                          curFrame_++;
                                                          if (curFrame_ == totalFrames_ + 1)
                                                          curFrame_ = 1;
                                                          return curFrame_;
                                                        },
                                                        arguments.callee,
                                                        "getNextFrame",
                                                        "gadgets/WikipediaSearch/wikipedia_all.js",
                                                        1);
                           var show = __typedjs(function  ()
                                                {
                                                  curFrame_ = 0;
                                                  element_.visible = true;
                                                  if (timer_ == null)
                                                  {
                                                    timer_ = setInterval(onAnimate,frameDelay_);
                                                    onAnimate();
                                                  };
                                                },
                                                arguments.callee,
                                                "show",
                                                "gadgets/WikipediaSearch/wikipedia_all.js",
                                                2);
                           var hide = __typedjs(function  ()
                                                {
                                                  element_.visible = false;
                                                  if (timer_ != null)
                                                  {
                                                    clearInterval(timer_);
                                                    timer_ = null;
                                                  };
                                                },
                                                arguments.callee,
                                                "hide",
                                                "gadgets/WikipediaSearch/wikipedia_all.js",
                                                3);
                           var onAnimate = __typedjs(function  ()
                                                     {
                                                       element_.src = getImageFilename(getNextFrame());
                                                     },
                                                     arguments.callee,
                                                     "onAnimate",
                                                     "gadgets/WikipediaSearch/wikipedia_all.js",
                                                     4);
                           __thisref(this,arguments.callee).show = show;
                           __thisref(this,arguments.callee).hide = hide;
                         },
                         undefined,
                         "Throbber",
                         "gadgets/WikipediaSearch/wikipedia_all.js",
                         22);
var ANIMATION_SLIDE_TIME = 200;
var DETAILS_IMAGE_WIDTH_MAX = 82;
var DETAILS_IMAGE_HEIGHT_MAX = 150;
var DETAILS_IMAGE_X = 260;
var DETAILS_IMAGE_Y = 11;
var DEFAULT_HTML_STYLE = "<html>" + "<style type=\"text/css\">" + "<!--" + "body,td,th {" + "\t font-family: Geneva, Arial, Helvetica, sans-serif;" + "\t font-size: 12px;" + "\t color: #333333;" + "}" + "body {" + "\t margin-left: 0px;" + "\t margin-top: 0px;" + "\t margin-right: 0px;" + "\t margin-bottom: 0px;" + "  overflow: auto;" + "}" + "-->" + "</style>" + "<script language=\"Javascript\">" + "  document.oncontextmenu=new Function(\"return false\")" + "</script>";
var DETAILS_HTML = DEFAULT_HTML_STYLE + "<style type=\"text/css\">" + "<!--" + ".articleimg {" + "  max-width: 82px;" + "  width:expression(this.width > 82 ? \"82px\" : this.width);" + "}" + "-->" + "</style>" + "<body>" + "<table border=0>" + "<tr><td>[ARTICLETEXT]</td><td valign=top>" + "[IMAGESRC]" + "</td></tr></table>" + "</body>" + "</html>";
var IMAGE_SRC = "<img src=\"[IMAGEURL]\" class=\"articleimg\">";
var DetailsController = __typedjs(function  (mainDivElement_,
                                             textElement_,
                                             imageElement_,
                                             popoutElement_,
                                             detailsLoadedCallback_,
                                             showArticleCallback_)
                                  {
                                    var curArticleText_ = null;
                                    var curImageURL_ = null;
                                    var curKeywords_ = null;
                                    var imageRequest_ = null;
                                    var animation_ = null;
                                    var inSidebar_ = (mainDivElement_ == null);
                                    var ieDetailsView_ = null;
                                    if (! inSidebar_)
                                    popoutElement_.onclick = onPopoutClick;
                                    var show = __typedjs(function  (articleText,imageURL,keywords)
                                                         {
                                                           curArticleText_ = articleText;
                                                           curImageURL_ = imageURL;
                                                           curKeywords_ = keywords;
                                                           if (! inSidebar_)
                                                           {
                                                             textElement_.innerText = curArticleText_;
                                                             if (imageURL != null)
                                                             {
                                                               imageRequest_ = __new(SimpleHTTPRequest,
                                                                                     []);
                                                               imageRequest_.request(curImageURL_,
                                                                                     onImageReceived,
                                                                                     true);
                                                             }
                                                             else {
                                                                    if (detailsLoadedCallback_ != null)
                                                                    detailsLoadedCallback_();
                                                                  };
                                                             mainDivElement_.y = - mainDivElement_.height;
                                                             mainDivElement_.visible = true;
                                                             if (animation_ != null)
                                                             animation_.stop();
                                                             animation_ = __new(SimpleAnimation,
                                                                                [onAnimateSlide,
                                                                                 - mainDivElement_.height,
                                                                                 0,
                                                                                 ANIMATION_SLIDE_TIME,
                                                                                 onOpenAnimationCompleted]);
                                                             animation_.start();
                                                           }
                                                           else {
                                                                  var html = DETAILS_HTML;
                                                                  html = html.replace("[ARTICLETEXT]",
                                                                                      curArticleText_);
                                                                  if ((curImageURL_ != null) && (curImageURL_ != ""))
                                                                  {
                                                                    html = html.replace("[IMAGESRC]",
                                                                                        IMAGE_SRC);
                                                                  }
                                                                  else {
                                                                         html = html.replace("[IMAGESRC]",
                                                                                             "");
                                                                       };
                                                                  html = html.replace("[IMAGEURL]",
                                                                                      curImageURL_);
                                                                  ieDetailsView_ = __new(DetailsView,
                                                                                         []);
                                                                  ieDetailsView_.html_content = true;
                                                                  ieDetailsView_.setContent("",
                                                                                            undefined,
                                                                                            html,
                                                                                            false,
                                                                                            0);
                                                                  pluginHelper.showDetailsView(ieDetailsView_,
                                                                                               VIEW_ON_WIKIPEDIA,
                                                                                               gddDetailsViewFlagToolbarOpen,
                                                                                               onDetailsViewFeedback);
                                                                  if (detailsLoadedCallback_ != null)
                                                                  detailsLoadedCallback_();
                                                                };
                                                         },
                                                         arguments.callee,
                                                         "show",
                                                         "gadgets/WikipediaSearch/wikipedia_all.js",
                                                         0);
                                    var onDetailsViewFeedback = __typedjs(function  (detailsViewFlags)
                                                                          {
                                                                            if (detailsViewFlags == gddDetailsViewFlagToolbarOpen)
                                                                            showArticleCallback_();
                                                                          },
                                                                          arguments.callee,
                                                                          "onDetailsViewFeedback",
                                                                          "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                          1);
                                    var hide = __typedjs(function  ()
                                                         {
                                                           curArticleText_ = null;
                                                           curImageURL_ = null;
                                                           if (! inSidebar_)
                                                           {
                                                             if (animation_ != null)
                                                             animation_.stop();
                                                             animation_ = __new(SimpleAnimation,
                                                                                [onAnimateSlide,
                                                                                 0,
                                                                                 - mainDivElement_.height,
                                                                                 ANIMATION_SLIDE_TIME,
                                                                                 onCloseAnimationCompleted]);
                                                             animation_.start();
                                                             if (imageRequest_ != null)
                                                             {
                                                               imageRequest_.stop();
                                                               imageRequest_ = null;
                                                             };
                                                           }
                                                           else {
                                                                };
                                                         },
                                                         arguments.callee,
                                                         "hide",
                                                         "gadgets/WikipediaSearch/wikipedia_all.js",
                                                         2);
                                    var onAnimateSlide = __typedjs(function  (value)
                                                                   {
                                                                     mainDivElement_.y = value;
                                                                   },
                                                                   arguments.callee,
                                                                   "onAnimateSlide",
                                                                   "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                   3);
                                    var resizeImageElement = __typedjs(function  ()
                                                                       {
                                                                         var heightRatio = imageElement_.srcHeight / DETAILS_IMAGE_HEIGHT_MAX;
                                                                         var widthRatio = imageElement_.srcWidth / DETAILS_IMAGE_WIDTH_MAX;
                                                                         var resizeRatio = 1;
                                                                         if (heightRatio > widthRatio)
                                                                         {
                                                                           resizeRatio = 1 / heightRatio;
                                                                         }
                                                                         else {
                                                                                resizeRatio = 1 / widthRatio;
                                                                              };
                                                                         var newWidth = imageElement_.srcWidth * resizeRatio;
                                                                         var newHeight = imageElement_.srcHeight * resizeRatio;
                                                                         var newX = DETAILS_IMAGE_X + (DETAILS_IMAGE_WIDTH_MAX - newWidth);
                                                                         var newY = DETAILS_IMAGE_Y;
                                                                         imageElement_.x = newX;
                                                                         imageElement_.y = newY;
                                                                         if ((imageElement_.srcWidth < DETAILS_IMAGE_WIDTH_MAX) && (imageElement_.srcHeight < DETAILS_IMAGE_HEIGHT_MAX))
                                                                         {
                                                                           imageElement_.width = imageElement_.srcWidth;
                                                                           imageElement_.height = imageElement_.srcHeight;
                                                                         }
                                                                         else {
                                                                                imageElement_.width = newWidth;
                                                                                imageElement_.height = newHeight;
                                                                              };
                                                                       },
                                                                       arguments.callee,
                                                                       "resizeImageElement",
                                                                       "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                       4);
                                    var onOpenAnimationCompleted = __typedjs(function  ()
                                                                             {
                                                                               animation_ = null;
                                                                             },
                                                                             arguments.callee,
                                                                             "onOpenAnimationCompleted",
                                                                             "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                             5);
                                    var onCloseAnimationCompleted = __typedjs(function  ()
                                                                              {
                                                                                animation_ = null;
                                                                                imageElement_.src = "";
                                                                                textElement_.innerText = "";
                                                                                mainDivElement_.visible = false;
                                                                              },
                                                                              arguments.callee,
                                                                              "onCloseAnimationCompleted",
                                                                              "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                              6);
                                    var onImageReceived = __typedjs(function  (data)
                                                                    {
                                                                      imageRequest_ = null;
                                                                      if (detailsLoadedCallback_ != null)
                                                                      detailsLoadedCallback_();
                                                                      if (data == null)
                                                                      return;
                                                                      imageElement_.visible = false;
                                                                      imageElement_.src = data;
                                                                      resizeImageElement();
                                                                      imageElement_.visible = true;
                                                                    },
                                                                    arguments.callee,
                                                                    "onImageReceived",
                                                                    "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                    7);
                                    var onPopoutClick = __typedjs(function  ()
                                                                  {
                                                                    showArticleCallback_();
                                                                  },
                                                                  arguments.callee,
                                                                  "onPopoutClick",
                                                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                  8);
                                    var onDetailsViewTitleClick = __typedjs(function  ()
                                                                            {
                                                                              showArticleCallback_();
                                                                            },
                                                                            arguments.callee,
                                                                            "onDetailsViewTitleClick",
                                                                            "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                            9);
                                    __thisref(this,arguments.callee).show = show;
                                    __thisref(this,arguments.callee).hide = hide;
                                  },
                                  undefined,
                                  "DetailsController",
                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                  23);
var DISAMBIGUATION_DETAILS_HTML = DEFAULT_HTML_STYLE + "<style type=\"text/css\">" + "#vertmenu {" + "  font-family: Verdana, Arial, Helvetica, sans-serif;" + "  font-size: 11px;" + "  color: #666666;" + "  width: 100%;" + "  padding: 0px;" + "  margin: 0px;" + "}" + "#vertmenu ul {" + "  list-style: none;" + "  margin: 0px;" + "  padding: 0px;" + "  border: none;" + "}" + "#vertmenu ul li {" + "  margin: 0px;" + "  padding: 0px;" + "}" + "#vertmenu ul li a {" + "  font-size: 80%;" + "  display: block;" + "  padding: 2px 0px 2px 7px;" + "  text-decoration: none;" + "  color: #008800;" + "  width:100%;" + "}" + "#vertmenu ul li a:hover, #vertmenu ul li a:focus {" + "  color: #000000;" + "  background-color: #eeeeee;" + "}" + "</style>" + "<body>" + "<div align=\"center\">" + "<div id=\"vertmenu\" align=\"left\">" + DISAMBIGUATION + "<ul>" + "[MENUITEMS]";
"</ul>" + "</div>" + "</div>" + "</body>";
var DISAMBIGUATION_DETAILS_HTML_ITEM = "<li><a href=\"\" onclick=\"window.external.selectedItem(\'[TEXT]\'); return false;\">&raquo; [TEXT]</a></li>";
var WikipediaDisambiguation = __typedjs(function  (disambiguationDiv_,
                                                   textboxElement_,
                                                   textboxOnKeyDown_,
                                                   textboxOnKeyPress_,
                                                   selectedDisambiguationItemCallback_)
                                        {
                                          var listbox_ = null;
                                          var ieDetailsView_ = null;
                                          var inSidebar_ = (textboxOnKeyPress_ == null);
                                          if (textboxOnKeyDown_ == null)
                                          {
                                            textboxOnKeyDown_ = __typedjs(function  ()
                                                                          {
                                                                          },
                                                                          arguments.callee,
                                                                          "textboxOnKeyDown_",
                                                                          "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                          0);
                                          };
                                          if (textboxOnKeyPress_ == null)
                                          {
                                            textboxOnKeyPress_ = __typedjs(function  ()
                                                                           {
                                                                           },
                                                                           arguments.callee,
                                                                           "textboxOnKeyPress_",
                                                                           "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                           1);
                                          };
                                          var show = __typedjs(function  (arrayOfItems)
                                                               {
                                                                 setTimeout(__typedjs(function  ()
                                                                                      {
                                                                                        if (! inSidebar_)
                                                                                        {
                                                                                          listbox_ = __new(ListBox,
                                                                                                           [historydiv,
                                                                                                            onAcceptedDisambiguation,
                                                                                                            onCancelledDisambiguation]);
                                                                                          for (var i = 0; i <= arrayOfItems.length - 1; i++)
                                                                                          listbox_.addItem(arrayOfItems[i]);
                                                                                          listbox_.show(0);
                                                                                          textboxElement_.onKeyDown = onTextBoxKeyDown;
                                                                                          textboxElement_.onKeyPress = __typedjs(function  ()
                                                                                                                                 {
                                                                                                                                 },
                                                                                                                                 arguments.callee,
                                                                                                                                 "textboxElement_.onKeyPress",
                                                                                                                                 "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                                                                 0);
                                                                                        }
                                                                                        else {
                                                                                               var itemsHtml = "";
                                                                                               for (var i = 0; i <= arrayOfItems.length - 1; i++)
                                                                                               {
                                                                                                 itemsHtml += DISAMBIGUATION_DETAILS_HTML_ITEM.replace(/\[TEXT\]/g,
                                                                                                                                                       arrayOfItems[i]);
                                                                                               };
                                                                                               var html = DISAMBIGUATION_DETAILS_HTML.replace("[MENUITEMS]",
                                                                                                                                              itemsHtml);
                                                                                               ieDetailsView_ = __new(DetailsView,
                                                                                                                      []);
                                                                                               ieDetailsView_.html_content = true;
                                                                                               ieDetailsView_.setContent("",
                                                                                                                         undefined,
                                                                                                                         html,
                                                                                                                         false,
                                                                                                                         0);
                                                                                               var externalObject = __new(Object,
                                                                                                                          []);
                                                                                               externalObject.selectedItem = onSelectedItem;
                                                                                               ieDetailsView_.external = externalObject;
                                                                                               pluginHelper.showDetailsView(ieDetailsView_,
                                                                                                                            WIKIPEDIA_SEARCH,
                                                                                                                            gddDetailsViewFlagNone,
                                                                                                                            onDisambiguationDetailsViewFeedback);
                                                                                             };
                                                                                      },
                                                                                      arguments.callee,
                                                                                      "",
                                                                                      "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                      0),
                                                                            1);
                                                               },
                                                               arguments.callee,
                                                               "show",
                                                               "gadgets/WikipediaSearch/wikipedia_all.js",
                                                               2);
                                          var hide = __typedjs(function  ()
                                                               {
                                                                 setTimeout(__typedjs(function  ()
                                                                                      {
                                                                                        if (listbox_ != null)
                                                                                        {
                                                                                          listbox_.hide();
                                                                                          listbox_ = null;
                                                                                        };
                                                                                        textboxElement_.onKeyDown = textboxOnKeyDown_;
                                                                                        textboxElement_.onKeyPress = textboxOnKeyPress_;
                                                                                      },
                                                                                      arguments.callee,
                                                                                      "",
                                                                                      "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                      0),
                                                                            1);
                                                               },
                                                               arguments.callee,
                                                               "hide",
                                                               "gadgets/WikipediaSearch/wikipedia_all.js",
                                                               3);
                                          var onSelectedItem = __typedjs(function  (itemText)
                                                                         {
                                                                           selectedDisambiguationItemCallback_(itemText);
                                                                         },
                                                                         arguments.callee,
                                                                         "onSelectedItem",
                                                                         "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                         4);
                                          var onDisambiguationDetailsViewFeedback = __typedjs(function  (detailsViewFlags)
                                                                                              {
                                                                                                if (detailsViewFlags == 0)
                                                                                                selectedDisambiguationItemCallback_(null);
                                                                                              },
                                                                                              arguments.callee,
                                                                                              "onDisambiguationDetailsViewFeedback",
                                                                                              "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                              5);
                                          var onTextBoxKeyDown = __typedjs(function  ()
                                                                           {
                                                                             if (listbox_.notifyKeyPress(event.keyCode))
                                                                             event.returnValue = false;
                                                                           },
                                                                           arguments.callee,
                                                                           "onTextBoxKeyDown",
                                                                           "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                           6);
                                          var onAcceptedDisambiguation = __typedjs(function  (value)
                                                                                   {
                                                                                     hide();
                                                                                     selectedDisambiguationItemCallback_(value);
                                                                                   },
                                                                                   arguments.callee,
                                                                                   "onAcceptedDisambiguation",
                                                                                   "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                   7);
                                          var onCancelledDisambiguation = __typedjs(function  ()
                                                                                    {
                                                                                      hide();
                                                                                      selectedDisambiguationItemCallback_(null);
                                                                                    },
                                                                                    arguments.callee,
                                                                                    "onCancelledDisambiguation",
                                                                                    "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                    8);
                                          __thisref(this,arguments.callee).show = show;
                                          __thisref(this,arguments.callee).hide = hide;
                                        },
                                        undefined,
                                        "WikipediaDisambiguation",
                                        "gadgets/WikipediaSearch/wikipedia_all.js",
                                        24);
var HISTORY_OPTIONS_NAME = "history";
var MAX_HISTORY_ENTRIES = 5;
var HISTORY_DETAILS_HTML = DEFAULT_HTML_STYLE + "<style type=\"text/css\">" + "#vertmenu {" + "  font-family: Verdana, Arial, Helvetica, sans-serif;" + "  font-size: 11px;" + "  color: #666666;" + "  width: 100%;" + "  padding: 0px;" + "  margin: 0px;" + "}" + "#vertmenu ul {" + "  list-style: none;" + "  margin: 0px;" + "  padding: 0px;" + "  border: none;" + "}" + "#vertmenu ul li {" + "  margin: 0px;" + "  padding: 0px;" + "}" + "#vertmenu ul li a {" + "  font-size: 80%;" + "  display: block;" + "  padding: 2px 0px 2px 7px;" + "  text-decoration: none;" + "  color: #008800;" + "  width:100%;" + "}" + "#vertmenu ul li a:hover, #vertmenu ul li a:focus {" + "  color: #000000;" + "  background-color: #eeeeee;" + "}" + "</style>" + "<body>" + "<div id=\"vertmenu\">" + RECENT_SEARCHES + "<ul>" + "[MENUITEMS]";
"</ul>" + "</div>" + "</body>";
var HISTORY_DETAILS_HTML_ITEM = "<li><a href=\"\" onclick=\"window.external.selectedItem(\'[TEXT]\'); return false;\">&raquo; [TEXT]</a></li>";
var MAX_HISTORY_ITEMS = 5;
var WikipediaHistory = __typedjs(function  (historyDiv_,
                                            selectedHistoryItemCallback_)
                                 {
                                   var historyListbox_ = null;
                                   var items_ = [];
                                   var inSidebar_ = (historyDiv_ == null);
                                   var ieDetailsView_ = null;
                                   if (options.exists(HISTORY_OPTIONS_NAME))
                                   {
                                     items_ = options(HISTORY_OPTIONS_NAME).split("|");
                                     if ((items_.length == 1) && (items_[0] == ""))
                                     items_.splice(0,1);
                                   };
                                   var show = __typedjs(function  ()
                                                        {
                                                          if (items_.length == 0)
                                                          return;
                                                          if (! inSidebar_)
                                                          {
                                                            if (historyListbox_ != null)
                                                            return;
                                                            historyListbox_ = __new(ListBox,
                                                                                    [historyDiv_,
                                                                                     onAcceptedEntry,
                                                                                     onCancelledEntry]);
                                                            for (var i = items_.length - 1; i >= 0; i--)
                                                            historyListbox_.addItem(items_[i]);
                                                            historyListbox_.show(null);
                                                          }
                                                          else {
                                                                 var itemsHtml = "";
                                                                 for (var i = items_.length - 1; i >= 0; i--)
                                                                 {
                                                                   itemsHtml += HISTORY_DETAILS_HTML_ITEM.replace(/\[TEXT\]/g,
                                                                                                                  items_[i]);
                                                                 };
                                                                 var html = HISTORY_DETAILS_HTML.replace("[MENUITEMS]",
                                                                                                         itemsHtml);
                                                                 ieDetailsView_ = __new(DetailsView,
                                                                                        []);
                                                                 ieDetailsView_.html_content = true;
                                                                 ieDetailsView_.setContent("",
                                                                                           undefined,
                                                                                           html,
                                                                                           false,
                                                                                           0);
                                                                 var externalObject = __new(Object,
                                                                                            []);
                                                                 externalObject.selectedItem = onSelectedItem;
                                                                 ieDetailsView_.external = externalObject;
                                                                 pluginHelper.showDetailsView(ieDetailsView_,
                                                                                              WIKIPEDIA_SEARCH,
                                                                                              gddDetailsViewFlagNone,
                                                                                              onHistoryDetailsViewFeedback);
                                                               };
                                                        },
                                                        arguments.callee,
                                                        "show",
                                                        "gadgets/WikipediaSearch/wikipedia_all.js",
                                                        0);
                                   var onSelectedItem = __typedjs(function  (itemText)
                                                                  {
                                                                    selectedHistoryItemCallback_(itemText);
                                                                  },
                                                                  arguments.callee,
                                                                  "onSelectedItem",
                                                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                  1);
                                   var onHistoryDetailsViewFeedback = __typedjs(function  (detailsViewFlags)
                                                                                {
                                                                                },
                                                                                arguments.callee,
                                                                                "onHistoryDetailsViewFeedback",
                                                                                "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                                2);
                                   var hide = __typedjs(function  ()
                                                        {
                                                          if (! inSidebar_)
                                                          {
                                                            if (historyListbox_ != null)
                                                            {
                                                              historyListbox_.hide();
                                                              historyListbox_ = null;
                                                            };
                                                          };
                                                        },
                                                        arguments.callee,
                                                        "hide",
                                                        "gadgets/WikipediaSearch/wikipedia_all.js",
                                                        3);
                                   var onAcceptedEntry = __typedjs(function  (text)
                                                                   {
                                                                     hide();
                                                                     selectedHistoryItemCallback_(text);
                                                                   },
                                                                   arguments.callee,
                                                                   "onAcceptedEntry",
                                                                   "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                   4);
                                   var onCancelledEntry = __typedjs(function  ()
                                                                    {
                                                                      hide();
                                                                    },
                                                                    arguments.callee,
                                                                    "onCancelledEntry",
                                                                    "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                    5);
                                   var addHistoryItem = __typedjs(function  (text)
                                                                  {
                                                                    text = text.replace(/\|/g," ");
                                                                    if (items_.length >= 1)
                                                                    {
                                                                      if (items_[items_.length - 1] == text)
                                                                      return;
                                                                    };
                                                                    if (items_.length == MAX_HISTORY_ENTRIES)
                                                                    items_.splice(0,1);
                                                                    items_.push(text);
                                                                    saveHistoryItems();
                                                                  },
                                                                  arguments.callee,
                                                                  "addHistoryItem",
                                                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                  6);
                                   var saveHistoryItems = __typedjs(function  ()
                                                                    {
                                                                      options.putValue(HISTORY_OPTIONS_NAME,
                                                                                       items_.join("|"));
                                                                    },
                                                                    arguments.callee,
                                                                    "saveHistoryItems",
                                                                    "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                    7);
                                   __thisref(this,arguments.callee).show = show;
                                   __thisref(this,arguments.callee).hide = hide;
                                   __thisref(this,arguments.callee).addHistoryItem = addHistoryItem;
                                 },
                                 undefined,
                                 "WikipediaHistory",
                                 "gadgets/WikipediaSearch/wikipedia_all.js",
                                 25);
var WikipediaQuery = __typedjs(function  ()
                               {
                                 var QUERY_URL = "http://en.wikipedia.org/wiki/Special:Search/";
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
                                 var BAD_IMAGES = ["WIKI_LETTER_W.PNG","LINKFA-STAR.PNG"];
                                 var MINIMUM_PARAGRAPH_LENGTH = 15;
                                 var receivedResultCallback_ = null;
                                 var request_ = null;
                                 var articleURL_ = null;
                                 var pageText_ = null;
                                 var originalQuery_ = null;
                                 var query = __typedjs(function  (name,receivedResultCallback)
                                                       {
                                                         assert(request_ == null);
                                                         if (request_ != null)
                                                         return;
                                                         assert(receivedResultCallback != null);
                                                         if (receivedResultCallback == null)
                                                         return;
                                                         receivedResultCallback_ = receivedResultCallback;
                                                         originalQuery_ = name;
                                                         var safeName = originalQuery_.replace(/ /g,
                                                                                               "_");
                                                         safeName = encodeURI(safeName);
                                                         request_ = __new(SimpleHTTPRequest,[]);
                                                         request_.request(QUERY_URL + safeName,
                                                                          onReceivedWebpage);
                                                         articleURL_ = QUERY_URL + safeName;
                                                         return articleURL_;
                                                       },
                                                       arguments.callee,
                                                       "query",
                                                       "gadgets/WikipediaSearch/wikipedia_all.js",
                                                       0);
                                 var onReceivedWebpage = __typedjs(function  (text)
                                                                   {
                                                                     request_ = null;
                                                                     pageText_ = text;
                                                                     if (pageText_ == null)
                                                                     {
                                                                       receivedResultCallback_(null,
                                                                                               null,
                                                                                               null);
                                                                       return;
                                                                     };
                                                                     if (pageText_.indexOf(MAIN_PAGE_TITLE) != - 1)
                                                                     {
                                                                       receivedResultCallback_(null,
                                                                                               null,
                                                                                               null);
                                                                       return;
                                                                     };
                                                                     var notDisambiguation = false;
                                                                     if (pageText_.indexOf(MULTIPLE_ARTICLES) != - 1)
                                                                     notDisambiguation = true;
                                                                     if (! notDisambiguation)
                                                                     {
                                                                       var disambiguationRegex = DISAMBIGUATION_PAGE_REGEX.exec(pageText_);
                                                                       if ((disambiguationRegex != null) && (disambiguationRegex.length == 1))
                                                                       {
                                                                         receivedResultCallback_("",
                                                                                                 null,
                                                                                                 null);
                                                                         return;
                                                                       };
                                                                     };
                                                                     var firstParagraph = getArticleFirstParagraph();
                                                                     if (firstParagraph == "")
                                                                     {
                                                                       receivedResultCallback_(null,
                                                                                               null,
                                                                                               null);
                                                                       return;
                                                                     };
                                                                     var notFoundRegex = ARTICLE_NOT_FOUND_REGEX.exec(firstParagraph);
                                                                     if ((notFoundRegex != null) && (notFoundRegex.length == 1))
                                                                     {
                                                                       receivedResultCallback_(null,
                                                                                               null,
                                                                                               null);
                                                                       return;
                                                                     };
                                                                     var imageURL = getFirstImageURL();
                                                                     receivedResultCallback_(firstParagraph,
                                                                                             imageURL,
                                                                                             articleURL_);
                                                                   },
                                                                   arguments.callee,
                                                                   "onReceivedWebpage",
                                                                   "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                   1);
                                 var getArticleFirstParagraph = __typedjs(function  ()
                                                                          {
                                                                            var result = null;
                                                                            var firstParagraph = "";
                                                                            while ((result = FIRST_PARAGRAPH_REGEX.exec(pageText_)) != null)
                                                                            {
                                                                              if (result.length >= 2)
                                                                              {
                                                                                var openTagLoc = pageText_.substr(0,
                                                                                                                  result.index).lastIndexOf("<");
                                                                                if (openTagLoc >= - 1)
                                                                                {
                                                                                  if (result[1].indexOf(TEMPLATE) == - 1)
                                                                                  {
                                                                                    var tdCheckText = pageText_.substr(openTagLoc,
                                                                                                                       result.index - openTagLoc).toUpperCase();
                                                                                    if (tdCheckText.indexOf("<TD") == - 1)
                                                                                    {
                                                                                      var plainText = htmlToPlainText(result[1]);
                                                                                      plainText = plainText.replace(/^\s+/g,
                                                                                                                    "");
                                                                                      plainText = plainText.replace(/\s+$/g,
                                                                                                                    "");
                                                                                      if (plainText.length >= MINIMUM_PARAGRAPH_LENGTH)
                                                                                      {
                                                                                        firstParagraph = plainText;
                                                                                        return firstParagraph;
                                                                                      };
                                                                                    };
                                                                                  };
                                                                                };
                                                                              };
                                                                            };
                                                                            return "";
                                                                          },
                                                                          arguments.callee,
                                                                          "getArticleFirstParagraph",
                                                                          "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                          2);
                                 var getFirstImageURL = __typedjs(function  ()
                                                                  {
                                                                    var result = null;
                                                                    var imageURL = "";
                                                                    while ((result = IMAGE_URL_REGEX.exec(pageText_)) != null)
                                                                    {
                                                                      if (result.length >= 2)
                                                                      {
                                                                        var lastCloseTableLoc = pageText_.lastIndexOf(CLOSE_TABLE_TEXT,
                                                                                                                      result.index);
                                                                        var lastOpenTableLoc = pageText_.lastIndexOf(OPEN_TABLE_TEXT,
                                                                                                                     result.index);
                                                                        if (lastCloseTableLoc >= lastOpenTableLoc)
                                                                        {
                                                                          var URL = result[1];
                                                                          var bad = false;
                                                                          for (var i = 0; ((i <= BAD_IMAGES.length - 1) && (! bad)); i++)
                                                                          {
                                                                            if (URL.toUpperCase().indexOf(BAD_IMAGES[i]) > - 1)
                                                                            bad = true;
                                                                          };
                                                                          if (! bad)
                                                                          return result[1];
                                                                        };
                                                                      };
                                                                    };
                                                                    return null;
                                                                  },
                                                                  arguments.callee,
                                                                  "getFirstImageURL",
                                                                  "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                  3);
                                 var stop = __typedjs(function  ()
                                                      {
                                                        if (request_ != null)
                                                        request_.stop();
                                                      },
                                                      arguments.callee,
                                                      "stop",
                                                      "gadgets/WikipediaSearch/wikipedia_all.js",
                                                      4);
                                 var getDisambiguationArray = __typedjs(function  ()
                                                                        {
                                                                          var articleTitles = [];
                                                                          var result = null;
                                                                          var upperQuery = originalQuery_.toUpperCase();
                                                                          while ((result = DISAMBIGUATION_TITLES_REGEX.exec(pageText_)) != null)
                                                                          {
                                                                            if (result.length >= 2)
                                                                            {
                                                                              if (result[1].indexOf(":") == - 1)
                                                                              {
                                                                                if (result[0].toUpperCase().indexOf(ACTION_EDIT_SEARCH) == - 1)
                                                                                {
                                                                                  if (result[1].toUpperCase().indexOf(upperQuery) != - 1)
                                                                                  {
                                                                                    var found = false;
                                                                                    var upperResult = htmlToPlainText(result[1].toUpperCase());
                                                                                    for (var i = 0; i <= articleTitles.length - 1; i++)
                                                                                    {
                                                                                      if (articleTitles[i].toUpperCase() == upperResult)
                                                                                      {
                                                                                        found = true;
                                                                                        break;
                                                                                      };
                                                                                    };
                                                                                    if (! found)
                                                                                    articleTitles.push(htmlToPlainText(result[1]));
                                                                                  };
                                                                                };
                                                                              };
                                                                            };
                                                                          };
                                                                          return articleTitles.sort();
                                                                        },
                                                                        arguments.callee,
                                                                        "getDisambiguationArray",
                                                                        "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                        5);
                                 var htmlToPlainText = __typedjs(function  (item)
                                                                 {
                                                                   if (item)
                                                                   {
                                                                     item = item.replace(/<([^>]|\n)*>/g,
                                                                                         "");
                                                                     item = item.replace(/&nbsp;/g,
                                                                                         " ");
                                                                     item = item.replace(/&quot;/g,
                                                                                         "\"");
                                                                     item = item.replace(/&amp;/g,
                                                                                         "&");
                                                                     item = item.replace(/&lt;/g,
                                                                                         "<");
                                                                     item = item.replace(/&gt;/g,
                                                                                         ">");
                                                                     item = item.replace(/&#160;/g,
                                                                                         " ");
                                                                     return item;
                                                                   }
                                                                   else {
                                                                          return "";
                                                                        };
                                                                 },
                                                                 arguments.callee,
                                                                 "htmlToPlainText",
                                                                 "gadgets/WikipediaSearch/wikipedia_all.js",
                                                                 6);
                                 __thisref(this,arguments.callee).query = query;
                                 __thisref(this,arguments.callee).stop = stop;
                                 __thisref(this,
                                           arguments.callee).getDisambiguationArray = getDisambiguationArray;
                               },
                               undefined,
                               "WikipediaQuery",
                               "gadgets/WikipediaSearch/wikipedia_all.js",
                               26);
var assert = __typedjs(function  (cond)
                       {
                         if (! cond)
                         {
                           throw 0;
                         };
                       },
                       undefined,
                       "assert",
                       "gadgets/WikipediaSearch/wikipedia_all.js",
                       27);
