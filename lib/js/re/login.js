// Generated by BUCKLESCRIPT VERSION 1.9.3, PLEASE EDIT WITH CARE
'use strict';

var Curry                = require("bs-platform/lib/js/curry.js");
var TextRe               = require("bs-react-native/lib/js/src/components/textRe.js");
var ReactNative          = require("bs-react-native/lib/js/src/reactNative.js");
var ReasonReact          = require("reason-react/lib/js/src/reasonReact.js");
var TouchableHighlightRe = require("bs-react-native/lib/js/src/components/touchableHighlightRe.js");

function render(increment, decrement, goToMainScreen) {
  return ReasonReact.element(/* None */0, /* None */0, Curry.app(ReactNative.View[/* make */0], [
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* None */0,
                  /* array */[
                    ReasonReact.element(/* None */0, /* None */0, TouchableHighlightRe.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[goToMainScreen], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* array */[ReasonReact.element(/* None */0, /* None */0, TextRe.Text[/* make */0](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */["GoToMainScreen"], /* array */[]))])),
                    ReasonReact.element(/* None */0, /* None */0, TouchableHighlightRe.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[increment], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* array */[ReasonReact.element(/* None */0, /* None */0, TextRe.Text[/* make */0](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */["Increment"], /* array */[]))])),
                    ReasonReact.element(/* None */0, /* None */0, TouchableHighlightRe.make(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */[decrement], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* array */[ReasonReact.element(/* None */0, /* None */0, TextRe.Text[/* make */0](/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* Some */["Decrement"], /* array */[]))]))
                  ]
                ]));
}

exports.render = render;
/* TextRe Not a pure module */