define("ace/mode/little_highlight_rules",["require", "exports","module","ace/lib/oop","ace/mode/text_highlight_rules"],function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var LittleHighlightRules = function() {
    var builtinFunctions = "if|pi|cos|sin|arccos|arcsin|floor|ceiling|round|toStringid|always|compose|fst|len|map|map2|foldl|foldr|append|concat|concatMap|cartProd|zip|nil|cons|snoc|hd|tl|reverse|range|list0N|list1N|repeat|intermingle|mult|minus|div|neg|not|implies|clamp|joinStrings|concatStrings|spaces|delimit|parens|circle|ring|ellipse|rect|square|line|polygon|polyline|path|text|addAttr|rectCenter|square|squareCenter|circle_|ellipse_|rect_|square_|line_|polygon_|path_|updateCanvas|twoPi|halfPi|nPointsOnUnitCircle|nPointsOnCircle|nStar|zones|hideZonesTail|basicZonesTail|hSlider_|hSlider|button_|button|rotate";
    
    // regexp must not have capturing parentheses. Use (?:) instead.
    // regexps are ordered -> the first match is used

    this.$rules = 
        {
    "start": [
        {
            token : "comment",
            regex : ";.*$"
        },
        { 
            token: "text",
            regex : /\(/,
            next : "funcname"
        },
        {
            token : "constant.numeric", // float
            regex : /\b\d+(?:\.\d+)?\b/
        },
        {
            token : "constant.library",
            regex : /true|false/
        },
        {
            token : "variable",
            regex : /\w+/
        },
        {
            token : "string",
            regex : '\'(?=.)',
            next  : "qqstring"
        }
    ],
    "funcname" : [
        {
            token : ["storage.type.function-type.little"],
            regex : /\b(?:def|defrec|let|letrec)\b/,
            next : "pattern"
        },
        {
            token : ["support.function.dom.little", "text", "entity.name.function.little"],
            regex : "(?:\\b(?:(svg|svgViewBox))\\b)(\\s+)((?:\\w)*)",
            next : "start"
        },
        {
            token :  ["support.function.little"],
            regex : /\\(?:(\(.*?\))|\w+)/,
            next : "start"
        },
        {
            token : ["keyword.operator.little"],
            regex : /\+|-|\*|\/|</,
            next : "start"
        },
        {
            token : ["function.buildin.little", "text", "entity.name.function.little"],
            regex: "(?:\\b(?:(" + builtinFunctions + "))\\b)(\\s+)((?:\\w)*)",
            next : "start"
        },
        {
            token : ["function.little"],
            regex: /(?:\w+)/,
            next : "start"
        },
    ],
    "pattern" : [
        {
            token : ["variable.parameter"],
            regex : /\[.*?\]/,
            next : "start"
        },
        {
            token : ["variable.parameter"],
            regex : /\w+/,
            next : "start"
        },
    ],
    "qqstring": [
        {
            token: "constant.character.escape.little",
            regex: "\\\\."
        },
        {
            token : "string",
            regex : '[^\'\\\\]+'
        }, 
        {
            token : "string",
            regex : '\'',
            next  : "start"
        }
    ]
    }

}

oop.inherits(LittleHighlightRules, TextHighlightRules);

exports.LittleHighlightRules = LittleHighlightRules;
});

define("ace/mode/little",["require","exports","module","ace/lib/oop","ace/mode/text","ace/mode/lisp_highlight_rules"], function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextMode = require("./text").Mode;
var LittleHighlightRules = require("./little_highlight_rules").LittleHighlightRules;

var Mode = function() {
    this.HighlightRules = LittleHighlightRules;
};
oop.inherits(Mode, TextMode);

(function() {
       
    this.lineCommentStart = ";";
    
    this.$id = "ace/mode/little";
}).call(Mode.prototype);

exports.Mode = Mode;
});
