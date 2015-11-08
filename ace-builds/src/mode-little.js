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
            regex : /\(/,
            onMatch : function(value, state, stack) {
                stack.push("start");
                this.next = "funcname";
                return "paren.lparen";
            }
        },
        {
            regex : /\)/,
            onMatch : function(value, state, stack) {
                this.next = stack.pop();
                return "paren.rparen";
            }
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
            next  : "qstring"
        }
    ],
    "funcname" : [
        {
            regex : /\b(?:def|defrec|let|letrec)\b/,
            onMatch : function(value, state, stack) {
                this.next = "pattern";
                return "storage.type.function-type.little";
            }
        },
        {
            regex : /\)/,
            onMatch : function(value, state, stack) {
                this.next = stack.pop();
                return "paren.rparen";
            }
        },
        {
            token : "support.function.dom.little",
            regex : /\b(?:svg|svgViewBox)\b/,
            next : "start"
        },
        {
            token :  "support.function.little",
            regex : /\\/,
            next : "lambda"
        },
        {
            token : "keyword.operator.little",
            regex : /\+|-|\*|\/|</,
            next : "start"
        },
        {
            token : ["function.buildin.little", "text", "entity.name.function.little"],
            regex: "(?:\\b(?:(" + builtinFunctions + "))\\b)(\\s+)((?:\\w)*)",
            next : "start"
        },
        {
            token : "function.little",
            regex: /(?:\w+)/,
            next : "start"
        },
    ],
    "lambda" : [
        {
            regex : /\[/,
            onMatch : function(value, state, stack) {
                stack.push("pattern");
                this.next = "pattern";
                return "paren.lparen";
            }
        },
        {
            regex : '\'(?=.)',
            next  : "pstring",
            onMatch : function(value, state, stack) {
                stack.push("startpat");
                this.next = "pstring";
                return "variable.parameter";
            }
        },
        {
            token : "paren.lparen",
            regex : /\(/,
            next : "varlist"
        },
        {
            token : "variable.parameter",
            regex : /\w+/,
            next  : "start"
        }
    ],
    "varlist" : [
        {
            token : "paren.rparen",
            regex : /\)/,
            next : "start"
        },  
        {
            token : "variable.parameter",
            regex : /\w+/
        }
    ],
    "pattern" : [
        {
            regex : /\[/,
            onMatch : function(value, state, stack) {
                stack.push("pattern");
                this.next = "pattern";
                return "paren.lparen";
            }
        },
        {
            regex : /\]/,
            onMatch : function(value, state, stack) {
                var lastPush = stack.pop();
                if (lastPush == "pattern") {
                    var nextLastPush = stack.pop();
                    if (nextLastPush != "pattern") {
                        this.next = nextLastPush;
                    } else {
                        stack.push(nextLastPush);
                        this.next = lastPush;
                    }
                } else {
                    //Should never get here, but just in case...
                    this.next = lastPush;
                }
                return "paren.rparen";
            }
        },
        {
            token : "variable.parameter", // float
            regex : /\b\d+(?:\.\d+)?\b/,
            onMatch : function(value, state, stack) {
                var lastPush = stack.pop();
                if (lastPush == "startpat") {
                    this.next = "start";
                } else {
                    stack.push(lastPush);
                    this.next = lastPush;
                }
                return "variable.parameter";
            }
        },
        {
            token : "variable.parameter",
            regex : '\'(?=.)',
            next  : "pstring"
        },
        {
            token : "variable.parameter",
            regex : /true|false/,
            onMatch : function(value, state, stack) {
                var lastPush = stack.pop();
                if (lastPush == "startpat") {
                    this.next = "start";
                } else {
                    stack.push(lastPush);
                    this.next = lastPush;
                }
                return "variable.parameter";
            }
        },
        {
            token : "variable.parameter",
            regex : /\w+/,
            onMatch : function(value, state, stack) {
                var lastPush = stack.pop();
                if (lastPush == "startpat") {
                    this.next = "start";
                } else {
                    stack.push(lastPush);
                    this.next = lastPush;
                }
                return "variable.parameter";
            }
        },
        
    ],
    "qstring": [
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
    ],
    "pstring": [
        {
            token: "constant.character.escape.little",
            regex: "\\\\."
        },
        {
            token : "variable.parameter",
            regex : '[^\'\\\\]+'
        }, 
        {
            token : "variable.parameter",
            regex : '\'',
            onMatch : function(value, state, stack) {
                var lastPush = stack.pop();
                if (lastPush == "start") {
                    this.next = "start";
                } else {
                    stack.push(lastPush);
                    this.next = lastPush;
                }
                return "variable.parameter";
            }
        }
    ]
    }

this.normalizeRules();

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
