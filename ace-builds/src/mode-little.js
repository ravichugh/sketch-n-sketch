define("ace/mode/little_highlight_rules",["require", "exports","module","ace/lib/oop","ace/mode/text_highlight_rules"],function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var LittleHighlightRules = function() {
    var builtinFunctions = "if|pi|cos|sin|arccos|arcsin|floor|ceiling|round|empty|insert|get|remove|always|compose|fst|len|map|map2|foldl|foldr|append|concat|concatMap|cartProd|zip|nil|cons|snoc|hd|tl|reverse|range|list0N|list1N|repeat|intermingle|mult|minus|div|neg|not|implies|clamp|joinStrings|concatStrings|spaces|delimit|parens|circle|ring|ellipse|rect|square|line|polygon|polyline|path|text|addAttr|rectCenter|square|squareCenter|circle_|ellipse_|rect_|square_|line_|polygon_|path_|updateCanvas|twoPi|halfPi|nPointsOnUnitCircle|nPointsOnCircle|nStar|zones|hideZonesTail|basicZonesTail|hSlider_|hSlider|button_|button|rotate";

    // regexp must not have capturing parentheses. Use (?:) instead.
    // regexps are ordered -> the first match is used

    var rParen = {
        regex : /\)/,
        onMatch : function(value, state, stack) {
            this.next = stack.pop();
            return "paren.rparen";
        }
    };

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
        rParen,
        {
            token : "constant.numeric", // float
            regex : /\b\d+(?:\.\d+)?\b/
        },
        {
            token : "constant.library",
            regex : /true|false|null/
        },
        {
            token : "variable",
            regex : /\w[\w']*/
        },
        {
            token : "string",
            regex : '\'(?=.)',
            next  : "qstring"
        },
        {
            token : "string",
            regex : '"(?=.)',
            next  : "qqstring"
        },
        {
            token : "keyword.type.little",
            regex : /:/,
            next  : "inType"
        }
    ],
    "funcname" : [
        {
            regex : /\b(?:def|defrec|let|letrec)\b/,
            onMatch : function(value, state, stack) {
                stack.push("start");
                this.next = "patternStart";
                return "storage.type.function-type.little";
            }
        },
        {
            regex : /\btyp\b/,
            onMatch : function(value, state, stack) {
                stack.push("inType");
                this.next = "patternStart";
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
            regex : /\+|-|\*|\/|<\s/,
            next : "start"
        },
        {
            token : ["function.buildin.little", "text", "entity.name.function.little"],
            regex: "(?:\\b(?:(" + builtinFunctions + "))\\b)(\\s+)(\\w[\\w']*)?",
            next : "start"
        },
        {
            regex : /\btypecase\b/,
            onMatch : function(value, state, stack) {
                stack.push("inTypeCase");
                this.next = "patternStart";
                return "storage.type.function-type.little";
            }
        },
        {
            token : "function.little",
            regex: /(?:\w[\w']*)/,
            next : "start"
        },
    ],
    "startType" : [
        {
            token : "keyword.operator.type.little",
            regex : /(?:List|->)(?:$|\s)/,
            next : "inType"
        }
    ],
    "inType" : [
        {
            regex : /\(/,
            onMatch : function(value, state, stack) {
                stack.push("inType");
                this.next = "startType";
                return "paren.lparen";
            }
        },
        rParen,
        {
            token : "support.type.little",
            regex : /\b(?:_|Num|Bool|String|Null|[A-Z]\w*)\b/,
        },
        {
            token : "support.type.little",
            regex : /\b[a-z]\w*\b/,
        },
    ],
    "startTypeCaseBranchPat" : [
        {
            regex : /\(/,
            onMatch : function(value, state, stack) {
                stack.push("start");
                this.next = "startType";
                return "paren.lparen";
            }
        },
        {
            token : "support.type.little",
            regex : /\b(?:_|Num|Bool|String|Null|[A-Z]\w*)\b/,
            next : "start"
        }
    ],
    "inTypeCase" : [
        {
            regex : /\(/,
            onMatch : function(value, state, stack) {
                stack.push("inTypeCase");
                this.next = "startTypeCaseBranchPat";
                return "paren.lparen";
            }
        },
        rParen,
    ],
    "lambda" : [
        {
            regex : /\[/,
            onMatch : function(value, state, stack) {
                stack.push("start");
                this.next = "patternStart";
                return "paren.lparen";
            }
        },
        {
            token : "paren.lparen",
            regex : /\(/,
            next : "varlist"
        },
        {
            token : "variable.parameter",
            regex : /\w[\w']*/,
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
            regex : /\w[\w']*/
        }
    ],
    "patternStart" : [
      {
          token : "paren.lparen",
          regex : /\[/,
          next : "pattern"
      },
      {
          token : "variable.parameter",
          regex : /\w[\w']*(?=\s*@)/,
          next : "at"
      },
      {
          regex : /\w[\w']*/,
          onMatch : function(value, state, stack) {
              this.next = stack.pop();
              return "variable.parameter";
          }
      },
    ],
    "at" : [
        {
            token : "keyword",
            regex : "@",
            next  : "patternStart"
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
                this.next = stack.pop();
                return "paren.rparen";
            }
        },
        {
            token : "variable.parameter",
            regex : /\w[\w']*(?=\s*@)/,
            next : "at"
        },
        {
            token : "variable.parameter",
            regex : /\w[\w']*/,
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
    "qqstring": [
        {
            token: "constant.character.escape.little",
            regex: "\\\\."
        },
        {
            token : "string",
            regex : '[^\"\\\\]+'
        },
        {
            token : "string",
            regex : '\"',
            next  : "start"
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

