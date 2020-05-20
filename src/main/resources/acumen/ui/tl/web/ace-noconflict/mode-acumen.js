ace.define("ace/mode/acumen_highlight_rules", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

    var AcumenHighlightRules = function () {
        var keywords = (
            "Continuous|Discrete|always|case|claim|class|create|do|" +
            "else|elseif|default|end|false|for|" +
            "foreach|function|hypothesis|if|in|initially|match|model|move|noelse|" +
            "none|private|splitby|sum|switch|terminate|then|true|type|with"
        );
        var builtinTypes = (
            "Box|Cone|Cylinder|Obj|Sphere|Surface|Text|Triangle"
        );
        var builtinFunctions = (
            "abs|acos|asin|atan|atan2|black|blue|cbrt|cos|cosh|cross|cyan|dot|exp|floor" +
            "green|log|log10|magenta|norm|not|pi|red|rint|round|signum|sin|sinh|sqrt|tan" +
            "tanh|toDegrees|toRadians|white|xor|yellow"
        );
        var keywordMapper = this.createKeywordMapper({
            "support.function": builtinFunctions,
            "keyword": keywords,
            "support.type": builtinTypes
        }, "identifier", true);

        this.$rules = {
            "start": [{
                token: "comment",
                regex: "//.*$"
            }, {
                token: "string",           // " string
                regex: '".*?"'
            }, {
                token: "string",           // character
                regex: "'.'"
            }, {
                token: "constant.numeric", // float
                regex: "[+-]?\\d+(?:(?:\\.\\d*)?(?:[eE][+-]?\\d+)?)?\\b"
            }, {
                token: keywordMapper,
                regex: "[a-zA-Z_$][a-zA-Z0-9_$]*\\b"
            }, {
                token: "keyword.operator",
                regex: "\\+\\/\\-|%|&|&&|\\*|\\+|\\-|\\->|\\.\\*|\\.\\+|\\.\\-|\\.\\.|\\.\\/|\\.\\^|\\/|\\:=|\\:|<|<<|<=|=|==|>|>>|@|\\^|\\||\\|\\||~="
            }, {
                token: "paren.lparen",
                regex: "[\\(]"
            }, {
                token: "paren.rparen",
                regex: "[\\)]"
            }, {
                token: "text",
                regex: "\\s+"
            }]
        };
    };

    oop.inherits(AcumenHighlightRules, TextHighlightRules);

    exports.AcumenHighlightRules = AcumenHighlightRules;
});

ace.define("ace/mode/acumen", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text", "ace/mode/acumen_highlight_rules", "ace/range"], function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var AcumenHighlightRules = require("./acumen_highlight_rules").AcumenHighlightRules;
    var Range = require("../range").Range;

    var Mode = function () {
        this.HighlightRules = AcumenHighlightRules;
        this.$behaviour = this.$defaultBehaviour;
    };
    oop.inherits(Mode, TextMode);

    (function () {

        this.lineCommentStart = "--";

        this.getNextLineIndent = function (state, line, tab) {
            var indent = this.$getIndent(line);

            var tokenizedLine = this.getTokenizer().getLineTokens(line, state);
            var tokens = tokenizedLine.tokens;

            if (tokens.length && tokens[tokens.length - 1].type == "comment") {
                return indent;
            }
            if (state == "start") {
                var match = line.match(/^.*(begin|loop|then|is|do)\s*$/);
                if (match) {
                    indent += tab;
                }
            }

            return indent;
        };

        this.checkOutdent = function (state, line, input) {
            var complete_line = line + input;
            if (complete_line.match(/^\s*(begin|end)$/)) {
                return true;
            }

            return false;
        };

        this.autoOutdent = function (state, session, row) {

            var line = session.getLine(row);
            var prevLine = session.getLine(row - 1);
            var prevIndent = this.$getIndent(prevLine).length;
            var indent = this.$getIndent(line).length;
            if (indent <= prevIndent) {
                return;
            }

            session.outdentRows(new Range(row, 0, row + 2, 0));
        };


        this.$id = "ace/mode/acumen";
    }).call(Mode.prototype);

    exports.Mode = Mode;

}); (function () {
    ace.require(["ace/mode/acumen"], function (m) {
        if (typeof module == "object" && typeof exports == "object" && module) {
            module.exports = m;
        }
    });
})();