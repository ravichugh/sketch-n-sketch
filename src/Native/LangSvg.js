function combineBounds(a, b) {
  if (!a) { return b; }
  if (!b) { return a; }

  return {
    left:  Math.min(a.left,  b.left),
    top:   Math.min(a.top,   b.top),
    right: Math.max(a.right, b.right),
    bot:   Math.max(a.bot,   b.bot)
  };
}

function defaultIfNaN(perhapsNaN, defaultNum) {
  if (isNaN(perhapsNaN)) {
    return defaultNum;
  } else {
    return perhapsNaN;
  }
}

function isGoodNumber(item) {
  return typeof item === 'number' && !isNaN(item);
}

function perhapsMakeBounds(x1, y1, x2, y2) {
  if (isGoodNumber(x1) && isGoodNumber(y1) && isGoodNumber(x2) && isGoodNumber(y2)) {
    return {
      left:  Math.min(x1, x2),
      top:   Math.min(y1, y2),
      right: Math.max(x1, x2),
      bot:   Math.max(y1, y2)
    }
  } else {
    return null;
  }
}

function expandBounds(perhapsBounds, amount) {
  if (perhapsBounds) {
    return {
      left:  perhapsBounds.left  - amount,
      top:   perhapsBounds.top   - amount,
      right: perhapsBounds.right + amount,
      bot:   perhapsBounds.bot   + amount
    }
  } else {
    return perhapsBounds;
  }
}

// Will cut off arcs.
// Incorrect if relative positioning used.
function pathCommandsToBounds(cmds) {
  var strPts = [];

  // Apparently, `replace` is how you loop over all matches in JS. 🔥
  cmds.replace(/[MLT]\s*([\-\d\.]+)\s+([\-\d\.]+)/, function (_, xStr, yStr) {
    strPts.push([xStr, yStr]);
  });
  cmds.replace(/C\s*([\-\d\.]+)\s+([\-\d\.]+)\s*[,\s]\s*([\-\d\.]+)\s+([\-\d\.]+)\s*[,\s]\s*([\-\d\.]+)\s+([\-\d\.]+)/, function (_, x1Str, y1Str, x2Str, y2Str, xStr, yStr) {
    strPts.push([x1Str, y1Str], [x2Str, y2Str], [xStr, yStr]);
  });
  cmds.replace(/[SQ]\s*([\-\d\.]+)\s+([\-\d\.]+)\s*[,\s]\s*([\-\d\.]+)\s+([\-\d\.]+)/, function (_, x1Str, y1Str, xStr, yStr) {
    strPts.push([x1Str, y1Str], [xStr, yStr]);
  });
  // Arcs. Imprecise.
  // A rx ry x-axis-rotation large-arc-flag sweep-flag x y
  cmds.replace(/A\s*([\-\d\.]+)\s+([\-\d\.]+)\s+([\-\d\.]+)\s+([\-\d\.]+)\s+([\-\d\.]+)\s+([\-\d\.]+)\s+([\-\d\.]+)/, function (_, rxStr, ryStr, __, ___, ____, xStr, yStr) {
    strPts.push([xStr, yStr])
  });

  return strPointsToBounds(strPts);
}

function polyPointsToBounds(pointsStr) {
  var strPts = [];

  // Apparently, `replace` is how you loop over all matches in JS. 🔥
  pointsStr.replace(/([\-\d\.]+)\s+([\-\d\.]+)/, function (_, xStr, yStr) {
    strPts.push([xStr, yStr]);
  });

  return strPointsToBounds(strPts);
}

function strPointsToBounds(strPts) {
  var bounds = null;
	for (var i = 0; i < strPts.length; i++)
	{
    var x = parseFloat(strPts[i][0]);
    var y = parseFloat(strPts[i][1]);
    bounds = combineBounds(bounds, perhapsMakeBounds(x, y, x, y));
  }
  return bounds;
}

function estimatedBounds(vNode) {
  var recurse = estimatedBounds;

  // Yanked from https://github.com/elm-lang/virtual-dom/blob/2.0.4/src/Native/VirtualDom.js
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return recurse(vNode.node);

		case 'tagger':
			return recurse(vNode.node);

		case 'text':
			return null;

		case 'node':
			var children = vNode.children;
      var childBounds = null;

			for (var i = 0; i < children.length; i++)
			{
        childBounds = combineBounds(childBounds, recurse(children[i]));
			}

      switch (vNode.tag)
      {
        case 'svg':
          return childBounds;

        case 'circle':
          var cx          = parseFloat(vNode.facts['ATTR']['cx']);
          var cy          = parseFloat(vNode.facts['ATTR']['cy']);
          var r           = parseFloat(vNode.facts['ATTR']['r']);
          var strokeWidth = defaultIfNaN(parseFloat(vNode.facts['ATTR']['stroke-width']), 0);
          var x1          = cx - r - strokeWidth/2.0;
          var y1          = cy - r - strokeWidth/2.0;
          var x2          = cx + r + strokeWidth/2.0;
          var y2          = cy + r + strokeWidth/2.0;

          return combineBounds(childBounds, perhapsMakeBounds(x1, y1, x2, y2));

        case 'ellipse':
          var cx          = parseFloat(vNode.facts['ATTR']['cx']);
          var cy          = parseFloat(vNode.facts['ATTR']['cy']);
          var rx          = parseFloat(vNode.facts['ATTR']['rx']);
          var ry          = parseFloat(vNode.facts['ATTR']['ry']);
          var strokeWidth = defaultIfNaN(parseFloat(vNode.facts['ATTR']['stroke-width']), 0);
          var x1          = cx - rx - strokeWidth/2.0;
          var y1          = cy - ry - strokeWidth/2.0;
          var x2          = cx + rx + strokeWidth/2.0;
          var y2          = cy + ry + strokeWidth/2.0;

          return combineBounds(childBounds, perhapsMakeBounds(x1, y1, x2, y2));

        case 'image':
          var x1          = parseFloat(vNode.facts['ATTR']['x']);
          var y1          = parseFloat(vNode.facts['ATTR']['y']);
          var width       = parseFloat(vNode.facts['ATTR']['width']);
          var height      = parseFloat(vNode.facts['ATTR']['height']);
          var x2          = x1 + width;
          var y2          = y1 + height;

          return combineBounds(childBounds, perhapsMakeBounds(x1, y1, x2, y2));

        case 'line':
          var x1          = parseFloat(vNode.facts['ATTR']['x1']);
          var y1          = parseFloat(vNode.facts['ATTR']['y1']);
          var x2          = parseFloat(vNode.facts['ATTR']['x2']);
          var y2          = parseFloat(vNode.facts['ATTR']['y2']);
          var strokeWidth = defaultIfNaN(parseFloat(vNode.facts['ATTR']['stroke-width']), 0);
          x1 -= strokeWidth/2.0;
          y1 -= strokeWidth/2.0;
          x2 += strokeWidth/2.0;
          y2 += strokeWidth/2.0;

          return combineBounds(childBounds, perhapsMakeBounds(x1, y1, x2, y2));

        case 'path':
          // May cut off arcs.
          // Incorrect if relative positioning used.
          var cmds        = vNode.facts['ATTR']['d'];
          var strokeWidth = defaultIfNaN(parseFloat(vNode.facts['ATTR']['stroke-width']), 0);
          if (cmds) {
            var pathBounds = pathCommandsToBounds(cmds);
            return combineBounds(childBounds, expandBounds(pathBounds, strokeWidth));
          } else {
            return childBounds;
          }

        case 'polygon':
          var pointsStr   = vNode.facts['ATTR']['points'];
          var strokeWidth = defaultIfNaN(parseFloat(vNode.facts['ATTR']['stroke-width']), 0);
          if (pointsStr) {
            var polyBounds = polyPointsToBounds(pointsStr);
            return combineBounds(childBounds, expandBounds(polyBounds, strokeWidth));
          } else {
            return childBounds;
          }

        case 'polyline':
          var pointsStr   = vNode.facts['ATTR']['points'];
          var strokeWidth = defaultIfNaN(parseFloat(vNode.facts['ATTR']['stroke-width']), 0);
          if (pointsStr) {
            var polyBounds = polyPointsToBounds(pointsStr);
            return combineBounds(childBounds, expandBounds(polyBounds, strokeWidth));
          } else {
            return childBounds;
          }

        case 'rect':
          var x           = parseFloat(vNode.facts['ATTR']['x']);
          var y           = parseFloat(vNode.facts['ATTR']['y']);
          var width       = parseFloat(vNode.facts['ATTR']['width']);
          var height      = parseFloat(vNode.facts['ATTR']['height']);
          var strokeWidth = defaultIfNaN(parseFloat(vNode.facts['ATTR']['stroke-width']), 0);
          var x1          = x - strokeWidth/2.0;
          var y1          = y - strokeWidth/2.0;
          var x2          = x + width + strokeWidth/2.0;
          var y2          = y + height + strokeWidth/2.0;

          return combineBounds(childBounds, perhapsMakeBounds(x1, y1, x2, y2));

        case 'text':
          var x           = parseFloat(vNode.facts['ATTR']['x']);
          var y           = parseFloat(vNode.facts['ATTR']['y']);

          return combineBounds(childBounds, perhapsMakeBounds(x, y, x, y));

        default:
          return childBounds;
      }

    // Todo if needed
		case 'keyed-node':
			return null;

    // Todo if needed
		case 'custom':
			return null;
	}
}


var _user$project$Native_LangSvg = {

  estimatedBounds : function (vNode) {
    var perhapsBounds = estimatedBounds(vNode);
    if (perhapsBounds) {
      return _elm_lang$core$Maybe$Just(perhapsBounds);
    } else {
      return _elm_lang$core$Maybe$Nothing;
    }
  },

  vNodeType : function (vNode) {
    return vNode.type;
  },

  vNodeTag : function (vNode) {
    return vNode.tag;
  },

  vNodeChildren : function (vNode) {
    var childrenArray = vNode.children;

    // Found this code somewhere inside Elm's core.
    var list = _elm_lang$core$Native_List.Nil;
    for (var i = childrenArray.length; i--; )
    {
      list = _elm_lang$core$Native_List.Cons(childrenArray[i], list);
    }
    return list;
  }

    // randomFloat : function(_) {
    //   return Math.random();
    // },
    //
    // crashToNothing : function(thunk) {
    //   try {
    //     // Just (thunk ())
    //     var result = thunk({ctor: '_Tuple0'});
    //     return _elm_lang$core$Maybe$Just(result);
    //   } catch(err) {
    //     if (err.ctor === undefined) { // Internal crash, not something thrown with ImpureGoodies.throw below.
    //       // Nothing
    //       console.log(err);
    //       return _elm_lang$core$Maybe$Nothing;
    //     } else {
    //       throw err;
    //     }
    //   }
    // },
    //
    // crashToError : function(thunk) {
    //   try {
    //     // Ok (thunk ())
    //     var result = thunk({ctor: '_Tuple0'});
    //     return _elm_lang$core$Result$Ok(result);
    //   } catch(err) {
    //     if (err.ctor === undefined) { // Internal crash, not something thrown with ImpureGoodies.throw below.
    //       // Err (toString err)
    //       return _elm_lang$core$Result$Err(err.toString());
    //     } else {
    //       throw err;
    //     }
    //   }
    // },
    //
    // throw : function(exception) {
    //   throw(exception)
    // },
    //
    // tryCatch : function(exceptionConstructorName) { return function(thunk) { return function(catchThunk) {
    //   try {
    //     return thunk({ctor: '_Tuple0'});
    //   } catch(exception) {
    //     if (exception.ctor === exceptionConstructorName) {
    //       return catchThunk(exception);
    //     } else {
    //       throw exception;
    //     }
    //   }
    // }}},
    //
    // mutateRecordField : function(record) { return function(fieldName) { return function(newValue) {
    //   // Sanity check.
    //   if (typeof record[fieldName] == typeof newValue) {
    //     record[fieldName] = newValue;
    //     return record;
    //   } else {
    //     throw "ImpureGoodies.mutateRecordField: types do not match" + (typeof record[fieldName]) + " vs " + (typeof newValue);
    //   }
    // }}},
    //
    // timedRun : function(thunk) {
    //   var start = (new Date()).getTime();
    //   var result = thunk(_elm_lang$core$Native_Utils.Tuple0);
    //   var end = (new Date()).getTime();
    //
    //   return _elm_lang$core$Native_Utils.Tuple2(result, end-start);
    // }

};
