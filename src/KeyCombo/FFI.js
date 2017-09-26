'use strict';

exports.getFunctionArgsImpl = function getFunctionArgsImpl(fn) {
    // for the sake of simplicity we'll only deal
    // with ascii characters, not unicode
    var m = /function [$A-Z_][0-9A-Z_$]*\((?:([$A-Z_][0-9A-Z_$]*)(,?[ ]?))*\)/i;
    var r = (fn && fn.toString) ? fn.toString().match(m)[0] : null;
    var args = [];
    if (r) {
      var fargs = /\(\s*([^)]+?)\s*\)/.exec(r);
      if (fargs && fargs[1]) {
        args = fargs[1].split(/\s*,\s*/);
      }
    }
    return args;   
}

exports.getArgumentCountImpl = function getArgumentCountImpl(fn) {
  return fn && typeof fn == "function" ? fn.length : 0;
}

exports.getMissingRequiredKeysImpl = function getMissingRequiredKeysImpl(foreign) {
  return function(keys) {
    
    function checkKey(acc, key, f) {
      if (!(f && f.hasOwnProperty(key))) {
        acc.push(key);
      }
      return acc;
    }

    return keys.reduce(function(acc, key) {
      if (key.indexOf('.') !== -1) {
        var nestedKeys = key.split('.');
        var focus = foreign;
        
        nestedKeys.some(function(nkey) {
          var r = checkKey([], nkey, focus);
          if (r.length !== 0) {
            acc.push(key)
            return true;
          } else {
            focus = focus[nkey];
            return false;
          }
        });

      } else {
        checkKey(acc, key, foreign);        
      }

      return acc;

    }, []);
  }
}

exports.lengthGTImpl = function(l) {
  return function(arr) {
    return arr.length > l;
  }
}

exports.mutateObjectImpl = function(o) {
  return function(keys) {
    return function(fn) {
      keys.forEach(function(key) {
        if (o.hasOwnProperty(key)) {
          o[key] = fn(o[key]);
        }
      })
      return o;
    }
  }  
}

exports.objectKeyEqualsImpl = function(o) {
  return function(key) {
    return function(value) {
      if (o.hasOwnProperty(key)) {
        return o[key] === value;
      } else {
        return false;
      }
    }
  }
}

exports.setObjectKeyValueImpl = function(o) {
  return function(key) {
    return function(value) {
      o[key] = value;
      return o;
    }
  }
}

exports.allObjectPairsImpl = function(o) {
  return function(keys) {
    return function (fn) {
      return keys.reduce(function(acc, key) {
        if (o.hasOwnProperty(key)) {
          acc = acc && fn(key, o[key])
        }
        
        return acc;
      }, true)

    }
  }
}

exports.isFunctionImpl = function(f) {
  var getType = {};
  return f && getType.toString.call(f) === '[object Function]';
}

exports.getObjectKeyImpl = function(o) {
  return function(key) {
    return function(just) {
      return function(nothing) {
        if (o && o.hasOwnProperty(key)) {
          return just(o[key]);
        } else {
          return nothing;
        }
      }
    }
  }
}

exports.ffiEffFn1ToAffImpl = function ffiEffFn1ToAffImpl(fn) {
  return function(v1) {
    return function(error, success) {
      fn(v1);
      success();
      return function (cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      }
    }    
  }
}

exports.ffiEffFn2ToAffImpl = function ffiEffFn2ToAffImpl(fn) {
  return function(v1) {
    return function(v2) {
      return function(error, success) {
        fn(v1, v2);
        success();
        return function (cancelError, onCancelerError, onCancelerSuccess) {
          onCancelerSuccess();
        }
      }    
    }
  }
}

exports.isStringImpl = function(s) {
  return typeof s == "string";
}