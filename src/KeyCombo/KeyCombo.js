(function (root, factory) {
    if (typeof define === 'function' && define.amd) {
        // AMD. Register as an anonymous module.
        define([], factory);
    } else if (typeof module === 'object' && module.exports) {
        // Node. Does not work with strict CommonJS, but
        // only CommonJS-like environments that support module.exports,
        // like Node.
        module.exports = factory();
    } else {
        // Browser globals (root is window)
        root.addKeyComboListener = factory();
  }
}(typeof self !== 'undefined' ? self : this, function () {


  // Map String Number
  var keyState = {};

  // Map String Number
  var releaseQueue = {};

  // Map String Number -> Int -> undefined
  function stateHasCombo(state, comboThreshold) {
    var isCombo = true, i, keys = [];
    if (Object.keys(state).length <= 1) return false;
    for(i in state) {
      keys.push({ key : i, time : state[i]});
    }
    keys.sort(function(v1, v2) {
      if (v1.time < v2.time) return -1;
      return 1;
    })

    keys.sort(function(v1, v2) {
        if (Math.abs(v1.time - v2.time) > comboThreshold) {
          isCombo = false;
        } else {
          isCombo = isCombo && true;
        }
        return 0;
    });

    return isCombo;
      
  }

  function setEquals(a1, a2) {
    if (a1.length !== a2.length) {
      return false;
    }

    return a1.reduce(function(acc, v, i) {
      return acc && (a2.indexOf(a1[i]) !== -1);
    }, true);
  }

  /*
  addKeyComboListener:
    (onKeyDown :: (String, Object) -> undefined) ->
    (onKeyup :: (Sting, Object) -> undefined) ->
    (onComboRelease :: [String] -> undefined) ->
    (onExactReleaseses :: [{
      keys :: [String],
      onExactRelease :: ([String] -> undefined)
    }]) -> CleanupFunction
  */
  function addKeyComboListener(onKeydown, onKeyup, onComboRelease, onExactReleaseses) {
    var config = {
      comboThreshold : 4
    }

    function keydownFn(e) {
      if (keyState[e.code]) return;
      keyState[e.code] = performance.now();
      releaseQueue = {};
      onKeydown(e.code, keyState);
    }

    function keyupFn(e) {
      releaseQueue[e.code] = performance.now();
      delete keyState[e.code];    

      if (Object.keys(keyState).length == 0) {
        if (stateHasCombo(releaseQueue, config.comboThreshold)) {
          onComboRelease(Object.keys(releaseQueue));

          onExactReleaseses.forEach(function(c) {
            if (setEquals(c.keys, Object.keys(releaseQueue))) {
              c.onExactRelease(c.keys);
            }
          });

          releaseQueue = {};
        }  
      }
      
      onKeyup(e.code, keyState);
    }

    document.addEventListener('keydown', keydownFn);
    document.addEventListener('keyup', keyupFn);

    return function() {
      document.removeEventListener('keydown', keydownFn);
      document.removeEventListener('keyup', keyupFn);
    }
  }

  return addKeyComboListener;

}));
