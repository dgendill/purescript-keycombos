'use strict';

exports.nowImpl = function() {
  return window.performance.now();    
}