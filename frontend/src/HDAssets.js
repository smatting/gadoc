/* global exports */

var Data_Maybe = require('../Data.Maybe/index.js');

exports.loadAsset_ = function (name) {
  return function (url) {
    return function () {
      return new Promise(function (resolve, reject) {
        if (typeof window.hdassets[name] === 'undefined') {
          var script = document.createElement('script');
          script.type = 'text/javascript';
          script.src = url;
          script.addEventListener('load', function () {
            if (typeof window.hdassets[name] === 'undefined') {
              reject(new Error("Couldn't load index for type name " + name));
            } else {
              resolve(window.hdassets[name]);
            }
          });
          script.addEventListener('error', reject);
          document.body.appendChild(script);
        } else {
          resolve(window.hdassets[name]);
        }
      });
    };
  };
};

exports.wrLookup = function(key) {
    return function(m) {
        const v = m[key];
        if (typeof v === 'undefined') {
            return Data_Maybe.Nothing.value;
        } else {
            return Data_Maybe.Just.create(v);
        }
    }
}

exports.wrKeys = function(m) {
    return Object.keys(m);
}

exports.maybeUndefined = function(b) {
    return function(f) {
        return function(mu) {
            if (typeof mu === 'undefined') {
                return b;
            } else {
                return f(mu);
            }
        }
    }
}

exports.emptyWrappedMap = {};

// foreign import maybeUndefined :: forall a b. MaybeUndefined a -> b -> (a -> b) -> b
