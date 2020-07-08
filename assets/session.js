// Copyright (c) 2020 Nathan Faubion
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// 1. Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// 
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// 
// 3. Neither the name of the copyright holder nor the names of its contributors
// may be used to endorse or promote products derived from this software without
// specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

function Session(initScope) {
  var scope = Object.assign(Object.create(null), initScope);
  var tick = 0;
  var canceler;

  function evalBindingGroup(bindings) {
    var names = bindings.map(function(binding) {
      return binding.name;
    });
    var init = names.map(function(name) {
      return "var " + name;
    });
    var unpacked = Object.keys(scope).map(function(name) {
      if (names.indexOf(name) >= 0) {
        return "";
      }
      return "var " + name + " = $$scope." + name;
    });
    var header = unpacked.concat(init).join(";");
    var defs = bindings.map(function(binding) {
      return binding.name + " = " + binding.expr
    }).join(";");
    var props = names.map(function(name) {
      return name + ": " + name;
    }).join(",");
    var result = new Function("$$scope", [
      header,
      defs,
      "return {" + props + "}"
    ].join(";"))(scope);
    names.forEach(function(name) {
      scope[name] = result[name];
    });
  }

  function evalExpr(expr, onError, onSuccess) {
    var tickId = ++tick;
    var unpacked = Object.keys(scope).map(function(name) {
      return "var " + name + " = $$scope." + name;
    });
    var thunk = new Function(
      "$$scope",
      unpacked.concat(["return (" + expr + ")"]).join(";")
    )(scope);
    canceler = thunk(function(err) {
      return function() {
        if (tick !== tickId) {
          return;
        }
        canceler = null;
        onError(err)();
      };
    })(function(result) {
      return function() {
        if (tick !== tickId) {
          return;
        }
        canceler = null;
        scope.it = result;
        onSuccess(result)();
      };
    })();
  }

  function reset() {
    if (canceler) {
      canceler();
    }
    scope = Object.assign(Object.create(null), initScope);
    Object.keys(require.cache).forEach(function(key) {
      delete require.cache[key];
    });
  }

  function getScope() {
    return scope;
  }

  return {
    evalBindingGroup: evalBindingGroup,
    evalExpr: evalExpr,
    reset: reset,
    scope: getScope,
  };
}

var env = {};
var sess = Session(env);

sess.evalBindingGroup([
  { name: "foo", expr: "42" }
]);

sess.evalBindingGroup([
  { name: "bar", expr: "foo + 1" }
]);

sess.evalBindingGroup([
  { name: "f", expr: "(n) => n <= 0 ? n : g(n - 1)" },
  { name: "g", expr: "(n) => n <= 0 ? n : f(n - 1)" }
]);

sess.evalBindingGroup([
  { name: "oh", expr: "f(100)" }
]);

sess.evalExpr(
  "(e) => (s) => () => { var t = setTimeout(() => s('wat!')(), 1000); return () => clearTimeout(t); }",
  (err) => () => {
    console.log("Error: ", err);
  },
  (res) => () => {
    console.log("Done: ", res);
    console.log(sess.scope());
  }
);