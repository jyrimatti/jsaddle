{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.WebSockets.Files
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@gmail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Run.Files (
    indexHtml
  , initState
  , runBatch
  , ghcjsHelpers
) where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Text.Heredoc (here)

indexHtml :: ByteString
indexHtml = [here|
    <!DOCTYPE html>
    <html>
    <head>
    <title>JSaddle</title>
    </head>
    <body>
    </body>
    <script src="/jsaddle.js"></script>
    </html>
|]

initState :: ByteString
initState = [here|
            var jsaddle_values = new Map();
            var jsaddle_free = new Map();
            jsaddle_values.set(0, null);
            jsaddle_values.set(1, undefined);
            jsaddle_values.set(2, false);
            jsaddle_values.set(3, true);
            jsaddle_values.set(4, globalThis);
            var jsaddle_index = 100;
            var expectedBatch = 1;
            var lastResults = [0, {"tag": "Success", "contents": [[], []]}];
            var inCallback = 0;
            var asyncBatch = null;
|]

runBatch :: (ByteString -> ByteString) -> Maybe (ByteString -> ByteString) -> ByteString
runBatch send sendSync = "\
    \  var runBatch = function(firstBatch, initialSyncDepth) {\n\
    \    var processBatch = function(timestamp) {\n\
    \      var batch = firstBatch;\n\
    \      var callbacksToFree = [];\n\
    \      var results = [];\n\
    \      inCallback++;\n\
    \      try {\n\
    \        syncDepth = initialSyncDepth || 0;\n\
    \        for(;;){\n\
    \          if(batch[2] === expectedBatch) {\n\
    \            expectedBatch++;\n\
    \            var nCommandsLength = batch[0].length;\n\
    \            for (var nCommand = 0; nCommand != nCommandsLength; nCommand++) {\n\
    \                var cmd = batch[0][nCommand];\n\
    \                if (cmd.Left) {\n\
    \                    var d = cmd.Left;\n\
    \                    switch (d.tag) {\n\
    \                            case \"FreeRef\":\n\
    \                                var refsToFree = jsaddle_free.get(d.contents[0]) || [];\n\
    \                                refsToFree.push(d.contents[1]);\n\
    \                                jsaddle_free.set(d.contents[0], refsToFree);\n\
    \                                break;\n\
    \                            case \"FreeRefs\":\n\
    \                                var refsToFree = jsaddle_free.get(d.contents) || [];\n\
    \                                for(var nRef = 0; nRef != refsToFree.length; nRef++)\n\
    \                                    jsaddle_values.delete(refsToFree[nRef]);\n\
    \                                jsaddle_free.delete(d.contents);\n\
    \                                break;\n\
    \                            case \"SetPropertyByName\":\n\
    \                                jsaddle_values.get(d.contents[0])[d.contents[1]]=jsaddle_values.get(d.contents[2]);\n\
    \                                break;\n\
    \                            case \"SetPropertyAtIndex\":\n\
    \                                jsaddle_values.get(d.contents[0])[d.contents[1]]=jsaddle_values.get(d.contents[2]);\n\
    \                                break;\n\
    \                            case \"EvaluateScript\":\n\
    \                                var n = d.contents[1];\n\
    \                                jsaddle_values.set(n, eval(d.contents[0]));\n\
    \                                break;\n\
    \                            case \"StringToValue\":\n\
    \                                var n = d.contents[1];\n\
    \                                jsaddle_values.set(n, d.contents[0]);\n\
    \                                break;\n\
    \                            case \"JSONValueToValue\":\n\
    \                                var n = d.contents[1];\n\
    \                                jsaddle_values.set(n, d.contents[0]);\n\
    \                                break;\n\
    \                            case \"GetPropertyByName\":\n\
    \                                var n = d.contents[2];\n\
    \                                jsaddle_values.set(n, jsaddle_values.get(d.contents[0])[d.contents[1]]);\n\
    \                                break;\n\
    \                            case \"GetPropertyAtIndex\":\n\
    \                                var n = d.contents[2];\n\
    \                                jsaddle_values.set(n, jsaddle_values.get(d.contents[0])[d.contents[1]]);\n\
    \                                break;\n\
    \                            case \"NumberToValue\":\n\
    \                                var n = d.contents[1];\n\
    \                                jsaddle_values.set(n, d.contents[0]);\n\
    \                                break;\n\
    \                            case \"NewEmptyObject\":\n\
    \                                var n = d.contents;\n\
    \                                jsaddle_values.set(n, {});\n\
    \                                break;\n\
    \                            case \"NewAsyncCallback\":\n\
    \                                (function() {\n\
    \                                    var nFunction = d.contents;\n\
    \                                    var func = function() {\n\
    \                                        var nFunctionInFunc = ++jsaddle_index;\n\
    \                                        jsaddle_values.set(nFunctionInFunc, func);\n\
    \                                        var nThis = ++jsaddle_index;\n\
    \                                        jsaddle_values.set(nThis, this);\n\
    \                                        var args = [];\n\
    \                                        for (var i = 0; i != arguments.length; i++) {\n\
    \                                            var nArg = ++jsaddle_index;\n\
    \                                            jsaddle_values.set(nArg, arguments[i]);\n\
    \                                            args[i] = nArg;\n\
    \                                        }\n\
    \                                        " <> send "{\"tag\": \"Callback\", \"contents\": [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args]}" <> "\n\
    \                                    };\n\
    \                                    jsaddle_values.set(nFunction, func);\n\
    \                                })();\n\
    \                                break;\n\
    \                            case \"NewSyncCallback\":\n\
    \                                (function() {\n\
    \                                    var nFunction = d.contents;\n\
    \                                    var func = function() {\n\
    \                                        var nFunctionInFunc = ++jsaddle_index;\n\
    \                                        jsaddle_values.set(nFunctionInFunc, func);\n\
    \                                        var nThis = ++jsaddle_index;\n\
    \                                        jsaddle_values.set(nThis, this);\n\
    \                                        var args = [];\n\
    \                                        for (var i = 0; i != arguments.length; i++) {\n\
    \                                            var nArg = ++jsaddle_index;\n\
    \                                            jsaddle_values.set(nArg, arguments[i]);\n\
    \                                            args[i] = nArg;\n\
    \                                        }\n" <> (
    case sendSync of
      Just s  ->
        "                                        if(inCallback > 0) {\n\
        \                                          " <> send "{\"tag\": \"Callback\", \"contents\": [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args]}" <> "\n\
        \                                        } else {\n\
        \                                          runBatch(" <> s "{\"tag\": \"Callback\", \"contents\": [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args]}" <> ", 1);\n\
        \                                        }\n"
      Nothing ->
        "                                        " <> send "{\"tag\": \"Callback\", \"contents\": [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args]}" <> "\n"
    ) <>
    "                                    };\n\
    \                                    jsaddle_values.set(nFunction, func);\n\
    \                                })();\n\
    \                                break;\n\
    \                            case \"FreeCallback\":\n\
    \                                callbacksToFree.push(d.contents);\n\
    \                                break;\n\
    \                            case \"CallAsFunction\":\n\
    \                                var n = d.contents[3];\n\
    \                                jsaddle_values.set(n,\n\
    \                                    jsaddle_values.get(d.contents[0]).apply(jsaddle_values.get(d.contents[1]),\n\
    \                                        d.contents[2].map(function(arg){return jsaddle_values.get(arg);})));\n\
    \                                break;\n\
    \                            case \"CallAsConstructor\":\n\
    \                                var n = d.contents[2];\n\
    \                                var r;\n\
    \                                var f = jsaddle_values.get(d.contents[0]);\n\
    \                                var a = d.contents[1].map(function(arg){return jsaddle_values.get(arg);});\n\
    \                                switch(a.length) {\n\
    \                                    case 0 : r = new f(); break;\n\
    \                                    case 1 : r = new f(a[0]); break;\n\
    \                                    case 2 : r = new f(a[0],a[1]); break;\n\
    \                                    case 3 : r = new f(a[0],a[1],a[2]); break;\n\
    \                                    case 4 : r = new f(a[0],a[1],a[2],a[3]); break;\n\
    \                                    case 5 : r = new f(a[0],a[1],a[2],a[3],a[4]); break;\n\
    \                                    case 6 : r = new f(a[0],a[1],a[2],a[3],a[4],a[5]); break;\n\
    \                                    case 7 : r = new f(a[0],a[1],a[2],a[3],a[4],a[5],a[6]); break;\n\
    \                                    default:\n\
    \                                        var ret;\n\
    \                                        var temp = function() {\n\
    \                                            ret = f.apply(this, a);\n\
    \                                        };\n\
    \                                        temp.prototype = f.prototype;\n\
    \                                        var i = new temp();\n\
    \                                        if(ret instanceof Object)\n\
    \                                            r = ret;\n\
    \                                        else {\n\
    \                                            i.constructor = f;\n\
    \                                            r = i;\n\
    \                                        }\n\
    \                                }\n\
    \                                jsaddle_values.set(n, r);\n\
    \                                break;\n\
    \                            case \"NewArray\":\n\
    \                                var n = d.contents[1];\n\
    \                                jsaddle_values.set(n, d.contents[0].map(function(v){return jsaddle_values.get(v);}));\n\
    \                                break;\n\
    \                            case \"SyncWithAnimationFrame\":\n\
    \                                var n = d.contents;\n\
    \                                jsaddle_values.set(n, timestamp);\n\
    \                                break;\n\
    \                            case \"StartSyncBlock\":\n\
    \                                syncDepth++;\n\
    \                                break;\n\
    \                            case \"EndSyncBlock\":\n\
    \                                syncDepth--;\n\
    \                                break;\n\
    \                            default:\n\
    \                                " <> send "{\"tag\": \"ProtocolError\", \"contents\": e.data}" <> "\n\
    \                                return;\n\
    \                    }\n\
    \                } else {\n\
    \                    var d = cmd.Right;\n\
    \                    switch (d.tag) {\n\
    \                            case \"ValueToString\":\n\
    \                                var val = jsaddle_values.get(d.contents);\n\
    \                                var s = val === null ? \"null\" : val === undefined ? \"undefined\" : val.toString();\n\
    \                                results.push({\"tag\": \"ValueToStringResult\", \"contents\": s});\n\
    \                                break;\n\
    \                            case \"ValueToBool\":\n\
    \                                results.push({\"tag\": \"ValueToBoolResult\", \"contents\": jsaddle_values.get(d.contents) ? true : false});\n\
    \                                break;\n\
    \                            case \"ValueToNumber\":\n\
    \                                results.push({\"tag\": \"ValueToNumberResult\", \"contents\": Number(jsaddle_values.get(d.contents))});\n\
    \                                break;\n\
    \                            case \"ValueToJSON\":\n\
    \                                var s = jsaddle_values.get(d.contents) === undefined ? \"\" : JSON.stringify(jsaddle_values.get(d.contents));\n\
    \                                results.push({\"tag\": \"ValueToJSONResult\", \"contents\": s});\n\
    \                                break;\n\
    \                            case \"ValueToJSONValue\":\n\
    \                                results.push({\"tag\": \"ValueToJSONValueResult\", \"contents\": jsaddle_values.get(d.contents)});\n\
    \                                break;\n\
    \                            case \"DeRefVal\":\n\
    \                                var n = d.contents;\n\
    \                                var v = jsaddle_values.get(n);\n\
    \                                var c = (v === null           ) ? [0, \"\"] :\n\
    \                                        (v === undefined      ) ? [1, \"\"] :\n\
    \                                        (v === false          ) ? [2, \"\"] :\n\
    \                                        (v === true           ) ? [3, \"\"] :\n\
    \                                        (typeof v === \"number\") ? [-1, v.toString()] :\n\
    \                                        (typeof v === \"string\") ? [-2, v]\n\
    \                                                                : [-3, \"\"];\n\
    \                                results.push({\"tag\": \"DeRefValResult\", \"contents\": c});\n\
    \                                break;\n\
    \                            case \"IsNull\":\n\
    \                                results.push({\"tag\": \"IsNullResult\", \"contents\": jsaddle_values.get(d.contents) === null});\n\
    \                                break;\n\
    \                            case \"IsUndefined\":\n\
    \                                results.push({\"tag\": \"IsUndefinedResult\", \"contents\": jsaddle_values.get(d.contents) === undefined});\n\
    \                                break;\n\
    \                            case \"InstanceOf\":\n\
    \                                results.push({\"tag\": \"InstanceOfResult\", \"contents\": jsaddle_values.get(d.contents[0]) instanceof jsaddle_values.get(d.contents[1])});\n\
    \                                break;\n\
    \                            case \"StrictEqual\":\n\
    \                                results.push({\"tag\": \"StrictEqualResult\", \"contents\": jsaddle_values.get(d.contents[0]) === jsaddle_values.get(d.contents[1])});\n\
    \                                break;\n\
    \                            case \"PropertyNames\":\n\
    \                                var result = [];\n\
    \                                for (name in jsaddle_values.get(d.contents)) { result.push(name); }\n\
    \                                results.push({\"tag\": \"PropertyNamesResult\", \"contents\": result});\n\
    \                                break;\n\
    \                            case \"Sync\":\n\
    \                                results.push({\"tag\": \"SyncResult\", \"contents\": []});\n\
    \                                break;\n\
    \                            default:\n\
    \                                results.push({\"tag\": \"ProtocolError\", \"contents\": e.data});\n\
    \                        }\n\
    \                }\n\
    \            }\n\
    \            if(syncDepth <= 0) {\n\
    \              lastResults = [batch[2], {\"tag\": \"Success\", \"contents\": [callbacksToFree, results]}];\n\
    \              " <> send "{\"tag\": \"BatchResults\", \"contents\": [lastResults[0], lastResults[1]]}" <> "\n\
    \              break;\n\
    \            } else {\n" <> (
    case sendSync of
      Just s  ->
        "              lastResults = [batch[2], {\"tag\": \"Success\", \"contents\": [callbacksToFree, results]}];\n\
        \              batch = " <> s "{\"tag\": \"BatchResults\", \"contents\": [lastResults[0], lastResults[1]]}" <> ";\n\
        \              results = [];\n\
        \              callbacksToFree = [];\n"
      Nothing ->
        "              " <> send "{\"tag\": \"BatchResults\", \"contents\": [batch[2], {\"tag\": \"Success\", \"contents\": [callbacksToFree, results]}]}" <> "\n\
        \              break;\n"
    ) <>
    "            }\n\
    \          } else {\n\
    \            if(syncDepth <= 0) {\n\
    \              break;\n\
    \            } else {\n" <> (
    case sendSync of
      Just s  ->
        "              if(batch[2] === expectedBatch - 1) {\n\
        \                batch = " <> s "{\"tag\": \"BatchResults\", \"contents\": [lastResults[0], lastResults[1]]}" <> ";\n\
        \              } else {\n\
        \                batch = " <> s "{\"tag\": \"Duplicate\", \"contents\": [batch[2], expectedBatch]}" <> ";\n\
        \              }\n\
        \              results = [];\n\
        \              callbacksToFree = [];\n"
      Nothing ->
        "              " <> send "{\"tag\": \"Duplicate\", \"contents\": [batch[2], expectedBatch]}" <> "\n\
        \              break;\n"
    ) <>
    "            }\n\
    \          }\n\
    \        }\n\
    \      }\n\
    \      catch (err) {\n\
    \        var n = ++jsaddle_index;\n\
    \        jsaddle_values.set(n, err);\n\
    \        console.log(err);\n\
    \        " <> send "{\"tag\": \"BatchResults\", \"contents\": [batch[2], {\"tag\": \"Failure\", \"contents\": [callbacksToFree, results, n, String(err)]}]}" <> "\n\
    \      }\n\
    \      if(inCallback == 1) {\n\
    \          while(asyncBatch !== null) {\n\
    \              var b = asyncBatch;\n\
    \              asyncBatch = null;\n\
    \              if(b[2] == expectedBatch) runBatch(b);\n\
    \          }\n\
    \      }\n\
    \      inCallback--;\n\
    \    };\n\
    \    if(batch[1] && (initialSyncDepth || 0) === 0) {\n\
    \        window.requestAnimationFrame(processBatch);\n\
    \    }\n\
    \    else {\n\
    \        processBatch(window.performance ? window.performance.now() : null);\n\
    \    }\n\
    \  };\n\
    \  runBatch(batch);\n\
    \"

ghcjsHelpers :: ByteString
ghcjsHelpers = [here|
function h$isNumber(o) {    return typeof(o) === 'number';
}

// returns true for null, but not for functions and host objects
function h$isObject(o) {
    return typeof(o) === 'object';
}

function h$isString(o) {
    return typeof(o) === 'string';
}

function h$isSymbol(o) {
    return typeof(o) === 'symbol';
}

function h$isBoolean(o) {
    return typeof(o) === 'boolean';
}

function h$isFunction(o) {
    return typeof(o) === 'function';
}

function h$jsTypeOf(o) {
    var t = typeof(o);
    if(t === 'undefined') return 0;
    if(t === 'object')    return 1;
    if(t === 'boolean')   return 2;
    if(t === 'number')    return 3;
    if(t === 'string')    return 4;
    if(t === 'symbol')    return 5;
    if(t === 'function')  return 6;
    return 7; // other, host object etc
}

function h$jsonTypeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            // it's an array
            return 5;
        } else if (!o) {
            // null 
            return 0;
        } else {
            // it's an object
            return 6;
        }
    }

}
function h$roundUpToMultipleOf(n,m) {
  var rem = n % m;
  return rem === 0 ? n : n - rem + m;
}

function h$newByteArray(len) {
  var len0 = Math.max(h$roundUpToMultipleOf(len, 8), 8);
  var buf = new ArrayBuffer(len0);
  return { buf: buf
         , len: len
         , i3: new Int32Array(buf)
         , u8: new Uint8Array(buf)
         , u1: new Uint16Array(buf)
         , f3: new Float32Array(buf)
         , f6: new Float64Array(buf)
         , dv: new DataView(buf)
         }
}
function h$wrapBuffer(buf, unalignedOk, offset, length) {
  if(!unalignedOk && offset && offset % 8 !== 0) {
    throw ("h$wrapBuffer: offset not aligned:" + offset);
  }
  if(!buf || !(buf instanceof ArrayBuffer))
    throw "h$wrapBuffer: not an ArrayBuffer"
  if(!offset) { offset = 0; }
  if(!length || length < 0) { length = buf.byteLength - offset; }
  return { buf: buf
         , len: length
         , i3: (offset%4) ? null : new Int32Array(buf, offset, length >> 2)
         , u8: new Uint8Array(buf, offset, length)
         , u1: (offset%2) ? null : new Uint16Array(buf, offset, length >> 1)
         , f3: (offset%4) ? null : new Float32Array(buf, offset, length >> 2)
         , f6: (offset%8) ? null : new Float64Array(buf, offset, length >> 3)
         , dv: new DataView(buf, offset, length)
         };
}
function h$newByteArrayFromBase64String(base64) {
  var bin = window.atob(base64);
  var ba = h$newByteArray(bin.length);
  var u8 = ba.u8;
  for (var i = 0; i < bin.length; i++) {
    u8[i] = bin.charCodeAt(i);
  }
  return ba;
}
function h$byteArrayToBase64String(off, len, ba) {
  var bin = '';
  var u8 = ba.u8;
  var end = off + len;
  for (var i = off; i < end; i++) {
    bin += String.fromCharCode(u8[i]);
  }
  return window.btoa(bin);
}
function h$isNumber(o) {    return typeof(o) === 'number';
}

// returns true for null, but not for functions and host objects
function h$isObject(o) {
    return typeof(o) === 'object';
}

function h$isString(o) {
    return typeof(o) === 'string';
}

function h$isSymbol(o) {
    return typeof(o) === 'symbol';
}

function h$isBoolean(o) {
    return typeof(o) === 'boolean';
}

function h$isFunction(o) {
    return typeof(o) === 'function';
}

function h$jsTypeOf(o) {
    var t = typeof(o);
    if(t === 'undefined') return 0;
    if(t === 'object')    return 1;
    if(t === 'boolean')   return 2;
    if(t === 'number')    return 3;
    if(t === 'string')    return 4;
    if(t === 'symbol')    return 5;
    if(t === 'function')  return 6;
    return 7; // other, host object etc
}

function h$jsonTypeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            // it's an array
            return 5;
        } else if (!o) {
            // null 
            return 0;
        } else {
            // it's an object
            return 6;
        }
    }

}
function h$roundUpToMultipleOf(n,m) {
  var rem = n % m;
  return rem === 0 ? n : n - rem + m;
}

function h$newByteArray(len) {
  var len0 = Math.max(h$roundUpToMultipleOf(len, 8), 8);
  var buf = new ArrayBuffer(len0);
  return { buf: buf
         , len: len
         , i3: new Int32Array(buf)
         , u8: new Uint8Array(buf)
         , u1: new Uint16Array(buf)
         , f3: new Float32Array(buf)
         , f6: new Float64Array(buf)
         , dv: new DataView(buf)
         }
}
function h$wrapBuffer(buf, unalignedOk, offset, length) {
  if(!unalignedOk && offset && offset % 8 !== 0) {
    throw ("h$wrapBuffer: offset not aligned:" + offset);
  }
  if(!buf || !(buf instanceof ArrayBuffer))
    throw "h$wrapBuffer: not an ArrayBuffer"
  if(!offset) { offset = 0; }
  if(!length || length < 0) { length = buf.byteLength - offset; }
  return { buf: buf
         , len: length
         , i3: (offset%4) ? null : new Int32Array(buf, offset, length >> 2)
         , u8: new Uint8Array(buf, offset, length)
         , u1: (offset%2) ? null : new Uint16Array(buf, offset, length >> 1)
         , f3: (offset%4) ? null : new Float32Array(buf, offset, length >> 2)
         , f6: (offset%8) ? null : new Float64Array(buf, offset, length >> 3)
         , dv: new DataView(buf, offset, length)
         };
}
function h$newByteArrayFromBase64String(base64) {
  var bin = window.atob(base64);
  var ba = h$newByteArray(bin.length);
  var u8 = ba.u8;
  for (var i = 0; i < bin.length; i++) {
    u8[i] = bin.charCodeAt(i);
  }
  return ba;
}
function h$byteArrayToBase64String(off, len, ba) {
  var bin = '';
  var u8 = ba.u8;
  var end = off + len;
  for (var i = off; i < end; i++) {
    bin += String.fromCharCode(u8[i]);
  }
  return window.btoa(bin);
}
function h$isNumber(o) {    return typeof(o) === 'number';
}

// returns true for null, but not for functions and host objects
function h$isObject(o) {
    return typeof(o) === 'object';
}

function h$isString(o) {
    return typeof(o) === 'string';
}

function h$isSymbol(o) {
    return typeof(o) === 'symbol';
}

function h$isBoolean(o) {
    return typeof(o) === 'boolean';
}

function h$isFunction(o) {
    return typeof(o) === 'function';
}

function h$jsTypeOf(o) {
    var t = typeof(o);
    if(t === 'undefined') return 0;
    if(t === 'object')    return 1;
    if(t === 'boolean')   return 2;
    if(t === 'number')    return 3;
    if(t === 'string')    return 4;
    if(t === 'symbol')    return 5;
    if(t === 'function')  return 6;
    return 7; // other, host object etc
}

function h$jsonTypeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            // it's an array
            return 5;
        } else if (!o) {
            // null 
            return 0;
        } else {
            // it's an object
            return 6;
        }
    }

}
function h$roundUpToMultipleOf(n,m) {
  var rem = n % m;
  return rem === 0 ? n : n - rem + m;
}

function h$newByteArray(len) {
  var len0 = Math.max(h$roundUpToMultipleOf(len, 8), 8);
  var buf = new ArrayBuffer(len0);
  return { buf: buf
         , len: len
         , i3: new Int32Array(buf)
         , u8: new Uint8Array(buf)
         , u1: new Uint16Array(buf)
         , f3: new Float32Array(buf)
         , f6: new Float64Array(buf)
         , dv: new DataView(buf)
         }
}
function h$wrapBuffer(buf, unalignedOk, offset, length) {
  if(!unalignedOk && offset && offset % 8 !== 0) {
    throw ("h$wrapBuffer: offset not aligned:" + offset);
  }
  if(!buf || !(buf instanceof ArrayBuffer))
    throw "h$wrapBuffer: not an ArrayBuffer"
  if(!offset) { offset = 0; }
  if(!length || length < 0) { length = buf.byteLength - offset; }
  return { buf: buf
         , len: length
         , i3: (offset%4) ? null : new Int32Array(buf, offset, length >> 2)
         , u8: new Uint8Array(buf, offset, length)
         , u1: (offset%2) ? null : new Uint16Array(buf, offset, length >> 1)
         , f3: (offset%4) ? null : new Float32Array(buf, offset, length >> 2)
         , f6: (offset%8) ? null : new Float64Array(buf, offset, length >> 3)
         , dv: new DataView(buf, offset, length)
         };
}
function h$newByteArrayFromBase64String(base64) {
  var bin = window.atob(base64);
  var ba = h$newByteArray(bin.length);
  var u8 = ba.u8;
  for (var i = 0; i < bin.length; i++) {
    u8[i] = bin.charCodeAt(i);
  }
  return ba;
}
function h$byteArrayToBase64String(off, len, ba) {
  var bin = '';
  var u8 = ba.u8;
  var end = off + len;
  for (var i = off; i < end; i++) {
    bin += String.fromCharCode(u8[i]);
  }
  return window.btoa(bin);
}
function h$isNumber(o) {    return typeof(o) === 'number';
}

// returns true for null, but not for functions and host objects
function h$isObject(o) {
    return typeof(o) === 'object';
}

function h$isString(o) {
    return typeof(o) === 'string';
}

function h$isSymbol(o) {
    return typeof(o) === 'symbol';
}

function h$isBoolean(o) {
    return typeof(o) === 'boolean';
}

function h$isFunction(o) {
    return typeof(o) === 'function';
}

function h$jsTypeOf(o) {
    var t = typeof(o);
    if(t === 'undefined') return 0;
    if(t === 'object')    return 1;
    if(t === 'boolean')   return 2;
    if(t === 'number')    return 3;
    if(t === 'string')    return 4;
    if(t === 'symbol')    return 5;
    if(t === 'function')  return 6;
    return 7; // other, host object etc
}

function h$jsonTypeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            // it's an array
            return 5;
        } else if (!o) {
            // null 
            return 0;
        } else {
            // it's an object
            return 6;
        }
    }

}
function h$roundUpToMultipleOf(n,m) {
  var rem = n % m;
  return rem === 0 ? n : n - rem + m;
}

function h$newByteArray(len) {
  var len0 = Math.max(h$roundUpToMultipleOf(len, 8), 8);
  var buf = new ArrayBuffer(len0);
  return { buf: buf
         , len: len
         , i3: new Int32Array(buf)
         , u8: new Uint8Array(buf)
         , u1: new Uint16Array(buf)
         , f3: new Float32Array(buf)
         , f6: new Float64Array(buf)
         , dv: new DataView(buf)
         }
}
function h$wrapBuffer(buf, unalignedOk, offset, length) {
  if(!unalignedOk && offset && offset % 8 !== 0) {
    throw ("h$wrapBuffer: offset not aligned:" + offset);
  }
  if(!buf || !(buf instanceof ArrayBuffer))
    throw "h$wrapBuffer: not an ArrayBuffer"
  if(!offset) { offset = 0; }
  if(!length || length < 0) { length = buf.byteLength - offset; }
  return { buf: buf
         , len: length
         , i3: (offset%4) ? null : new Int32Array(buf, offset, length >> 2)
         , u8: new Uint8Array(buf, offset, length)
         , u1: (offset%2) ? null : new Uint16Array(buf, offset, length >> 1)
         , f3: (offset%4) ? null : new Float32Array(buf, offset, length >> 2)
         , f6: (offset%8) ? null : new Float64Array(buf, offset, length >> 3)
         , dv: new DataView(buf, offset, length)
         };
}
function h$newByteArrayFromBase64String(base64) {
  var bin = window.atob(base64);
  var ba = h$newByteArray(bin.length);
  var u8 = ba.u8;
  for (var i = 0; i < bin.length; i++) {
    u8[i] = bin.charCodeAt(i);
  }
  return ba;
}
function h$byteArrayToBase64String(off, len, ba) {
  var bin = '';
  var u8 = ba.u8;
  var end = off + len;
  for (var i = off; i < end; i++) {
    bin += String.fromCharCode(u8[i]);
  }
  return window.btoa(bin);
}
|]
