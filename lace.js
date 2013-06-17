(function(jQuery) {

"use strict";

var	OP_TO_STRING	= Object.prototype.toString;
var ARR_PROTO_SLICE	= Array.prototype.slice;

var ROUTES		= [];
var STORE		= {};
var LAST_OPEN	= null;

//	Adjustment for trim methods.
//
//	See http://forum.jquery.com/topic/faster-jquery-trim.
//	See: http://code.google.com/p/chromium/issues/detail?id=5206
//	Below is a fix for browsers which do not recognize &nbsp; as a whitespace character.
//
//	@see		#trim
//	@see		#trimLeft
//	@see		#trimRight
//
var TRIM_LEFT	= /^\s+/;
var TRIM_RIGHT	= /\s+$/;

if(!/\s/.test("\xA0")) {
	TRIM_LEFT 	= /^[\s\xA0]+/;
	TRIM_RIGHT 	= /[\s\xA0]+$/;
}

//	Whether #trim is a native String method.
//
var NATIVE_TRIM	= !!("".trim);

//	@see	#nextId
//
var COUNTER		= 2112;

//	The events which can create a UI action (which will be routed).
//
//	@see	#bindUI
//
var BOUND_UI_EVENTS = "abort change click dblclick error mouseup mousedown mouseout mouseover mouseenter mouseleave keydown keyup keypress focus blur focusin focusout load unload submit reset resize select scroll";

//	To enable a UI element to fire actions you would so something like:
//
//	<div data-action="click/run/this/route/">ROUTE!</div>
//
var ACTION_SELECTOR	= "[data-action]";

//	##ITERATOR
//
//	Also used by #iterate, being a general iterator over either objects or arrays.
//	NOTE: It is usually more efficient to write your own loop.
//
//	You may break the iteration by returning Boolean `true` from your selective function.
//
//	@param		{Function}		fn		The selective function.
//	@param		{Object}		[targ]	The object to work against. If not sent
//										the default becomes Subject.
//	@param		{Mixed}			[acc]	An accumulator, which is set to result of selective
//										function on each interation through target.
//  @param      {Object}        [ctxt]  A context to run the iterator in.
//	@see	#arrayMethod
//	@see	#iterate
//
var	ITERATOR = function(targ, fn, acc, ctxt) {

	targ = typeof targ === "string" ? lace.get(targ) : targ;
	if(typeof targ !== "object") {
		throw("Non-object as target. Got: " + targ);
	}

	ctxt    = ctxt || targ;
	acc		= acc || [];
	var x = 0;
	var len;
	var n;

	if(lace.is(Array, targ)) {
		len = targ.length;
		while(x < len) {
			acc = fn.call(ctxt, targ[x], x, targ, acc);

			if(acc === true) {
				break;
			}
			x++;
		}
	} else {
		for(n in targ) {
			if(targ.hasOwnProperty(n)) {
				acc = fn.call(ctxt, targ[n], n, targ, acc);
				if(acc === true) {
					break;
				}
			}
		}
	}

	return acc;
};

//	##FIND
//
//	Find nodes in an object.
//
//	@see	#find
//	@see	#locate
//
var FIND 	= function(key, val, path, obj, acc, curKey) {

    //  Keep @path a string
    //
    path = !!path ? path : "";
	acc	= acc || {
		first	: null,
		last	: null,
		node	: null,
		nodes	: [],
		paths	: [],
		key		: key,
		value	: val
	};

	var node = obj;
	var p;

	//	Accumulate info on any hits against this node.
	//
	if(typeof val === "function" ? val(node, curKey, key, path) : node[key] === val) {
		if(!acc.first) {
			acc.first = path;
		}
		acc.last = path;
		acc.node = node;
		acc.nodes.push(node);
		acc.paths.push(path);
	}

	//	Recurse if children.
	//
	if(typeof node === "object") {
		for(p in node) {
			if(node[p]) {
				FIND(key, val, path + (path ? "." : "") + p, node[p], acc, p);
			}
		}
	}

	return acc;
};

//	##ACCESS
//
//	General accessor for an object.  Will get or set a value on an object.
//
//	@param	{Object}	ob		The object to traverse.
//	@param	{String}	path	A path to follow in the object tree, such as "this.is.a.path". 
//								To #get root (#STORE) pass an empty string ("").
//								To #set root (#STORE) pass an empty string ("").
//				
//	@param	{Mixed}		[val]	When setting, send a value.
//
var ACCESS 	= function(ob, path, val) {

	var props 	= path ? path.split('.') : [];
	var	pL		= props.length;
	var nopath	= path === "";
	var	i 		= 0;
	var	p;

	// 	Setting
	//
	//	Note that requesting a path that does not exist will result in that
	//	path being created. This may or may not be what you want. ie:
	//	{ top: { middle: { bottom: { foo: "bar" }}}}
	//
	//	.set("top.middle.new.path", "value") will create:
	//
	//	{ top: { middle: {
	//						bottom: {...}
	//						new:	{ path: "value" }
	//					 }}}
	//
	if(arguments.length > 2) {

		while(i < (pL -1)) {
			p 	= props[i];
			ob 	= ob[p] = typeof ob[p] === "object" ? ob[p] : {};
			i++;
		}

		//	If #set was called with an empty string as path (ie. the root), simply
		//	update #ob. Otherwise, update at path position.
		//
		if(nopath) {
			ob = val;
		} else {
			ob[props[i]] = val;
		}

		return val;

	// 	Getting
	//
	} else {
		if(nopath) {
			return STORE;
		}
		while(((ob = ob[props[i]]) !== void 0) && ++i < pL) {};
	}

	return (ob !== void 0 && ob !== null) ? ob : null;
};

var lace = {
	
	//	##set
	//
	//	Set a value at key.
	//
	//	If you would like to have the value you've just set returned, use #getset.
	//	Otherwise, `this` (Lace) is returned.
	//
	set : function(key, value, obj) {
		ACCESS(obj || STORE, key, value);
		return this;
	},

	//	##setnx
	//
	//	Set only if the value of key is undefined.
	//
	setnx : function(key, value, obj) {
		var c = lace.get(key, obj);
		if(c === void 0) {
			lace.set(key, value, obj);
		}

		return this;
	},

	//	##getset
	//
	//	Set a value at key AND return the value set.
	//
	getset : function(key, value, obj, cb) {
		lace.set(key, value, obj);
		return lace.get(key, obj, cb);
	},

	//	##get
	//
	//	Get value at key.
	//
	get : function(key, obj, cb) {
		if(typeof obj === "function") {
			cb = obj;
			obj = null;
		}
		var r = ACCESS(obj || STORE, key);
		cb && cb.call(lace,r);
		return r;
	},

	//  ##locate
	//
	//	Returns dot-delimited paths to objects containing sought k/v pair
	//  Given object {
	//      level1 {
	//          level2 {
	//              foo : "bar"
	//          }
	//      }
	//  }
	//  lace.locate("foo", "bar") === "level1.level2"
	//
	//	@param	{String}	key		The key to check.
	//	@param	{Mixed}		val		The sought value of key. Can pass a function.
	//	@param	{String}	[path]	A base path to start from. Useful to avoid searching the
	//								entire tree if we know value is in a given branch.
	//	@param	{Object}	[targ]	An object to search in. Defaults to #STORE.
	//
	locate : function(key, val, path, targ) {
		return FIND(key, val, path, targ || STORE);
	},
	
	//	##find
	//
	//	Searches for a specified value, returning path information as to locations.
	//	There may be multiple locations.
	//
	//	@param	{Mixed}	val		The value to search for. Can also be a function, which 
	//							will cause a hit if it returns Bool true. Receives 
	//							arguments : f(curVal, curKey, origKey, path)
	//								currentValue ^      ^ currentKey
	//	@param	{Mixed}	[targ]	The object to search in. Defaults to #STORE
	//
	find : function(val, targ) {
		var fn = typeof val === "function" ? val : function(curVal, curKey, origKey, path) {
			return curVal === val;
		};
		
		return FIND("", fn, "", targ || STORE);
	},
	
	//	##branch
	//
	//	Allows execution of an anon function while maintaining chain.
	//	Additionally, can set conditions for execution of a branch.
	//
	//	@param	{Function}	fn		A function to execute
	//	@param	{Mixed}		[cond]	If a function and returns Bool true execute branch.
	//								Anything else if truthy (!cond) execute branch.
	//
	branch : function(fn, cond) {
		if(cond !== void 0) {
			if(typeof cond === "function") {
				if(!cond()) {
					return lace;
				}
			} else if(!cond) {
				return lace;
			}
		}
		
		fn();
		
		return lace;
	},

    //  ##push
    //
    //	As expected, but only works on internal #STORE 
    //
    //	@param	{String}	key 	The key to manipulate
    //	@param	{Mixed}		val		The value to push. Any number of additional arguments
    //								can be sent:
    //								lace.push("foo.bar", 'a', 'b', 'c' ...)
    //
    push : function(key, val) {
        var arr = lace.get(key);
        if(lace.is(Array, arr)) {
            arr = arr.concat(ARR_PROTO_SLICE.call(arguments, 1));
            lace.set(key, arr);
        }
        
        return this;
    },
    
    //  ##pop
    //
    //	As expected, but only works on internal #STORE 
    //
    //	@param	{String}	key 	The key to manipulate
    //
    pop : function(key) {
        var arr = lace.get(key);
        var r;
        if(lace.is(Array, arr)) {
            r = arr.pop();
            lace.set(key, arr);
        }
        
        return r;
    },
    
    //  ##unshift
    //
    //	As expected, but only works on internal #STORE 
    //
    //	@param	{String}	key 	The key to manipulate
    //	@param	{Mixed}		val		The value to unshift. Any number of additional arguments
    //								can be sent:
    //								lace.unshift("foo.bar", 'a', 'b', 'c' ...)
    //
    unshift : function(key, val) {
        var arr = lace.get(key);
        if(lace.is(Array, arr)) {
            arr = ARR_PROTO_SLICE.call(arguments, 1).concat(arr);
            lace.set(key, arr);
        }
        
        return this;
    },
    
    //  ##shift
    //
    //	As expected, but only works on internal #STORE 
    //
    //	@param	{String}	key 	The key to manipulate
    //
    shift : function(key) {
        var arr = lace.get(key);
        if(lace.is(Array, arr)) {
            var r = arr.shift();
            lace.set(key, arr);
        }
        
        return r;
    },

	//	##each
	//
    each : function(targ, fn, scope) {
        return ITERATOR(targ, function(elem, idx, targ) {
        	fn.call(scope, elem, idx, targ);
        });
    },

	//	##map
	//
	map : function(targ, fn, scope) {
		return ITERATOR(targ, function(elem, idx, targ, acc) {
            acc[idx] = fn.call(scope, elem, idx, targ);
            return acc;
        });
   	},

	//	##filter
	//
	filter : function(targ, fn, scope) {
        return ITERATOR(targ, function(elem, idx, targ, acc) {
            fn.call(scope, elem, idx, targ) && acc.push(elem);
        	return acc;
        });
    },
    
    //	##unique
    //
    unique : function(targ) {
        var len = targ.length;
        var hit = {};
        
        while(len--) {
            hit[targ[len]] && targ.splice(len, 1);
            hit[targ[len]] = len;
        }
        
        return targ;
    },

	//	##leftTrim
	//
	//	Removes whitespace from beginning of a string.
	//
	//	@param		{String}	t	The string to trim.
	//
	leftTrim : function(t) {
		return t.replace(TRIM_LEFT, "");
	},

	//	##rightTrim
	//
	//	Removes whitespace from end of a string.
	//
	//	@param		{String}	t	The string to trim.
	//
	rightTrim : function(t) {
		return t.replace(TRIM_RIGHT, "");
	},

	//	##trim
	//
	//	Removes whitespace from beginning and end of a string.
	//
	//	@param		{String}	[t]	The string to trim.
	//
	trim : function(t) {
		return 	NATIVE_TRIM
					? t.trim()
					: t.replace(TRIM_LEFT, "").replace(TRIM_RIGHT, "");
	},

    //	##nextId
    //
    //	Increments and returns the counter.
    //
    nextId : function(pref) {
    	COUNTER += 1;
    	return pref ? pref + COUNTER : COUNTER;
    },

	// 	##is
	//
	//	@param		{Mixed}		type		An object type.
	// 	@param		{Mixed}		val			The value to check.
	//	@type		{Boolean}
	//
	// Checks whether `val` is of requested `type`.
	//
	is : function(type, val) {

		//	Here we're allowing for a check of undefined:
		//	lace.is(undefined, [some undefined var]) // true
		//
		//	Otherwise, we throw an error (rare case
		//
		if(type === void 0) {
			return val === type;
		}

		if(val === void 0) {
			return false;
		}

		var p;

		switch(type) {
			case Array:
				return OP_TO_STRING.call(val) === '[object Array]';
			break;

			case Object:
				return OP_TO_STRING.call(val) === '[object Object]';
			break;

			case "numeric":
				return !isNaN(parseFloat(val)) && isFinite(val);
			break;

			case "emptyObject":
				for(p in val) {
					return false;
				}
				return true;
			break;

			default:
				return val.constructor === type;
			break;
		}
	},

	//	##extend
	//
	//	Adds a method to Lace. Does some simple checking to ensure validity.
	//
	//	@param	{Mixed}		name	The name of the method. You may send multiple
	//								meth/func pairs by passing a map as #name.
	//	@param	{Function}	[func]	If sending a {String} #name, the method.
	//
	extend	: function(name, func) {
		if(lace.is(Object, name)) {
			var p;
			for(p in name) {
				lace.extend(n, name[p]);
			}
		} else if(!lace.hasOwnProperty(name) && typeof func === "function") {
			lace[name] = func;
		}

		return this;
	},

	//	##open
	//
	//	Introduce a route. 
	//
	//	@param	{String}	route
	//	@example	lace.open("/nav/home/")
	//
	open : function(route) {
		var p = lace.parseRoute(route);

		//	Note that no checking is done for duplicate route subscription. This
		//	may or may not be what you want. To avoid duplicate subscriptions,
		//	use #opennx
		//
		if(typeof p === "object" && p.compiled) {
			LAST_OPEN = ROUTES[ROUTES.push(p) -1];
			
			//	@see	#route
			//
			LAST_OPEN.execute = function(meth, args, resultContext) {
				
				if(!this[meth] || args === void 0 || resultContext === void 0) {
					return false;
				}
				
				var i = 0;
				var len = this[meth].length;
				var fx;
				
				for(; i < len; i++) {
					fx = this[meth][i].apply(resultContext, args);
				
					if(typeof fx === "function") {
						fx.call(this);		
					}
				}
				return true;
			}
		}

		return lace;
	},
	
	//	##times
	//	
	//	Limit execution of route to # of times.
	//	On hitting that limit the route is #close'd
	//
	times : function(cnt) {
		if(+cnt) {
			LAST_OPEN.times = cnt;
		}
		
		return lace;
	},

	//	##opennx
	//
	//	Only open if new (OPEN if does Not eXist)
	//
	opennx : function(route, handler) {
		var p = lace.parseRoute(route);
		var i = ROUTES.length;
		//	An identical route has already been registered. Exit.
		//
		while(i--) {
			if(ROUTES[i].serialized === p.serialized) {
				return lace;
			}
		}

		return lace.open(route, handler);
	},

	//	##close
	//
	close : function(route, handler) {
	
		var p = lace.parseRoute(route);

		ROUTES = lace.filter(ROUTES, function(i) {
		    if(!handler) {
			    return i.serialized !== p.serialized;
			}
		});

		return this;
	},

	//	##route
	//
	//	Routes an action. 
	//
	//	@param	{String}	r			The route.
	//	@param	{String}	action		The action which caused this routing. This is an event, 
	//									like "click" or "mousedown", or a named event, like "foo"
	//	@param	{Object}	result		The action result. This is a jQuery event object.
	//	@param	{Mixed}		[passed]	Any data passed by the original call.
	//
	//	@see	#bindUI
	//	@see	#join
	//
	route : function(r, action, result, passed) {
		var i 	= ROUTES.length;
		var m;
		var rob;
		var args;
		
		//	When routing "by hand" there may be no #result sent.
		//
		result = lace.is(Object, result) ? result : {data:result};

		while(i--) {
			rob = ROUTES[i];

			//	If the regex matches (not null) will receive an array, whose first argument
			//	is the full route, and subsequent args will be any :part matches on
			//	the route. The first arg is shifted out, below, leaving only :part matches,
			//	which come to form the first arguments sent to route event handlers.
			//
			//	@example	route: "/foo/bar/:baz" < "foo/bar/something"
			//				m = ["foo/bar/something","something"]
			//
			if(m = r.match(rob.compiled)) {
				//	This is the full route, first arg of successful match.
				//
				r = m.shift();
				
				//  As a convenience, allow methodless routing by simply passing
				//  result as second argument:
				//  allow   : lace.open("foobar").route("foobar", {foo: 'bar'})
				//  spec    : lace.open("foobar", "actionname", {foo: 'bar'})
				//
				if(typeof action !== "string") {
				    result = action || {};
				    action = "_custom";
				}

				//	All #open handlers receive three arguments:
				//
				//	1. 	The action, something like "click" or "mouseup".
				//	2.	Passed object.
				//	3. 	The route. 
				//
				//	All handlers are called in the scope of #result, which
				//	*may* have a #data attribute, and *may* have an #error attribute.
				//
				//	#always is (ahem) always called.
				//	#error is only called if #result#error is not undefined.
				//
				args = m.concat(action, passed, r);
	
				if(!rob.execute("error", args, result)) {
					rob.execute("action", args, result)
					//	Specific (.click, .mousedown) binding.
					//
					rob.execute(action, args, result)
				}
				
				rob.execute("always", args, result)

				//	For routes with a limit on call # remove if we've reached it.
				//
				rob.times && --rob.times < 1 && ROUTES.splice(i, 1);
			}
		}

		return lace;
	},

	//	##parseRoute
	//
	//	Accepts a route (eg. /users/:userid/:type) and returns an object containing:
	//
	//	#serialized	: A regular expression matching the route, as a string.
	//	#compiled	: A regular expression matching the route.
	//
	//	Also accepts RegExp objects as a route.
	//
	//	@param	{Mixed}	route	The route. Either a string to be converted to a regex,
	//							or a regex.
	//
	parseRoute : function(route) {

		var ret = {};

		if(route.constructor === RegExp) {
			ret.serialized 	= new String(route);
			ret.compiled	= route;

			return ret;
		}

		//	Leading and trailing slashes are optional. We remove these from the
		//	route and bracket all route regexes with `/?`.
		//
		if(route.charAt(route.length -1) === "/") {
			route = route.substring(0, route.length -1);
		};
		if(route.charAt(0) === "/") {
			route = route.substring(1, Infinity);
		};

		//	Replace
		//	1. All splats with a all-inclusive match (capture all that remains in the route).
		//	2. All :key tokens with a group which captures all characters until first slash.
		//
		//	Note as well that the "intra-slash" matcher ([^/]*) will match any non-slash
		//	character between 0(zero) and N times -- which means that a route like
		//	/foo/:bar/:baz will be matched given foo/// or foo/23// or foo/23/
		//	(but not foo/23).
		//
		ret.serialized	= new String('^/?' + route + '/?$').replace(/\*/g, function() {
			return "(.*)";
		}).replace(/:([\w]+)/g, function(token, key, idx, orig) {
			return "([^/]*)";
		})
		ret.compiled = new RegExp(ret.serialized);

		return ret;
	},
	
	//	##addRouteEvent
	//
	//	Add a named method bindable within an #open block.
	//
	//	@example	lace.addRouteEvent("foo")
	//				lace.open("/some/route")
	//				lace.foo(function() {
	//					// This can now be fired with lace.route("/some/route", "foo", {data: "here"})
	//				})
	//
	//	This is the method used to bind #action, #click, etc.
	//
	//	@param	{String}	ev		A string name for this event.
	//
	addRouteEvent : function(ev) {
		lace[ev] = function(fn) {
			if(LAST_OPEN) {
				LAST_OPEN[ev] = LAST_OPEN[ev] || [];
				LAST_OPEN[ev].push(fn);
			}
			return lace;
		}
		return lace;
	},
	
	//////////////////////////////////////////////////////////////////////////////////////
	//																					//
	//										UI Binding									//
	//																					//
	//////////////////////////////////////////////////////////////////////////////////////

	//	##bindUI
	//
	//	Route all events originating on elements with a `data-action` attribute.
	//
	//	NOTE: you may set any number of space-separated routes.
	//	@see	#route
	//
	bindUI : function() {
		jQuery(document.body).on(BOUND_UI_EVENTS, ACTION_SELECTOR, function(event) {

			var $target 	= jQuery(event.currentTarget);
			var actionRoute	= $target.attr("data-action").split(" ");
			var pass		= {};
			var readfrom	= $target.attr("readfrom");
			var form;
			var parent;

			lace.each(actionRoute, function(ar) {

				var rData 	= ar.match(/([\w]+)\/([\/\w]+)([\/]?.*)/);
				var type	= event.type;

				//	If malformed, exit
				//
				if(!rData || !rData[1] || !rData[2]) {
					return lace;
				}

				var action 		= rData[1];
				var route		= rData[2];

				if(action !== type) {
					return lace;
				}

				//	If the action is anything other than a `mousemove`, fetch and pass some
				//	useful target and event data, such as a bound form.
				//	(`mousemove` is unlikely to be the active interaction for a form change,
				//	and as such the expense of seeking unnecessary form references on
				//	invocations  with potential microsecond periodicity is too great. Note
				//	handlers are called within the scope of the $target, so the handler
				//	is free to replicate the given seek).
				//
				if(type !== "mousemove") {
					if(readfrom) {
						form = jQuery("#" + readfrom);
					} else {
						form = $target.closest("form");
					}

					if(form.length) {
						pass.$form		= form;
						pass.formData	= form.length ? form.serialize() : null;
					}
				}

				lace.route.call($target, route, type, event, pass);
			});
		});

		return lace;
	},

	unbindUI : function() {
		jQuery(document.body).off(BOUND_UI_EVENTS, ACTION_SELECTOR);

		return lace;
	}
};

//	Add the #open block handlers (all ui events [click, mousedown, etc] + internals)
//
lace.each(BOUND_UI_EVENTS.split(" ").concat("action","always"), function(e) {
	lace.addRouteEvent(e);
});

//	Add re-chaining aliases for unchained methods
//
lace.each("get getset".split(" "), function(mn) {
	lace["$" + mn] = function(a,b,c,d,e) {
		lace.set("$$", lace[mn].apply(lace, [a,b,c,d,e]));
		return lace;
	};
});

//	When the document is ready, bind
//
jQuery(lace.bindUI);

(typeof exports === 'object' ? exports : window)["lace"] = lace;

})(jQuery);