// Copyright 2006 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Bootstrap for the Google JS Library (Closure).
 *
 * In uncompiled mode base.js will write out Closure's deps file, unless the
 * global <code>CLOSURE_NO_DEPS</code> is set to true.  This allows projects to
 * include their own deps file(s) from different locations.
 *
 * @author arv@google.com (Erik Arvidsson)
 *
 * @provideGoog
 */


/**
 * @define {boolean} Overridden to true by the compiler when
 *     --process_closure_primitives is specified.
 */
var COMPILED = false;


/**
 * Base namespace for the Closure library.  Checks to see goog is already
 * defined in the current scope before assigning to prevent clobbering if
 * base.js is loaded more than once.
 *
 * @const
 */
var goog = goog || {};


/**
 * Reference to the global context.  In most cases this will be 'window'.
 */
goog.global = this;


/**
 * A hook for overriding the define values in uncompiled mode.
 *
 * In uncompiled mode, {@code CLOSURE_UNCOMPILED_DEFINES} may be defined before
 * loading base.js.  If a key is defined in {@code CLOSURE_UNCOMPILED_DEFINES},
 * {@code goog.define} will use the value instead of the default value.  This
 * allows flags to be overwritten without compilation (this is normally
 * accomplished with the compiler's "define" flag).
 *
 * Example:
 * <pre>
 *   var CLOSURE_UNCOMPILED_DEFINES = {'goog.DEBUG': false};
 * </pre>
 *
 * @type {Object<string, (string|number|boolean)>|undefined}
 */
goog.global.CLOSURE_UNCOMPILED_DEFINES;


/**
 * A hook for overriding the define values in uncompiled or compiled mode,
 * like CLOSURE_UNCOMPILED_DEFINES but effective in compiled code.  In
 * uncompiled code CLOSURE_UNCOMPILED_DEFINES takes precedence.
 *
 * Also unlike CLOSURE_UNCOMPILED_DEFINES the values must be number, boolean or
 * string literals or the compiler will emit an error.
 *
 * While any @define value may be set, only those set with goog.define will be
 * effective for uncompiled code.
 *
 * Example:
 * <pre>
 *   var CLOSURE_DEFINES = {'goog.DEBUG': false} ;
 * </pre>
 *
 * @type {Object<string, (string|number|boolean)>|undefined}
 */
goog.global.CLOSURE_DEFINES;


/**
 * Returns true if the specified value is not undefined.
 * WARNING: Do not use this to test if an object has a property. Use the in
 * operator instead.
 *
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is defined.
 */
goog.isDef = function(val) {
  // void 0 always evaluates to undefined and hence we do not need to depend on
  // the definition of the global variable named 'undefined'.
  return val !== void 0;
};


/**
 * Builds an object structure for the provided namespace path, ensuring that
 * names that already exist are not overwritten. For example:
 * "a.b.c" -> a = {};a.b={};a.b.c={};
 * Used by goog.provide and goog.exportSymbol.
 * @param {string} name name of the object that this file defines.
 * @param {*=} opt_object the object to expose at the end of the path.
 * @param {Object=} opt_objectToExportTo The object to add the path to; default
 *     is |goog.global|.
 * @private
 */
goog.exportPath_ = function(name, opt_object, opt_objectToExportTo) {
  var parts = name.split('.');
  var cur = opt_objectToExportTo || goog.global;

  // Internet Explorer exhibits strange behavior when throwing errors from
  // methods externed in this manner.  See the testExportSymbolExceptions in
  // base_test.html for an example.
  if (!(parts[0] in cur) && cur.execScript) {
    cur.execScript('var ' + parts[0]);
  }

  // Certain browsers cannot parse code in the form for((a in b); c;);
  // This pattern is produced by the JSCompiler when it collapses the
  // statement above into the conditional loop below. To prevent this from
  // happening, use a for-loop and reserve the init logic as below.

  // Parentheses added to eliminate strict JS warning in Firefox.
  for (var part; parts.length && (part = parts.shift());) {
    if (!parts.length && goog.isDef(opt_object)) {
      // last part and we have an object; use it
      cur[part] = opt_object;
    } else if (cur[part]) {
      cur = cur[part];
    } else {
      cur = cur[part] = {};
    }
  }
};


/**
 * Defines a named value. In uncompiled mode, the value is retrieved from
 * CLOSURE_DEFINES or CLOSURE_UNCOMPILED_DEFINES if the object is defined and
 * has the property specified, and otherwise used the defined defaultValue.
 * When compiled the default can be overridden using the compiler
 * options or the value set in the CLOSURE_DEFINES object.
 *
 * @param {string} name The distinguished name to provide.
 * @param {string|number|boolean} defaultValue
 */
goog.define = function(name, defaultValue) {
  var value = defaultValue;
  if (!COMPILED) {
    if (goog.global.CLOSURE_UNCOMPILED_DEFINES &&
        Object.prototype.hasOwnProperty.call(
            goog.global.CLOSURE_UNCOMPILED_DEFINES, name)) {
      value = goog.global.CLOSURE_UNCOMPILED_DEFINES[name];
    } else if (
        goog.global.CLOSURE_DEFINES &&
        Object.prototype.hasOwnProperty.call(
            goog.global.CLOSURE_DEFINES, name)) {
      value = goog.global.CLOSURE_DEFINES[name];
    }
  }
  goog.exportPath_(name, value);
};


/**
 * @define {boolean} DEBUG is provided as a convenience so that debugging code
 * that should not be included in a production js_binary can be easily stripped
 * by specifying --define goog.DEBUG=false to the JSCompiler. For example, most
 * toString() methods should be declared inside an "if (goog.DEBUG)" conditional
 * because they are generally used for debugging purposes and it is difficult
 * for the JSCompiler to statically determine whether they are used.
 */
goog.define('goog.DEBUG', true);


/**
 * @define {string} LOCALE defines the locale being used for compilation. It is
 * used to select locale specific data to be compiled in js binary. BUILD rule
 * can specify this value by "--define goog.LOCALE=<locale_name>" as JSCompiler
 * option.
 *
 * Take into account that the locale code format is important. You should use
 * the canonical Unicode format with hyphen as a delimiter. Language must be
 * lowercase, Language Script - Capitalized, Region - UPPERCASE.
 * There are few examples: pt-BR, en, en-US, sr-Latin-BO, zh-Hans-CN.
 *
 * See more info about locale codes here:
 * http://www.unicode.org/reports/tr35/#Unicode_Language_and_Locale_Identifiers
 *
 * For language codes you should use values defined by ISO 693-1. See it here
 * http://www.w3.org/WAI/ER/IG/ert/iso639.htm. There is only one exception from
 * this rule: the Hebrew language. For legacy reasons the old code (iw) should
 * be used instead of the new code (he), see http://wiki/Main/IIISynonyms.
 */
goog.define('goog.LOCALE', 'en');  // default to en


/**
 * @define {boolean} Whether this code is running on trusted sites.
 *
 * On untrusted sites, several native functions can be defined or overridden by
 * external libraries like Prototype, Datejs, and JQuery and setting this flag
 * to false forces closure to use its own implementations when possible.
 *
 * If your JavaScript can be loaded by a third party site and you are wary about
 * relying on non-standard implementations, specify
 * "--define goog.TRUSTED_SITE=false" to the JSCompiler.
 */
goog.define('goog.TRUSTED_SITE', true);


/**
 * @define {boolean} Whether a project is expected to be running in strict mode.
 *
 * This define can be used to trigger alternate implementations compatible with
 * running in EcmaScript Strict mode or warn about unavailable functionality.
 * @see https://goo.gl/g5EoHI
 *
 */
goog.define('goog.STRICT_MODE_COMPATIBLE', false);


/**
 * @define {boolean} Whether code that calls {@link goog.setTestOnly} should
 *     be disallowed in the compilation unit.
 */
goog.define('goog.DISALLOW_TEST_ONLY_CODE', COMPILED && !goog.DEBUG);


/**
 * @define {boolean} Whether to use a Chrome app CSP-compliant method for
 *     loading scripts via goog.require. @see appendScriptSrcNode_.
 */
goog.define('goog.ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING', false);


/**
 * Defines a namespace in Closure.
 *
 * A namespace may only be defined once in a codebase. It may be defined using
 * goog.provide() or goog.module().
 *
 * The presence of one or more goog.provide() calls in a file indicates
 * that the file defines the given objects/namespaces.
 * Provided symbols must not be null or undefined.
 *
 * In addition, goog.provide() creates the object stubs for a namespace
 * (for example, goog.provide("goog.foo.bar") will create the object
 * goog.foo.bar if it does not already exist).
 *
 * Build tools also scan for provide/require/module statements
 * to discern dependencies, build dependency files (see deps.js), etc.
 *
 * @see goog.require
 * @see goog.module
 * @param {string} name Namespace provided by this file in the form
 *     "goog.package.part".
 */
goog.provide = function(name) {
  if (!COMPILED) {
    // Ensure that the same namespace isn't provided twice.
    // A goog.module/goog.provide maps a goog.require to a specific file
    if (goog.isProvided_(name)) {
      throw Error('Namespace "' + name + '" already declared.');
    }
  }

  goog.constructNamespace_(name);
};


/**
 * @param {string} name Namespace provided by this file in the form
 *     "goog.package.part".
 * @param {Object=} opt_obj The object to embed in the namespace.
 * @private
 */
goog.constructNamespace_ = function(name, opt_obj) {
  if (!COMPILED) {
    delete goog.implicitNamespaces_[name];

    var namespace = name;
    while ((namespace = namespace.substring(0, namespace.lastIndexOf('.')))) {
      if (goog.getObjectByName(namespace)) {
        break;
      }
      goog.implicitNamespaces_[namespace] = true;
    }
  }

  goog.exportPath_(name, opt_obj);
};


/**
 * Module identifier validation regexp.
 * Note: This is a conservative check, it is very possible to be more lenient,
 *   the primary exclusion here is "/" and "\" and a leading ".", these
 *   restrictions are intended to leave the door open for using goog.require
 *   with relative file paths rather than module identifiers.
 * @private
 */
goog.VALID_MODULE_RE_ = /^[a-zA-Z_$][a-zA-Z0-9._$]*$/;


/**
 * Defines a module in Closure.
 *
 * Marks that this file must be loaded as a module and claims the namespace.
 *
 * A namespace may only be defined once in a codebase. It may be defined using
 * goog.provide() or goog.module().
 *
 * goog.module() has three requirements:
 * - goog.module may not be used in the same file as goog.provide.
 * - goog.module must be the first statement in the file.
 * - only one goog.module is allowed per file.
 *
 * When a goog.module annotated file is loaded, it is enclosed in
 * a strict function closure. This means that:
 * - any variables declared in a goog.module file are private to the file
 * (not global), though the compiler is expected to inline the module.
 * - The code must obey all the rules of "strict" JavaScript.
 * - the file will be marked as "use strict"
 *
 * NOTE: unlike goog.provide, goog.module does not declare any symbols by
 * itself. If declared symbols are desired, use
 * goog.module.declareLegacyNamespace().
 *
 *
 * See the public goog.module proposal: http://goo.gl/Va1hin
 *
 * @param {string} name Namespace provided by this file in the form
 *     "goog.package.part", is expected but not required.
 */
goog.module = function(name) {
  if (!goog.isString(name) || !name ||
      name.search(goog.VALID_MODULE_RE_) == -1) {
    throw Error('Invalid module identifier');
  }
  if (!goog.isInModuleLoader_()) {
    throw Error('Module ' + name + ' has been loaded incorrectly.');
  }
  if (goog.moduleLoaderState_.moduleName) {
    throw Error('goog.module may only be called once per module.');
  }

  // Store the module name for the loader.
  goog.moduleLoaderState_.moduleName = name;
  if (!COMPILED) {
    // Ensure that the same namespace isn't provided twice.
    // A goog.module/goog.provide maps a goog.require to a specific file
    if (goog.isProvided_(name)) {
      throw Error('Namespace "' + name + '" already declared.');
    }
    delete goog.implicitNamespaces_[name];
  }
};


/**
 * @param {string} name The module identifier.
 * @return {?} The module exports for an already loaded module or null.
 *
 * Note: This is not an alternative to goog.require, it does not
 * indicate a hard dependency, instead it is used to indicate
 * an optional dependency or to access the exports of a module
 * that has already been loaded.
 * @suppress {missingProvide}
 */
goog.module.get = function(name) {
  return goog.module.getInternal_(name);
};


/**
 * @param {string} name The module identifier.
 * @return {?} The module exports for an already loaded module or null.
 * @private
 */
goog.module.getInternal_ = function(name) {
  if (!COMPILED) {
    if (goog.isProvided_(name)) {
      // goog.require only return a value with-in goog.module files.
      return name in goog.loadedModules_ ? goog.loadedModules_[name] :
                                           goog.getObjectByName(name);
    } else {
      return null;
    }
  }
};


/**
 * @private {?{moduleName: (string|undefined), declareLegacyNamespace:boolean}}
 */
goog.moduleLoaderState_ = null;


/**
 * @private
 * @return {boolean} Whether a goog.module is currently being initialized.
 */
goog.isInModuleLoader_ = function() {
  return goog.moduleLoaderState_ != null;
};


/**
 * Provide the module's exports as a globally accessible object under the
 * module's declared name.  This is intended to ease migration to goog.module
 * for files that have existing usages.
 * @suppress {missingProvide}
 */
goog.module.declareLegacyNamespace = function() {
  if (!COMPILED && !goog.isInModuleLoader_()) {
    throw new Error(
        'goog.module.declareLegacyNamespace must be called from ' +
        'within a goog.module');
  }
  if (!COMPILED && !goog.moduleLoaderState_.moduleName) {
    throw Error(
        'goog.module must be called prior to ' +
        'goog.module.declareLegacyNamespace.');
  }
  goog.moduleLoaderState_.declareLegacyNamespace = true;
};


/**
 * Marks that the current file should only be used for testing, and never for
 * live code in production.
 *
 * In the case of unit tests, the message may optionally be an exact namespace
 * for the test (e.g. 'goog.stringTest'). The linter will then ignore the extra
 * provide (if not explicitly defined in the code).
 *
 * @param {string=} opt_message Optional message to add to the error that's
 *     raised when used in production code.
 */
goog.setTestOnly = function(opt_message) {
  if (goog.DISALLOW_TEST_ONLY_CODE) {
    opt_message = opt_message || '';
    throw Error(
        'Importing test-only code into non-debug environment' +
        (opt_message ? ': ' + opt_message : '.'));
  }
};


/**
 * Forward declares a symbol. This is an indication to the compiler that the
 * symbol may be used in the source yet is not required and may not be provided
 * in compilation.
 *
 * The most common usage of forward declaration is code that takes a type as a
 * function parameter but does not need to require it. By forward declaring
 * instead of requiring, no hard dependency is made, and (if not required
 * elsewhere) the namespace may never be required and thus, not be pulled
 * into the JavaScript binary. If it is required elsewhere, it will be type
 * checked as normal.
 *
 *
 * @param {string} name The namespace to forward declare in the form of
 *     "goog.package.part".
 */
goog.forwardDeclare = function(name) {};


/**
 * Forward declare type information. Used to assign types to goog.global
 * referenced object that would otherwise result in unknown type references
 * and thus block property disambiguation.
 */
goog.forwardDeclare('Document');
goog.forwardDeclare('HTMLScriptElement');
goog.forwardDeclare('XMLHttpRequest');


if (!COMPILED) {
  /**
   * Check if the given name has been goog.provided. This will return false for
   * names that are available only as implicit namespaces.
   * @param {string} name name of the object to look for.
   * @return {boolean} Whether the name has been provided.
   * @private
   */
  goog.isProvided_ = function(name) {
    return (name in goog.loadedModules_) ||
        (!goog.implicitNamespaces_[name] &&
         goog.isDefAndNotNull(goog.getObjectByName(name)));
  };

  /**
   * Namespaces implicitly defined by goog.provide. For example,
   * goog.provide('goog.events.Event') implicitly declares that 'goog' and
   * 'goog.events' must be namespaces.
   *
   * @type {!Object<string, (boolean|undefined)>}
   * @private
   */
  goog.implicitNamespaces_ = {'goog.module': true};

  // NOTE: We add goog.module as an implicit namespace as goog.module is defined
  // here and because the existing module package has not been moved yet out of
  // the goog.module namespace. This satisifies both the debug loader and
  // ahead-of-time dependency management.
}


/**
 * Returns an object based on its fully qualified external name.  The object
 * is not found if null or undefined.  If you are using a compilation pass that
 * renames property names beware that using this function will not find renamed
 * properties.
 *
 * @param {string} name The fully qualified name.
 * @param {Object=} opt_obj The object within which to look; default is
 *     |goog.global|.
 * @return {?} The value (object or primitive) or, if not found, null.
 */
goog.getObjectByName = function(name, opt_obj) {
  var parts = name.split('.');
  var cur = opt_obj || goog.global;
  for (var part; part = parts.shift();) {
    if (goog.isDefAndNotNull(cur[part])) {
      cur = cur[part];
    } else {
      return null;
    }
  }
  return cur;
};


/**
 * Globalizes a whole namespace, such as goog or goog.lang.
 *
 * @param {!Object} obj The namespace to globalize.
 * @param {Object=} opt_global The object to add the properties to.
 * @deprecated Properties may be explicitly exported to the global scope, but
 *     this should no longer be done in bulk.
 */
goog.globalize = function(obj, opt_global) {
  var global = opt_global || goog.global;
  for (var x in obj) {
    global[x] = obj[x];
  }
};


/**
 * Adds a dependency from a file to the files it requires.
 * @param {string} relPath The path to the js file.
 * @param {!Array<string>} provides An array of strings with
 *     the names of the objects this file provides.
 * @param {!Array<string>} requires An array of strings with
 *     the names of the objects this file requires.
 * @param {boolean=} opt_isModule Whether this dependency must be loaded as
 *     a module as declared by goog.module.
 */
goog.addDependency = function(relPath, provides, requires, opt_isModule) {
  if (goog.DEPENDENCIES_ENABLED) {
    var provide, require;
    var path = relPath.replace(/\\/g, '/');
    var deps = goog.dependencies_;
    for (var i = 0; provide = provides[i]; i++) {
      deps.nameToPath[provide] = path;
      deps.pathIsModule[path] = !!opt_isModule;
    }
    for (var j = 0; require = requires[j]; j++) {
      if (!(path in deps.requires)) {
        deps.requires[path] = {};
      }
      deps.requires[path][require] = true;
    }
  }
};




// NOTE(nnaze): The debug DOM loader was included in base.js as an original way
// to do "debug-mode" development.  The dependency system can sometimes be
// confusing, as can the debug DOM loader's asynchronous nature.
//
// With the DOM loader, a call to goog.require() is not blocking -- the script
// will not load until some point after the current script.  If a namespace is
// needed at runtime, it needs to be defined in a previous script, or loaded via
// require() with its registered dependencies.
//
// User-defined namespaces may need their own deps file. For a reference on
// creating a deps file, see:
// Externally: https://developers.google.com/closure/library/docs/depswriter
//
// Because of legacy clients, the DOM loader can't be easily removed from
// base.js.  Work is being done to make it disableable or replaceable for
// different environments (DOM-less JavaScript interpreters like Rhino or V8,
// for example). See bootstrap/ for more information.


/**
 * @define {boolean} Whether to enable the debug loader.
 *
 * If enabled, a call to goog.require() will attempt to load the namespace by
 * appending a script tag to the DOM (if the namespace has been registered).
 *
 * If disabled, goog.require() will simply assert that the namespace has been
 * provided (and depend on the fact that some outside tool correctly ordered
 * the script).
 */
goog.define('goog.ENABLE_DEBUG_LOADER', true);


/**
 * @param {string} msg
 * @private
 */
goog.logToConsole_ = function(msg) {
  if (goog.global.console) {
    goog.global.console['error'](msg);
  }
};


/**
 * Implements a system for the dynamic resolution of dependencies that works in
 * parallel with the BUILD system. Note that all calls to goog.require will be
 * stripped by the JSCompiler when the --process_closure_primitives option is
 * used.
 * @see goog.provide
 * @param {string} name Namespace to include (as was given in goog.provide()) in
 *     the form "goog.package.part".
 * @return {?} If called within a goog.module file, the associated namespace or
 *     module otherwise null.
 */
goog.require = function(name) {
  // If the object already exists we do not need do do anything.
  if (!COMPILED) {
    if (goog.ENABLE_DEBUG_LOADER && goog.IS_OLD_IE_) {
      goog.maybeProcessDeferredDep_(name);
    }

    if (goog.isProvided_(name)) {
      if (goog.isInModuleLoader_()) {
        return goog.module.getInternal_(name);
      } else {
        return null;
      }
    }

    if (goog.ENABLE_DEBUG_LOADER) {
      var path = goog.getPathFromDeps_(name);
      if (path) {
        goog.writeScripts_(path);
        return null;
      }
    }

    var errorMessage = 'goog.require could not find: ' + name;
    goog.logToConsole_(errorMessage);

    throw Error(errorMessage);
  }
};


/**
 * Path for included scripts.
 * @type {string}
 */
goog.basePath = '';


/**
 * A hook for overriding the base path.
 * @type {string|undefined}
 */
goog.global.CLOSURE_BASE_PATH;


/**
 * Whether to write out Closure's deps file. By default, the deps are written.
 * @type {boolean|undefined}
 */
goog.global.CLOSURE_NO_DEPS;


/**
 * A function to import a single script. This is meant to be overridden when
 * Closure is being run in non-HTML contexts, such as web workers. It's defined
 * in the global scope so that it can be set before base.js is loaded, which
 * allows deps.js to be imported properly.
 *
 * The function is passed the script source, which is a relative URI. It should
 * return true if the script was imported, false otherwise.
 * @type {(function(string): boolean)|undefined}
 */
goog.global.CLOSURE_IMPORT_SCRIPT;


/**
 * Null function used for default values of callbacks, etc.
 * @return {void} Nothing.
 */
goog.nullFunction = function() {};


/**
 * When defining a class Foo with an abstract method bar(), you can do:
 * Foo.prototype.bar = goog.abstractMethod
 *
 * Now if a subclass of Foo fails to override bar(), an error will be thrown
 * when bar() is invoked.
 *
 * Note: This does not take the name of the function to override as an argument
 * because that would make it more difficult to obfuscate our JavaScript code.
 *
 * @type {!Function}
 * @throws {Error} when invoked to indicate the method should be overridden.
 */
goog.abstractMethod = function() {
  throw Error('unimplemented abstract method');
};


/**
 * Adds a {@code getInstance} static method that always returns the same
 * instance object.
 * @param {!Function} ctor The constructor for the class to add the static
 *     method to.
 */
goog.addSingletonGetter = function(ctor) {
  ctor.getInstance = function() {
    if (ctor.instance_) {
      return ctor.instance_;
    }
    if (goog.DEBUG) {
      // NOTE: JSCompiler can't optimize away Array#push.
      goog.instantiatedSingletons_[goog.instantiatedSingletons_.length] = ctor;
    }
    return ctor.instance_ = new ctor;
  };
};


/**
 * All singleton classes that have been instantiated, for testing. Don't read
 * it directly, use the {@code goog.testing.singleton} module. The compiler
 * removes this variable if unused.
 * @type {!Array<!Function>}
 * @private
 */
goog.instantiatedSingletons_ = [];


/**
 * @define {boolean} Whether to load goog.modules using {@code eval} when using
 * the debug loader.  This provides a better debugging experience as the
 * source is unmodified and can be edited using Chrome Workspaces or similar.
 * However in some environments the use of {@code eval} is banned
 * so we provide an alternative.
 */
goog.define('goog.LOAD_MODULE_USING_EVAL', true);


/**
 * @define {boolean} Whether the exports of goog.modules should be sealed when
 * possible.
 */
goog.define('goog.SEAL_MODULE_EXPORTS', goog.DEBUG);


/**
 * The registry of initialized modules:
 * the module identifier to module exports map.
 * @private @const {!Object<string, ?>}
 */
goog.loadedModules_ = {};


/**
 * True if goog.dependencies_ is available.
 * @const {boolean}
 */
goog.DEPENDENCIES_ENABLED = !COMPILED && goog.ENABLE_DEBUG_LOADER;


if (goog.DEPENDENCIES_ENABLED) {
  /**
   * This object is used to keep track of dependencies and other data that is
   * used for loading scripts.
   * @private
   * @type {{
   *   pathIsModule: !Object<string, boolean>,
   *   nameToPath: !Object<string, string>,
   *   requires: !Object<string, !Object<string, boolean>>,
   *   visited: !Object<string, boolean>,
   *   written: !Object<string, boolean>,
   *   deferred: !Object<string, string>
   * }}
   */
  goog.dependencies_ = {
    pathIsModule: {},  // 1 to 1

    nameToPath: {},  // 1 to 1

    requires: {},  // 1 to many

    // Used when resolving dependencies to prevent us from visiting file twice.
    visited: {},

    written: {},  // Used to keep track of script files we have written.

    deferred: {}  // Used to track deferred module evaluations in old IEs
  };


  /**
   * Tries to detect whether is in the context of an HTML document.
   * @return {boolean} True if it looks like HTML document.
   * @private
   */
  goog.inHtmlDocument_ = function() {
    /** @type {Document} */
    var doc = goog.global.document;
    return doc != null && 'write' in doc;  // XULDocument misses write.
  };


  /**
   * Tries to detect the base path of base.js script that bootstraps Closure.
   * @private
   */
  goog.findBasePath_ = function() {
    if (goog.isDef(goog.global.CLOSURE_BASE_PATH)) {
      goog.basePath = goog.global.CLOSURE_BASE_PATH;
      return;
    } else if (!goog.inHtmlDocument_()) {
      return;
    }
    /** @type {Document} */
    var doc = goog.global.document;
    var scripts = doc.getElementsByTagName('SCRIPT');
    // Search backwards since the current script is in almost all cases the one
    // that has base.js.
    for (var i = scripts.length - 1; i >= 0; --i) {
      var script = /** @type {!HTMLScriptElement} */ (scripts[i]);
      var src = script.src;
      var qmark = src.lastIndexOf('?');
      var l = qmark == -1 ? src.length : qmark;
      if (src.substr(l - 7, 7) == 'base.js') {
        goog.basePath = src.substr(0, l - 7);
        return;
      }
    }
  };


  /**
   * Imports a script if, and only if, that script hasn't already been imported.
   * (Must be called at execution time)
   * @param {string} src Script source.
   * @param {string=} opt_sourceText The optionally source text to evaluate
   * @private
   */
  goog.importScript_ = function(src, opt_sourceText) {
    var importScript =
        goog.global.CLOSURE_IMPORT_SCRIPT || goog.writeScriptTag_;
    if (importScript(src, opt_sourceText)) {
      goog.dependencies_.written[src] = true;
    }
  };


  /** @const @private {boolean} */
  goog.IS_OLD_IE_ =
      !!(!goog.global.atob && goog.global.document && goog.global.document.all);


  /**
   * Given a URL initiate retrieval and execution of the module.
   * @param {string} src Script source URL.
   * @private
   */
  goog.importModule_ = function(src) {
    // In an attempt to keep browsers from timing out loading scripts using
    // synchronous XHRs, put each load in its own script block.
    var bootstrap = 'goog.retrieveAndExecModule_("' + src + '");';

    if (goog.importScript_('', bootstrap)) {
      goog.dependencies_.written[src] = true;
    }
  };


  /** @private {!Array<string>} */
  goog.queuedModules_ = [];


  /**
   * Return an appropriate module text. Suitable to insert into
   * a script tag (that is unescaped).
   * @param {string} srcUrl
   * @param {string} scriptText
   * @return {string}
   * @private
   */
  goog.wrapModule_ = function(srcUrl, scriptText) {
    if (!goog.LOAD_MODULE_USING_EVAL || !goog.isDef(goog.global.JSON)) {
      return '' +
          'goog.loadModule(function(exports) {' +
          '"use strict";' + scriptText +
          '\n' +  // terminate any trailing single line comment.
          ';return exports' +
          '});' +
          '\n//# sourceURL=' + srcUrl + '\n';
    } else {
      return '' +
          'goog.loadModule(' +
          goog.global.JSON.stringify(
              scriptText + '\n//# sourceURL=' + srcUrl + '\n') +
          ');';
    }
  };

  // On IE9 and earlier, it is necessary to handle
  // deferred module loads. In later browsers, the
  // code to be evaluated is simply inserted as a script
  // block in the correct order. To eval deferred
  // code at the right time, we piggy back on goog.require to call
  // goog.maybeProcessDeferredDep_.
  //
  // The goog.requires are used both to bootstrap
  // the loading process (when no deps are available) and
  // declare that they should be available.
  //
  // Here we eval the sources, if all the deps are available
  // either already eval'd or goog.require'd.  This will
  // be the case when all the dependencies have already
  // been loaded, and the dependent module is loaded.
  //
  // But this alone isn't sufficient because it is also
  // necessary to handle the case where there is no root
  // that is not deferred.  For that there we register for an event
  // and trigger goog.loadQueuedModules_ handle any remaining deferred
  // evaluations.

  /**
   * Handle any remaining deferred goog.module evals.
   * @private
   */
  goog.loadQueuedModules_ = function() {
    var count = goog.queuedModules_.length;
    if (count > 0) {
      var queue = goog.queuedModules_;
      goog.queuedModules_ = [];
      for (var i = 0; i < count; i++) {
        var path = queue[i];
        goog.maybeProcessDeferredPath_(path);
      }
    }
  };


  /**
   * Eval the named module if its dependencies are
   * available.
   * @param {string} name The module to load.
   * @private
   */
  goog.maybeProcessDeferredDep_ = function(name) {
    if (goog.isDeferredModule_(name) && goog.allDepsAreAvailable_(name)) {
      var path = goog.getPathFromDeps_(name);
      goog.maybeProcessDeferredPath_(goog.basePath + path);
    }
  };

  /**
   * @param {string} name The module to check.
   * @return {boolean} Whether the name represents a
   *     module whose evaluation has been deferred.
   * @private
   */
  goog.isDeferredModule_ = function(name) {
    var path = goog.getPathFromDeps_(name);
    if (path && goog.dependencies_.pathIsModule[path]) {
      var abspath = goog.basePath + path;
      return (abspath) in goog.dependencies_.deferred;
    }
    return false;
  };

  /**
   * @param {string} name The module to check.
   * @return {boolean} Whether the name represents a
   *     module whose declared dependencies have all been loaded
   *     (eval'd or a deferred module load)
   * @private
   */
  goog.allDepsAreAvailable_ = function(name) {
    var path = goog.getPathFromDeps_(name);
    if (path && (path in goog.dependencies_.requires)) {
      for (var requireName in goog.dependencies_.requires[path]) {
        if (!goog.isProvided_(requireName) &&
            !goog.isDeferredModule_(requireName)) {
          return false;
        }
      }
    }
    return true;
  };


  /**
   * @param {string} abspath
   * @private
   */
  goog.maybeProcessDeferredPath_ = function(abspath) {
    if (abspath in goog.dependencies_.deferred) {
      var src = goog.dependencies_.deferred[abspath];
      delete goog.dependencies_.deferred[abspath];
      goog.globalEval(src);
    }
  };


  /**
   * Load a goog.module from the provided URL.  This is not a general purpose
   * code loader and does not support late loading code, that is it should only
   * be used during page load. This method exists to support unit tests and
   * "debug" loaders that would otherwise have inserted script tags. Under the
   * hood this needs to use a synchronous XHR and is not recommeneded for
   * production code.
   *
   * The module's goog.requires must have already been satisified; an exception
   * will be thrown if this is not the case. This assumption is that no
   * "deps.js" file exists, so there is no way to discover and locate the
   * module-to-be-loaded's dependencies and no attempt is made to do so.
   *
   * There should only be one attempt to load a module.  If
   * "goog.loadModuleFromUrl" is called for an already loaded module, an
   * exception will be throw.
   *
   * @param {string} url The URL from which to attempt to load the goog.module.
   */
  goog.loadModuleFromUrl = function(url) {
    // Because this executes synchronously, we don't need to do any additional
    // bookkeeping. When "goog.loadModule" the namespace will be marked as
    // having been provided which is sufficient.
    goog.retrieveAndExecModule_(url);
  };


  /**
   * @param {function(?):?|string} moduleDef The module definition.
   */
  goog.loadModule = function(moduleDef) {
    // NOTE: we allow function definitions to be either in the from
    // of a string to eval (which keeps the original source intact) or
    // in a eval forbidden environment (CSP) we allow a function definition
    // which in its body must call {@code goog.module}, and return the exports
    // of the module.
    var previousState = goog.moduleLoaderState_;
    try {
      goog.moduleLoaderState_ = {
        moduleName: undefined,
        declareLegacyNamespace: false
      };
      var exports;
      if (goog.isFunction(moduleDef)) {
        exports = moduleDef.call(goog.global, {});
      } else if (goog.isString(moduleDef)) {
        exports = goog.loadModuleFromSource_.call(goog.global, moduleDef);
      } else {
        throw Error('Invalid module definition');
      }

      var moduleName = goog.moduleLoaderState_.moduleName;
      if (!goog.isString(moduleName) || !moduleName) {
        throw Error('Invalid module name \"' + moduleName + '\"');
      }

      // Don't seal legacy namespaces as they may be uses as a parent of
      // another namespace
      if (goog.moduleLoaderState_.declareLegacyNamespace) {
        goog.constructNamespace_(moduleName, exports);
      } else if (goog.SEAL_MODULE_EXPORTS && Object.seal) {
        Object.seal(exports);
      }

      goog.loadedModules_[moduleName] = exports;
    } finally {
      goog.moduleLoaderState_ = previousState;
    }
  };


  /**
   * @private @const {function(string):?}
   *
   * The new type inference warns because this function has no formal
   * parameters, but its jsdoc says that it takes one argument.
   * (The argument is used via arguments[0], but NTI does not detect this.)
   * @suppress {newCheckTypes}
   */
  goog.loadModuleFromSource_ = function() {
    // NOTE: we avoid declaring parameters or local variables here to avoid
    // masking globals or leaking values into the module definition.
    'use strict';
    var exports = {};
    eval(arguments[0]);
    return exports;
  };


  /**
   * Writes a new script pointing to {@code src} directly into the DOM.
   *
   * NOTE: This method is not CSP-compliant. @see goog.appendScriptSrcNode_ for
   * the fallback mechanism.
   *
   * @param {string} src The script URL.
   * @private
   */
  goog.writeScriptSrcNode_ = function(src) {
    goog.global.document.write(
        '<script type="text/javascript" src="' + src + '"></' +
        'script>');
  };


  /**
   * Appends a new script node to the DOM using a CSP-compliant mechanism. This
   * method exists as a fallback for document.write (which is not allowed in a
   * strict CSP context, e.g., Chrome apps).
   *
   * NOTE: This method is not analogous to using document.write to insert a
   * <script> tag; specifically, the user agent will execute a script added by
   * document.write immediately after the current script block finishes
   * executing, whereas the DOM-appended script node will not be executed until
   * the entire document is parsed and executed. That is to say, this script is
   * added to the end of the script execution queue.
   *
   * The page must not attempt to call goog.required entities until after the
   * document has loaded, e.g., in or after the window.onload callback.
   *
   * @param {string} src The script URL.
   * @private
   */
  goog.appendScriptSrcNode_ = function(src) {
    /** @type {Document} */
    var doc = goog.global.document;
    var scriptEl =
        /** @type {HTMLScriptElement} */ (doc.createElement('script'));
    scriptEl.type = 'text/javascript';
    scriptEl.src = src;
    scriptEl.defer = false;
    scriptEl.async = false;
    doc.head.appendChild(scriptEl);
  };


  /**
   * The default implementation of the import function. Writes a script tag to
   * import the script.
   *
   * @param {string} src The script url.
   * @param {string=} opt_sourceText The optionally source text to evaluate
   * @return {boolean} True if the script was imported, false otherwise.
   * @private
   */
  goog.writeScriptTag_ = function(src, opt_sourceText) {
    if (goog.inHtmlDocument_()) {
      /** @type {!HTMLDocument} */
      var doc = goog.global.document;

      // If the user tries to require a new symbol after document load,
      // something has gone terribly wrong. Doing a document.write would
      // wipe out the page. This does not apply to the CSP-compliant method
      // of writing script tags.
      if (!goog.ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING &&
          doc.readyState == 'complete') {
        // Certain test frameworks load base.js multiple times, which tries
        // to write deps.js each time. If that happens, just fail silently.
        // These frameworks wipe the page between each load of base.js, so this
        // is OK.
        var isDeps = /\bdeps.js$/.test(src);
        if (isDeps) {
          return false;
        } else {
          throw Error('Cannot write "' + src + '" after document load');
        }
      }

      var isOldIE = goog.IS_OLD_IE_;

      if (opt_sourceText === undefined) {
        if (!isOldIE) {
          if (goog.ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING) {
            goog.appendScriptSrcNode_(src);
          } else {
            goog.writeScriptSrcNode_(src);
          }
        } else {
          var state = " onreadystatechange='goog.onScriptLoad_(this, " +
              ++goog.lastNonModuleScriptIndex_ + ")' ";
          doc.write(
              '<script type="text/javascript" src="' + src + '"' + state +
              '></' +
              'script>');
        }
      } else {
        doc.write(
            '<script type="text/javascript">' + opt_sourceText + '</' +
            'script>');
      }
      return true;
    } else {
      return false;
    }
  };


  /** @private {number} */
  goog.lastNonModuleScriptIndex_ = 0;


  /**
   * A readystatechange handler for legacy IE
   * @param {!HTMLScriptElement} script
   * @param {number} scriptIndex
   * @return {boolean}
   * @private
   */
  goog.onScriptLoad_ = function(script, scriptIndex) {
    // for now load the modules when we reach the last script,
    // later allow more inter-mingling.
    if (script.readyState == 'complete' &&
        goog.lastNonModuleScriptIndex_ == scriptIndex) {
      goog.loadQueuedModules_();
    }
    return true;
  };

  /**
   * Resolves dependencies based on the dependencies added using addDependency
   * and calls importScript_ in the correct order.
   * @param {string} pathToLoad The path from which to start discovering
   *     dependencies.
   * @private
   */
  goog.writeScripts_ = function(pathToLoad) {
    /** @type {!Array<string>} The scripts we need to write this time. */
    var scripts = [];
    var seenScript = {};
    var deps = goog.dependencies_;

    /** @param {string} path */
    function visitNode(path) {
      if (path in deps.written) {
        return;
      }

      // We have already visited this one. We can get here if we have cyclic
      // dependencies.
      if (path in deps.visited) {
        return;
      }

      deps.visited[path] = true;

      if (path in deps.requires) {
        for (var requireName in deps.requires[path]) {
          // If the required name is defined, we assume that it was already
          // bootstrapped by other means.
          if (!goog.isProvided_(requireName)) {
            if (requireName in deps.nameToPath) {
              visitNode(deps.nameToPath[requireName]);
            } else {
              throw Error('Undefined nameToPath for ' + requireName);
            }
          }
        }
      }

      if (!(path in seenScript)) {
        seenScript[path] = true;
        scripts.push(path);
      }
    }

    visitNode(pathToLoad);

    // record that we are going to load all these scripts.
    for (var i = 0; i < scripts.length; i++) {
      var path = scripts[i];
      goog.dependencies_.written[path] = true;
    }

    // If a module is loaded synchronously then we need to
    // clear the current inModuleLoader value, and restore it when we are
    // done loading the current "requires".
    var moduleState = goog.moduleLoaderState_;
    goog.moduleLoaderState_ = null;

    for (var i = 0; i < scripts.length; i++) {
      var path = scripts[i];
      if (path) {
        if (!deps.pathIsModule[path]) {
          goog.importScript_(goog.basePath + path);
        } else {
          goog.importModule_(goog.basePath + path);
        }
      } else {
        goog.moduleLoaderState_ = moduleState;
        throw Error('Undefined script input');
      }
    }

    // restore the current "module loading state"
    goog.moduleLoaderState_ = moduleState;
  };


  /**
   * Looks at the dependency rules and tries to determine the script file that
   * fulfills a particular rule.
   * @param {string} rule In the form goog.namespace.Class or project.script.
   * @return {?string} Url corresponding to the rule, or null.
   * @private
   */
  goog.getPathFromDeps_ = function(rule) {
    if (rule in goog.dependencies_.nameToPath) {
      return goog.dependencies_.nameToPath[rule];
    } else {
      return null;
    }
  };

  goog.findBasePath_();

  // Allow projects to manage the deps files themselves.
  if (!goog.global.CLOSURE_NO_DEPS) {
    goog.importScript_(goog.basePath + 'deps.js');
  }
}


/**
 * Normalize a file path by removing redundant ".." and extraneous "." file
 * path components.
 * @param {string} path
 * @return {string}
 * @private
 */
goog.normalizePath_ = function(path) {
  var components = path.split('/');
  var i = 0;
  while (i < components.length) {
    if (components[i] == '.') {
      components.splice(i, 1);
    } else if (
        i && components[i] == '..' && components[i - 1] &&
        components[i - 1] != '..') {
      components.splice(--i, 2);
    } else {
      i++;
    }
  }
  return components.join('/');
};


/**
 * Loads file by synchronous XHR. Should not be used in production environments.
 * @param {string} src Source URL.
 * @return {string} File contents.
 * @private
 */
goog.loadFileSync_ = function(src) {
  if (goog.global.CLOSURE_LOAD_FILE_SYNC) {
    return goog.global.CLOSURE_LOAD_FILE_SYNC(src);
  } else {
    /** @type {XMLHttpRequest} */
    var xhr = new goog.global['XMLHttpRequest']();
    xhr.open('get', src, false);
    xhr.send();
    return xhr.responseText;
  }
};


/**
 * Retrieve and execute a module.
 * @param {string} src Script source URL.
 * @private
 */
goog.retrieveAndExecModule_ = function(src) {
  if (!COMPILED) {
    // The full but non-canonicalized URL for later use.
    var originalPath = src;
    // Canonicalize the path, removing any /./ or /../ since Chrome's debugging
    // console doesn't auto-canonicalize XHR loads as it does <script> srcs.
    src = goog.normalizePath_(src);

    var importScript =
        goog.global.CLOSURE_IMPORT_SCRIPT || goog.writeScriptTag_;

    var scriptText = goog.loadFileSync_(src);

    if (scriptText != null) {
      var execModuleScript = goog.wrapModule_(src, scriptText);
      var isOldIE = goog.IS_OLD_IE_;
      if (isOldIE) {
        goog.dependencies_.deferred[originalPath] = execModuleScript;
        goog.queuedModules_.push(originalPath);
      } else {
        importScript(src, execModuleScript);
      }
    } else {
      throw new Error('load of ' + src + 'failed');
    }
  }
};


//==============================================================================
// Language Enhancements
//==============================================================================


/**
 * This is a "fixed" version of the typeof operator.  It differs from the typeof
 * operator in such a way that null returns 'null' and arrays return 'array'.
 * @param {?} value The value to get the type of.
 * @return {string} The name of the type.
 */
goog.typeOf = function(value) {
  var s = typeof value;
  if (s == 'object') {
    if (value) {
      // Check these first, so we can avoid calling Object.prototype.toString if
      // possible.
      //
      // IE improperly marshals typeof across execution contexts, but a
      // cross-context object will still return false for "instanceof Object".
      if (value instanceof Array) {
        return 'array';
      } else if (value instanceof Object) {
        return s;
      }

      // HACK: In order to use an Object prototype method on the arbitrary
      //   value, the compiler requires the value be cast to type Object,
      //   even though the ECMA spec explicitly allows it.
      var className = Object.prototype.toString.call(
          /** @type {!Object} */ (value));
      // In Firefox 3.6, attempting to access iframe window objects' length
      // property throws an NS_ERROR_FAILURE, so we need to special-case it
      // here.
      if (className == '[object Window]') {
        return 'object';
      }

      // We cannot always use constructor == Array or instanceof Array because
      // different frames have different Array objects. In IE6, if the iframe
      // where the array was created is destroyed, the array loses its
      // prototype. Then dereferencing val.splice here throws an exception, so
      // we can't use goog.isFunction. Calling typeof directly returns 'unknown'
      // so that will work. In this case, this function will return false and
      // most array functions will still work because the array is still
      // array-like (supports length and []) even though it has lost its
      // prototype.
      // Mark Miller noticed that Object.prototype.toString
      // allows access to the unforgeable [[Class]] property.
      //  15.2.4.2 Object.prototype.toString ( )
      //  When the toString method is called, the following steps are taken:
      //      1. Get the [[Class]] property of this object.
      //      2. Compute a string value by concatenating the three strings
      //         "[object ", Result(1), and "]".
      //      3. Return Result(2).
      // and this behavior survives the destruction of the execution context.
      if ((className == '[object Array]' ||
           // In IE all non value types are wrapped as objects across window
           // boundaries (not iframe though) so we have to do object detection
           // for this edge case.
           typeof value.length == 'number' &&
               typeof value.splice != 'undefined' &&
               typeof value.propertyIsEnumerable != 'undefined' &&
               !value.propertyIsEnumerable('splice')

               )) {
        return 'array';
      }
      // HACK: There is still an array case that fails.
      //     function ArrayImpostor() {}
      //     ArrayImpostor.prototype = [];
      //     var impostor = new ArrayImpostor;
      // this can be fixed by getting rid of the fast path
      // (value instanceof Array) and solely relying on
      // (value && Object.prototype.toString.vall(value) === '[object Array]')
      // but that would require many more function calls and is not warranted
      // unless closure code is receiving objects from untrusted sources.

      // IE in cross-window calls does not correctly marshal the function type
      // (it appears just as an object) so we cannot use just typeof val ==
      // 'function'. However, if the object has a call property, it is a
      // function.
      if ((className == '[object Function]' ||
           typeof value.call != 'undefined' &&
               typeof value.propertyIsEnumerable != 'undefined' &&
               !value.propertyIsEnumerable('call'))) {
        return 'function';
      }

    } else {
      return 'null';
    }

  } else if (s == 'function' && typeof value.call == 'undefined') {
    // In Safari typeof nodeList returns 'function', and on Firefox typeof
    // behaves similarly for HTML{Applet,Embed,Object}, Elements and RegExps. We
    // would like to return object for those and we can detect an invalid
    // function by making sure that the function object has a call method.
    return 'object';
  }
  return s;
};


/**
 * Returns true if the specified value is null.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is null.
 */
goog.isNull = function(val) {
  return val === null;
};


/**
 * Returns true if the specified value is defined and not null.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is defined and not null.
 */
goog.isDefAndNotNull = function(val) {
  // Note that undefined == null.
  return val != null;
};


/**
 * Returns true if the specified value is an array.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is an array.
 */
goog.isArray = function(val) {
  return goog.typeOf(val) == 'array';
};


/**
 * Returns true if the object looks like an array. To qualify as array like
 * the value needs to be either a NodeList or an object with a Number length
 * property. As a special case, a function value is not array like, because its
 * length property is fixed to correspond to the number of expected arguments.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is an array.
 */
goog.isArrayLike = function(val) {
  var type = goog.typeOf(val);
  // We do not use goog.isObject here in order to exclude function values.
  return type == 'array' || type == 'object' && typeof val.length == 'number';
};


/**
 * Returns true if the object looks like a Date. To qualify as Date-like the
 * value needs to be an object and have a getFullYear() function.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is a like a Date.
 */
goog.isDateLike = function(val) {
  return goog.isObject(val) && typeof val.getFullYear == 'function';
};


/**
 * Returns true if the specified value is a string.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is a string.
 */
goog.isString = function(val) {
  return typeof val == 'string';
};


/**
 * Returns true if the specified value is a boolean.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is boolean.
 */
goog.isBoolean = function(val) {
  return typeof val == 'boolean';
};


/**
 * Returns true if the specified value is a number.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is a number.
 */
goog.isNumber = function(val) {
  return typeof val == 'number';
};


/**
 * Returns true if the specified value is a function.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is a function.
 */
goog.isFunction = function(val) {
  return goog.typeOf(val) == 'function';
};


/**
 * Returns true if the specified value is an object.  This includes arrays and
 * functions.
 * @param {?} val Variable to test.
 * @return {boolean} Whether variable is an object.
 */
goog.isObject = function(val) {
  var type = typeof val;
  return type == 'object' && val != null || type == 'function';
  // return Object(val) === val also works, but is slower, especially if val is
  // not an object.
};


/**
 * Gets a unique ID for an object. This mutates the object so that further calls
 * with the same object as a parameter returns the same value. The unique ID is
 * guaranteed to be unique across the current session amongst objects that are
 * passed into {@code getUid}. There is no guarantee that the ID is unique or
 * consistent across sessions. It is unsafe to generate unique ID for function
 * prototypes.
 *
 * @param {Object} obj The object to get the unique ID for.
 * @return {number} The unique ID for the object.
 */
goog.getUid = function(obj) {
  // TODO(arv): Make the type stricter, do not accept null.

  // In Opera window.hasOwnProperty exists but always returns false so we avoid
  // using it. As a consequence the unique ID generated for BaseClass.prototype
  // and SubClass.prototype will be the same.
  return obj[goog.UID_PROPERTY_] ||
      (obj[goog.UID_PROPERTY_] = ++goog.uidCounter_);
};


/**
 * Whether the given object is already assigned a unique ID.
 *
 * This does not modify the object.
 *
 * @param {!Object} obj The object to check.
 * @return {boolean} Whether there is an assigned unique id for the object.
 */
goog.hasUid = function(obj) {
  return !!obj[goog.UID_PROPERTY_];
};


/**
 * Removes the unique ID from an object. This is useful if the object was
 * previously mutated using {@code goog.getUid} in which case the mutation is
 * undone.
 * @param {Object} obj The object to remove the unique ID field from.
 */
goog.removeUid = function(obj) {
  // TODO(arv): Make the type stricter, do not accept null.

  // In IE, DOM nodes are not instances of Object and throw an exception if we
  // try to delete.  Instead we try to use removeAttribute.
  if (obj !== null && 'removeAttribute' in obj) {
    obj.removeAttribute(goog.UID_PROPERTY_);
  }
  /** @preserveTry */
  try {
    delete obj[goog.UID_PROPERTY_];
  } catch (ex) {
  }
};


/**
 * Name for unique ID property. Initialized in a way to help avoid collisions
 * with other closure JavaScript on the same page.
 * @type {string}
 * @private
 */
goog.UID_PROPERTY_ = 'closure_uid_' + ((Math.random() * 1e9) >>> 0);


/**
 * Counter for UID.
 * @type {number}
 * @private
 */
goog.uidCounter_ = 0;


/**
 * Adds a hash code field to an object. The hash code is unique for the
 * given object.
 * @param {Object} obj The object to get the hash code for.
 * @return {number} The hash code for the object.
 * @deprecated Use goog.getUid instead.
 */
goog.getHashCode = goog.getUid;


/**
 * Removes the hash code field from an object.
 * @param {Object} obj The object to remove the field from.
 * @deprecated Use goog.removeUid instead.
 */
goog.removeHashCode = goog.removeUid;


/**
 * Clones a value. The input may be an Object, Array, or basic type. Objects and
 * arrays will be cloned recursively.
 *
 * WARNINGS:
 * <code>goog.cloneObject</code> does not detect reference loops. Objects that
 * refer to themselves will cause infinite recursion.
 *
 * <code>goog.cloneObject</code> is unaware of unique identifiers, and copies
 * UIDs created by <code>getUid</code> into cloned results.
 *
 * @param {*} obj The value to clone.
 * @return {*} A clone of the input value.
 * @deprecated goog.cloneObject is unsafe. Prefer the goog.object methods.
 */
goog.cloneObject = function(obj) {
  var type = goog.typeOf(obj);
  if (type == 'object' || type == 'array') {
    if (obj.clone) {
      return obj.clone();
    }
    var clone = type == 'array' ? [] : {};
    for (var key in obj) {
      clone[key] = goog.cloneObject(obj[key]);
    }
    return clone;
  }

  return obj;
};


/**
 * A native implementation of goog.bind.
 * @param {Function} fn A function to partially apply.
 * @param {Object|undefined} selfObj Specifies the object which this should
 *     point to when the function is run.
 * @param {...*} var_args Additional arguments that are partially applied to the
 *     function.
 * @return {!Function} A partially-applied form of the function bind() was
 *     invoked as a method of.
 * @private
 * @suppress {deprecated} The compiler thinks that Function.prototype.bind is
 *     deprecated because some people have declared a pure-JS version.
 *     Only the pure-JS version is truly deprecated.
 */
goog.bindNative_ = function(fn, selfObj, var_args) {
  return /** @type {!Function} */ (fn.call.apply(fn.bind, arguments));
};


/**
 * A pure-JS implementation of goog.bind.
 * @param {Function} fn A function to partially apply.
 * @param {Object|undefined} selfObj Specifies the object which this should
 *     point to when the function is run.
 * @param {...*} var_args Additional arguments that are partially applied to the
 *     function.
 * @return {!Function} A partially-applied form of the function bind() was
 *     invoked as a method of.
 * @private
 */
goog.bindJs_ = function(fn, selfObj, var_args) {
  if (!fn) {
    throw new Error();
  }

  if (arguments.length > 2) {
    var boundArgs = Array.prototype.slice.call(arguments, 2);
    return function() {
      // Prepend the bound arguments to the current arguments.
      var newArgs = Array.prototype.slice.call(arguments);
      Array.prototype.unshift.apply(newArgs, boundArgs);
      return fn.apply(selfObj, newArgs);
    };

  } else {
    return function() { return fn.apply(selfObj, arguments); };
  }
};


/**
 * Partially applies this function to a particular 'this object' and zero or
 * more arguments. The result is a new function with some arguments of the first
 * function pre-filled and the value of this 'pre-specified'.
 *
 * Remaining arguments specified at call-time are appended to the pre-specified
 * ones.
 *
 * Also see: {@link #partial}.
 *
 * Usage:
 * <pre>var barMethBound = goog.bind(myFunction, myObj, 'arg1', 'arg2');
 * barMethBound('arg3', 'arg4');</pre>
 *
 * @param {?function(this:T, ...)} fn A function to partially apply.
 * @param {T} selfObj Specifies the object which this should point to when the
 *     function is run.
 * @param {...*} var_args Additional arguments that are partially applied to the
 *     function.
 * @return {!Function} A partially-applied form of the function goog.bind() was
 *     invoked as a method of.
 * @template T
 * @suppress {deprecated} See above.
 */
goog.bind = function(fn, selfObj, var_args) {
  // TODO(nicksantos): narrow the type signature.
  if (Function.prototype.bind &&
      // NOTE(nicksantos): Somebody pulled base.js into the default Chrome
      // extension environment. This means that for Chrome extensions, they get
      // the implementation of Function.prototype.bind that calls goog.bind
      // instead of the native one. Even worse, we don't want to introduce a
      // circular dependency between goog.bind and Function.prototype.bind, so
      // we have to hack this to make sure it works correctly.
      Function.prototype.bind.toString().indexOf('native code') != -1) {
    goog.bind = goog.bindNative_;
  } else {
    goog.bind = goog.bindJs_;
  }
  return goog.bind.apply(null, arguments);
};


/**
 * Like goog.bind(), except that a 'this object' is not required. Useful when
 * the target function is already bound.
 *
 * Usage:
 * var g = goog.partial(f, arg1, arg2);
 * g(arg3, arg4);
 *
 * @param {Function} fn A function to partially apply.
 * @param {...*} var_args Additional arguments that are partially applied to fn.
 * @return {!Function} A partially-applied form of the function goog.partial()
 *     was invoked as a method of.
 */
goog.partial = function(fn, var_args) {
  var args = Array.prototype.slice.call(arguments, 1);
  return function() {
    // Clone the array (with slice()) and append additional arguments
    // to the existing arguments.
    var newArgs = args.slice();
    newArgs.push.apply(newArgs, arguments);
    return fn.apply(this, newArgs);
  };
};


/**
 * Copies all the members of a source object to a target object. This method
 * does not work on all browsers for all objects that contain keys such as
 * toString or hasOwnProperty. Use goog.object.extend for this purpose.
 * @param {Object} target Target.
 * @param {Object} source Source.
 */
goog.mixin = function(target, source) {
  for (var x in source) {
    target[x] = source[x];
  }

  // For IE7 or lower, the for-in-loop does not contain any properties that are
  // not enumerable on the prototype object (for example, isPrototypeOf from
  // Object.prototype) but also it will not include 'replace' on objects that
  // extend String and change 'replace' (not that it is common for anyone to
  // extend anything except Object).
};


/**
 * @return {number} An integer value representing the number of milliseconds
 *     between midnight, January 1, 1970 and the current time.
 */
goog.now = (goog.TRUSTED_SITE && Date.now) || (function() {
             // Unary plus operator converts its operand to a number which in
             // the case of
             // a date is done by calling getTime().
             return +new Date();
           });


/**
 * Evals JavaScript in the global scope.  In IE this uses execScript, other
 * browsers use goog.global.eval. If goog.global.eval does not evaluate in the
 * global scope (for example, in Safari), appends a script tag instead.
 * Throws an exception if neither execScript or eval is defined.
 * @param {string} script JavaScript string.
 */
goog.globalEval = function(script) {
  if (goog.global.execScript) {
    goog.global.execScript(script, 'JavaScript');
  } else if (goog.global.eval) {
    // Test to see if eval works
    if (goog.evalWorksForGlobals_ == null) {
      goog.global.eval('var _evalTest_ = 1;');
      if (typeof goog.global['_evalTest_'] != 'undefined') {
        try {
          delete goog.global['_evalTest_'];
        } catch (ignore) {
          // Microsoft edge fails the deletion above in strict mode.
        }
        goog.evalWorksForGlobals_ = true;
      } else {
        goog.evalWorksForGlobals_ = false;
      }
    }

    if (goog.evalWorksForGlobals_) {
      goog.global.eval(script);
    } else {
      /** @type {Document} */
      var doc = goog.global.document;
      var scriptElt =
          /** @type {!HTMLScriptElement} */ (doc.createElement('SCRIPT'));
      scriptElt.type = 'text/javascript';
      scriptElt.defer = false;
      // Note(user): can't use .innerHTML since "t('<test>')" will fail and
      // .text doesn't work in Safari 2.  Therefore we append a text node.
      scriptElt.appendChild(doc.createTextNode(script));
      doc.body.appendChild(scriptElt);
      doc.body.removeChild(scriptElt);
    }
  } else {
    throw Error('goog.globalEval not available');
  }
};


/**
 * Indicates whether or not we can call 'eval' directly to eval code in the
 * global scope. Set to a Boolean by the first call to goog.globalEval (which
 * empirically tests whether eval works for globals). @see goog.globalEval
 * @type {?boolean}
 * @private
 */
goog.evalWorksForGlobals_ = null;


/**
 * Optional map of CSS class names to obfuscated names used with
 * goog.getCssName().
 * @private {!Object<string, string>|undefined}
 * @see goog.setCssNameMapping
 */
goog.cssNameMapping_;


/**
 * Optional obfuscation style for CSS class names. Should be set to either
 * 'BY_WHOLE' or 'BY_PART' if defined.
 * @type {string|undefined}
 * @private
 * @see goog.setCssNameMapping
 */
goog.cssNameMappingStyle_;


/**
 * Handles strings that are intended to be used as CSS class names.
 *
 * This function works in tandem with @see goog.setCssNameMapping.
 *
 * Without any mapping set, the arguments are simple joined with a hyphen and
 * passed through unaltered.
 *
 * When there is a mapping, there are two possible styles in which these
 * mappings are used. In the BY_PART style, each part (i.e. in between hyphens)
 * of the passed in css name is rewritten according to the map. In the BY_WHOLE
 * style, the full css name is looked up in the map directly. If a rewrite is
 * not specified by the map, the compiler will output a warning.
 *
 * When the mapping is passed to the compiler, it will replace calls to
 * goog.getCssName with the strings from the mapping, e.g.
 *     var x = goog.getCssName('foo');
 *     var y = goog.getCssName(this.baseClass, 'active');
 *  becomes:
 *     var x = 'foo';
 *     var y = this.baseClass + '-active';
 *
 * If one argument is passed it will be processed, if two are passed only the
 * modifier will be processed, as it is assumed the first argument was generated
 * as a result of calling goog.getCssName.
 *
 * @param {string} className The class name.
 * @param {string=} opt_modifier A modifier to be appended to the class name.
 * @return {string} The class name or the concatenation of the class name and
 *     the modifier.
 */
goog.getCssName = function(className, opt_modifier) {
  var getMapping = function(cssName) {
    return goog.cssNameMapping_[cssName] || cssName;
  };

  var renameByParts = function(cssName) {
    // Remap all the parts individually.
    var parts = cssName.split('-');
    var mapped = [];
    for (var i = 0; i < parts.length; i++) {
      mapped.push(getMapping(parts[i]));
    }
    return mapped.join('-');
  };

  var rename;
  if (goog.cssNameMapping_) {
    rename =
        goog.cssNameMappingStyle_ == 'BY_WHOLE' ? getMapping : renameByParts;
  } else {
    rename = function(a) { return a; };
  }

  if (opt_modifier) {
    return className + '-' + rename(opt_modifier);
  } else {
    return rename(className);
  }
};


/**
 * Sets the map to check when returning a value from goog.getCssName(). Example:
 * <pre>
 * goog.setCssNameMapping({
 *   "goog": "a",
 *   "disabled": "b",
 * });
 *
 * var x = goog.getCssName('goog');
 * // The following evaluates to: "a a-b".
 * goog.getCssName('goog') + ' ' + goog.getCssName(x, 'disabled')
 * </pre>
 * When declared as a map of string literals to string literals, the JSCompiler
 * will replace all calls to goog.getCssName() using the supplied map if the
 * --process_closure_primitives flag is set.
 *
 * @param {!Object} mapping A map of strings to strings where keys are possible
 *     arguments to goog.getCssName() and values are the corresponding values
 *     that should be returned.
 * @param {string=} opt_style The style of css name mapping. There are two valid
 *     options: 'BY_PART', and 'BY_WHOLE'.
 * @see goog.getCssName for a description.
 */
goog.setCssNameMapping = function(mapping, opt_style) {
  goog.cssNameMapping_ = mapping;
  goog.cssNameMappingStyle_ = opt_style;
};


/**
 * To use CSS renaming in compiled mode, one of the input files should have a
 * call to goog.setCssNameMapping() with an object literal that the JSCompiler
 * can extract and use to replace all calls to goog.getCssName(). In uncompiled
 * mode, JavaScript code should be loaded before this base.js file that declares
 * a global variable, CLOSURE_CSS_NAME_MAPPING, which is used below. This is
 * to ensure that the mapping is loaded before any calls to goog.getCssName()
 * are made in uncompiled mode.
 *
 * A hook for overriding the CSS name mapping.
 * @type {!Object<string, string>|undefined}
 */
goog.global.CLOSURE_CSS_NAME_MAPPING;


if (!COMPILED && goog.global.CLOSURE_CSS_NAME_MAPPING) {
  // This does not call goog.setCssNameMapping() because the JSCompiler
  // requires that goog.setCssNameMapping() be called with an object literal.
  goog.cssNameMapping_ = goog.global.CLOSURE_CSS_NAME_MAPPING;
}


/**
 * Gets a localized message.
 *
 * This function is a compiler primitive. If you give the compiler a localized
 * message bundle, it will replace the string at compile-time with a localized
 * version, and expand goog.getMsg call to a concatenated string.
 *
 * Messages must be initialized in the form:
 * <code>
 * var MSG_NAME = goog.getMsg('Hello {$placeholder}', {'placeholder': 'world'});
 * </code>
 *
 * @param {string} str Translatable string, places holders in the form {$foo}.
 * @param {Object<string, string>=} opt_values Maps place holder name to value.
 * @return {string} message with placeholders filled.
 */
goog.getMsg = function(str, opt_values) {
  if (opt_values) {
    str = str.replace(/\{\$([^}]+)}/g, function(match, key) {
      return (opt_values != null && key in opt_values) ? opt_values[key] :
                                                         match;
    });
  }
  return str;
};


/**
 * Gets a localized message. If the message does not have a translation, gives a
 * fallback message.
 *
 * This is useful when introducing a new message that has not yet been
 * translated into all languages.
 *
 * This function is a compiler primitive. Must be used in the form:
 * <code>var x = goog.getMsgWithFallback(MSG_A, MSG_B);</code>
 * where MSG_A and MSG_B were initialized with goog.getMsg.
 *
 * @param {string} a The preferred message.
 * @param {string} b The fallback message.
 * @return {string} The best translated message.
 */
goog.getMsgWithFallback = function(a, b) {
  return a;
};


/**
 * Exposes an unobfuscated global namespace path for the given object.
 * Note that fields of the exported object *will* be obfuscated, unless they are
 * exported in turn via this function or goog.exportProperty.
 *
 * Also handy for making public items that are defined in anonymous closures.
 *
 * ex. goog.exportSymbol('public.path.Foo', Foo);
 *
 * ex. goog.exportSymbol('public.path.Foo.staticFunction', Foo.staticFunction);
 *     public.path.Foo.staticFunction();
 *
 * ex. goog.exportSymbol('public.path.Foo.prototype.myMethod',
 *                       Foo.prototype.myMethod);
 *     new public.path.Foo().myMethod();
 *
 * @param {string} publicPath Unobfuscated name to export.
 * @param {*} object Object the name should point to.
 * @param {Object=} opt_objectToExportTo The object to add the path to; default
 *     is goog.global.
 */
goog.exportSymbol = function(publicPath, object, opt_objectToExportTo) {
  goog.exportPath_(publicPath, object, opt_objectToExportTo);
};


/**
 * Exports a property unobfuscated into the object's namespace.
 * ex. goog.exportProperty(Foo, 'staticFunction', Foo.staticFunction);
 * ex. goog.exportProperty(Foo.prototype, 'myMethod', Foo.prototype.myMethod);
 * @param {Object} object Object whose static property is being exported.
 * @param {string} publicName Unobfuscated name to export.
 * @param {*} symbol Object the name should point to.
 */
goog.exportProperty = function(object, publicName, symbol) {
  object[publicName] = symbol;
};


/**
 * Inherit the prototype methods from one constructor into another.
 *
 * Usage:
 * <pre>
 * function ParentClass(a, b) { }
 * ParentClass.prototype.foo = function(a) { };
 *
 * function ChildClass(a, b, c) {
 *   ChildClass.base(this, 'constructor', a, b);
 * }
 * goog.inherits(ChildClass, ParentClass);
 *
 * var child = new ChildClass('a', 'b', 'see');
 * child.foo(); // This works.
 * </pre>
 *
 * @param {!Function} childCtor Child class.
 * @param {!Function} parentCtor Parent class.
 */
goog.inherits = function(childCtor, parentCtor) {
  /** @constructor */
  function tempCtor() {}
  tempCtor.prototype = parentCtor.prototype;
  childCtor.superClass_ = parentCtor.prototype;
  childCtor.prototype = new tempCtor();
  /** @override */
  childCtor.prototype.constructor = childCtor;

  /**
   * Calls superclass constructor/method.
   *
   * This function is only available if you use goog.inherits to
   * express inheritance relationships between classes.
   *
   * NOTE: This is a replacement for goog.base and for superClass_
   * property defined in childCtor.
   *
   * @param {!Object} me Should always be "this".
   * @param {string} methodName The method name to call. Calling
   *     superclass constructor can be done with the special string
   *     'constructor'.
   * @param {...*} var_args The arguments to pass to superclass
   *     method/constructor.
   * @return {*} The return value of the superclass method/constructor.
   */
  childCtor.base = function(me, methodName, var_args) {
    // Copying using loop to avoid deop due to passing arguments object to
    // function. This is faster in many JS engines as of late 2014.
    var args = new Array(arguments.length - 2);
    for (var i = 2; i < arguments.length; i++) {
      args[i - 2] = arguments[i];
    }
    return parentCtor.prototype[methodName].apply(me, args);
  };
};


/**
 * Call up to the superclass.
 *
 * If this is called from a constructor, then this calls the superclass
 * constructor with arguments 1-N.
 *
 * If this is called from a prototype method, then you must pass the name of the
 * method as the second argument to this function. If you do not, you will get a
 * runtime error. This calls the superclass' method with arguments 2-N.
 *
 * This function only works if you use goog.inherits to express inheritance
 * relationships between your classes.
 *
 * This function is a compiler primitive. At compile-time, the compiler will do
 * macro expansion to remove a lot of the extra overhead that this function
 * introduces. The compiler will also enforce a lot of the assumptions that this
 * function makes, and treat it as a compiler error if you break them.
 *
 * @param {!Object} me Should always be "this".
 * @param {*=} opt_methodName The method name if calling a super method.
 * @param {...*} var_args The rest of the arguments.
 * @return {*} The return value of the superclass method.
 * @suppress {es5Strict} This method can not be used in strict mode, but
 *     all Closure Library consumers must depend on this file.
 */
goog.base = function(me, opt_methodName, var_args) {
  var caller = arguments.callee.caller;

  if (goog.STRICT_MODE_COMPATIBLE || (goog.DEBUG && !caller)) {
    throw Error(
        'arguments.caller not defined.  goog.base() cannot be used ' +
        'with strict mode code. See ' +
        'http://www.ecma-international.org/ecma-262/5.1/#sec-C');
  }

  if (caller.superClass_) {
    // Copying using loop to avoid deop due to passing arguments object to
    // function. This is faster in many JS engines as of late 2014.
    var ctorArgs = new Array(arguments.length - 1);
    for (var i = 1; i < arguments.length; i++) {
      ctorArgs[i - 1] = arguments[i];
    }
    // This is a constructor. Call the superclass constructor.
    return caller.superClass_.constructor.apply(me, ctorArgs);
  }

  // Copying using loop to avoid deop due to passing arguments object to
  // function. This is faster in many JS engines as of late 2014.
  var args = new Array(arguments.length - 2);
  for (var i = 2; i < arguments.length; i++) {
    args[i - 2] = arguments[i];
  }
  var foundCaller = false;
  for (var ctor = me.constructor; ctor;
       ctor = ctor.superClass_ && ctor.superClass_.constructor) {
    if (ctor.prototype[opt_methodName] === caller) {
      foundCaller = true;
    } else if (foundCaller) {
      return ctor.prototype[opt_methodName].apply(me, args);
    }
  }

  // If we did not find the caller in the prototype chain, then one of two
  // things happened:
  // 1) The caller is an instance method.
  // 2) This method was not called by the right caller.
  if (me[opt_methodName] === caller) {
    return me.constructor.prototype[opt_methodName].apply(me, args);
  } else {
    throw Error(
        'goog.base called from a method of one name ' +
        'to a method of a different name');
  }
};


/**
 * Allow for aliasing within scope functions.  This function exists for
 * uncompiled code - in compiled code the calls will be inlined and the aliases
 * applied.  In uncompiled code the function is simply run since the aliases as
 * written are valid JavaScript.
 *
 *
 * @param {function()} fn Function to call.  This function can contain aliases
 *     to namespaces (e.g. "var dom = goog.dom") or classes
 *     (e.g. "var Timer = goog.Timer").
 */
goog.scope = function(fn) {
  fn.call(goog.global);
};


/*
 * To support uncompiled, strict mode bundles that use eval to divide source
 * like so:
 *    eval('someSource;//# sourceUrl sourcefile.js');
 * We need to export the globally defined symbols "goog" and "COMPILED".
 * Exporting "goog" breaks the compiler optimizations, so we required that
 * be defined externally.
 * NOTE: We don't use goog.exportSymbol here because we don't want to trigger
 * extern generation when that compiler option is enabled.
 */
if (!COMPILED) {
  goog.global['COMPILED'] = COMPILED;
}


//==============================================================================
// goog.defineClass implementation
//==============================================================================


/**
 * Creates a restricted form of a Closure "class":
 *   - from the compiler's perspective, the instance returned from the
 *     constructor is sealed (no new properties may be added).  This enables
 *     better checks.
 *   - the compiler will rewrite this definition to a form that is optimal
 *     for type checking and optimization (initially this will be a more
 *     traditional form).
 *
 * @param {Function} superClass The superclass, Object or null.
 * @param {goog.defineClass.ClassDescriptor} def
 *     An object literal describing
 *     the class.  It may have the following properties:
 *     "constructor": the constructor function
 *     "statics": an object literal containing methods to add to the constructor
 *        as "static" methods or a function that will receive the constructor
 *        function as its only parameter to which static properties can
 *        be added.
 *     all other properties are added to the prototype.
 * @return {!Function} The class constructor.
 */
goog.defineClass = function(superClass, def) {
  // TODO(johnlenz): consider making the superClass an optional parameter.
  var constructor = def.constructor;
  var statics = def.statics;
  // Wrap the constructor prior to setting up the prototype and static methods.
  if (!constructor || constructor == Object.prototype.constructor) {
    constructor = function() {
      throw Error('cannot instantiate an interface (no constructor defined).');
    };
  }

  var cls = goog.defineClass.createSealingConstructor_(constructor, superClass);
  if (superClass) {
    goog.inherits(cls, superClass);
  }

  // Remove all the properties that should not be copied to the prototype.
  delete def.constructor;
  delete def.statics;

  goog.defineClass.applyProperties_(cls.prototype, def);
  if (statics != null) {
    if (statics instanceof Function) {
      statics(cls);
    } else {
      goog.defineClass.applyProperties_(cls, statics);
    }
  }

  return cls;
};


/**
 * @typedef {{
 *   constructor: (!Function|undefined),
 *   statics: (Object|undefined|function(Function):void)
 * }}
 * @suppress {missingProvide}
 */
goog.defineClass.ClassDescriptor;


/**
 * @define {boolean} Whether the instances returned by
 * goog.defineClass should be sealed when possible.
 */
goog.define('goog.defineClass.SEAL_CLASS_INSTANCES', goog.DEBUG);


/**
 * If goog.defineClass.SEAL_CLASS_INSTANCES is enabled and Object.seal is
 * defined, this function will wrap the constructor in a function that seals the
 * results of the provided constructor function.
 *
 * @param {!Function} ctr The constructor whose results maybe be sealed.
 * @param {Function} superClass The superclass constructor.
 * @return {!Function} The replacement constructor.
 * @private
 */
goog.defineClass.createSealingConstructor_ = function(ctr, superClass) {
  if (goog.defineClass.SEAL_CLASS_INSTANCES &&
      Object.seal instanceof Function) {
    // Don't seal subclasses of unsealable-tagged legacy classes.
    if (superClass && superClass.prototype &&
        superClass.prototype[goog.UNSEALABLE_CONSTRUCTOR_PROPERTY_]) {
      return ctr;
    }
    /**
     * @this {Object}
     * @return {?}
     */
    var wrappedCtr = function() {
      // Don't seal an instance of a subclass when it calls the constructor of
      // its super class as there is most likely still setup to do.
      var instance = ctr.apply(this, arguments) || this;
      instance[goog.UID_PROPERTY_] = instance[goog.UID_PROPERTY_];
      if (this.constructor === wrappedCtr) {
        Object.seal(instance);
      }
      return instance;
    };
    return wrappedCtr;
  }
  return ctr;
};


// TODO(johnlenz): share these values with the goog.object
/**
 * The names of the fields that are defined on Object.prototype.
 * @type {!Array<string>}
 * @private
 * @const
 */
goog.defineClass.OBJECT_PROTOTYPE_FIELDS_ = [
  'constructor', 'hasOwnProperty', 'isPrototypeOf', 'propertyIsEnumerable',
  'toLocaleString', 'toString', 'valueOf'
];


// TODO(johnlenz): share this function with the goog.object
/**
 * @param {!Object} target The object to add properties to.
 * @param {!Object} source The object to copy properties from.
 * @private
 */
goog.defineClass.applyProperties_ = function(target, source) {
  // TODO(johnlenz): update this to support ES5 getters/setters

  var key;
  for (key in source) {
    if (Object.prototype.hasOwnProperty.call(source, key)) {
      target[key] = source[key];
    }
  }

  // For IE the for-in-loop does not contain any properties that are not
  // enumerable on the prototype object (for example isPrototypeOf from
  // Object.prototype) and it will also not include 'replace' on objects that
  // extend String and change 'replace' (not that it is common for anyone to
  // extend anything except Object).
  for (var i = 0; i < goog.defineClass.OBJECT_PROTOTYPE_FIELDS_.length; i++) {
    key = goog.defineClass.OBJECT_PROTOTYPE_FIELDS_[i];
    if (Object.prototype.hasOwnProperty.call(source, key)) {
      target[key] = source[key];
    }
  }
};


/**
 * Sealing classes breaks the older idiom of assigning properties on the
 * prototype rather than in the constructor.  As such, goog.defineClass
 * must not seal subclasses of these old-style classes until they are fixed.
 * Until then, this marks a class as "broken", instructing defineClass
 * not to seal subclasses.
 * @param {!Function} ctr The legacy constructor to tag as unsealable.
 */
goog.tagUnsealableClass = function(ctr) {
  if (!COMPILED && goog.defineClass.SEAL_CLASS_INSTANCES) {
    ctr.prototype[goog.UNSEALABLE_CONSTRUCTOR_PROPERTY_] = true;
  }
};


/**
 * Name for unsealable tag property.
 * @const @private {string}
 */
goog.UNSEALABLE_CONSTRUCTOR_PROPERTY_ = 'goog_defineClass_legacy_unsealable';

// Protocol Buffers - Google's data interchange format
// Copyright 2008 Google Inc.  All rights reserved.
// https://developers.google.com/protocol-buffers/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/**
 * @fileoverview This file contains constants and typedefs used by
 * jspb.BinaryReader and BinaryWriter.
 *
 * @author aappleby@google.com (Austin Appleby)
 */

goog.provide('jspb.AnyFieldType');
goog.provide('jspb.BinaryConstants');
goog.provide('jspb.BinaryMessage');
goog.provide('jspb.BuilderFunction');
goog.provide('jspb.ByteSource');
goog.provide('jspb.ClonerFunction');
goog.provide('jspb.ConstBinaryMessage');
goog.provide('jspb.ReaderFunction');
goog.provide('jspb.RecyclerFunction');
goog.provide('jspb.WriterFunction');

goog.forwardDeclare('jspb.Message');
goog.forwardDeclare('jsproto.BinaryExtension');



/**
 * Base interface class for all const messages. Does __not__ define any
 * methods, as doing so on a widely-used interface defeats dead-code
 * elimination.
 * @interface
 */
jspb.ConstBinaryMessage = function() {};



/**
 * Base interface class for all messages. Does __not__ define any methods, as
 * doing so on a widely-used interface defeats dead-code elimination.
 * @interface
 * @extends {jspb.ConstBinaryMessage}
 */
jspb.BinaryMessage = function() {};


/**
 * The types convertible to Uint8Arrays. Strings are assumed to be
 * base64-encoded.
 * @typedef {ArrayBuffer|Uint8Array|Array<number>|string}
 */
jspb.ByteSource;


/**
 * A field in jspb can be a scalar, a block of bytes, another proto, or an
 * array of any of the above.
 * @typedef {boolean|number|string|Uint8Array|
             jspb.BinaryMessage|jsproto.BinaryExtension|
             Array<jspb.AnyFieldType>}
 */
jspb.AnyFieldType;


/**
 * A builder function creates an instance of a message object.
 * @typedef {function():!jspb.BinaryMessage}
 */
jspb.BuilderFunction;


/**
 * A cloner function creates a deep copy of a message object.
 * @typedef {function(jspb.ConstBinaryMessage):jspb.BinaryMessage}
 */
jspb.ClonerFunction;


/**
 * A recycler function destroys an instance of a message object.
 * @typedef {function(!jspb.BinaryMessage):void}
 */
jspb.RecyclerFunction;


/**
 * A reader function initializes a message using data from a BinaryReader.
 * @typedef {function(!jspb.BinaryMessage, !jspb.BinaryReader):void}
 */
jspb.ReaderFunction;


/**
 * A writer function serializes a message to a BinaryWriter.
 * @typedef {!function(!jspb.Message, !jspb.BinaryWriter):void |
 *           !function(!jspb.ConstBinaryMessage, !jspb.BinaryWriter):void}
 */
jspb.WriterFunction;


/**
 * Field type codes, taken from proto2/public/wire_format_lite.h.
 * @enum {number}
 */
jspb.BinaryConstants.FieldType = {
  INVALID: -1,
  DOUBLE: 1,
  FLOAT: 2,
  INT64: 3,
  UINT64: 4,
  INT32: 5,
  FIXED64: 6,
  FIXED32: 7,
  BOOL: 8,
  STRING: 9,
  GROUP: 10,
  MESSAGE: 11,
  BYTES: 12,
  UINT32: 13,
  ENUM: 14,
  SFIXED32: 15,
  SFIXED64: 16,
  SINT32: 17,
  SINT64: 18,

  // Extended types for Javascript

  FHASH64: 30, // 64-bit hash string, fixed-length encoding.
  VHASH64: 31  // 64-bit hash string, varint encoding.
};


/**
 * Wire-format type codes, taken from proto2/public/wire_format_lite.h.
 * @enum {number}
 */
jspb.BinaryConstants.WireType = {
  INVALID: -1,
  VARINT: 0,
  FIXED64: 1,
  DELIMITED: 2,
  START_GROUP: 3,
  END_GROUP: 4,
  FIXED32: 5
};


/**
 * Translates field type to wire type.
 * @param {jspb.BinaryConstants.FieldType} fieldType
 * @return {jspb.BinaryConstants.WireType}
 */
jspb.BinaryConstants.FieldTypeToWireType = function(fieldType) {
  var fieldTypes = jspb.BinaryConstants.FieldType;
  var wireTypes = jspb.BinaryConstants.WireType;
  switch (fieldType) {
    case fieldTypes.INT32:
    case fieldTypes.INT64:
    case fieldTypes.UINT32:
    case fieldTypes.UINT64:
    case fieldTypes.SINT32:
    case fieldTypes.SINT64:
    case fieldTypes.BOOL:
    case fieldTypes.ENUM:
    case fieldTypes.VHASH64:
      return wireTypes.VARINT;

    case fieldTypes.DOUBLE:
    case fieldTypes.FIXED64:
    case fieldTypes.SFIXED64:
    case fieldTypes.FHASH64:
      return wireTypes.FIXED64;

    case fieldTypes.STRING:
    case fieldTypes.MESSAGE:
    case fieldTypes.BYTES:
      return wireTypes.DELIMITED;

    case fieldTypes.FLOAT:
    case fieldTypes.FIXED32:
    case fieldTypes.SFIXED32:
      return wireTypes.FIXED32;

    case fieldTypes.INVALID:
    case fieldTypes.GROUP:
    default:
      return wireTypes.INVALID;
  }
};


/**
 * Flag to indicate a missing field.
 * @const {number}
 */
jspb.BinaryConstants.INVALID_FIELD_NUMBER = -1;


/**
 * The smallest denormal float32 value.
 * @const {number}
 */
jspb.BinaryConstants.FLOAT32_EPS = 1.401298464324817e-45;


/**
 * The smallest normal float64 value.
 * @const {number}
 */
jspb.BinaryConstants.FLOAT32_MIN = 1.1754943508222875e-38;


/**
 * The largest finite float32 value.
 * @const {number}
 */
jspb.BinaryConstants.FLOAT32_MAX = 3.4028234663852886e+38;


/**
 * The smallest denormal float64 value.
 * @const {number}
 */
jspb.BinaryConstants.FLOAT64_EPS = 5e-324;


/**
 * The smallest normal float64 value.
 * @const {number}
 */
jspb.BinaryConstants.FLOAT64_MIN = 2.2250738585072014e-308;


/**
 * The largest finite float64 value.
 * @const {number}
 */
jspb.BinaryConstants.FLOAT64_MAX = 1.7976931348623157e+308;


/**
 * Convenience constant equal to 2^20.
 * @const {number}
 */
jspb.BinaryConstants.TWO_TO_20 = 1048576;


/**
 * Convenience constant equal to 2^23.
 * @const {number}
 */
jspb.BinaryConstants.TWO_TO_23 = 8388608;


/**
 * Convenience constant equal to 2^31.
 * @const {number}
 */
jspb.BinaryConstants.TWO_TO_31 = 2147483648;


/**
 * Convenience constant equal to 2^32.
 * @const {number}
 */
jspb.BinaryConstants.TWO_TO_32 = 4294967296;


/**
 * Convenience constant equal to 2^52.
 * @const {number}
 */
jspb.BinaryConstants.TWO_TO_52 = 4503599627370496;


/**
 * Convenience constant equal to 2^63.
 * @const {number}
 */
jspb.BinaryConstants.TWO_TO_63 = 9223372036854775808;


/**
 * Convenience constant equal to 2^64.
 * @const {number}
 */
jspb.BinaryConstants.TWO_TO_64 = 18446744073709551616;


/**
 * Eight-character string of zeros, used as the default 64-bit hash value.
 * @const {string}
 */
jspb.BinaryConstants.ZERO_HASH = '\0\0\0\0\0\0\0\0';

// Copyright 2006 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Definition of goog.dom.NodeType.
 */

goog.provide('goog.dom.NodeType');


/**
 * Constants for the nodeType attribute in the Node interface.
 *
 * These constants match those specified in the Node interface. These are
 * usually present on the Node object in recent browsers, but not in older
 * browsers (specifically, early IEs) and thus are given here.
 *
 * In some browsers (early IEs), these are not defined on the Node object,
 * so they are provided here.
 *
 * See http://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-1950641247
 * @enum {number}
 */
goog.dom.NodeType = {
  ELEMENT: 1,
  ATTRIBUTE: 2,
  TEXT: 3,
  CDATA_SECTION: 4,
  ENTITY_REFERENCE: 5,
  ENTITY: 6,
  PROCESSING_INSTRUCTION: 7,
  COMMENT: 8,
  DOCUMENT: 9,
  DOCUMENT_TYPE: 10,
  DOCUMENT_FRAGMENT: 11,
  NOTATION: 12
};

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Provides a base class for custom Error objects such that the
 * stack is correctly maintained.
 *
 * You should never need to throw goog.debug.Error(msg) directly, Error(msg) is
 * sufficient.
 *
 */

goog.provide('goog.debug.Error');



/**
 * Base class for custom error objects.
 * @param {*=} opt_msg The message associated with the error.
 * @constructor
 * @extends {Error}
 */
goog.debug.Error = function(opt_msg) {

  // Attempt to ensure there is a stack trace.
  if (Error.captureStackTrace) {
    Error.captureStackTrace(this, goog.debug.Error);
  } else {
    var stack = new Error().stack;
    if (stack) {
      this.stack = stack;
    }
  }

  if (opt_msg) {
    this.message = String(opt_msg);
  }

  /**
   * Whether to report this error to the server. Setting this to false will
   * cause the error reporter to not report the error back to the server,
   * which can be useful if the client knows that the error has already been
   * logged on the server.
   * @type {boolean}
   */
  this.reportErrorToServer = true;
};
goog.inherits(goog.debug.Error, Error);


/** @override */
goog.debug.Error.prototype.name = 'CustomError';

// Copyright 2006 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Utilities for string manipulation.
 * @author arv@google.com (Erik Arvidsson)
 */


/**
 * Namespace for string utilities
 */
goog.provide('goog.string');
goog.provide('goog.string.Unicode');


/**
 * @define {boolean} Enables HTML escaping of lowercase letter "e" which helps
 * with detection of double-escaping as this letter is frequently used.
 */
goog.define('goog.string.DETECT_DOUBLE_ESCAPING', false);


/**
 * @define {boolean} Whether to force non-dom html unescaping.
 */
goog.define('goog.string.FORCE_NON_DOM_HTML_UNESCAPING', false);


/**
 * Common Unicode string characters.
 * @enum {string}
 */
goog.string.Unicode = {
  NBSP: '\xa0'
};


/**
 * Fast prefix-checker.
 * @param {string} str The string to check.
 * @param {string} prefix A string to look for at the start of {@code str}.
 * @return {boolean} True if {@code str} begins with {@code prefix}.
 */
goog.string.startsWith = function(str, prefix) {
  return str.lastIndexOf(prefix, 0) == 0;
};


/**
 * Fast suffix-checker.
 * @param {string} str The string to check.
 * @param {string} suffix A string to look for at the end of {@code str}.
 * @return {boolean} True if {@code str} ends with {@code suffix}.
 */
goog.string.endsWith = function(str, suffix) {
  var l = str.length - suffix.length;
  return l >= 0 && str.indexOf(suffix, l) == l;
};


/**
 * Case-insensitive prefix-checker.
 * @param {string} str The string to check.
 * @param {string} prefix  A string to look for at the end of {@code str}.
 * @return {boolean} True if {@code str} begins with {@code prefix} (ignoring
 *     case).
 */
goog.string.caseInsensitiveStartsWith = function(str, prefix) {
  return goog.string.caseInsensitiveCompare(
             prefix, str.substr(0, prefix.length)) == 0;
};


/**
 * Case-insensitive suffix-checker.
 * @param {string} str The string to check.
 * @param {string} suffix A string to look for at the end of {@code str}.
 * @return {boolean} True if {@code str} ends with {@code suffix} (ignoring
 *     case).
 */
goog.string.caseInsensitiveEndsWith = function(str, suffix) {
  return goog.string.caseInsensitiveCompare(
             suffix, str.substr(str.length - suffix.length, suffix.length)) ==
      0;
};


/**
 * Case-insensitive equality checker.
 * @param {string} str1 First string to check.
 * @param {string} str2 Second string to check.
 * @return {boolean} True if {@code str1} and {@code str2} are the same string,
 *     ignoring case.
 */
goog.string.caseInsensitiveEquals = function(str1, str2) {
  return str1.toLowerCase() == str2.toLowerCase();
};


/**
 * Does simple python-style string substitution.
 * subs("foo%s hot%s", "bar", "dog") becomes "foobar hotdog".
 * @param {string} str The string containing the pattern.
 * @param {...*} var_args The items to substitute into the pattern.
 * @return {string} A copy of {@code str} in which each occurrence of
 *     {@code %s} has been replaced an argument from {@code var_args}.
 */
goog.string.subs = function(str, var_args) {
  var splitParts = str.split('%s');
  var returnString = '';

  var subsArguments = Array.prototype.slice.call(arguments, 1);
  while (subsArguments.length &&
         // Replace up to the last split part. We are inserting in the
         // positions between split parts.
         splitParts.length > 1) {
    returnString += splitParts.shift() + subsArguments.shift();
  }

  return returnString + splitParts.join('%s');  // Join unused '%s'
};


/**
 * Converts multiple whitespace chars (spaces, non-breaking-spaces, new lines
 * and tabs) to a single space, and strips leading and trailing whitespace.
 * @param {string} str Input string.
 * @return {string} A copy of {@code str} with collapsed whitespace.
 */
goog.string.collapseWhitespace = function(str) {
  // Since IE doesn't include non-breaking-space (0xa0) in their \s character
  // class (as required by section 7.2 of the ECMAScript spec), we explicitly
  // include it in the regexp to enforce consistent cross-browser behavior.
  return str.replace(/[\s\xa0]+/g, ' ').replace(/^\s+|\s+$/g, '');
};


/**
 * Checks if a string is empty or contains only whitespaces.
 * @param {string} str The string to check.
 * @return {boolean} Whether {@code str} is empty or whitespace only.
 */
goog.string.isEmptyOrWhitespace = function(str) {
  // testing length == 0 first is actually slower in all browsers (about the
  // same in Opera).
  // Since IE doesn't include non-breaking-space (0xa0) in their \s character
  // class (as required by section 7.2 of the ECMAScript spec), we explicitly
  // include it in the regexp to enforce consistent cross-browser behavior.
  return /^[\s\xa0]*$/.test(str);
};


/**
 * Checks if a string is empty.
 * @param {string} str The string to check.
 * @return {boolean} Whether {@code str} is empty.
 */
goog.string.isEmptyString = function(str) {
  return str.length == 0;
};


/**
 * Checks if a string is empty or contains only whitespaces.
 *
 * TODO(user): Deprecate this when clients have been switched over to
 * goog.string.isEmptyOrWhitespace.
 *
 * @param {string} str The string to check.
 * @return {boolean} Whether {@code str} is empty or whitespace only.
 */
goog.string.isEmpty = goog.string.isEmptyOrWhitespace;


/**
 * Checks if a string is null, undefined, empty or contains only whitespaces.
 * @param {*} str The string to check.
 * @return {boolean} Whether {@code str} is null, undefined, empty, or
 *     whitespace only.
 * @deprecated Use goog.string.isEmptyOrWhitespace(goog.string.makeSafe(str))
 *     instead.
 */
goog.string.isEmptyOrWhitespaceSafe = function(str) {
  return goog.string.isEmptyOrWhitespace(goog.string.makeSafe(str));
};


/**
 * Checks if a string is null, undefined, empty or contains only whitespaces.
 *
 * TODO(user): Deprecate this when clients have been switched over to
 * goog.string.isEmptyOrWhitespaceSafe.
 *
 * @param {*} str The string to check.
 * @return {boolean} Whether {@code str} is null, undefined, empty, or
 *     whitespace only.
 */
goog.string.isEmptySafe = goog.string.isEmptyOrWhitespaceSafe;


/**
 * Checks if a string is all breaking whitespace.
 * @param {string} str The string to check.
 * @return {boolean} Whether the string is all breaking whitespace.
 */
goog.string.isBreakingWhitespace = function(str) {
  return !/[^\t\n\r ]/.test(str);
};


/**
 * Checks if a string contains all letters.
 * @param {string} str string to check.
 * @return {boolean} True if {@code str} consists entirely of letters.
 */
goog.string.isAlpha = function(str) {
  return !/[^a-zA-Z]/.test(str);
};


/**
 * Checks if a string contains only numbers.
 * @param {*} str string to check. If not a string, it will be
 *     casted to one.
 * @return {boolean} True if {@code str} is numeric.
 */
goog.string.isNumeric = function(str) {
  return !/[^0-9]/.test(str);
};


/**
 * Checks if a string contains only numbers or letters.
 * @param {string} str string to check.
 * @return {boolean} True if {@code str} is alphanumeric.
 */
goog.string.isAlphaNumeric = function(str) {
  return !/[^a-zA-Z0-9]/.test(str);
};


/**
 * Checks if a character is a space character.
 * @param {string} ch Character to check.
 * @return {boolean} True if {@code ch} is a space.
 */
goog.string.isSpace = function(ch) {
  return ch == ' ';
};


/**
 * Checks if a character is a valid unicode character.
 * @param {string} ch Character to check.
 * @return {boolean} True if {@code ch} is a valid unicode character.
 */
goog.string.isUnicodeChar = function(ch) {
  return ch.length == 1 && ch >= ' ' && ch <= '~' ||
      ch >= '\u0080' && ch <= '\uFFFD';
};


/**
 * Takes a string and replaces newlines with a space. Multiple lines are
 * replaced with a single space.
 * @param {string} str The string from which to strip newlines.
 * @return {string} A copy of {@code str} stripped of newlines.
 */
goog.string.stripNewlines = function(str) {
  return str.replace(/(\r\n|\r|\n)+/g, ' ');
};


/**
 * Replaces Windows and Mac new lines with unix style: \r or \r\n with \n.
 * @param {string} str The string to in which to canonicalize newlines.
 * @return {string} {@code str} A copy of {@code} with canonicalized newlines.
 */
goog.string.canonicalizeNewlines = function(str) {
  return str.replace(/(\r\n|\r|\n)/g, '\n');
};


/**
 * Normalizes whitespace in a string, replacing all whitespace chars with
 * a space.
 * @param {string} str The string in which to normalize whitespace.
 * @return {string} A copy of {@code str} with all whitespace normalized.
 */
goog.string.normalizeWhitespace = function(str) {
  return str.replace(/\xa0|\s/g, ' ');
};


/**
 * Normalizes spaces in a string, replacing all consecutive spaces and tabs
 * with a single space. Replaces non-breaking space with a space.
 * @param {string} str The string in which to normalize spaces.
 * @return {string} A copy of {@code str} with all consecutive spaces and tabs
 *    replaced with a single space.
 */
goog.string.normalizeSpaces = function(str) {
  return str.replace(/\xa0|[ \t]+/g, ' ');
};


/**
 * Removes the breaking spaces from the left and right of the string and
 * collapses the sequences of breaking spaces in the middle into single spaces.
 * The original and the result strings render the same way in HTML.
 * @param {string} str A string in which to collapse spaces.
 * @return {string} Copy of the string with normalized breaking spaces.
 */
goog.string.collapseBreakingSpaces = function(str) {
  return str.replace(/[\t\r\n ]+/g, ' ')
      .replace(/^[\t\r\n ]+|[\t\r\n ]+$/g, '');
};


/**
 * Trims white spaces to the left and right of a string.
 * @param {string} str The string to trim.
 * @return {string} A trimmed copy of {@code str}.
 */
goog.string.trim =
    (goog.TRUSTED_SITE && String.prototype.trim) ? function(str) {
      return str.trim();
    } : function(str) {
      // Since IE doesn't include non-breaking-space (0xa0) in their \s
      // character class (as required by section 7.2 of the ECMAScript spec),
      // we explicitly include it in the regexp to enforce consistent
      // cross-browser behavior.
      return str.replace(/^[\s\xa0]+|[\s\xa0]+$/g, '');
    };


/**
 * Trims whitespaces at the left end of a string.
 * @param {string} str The string to left trim.
 * @return {string} A trimmed copy of {@code str}.
 */
goog.string.trimLeft = function(str) {
  // Since IE doesn't include non-breaking-space (0xa0) in their \s character
  // class (as required by section 7.2 of the ECMAScript spec), we explicitly
  // include it in the regexp to enforce consistent cross-browser behavior.
  return str.replace(/^[\s\xa0]+/, '');
};


/**
 * Trims whitespaces at the right end of a string.
 * @param {string} str The string to right trim.
 * @return {string} A trimmed copy of {@code str}.
 */
goog.string.trimRight = function(str) {
  // Since IE doesn't include non-breaking-space (0xa0) in their \s character
  // class (as required by section 7.2 of the ECMAScript spec), we explicitly
  // include it in the regexp to enforce consistent cross-browser behavior.
  return str.replace(/[\s\xa0]+$/, '');
};


/**
 * A string comparator that ignores case.
 * -1 = str1 less than str2
 *  0 = str1 equals str2
 *  1 = str1 greater than str2
 *
 * @param {string} str1 The string to compare.
 * @param {string} str2 The string to compare {@code str1} to.
 * @return {number} The comparator result, as described above.
 */
goog.string.caseInsensitiveCompare = function(str1, str2) {
  var test1 = String(str1).toLowerCase();
  var test2 = String(str2).toLowerCase();

  if (test1 < test2) {
    return -1;
  } else if (test1 == test2) {
    return 0;
  } else {
    return 1;
  }
};


/**
 * Compares two strings interpreting their numeric substrings as numbers.
 *
 * @param {string} str1 First string.
 * @param {string} str2 Second string.
 * @param {!RegExp} tokenizerRegExp Splits a string into substrings of
 *     non-negative integers, non-numeric characters and optionally fractional
 *     numbers starting with a decimal point.
 * @return {number} Negative if str1 < str2, 0 is str1 == str2, positive if
 *     str1 > str2.
 * @private
 */
goog.string.numberAwareCompare_ = function(str1, str2, tokenizerRegExp) {
  if (str1 == str2) {
    return 0;
  }
  if (!str1) {
    return -1;
  }
  if (!str2) {
    return 1;
  }

  // Using match to split the entire string ahead of time turns out to be faster
  // for most inputs than using RegExp.exec or iterating over each character.
  var tokens1 = str1.toLowerCase().match(tokenizerRegExp);
  var tokens2 = str2.toLowerCase().match(tokenizerRegExp);

  var count = Math.min(tokens1.length, tokens2.length);

  for (var i = 0; i < count; i++) {
    var a = tokens1[i];
    var b = tokens2[i];

    // Compare pairs of tokens, returning if one token sorts before the other.
    if (a != b) {
      // Only if both tokens are integers is a special comparison required.
      // Decimal numbers are sorted as strings (e.g., '.09' < '.1').
      var num1 = parseInt(a, 10);
      if (!isNaN(num1)) {
        var num2 = parseInt(b, 10);
        if (!isNaN(num2) && num1 - num2) {
          return num1 - num2;
        }
      }
      return a < b ? -1 : 1;
    }
  }

  // If one string is a substring of the other, the shorter string sorts first.
  if (tokens1.length != tokens2.length) {
    return tokens1.length - tokens2.length;
  }

  // The two strings must be equivalent except for case (perfect equality is
  // tested at the head of the function.) Revert to default ASCII string
  // comparison to stabilize the sort.
  return str1 < str2 ? -1 : 1;
};


/**
 * String comparison function that handles non-negative integer numbers in a
 * way humans might expect. Using this function, the string 'File 2.jpg' sorts
 * before 'File 10.jpg', and 'Version 1.9' before 'Version 1.10'. The comparison
 * is mostly case-insensitive, though strings that are identical except for case
 * are sorted with the upper-case strings before lower-case.
 *
 * This comparison function is up to 50x slower than either the default or the
 * case-insensitive compare. It should not be used in time-critical code, but
 * should be fast enough to sort several hundred short strings (like filenames)
 * with a reasonable delay.
 *
 * @param {string} str1 The string to compare in a numerically sensitive way.
 * @param {string} str2 The string to compare {@code str1} to.
 * @return {number} less than 0 if str1 < str2, 0 if str1 == str2, greater than
 *     0 if str1 > str2.
 */
goog.string.intAwareCompare = function(str1, str2) {
  return goog.string.numberAwareCompare_(str1, str2, /\d+|\D+/g);
};


/**
 * String comparison function that handles non-negative integer and fractional
 * numbers in a way humans might expect. Using this function, the string
 * 'File 2.jpg' sorts before 'File 10.jpg', and '3.14' before '3.2'. Equivalent
 * to {@link goog.string.intAwareCompare} apart from the way how it interprets
 * dots.
 *
 * @param {string} str1 The string to compare in a numerically sensitive way.
 * @param {string} str2 The string to compare {@code str1} to.
 * @return {number} less than 0 if str1 < str2, 0 if str1 == str2, greater than
 *     0 if str1 > str2.
 */
goog.string.floatAwareCompare = function(str1, str2) {
  return goog.string.numberAwareCompare_(str1, str2, /\d+|\.\d+|\D+/g);
};


/**
 * Alias for {@link goog.string.floatAwareCompare}.
 *
 * @param {string} str1
 * @param {string} str2
 * @return {number}
 */
goog.string.numerateCompare = goog.string.floatAwareCompare;


/**
 * URL-encodes a string
 * @param {*} str The string to url-encode.
 * @return {string} An encoded copy of {@code str} that is safe for urls.
 *     Note that '#', ':', and other characters used to delimit portions
 *     of URLs *will* be encoded.
 */
goog.string.urlEncode = function(str) {
  return encodeURIComponent(String(str));
};


/**
 * URL-decodes the string. We need to specially handle '+'s because
 * the javascript library doesn't convert them to spaces.
 * @param {string} str The string to url decode.
 * @return {string} The decoded {@code str}.
 */
goog.string.urlDecode = function(str) {
  return decodeURIComponent(str.replace(/\+/g, ' '));
};


/**
 * Converts \n to <br>s or <br />s.
 * @param {string} str The string in which to convert newlines.
 * @param {boolean=} opt_xml Whether to use XML compatible tags.
 * @return {string} A copy of {@code str} with converted newlines.
 */
goog.string.newLineToBr = function(str, opt_xml) {
  return str.replace(/(\r\n|\r|\n)/g, opt_xml ? '<br />' : '<br>');
};


/**
 * Escapes double quote '"' and single quote '\'' characters in addition to
 * '&', '<', and '>' so that a string can be included in an HTML tag attribute
 * value within double or single quotes.
 *
 * It should be noted that > doesn't need to be escaped for the HTML or XML to
 * be valid, but it has been decided to escape it for consistency with other
 * implementations.
 *
 * With goog.string.DETECT_DOUBLE_ESCAPING, this function escapes also the
 * lowercase letter "e".
 *
 * NOTE(user):
 * HtmlEscape is often called during the generation of large blocks of HTML.
 * Using statics for the regular expressions and strings is an optimization
 * that can more than half the amount of time IE spends in this function for
 * large apps, since strings and regexes both contribute to GC allocations.
 *
 * Testing for the presence of a character before escaping increases the number
 * of function calls, but actually provides a speed increase for the average
 * case -- since the average case often doesn't require the escaping of all 4
 * characters and indexOf() is much cheaper than replace().
 * The worst case does suffer slightly from the additional calls, therefore the
 * opt_isLikelyToContainHtmlChars option has been included for situations
 * where all 4 HTML entities are very likely to be present and need escaping.
 *
 * Some benchmarks (times tended to fluctuate +-0.05ms):
 *                                     FireFox                     IE6
 * (no chars / average (mix of cases) / all 4 chars)
 * no checks                     0.13 / 0.22 / 0.22         0.23 / 0.53 / 0.80
 * indexOf                       0.08 / 0.17 / 0.26         0.22 / 0.54 / 0.84
 * indexOf + re test             0.07 / 0.17 / 0.28         0.19 / 0.50 / 0.85
 *
 * An additional advantage of checking if replace actually needs to be called
 * is a reduction in the number of object allocations, so as the size of the
 * application grows the difference between the various methods would increase.
 *
 * @param {string} str string to be escaped.
 * @param {boolean=} opt_isLikelyToContainHtmlChars Don't perform a check to see
 *     if the character needs replacing - use this option if you expect each of
 *     the characters to appear often. Leave false if you expect few html
 *     characters to occur in your strings, such as if you are escaping HTML.
 * @return {string} An escaped copy of {@code str}.
 */
goog.string.htmlEscape = function(str, opt_isLikelyToContainHtmlChars) {

  if (opt_isLikelyToContainHtmlChars) {
    str = str.replace(goog.string.AMP_RE_, '&amp;')
              .replace(goog.string.LT_RE_, '&lt;')
              .replace(goog.string.GT_RE_, '&gt;')
              .replace(goog.string.QUOT_RE_, '&quot;')
              .replace(goog.string.SINGLE_QUOTE_RE_, '&#39;')
              .replace(goog.string.NULL_RE_, '&#0;');
    if (goog.string.DETECT_DOUBLE_ESCAPING) {
      str = str.replace(goog.string.E_RE_, '&#101;');
    }
    return str;

  } else {
    // quick test helps in the case when there are no chars to replace, in
    // worst case this makes barely a difference to the time taken
    if (!goog.string.ALL_RE_.test(str)) return str;

    // str.indexOf is faster than regex.test in this case
    if (str.indexOf('&') != -1) {
      str = str.replace(goog.string.AMP_RE_, '&amp;');
    }
    if (str.indexOf('<') != -1) {
      str = str.replace(goog.string.LT_RE_, '&lt;');
    }
    if (str.indexOf('>') != -1) {
      str = str.replace(goog.string.GT_RE_, '&gt;');
    }
    if (str.indexOf('"') != -1) {
      str = str.replace(goog.string.QUOT_RE_, '&quot;');
    }
    if (str.indexOf('\'') != -1) {
      str = str.replace(goog.string.SINGLE_QUOTE_RE_, '&#39;');
    }
    if (str.indexOf('\x00') != -1) {
      str = str.replace(goog.string.NULL_RE_, '&#0;');
    }
    if (goog.string.DETECT_DOUBLE_ESCAPING && str.indexOf('e') != -1) {
      str = str.replace(goog.string.E_RE_, '&#101;');
    }
    return str;
  }
};


/**
 * Regular expression that matches an ampersand, for use in escaping.
 * @const {!RegExp}
 * @private
 */
goog.string.AMP_RE_ = /&/g;


/**
 * Regular expression that matches a less than sign, for use in escaping.
 * @const {!RegExp}
 * @private
 */
goog.string.LT_RE_ = /</g;


/**
 * Regular expression that matches a greater than sign, for use in escaping.
 * @const {!RegExp}
 * @private
 */
goog.string.GT_RE_ = />/g;


/**
 * Regular expression that matches a double quote, for use in escaping.
 * @const {!RegExp}
 * @private
 */
goog.string.QUOT_RE_ = /"/g;


/**
 * Regular expression that matches a single quote, for use in escaping.
 * @const {!RegExp}
 * @private
 */
goog.string.SINGLE_QUOTE_RE_ = /'/g;


/**
 * Regular expression that matches null character, for use in escaping.
 * @const {!RegExp}
 * @private
 */
goog.string.NULL_RE_ = /\x00/g;


/**
 * Regular expression that matches a lowercase letter "e", for use in escaping.
 * @const {!RegExp}
 * @private
 */
goog.string.E_RE_ = /e/g;


/**
 * Regular expression that matches any character that needs to be escaped.
 * @const {!RegExp}
 * @private
 */
goog.string.ALL_RE_ =
    (goog.string.DETECT_DOUBLE_ESCAPING ? /[\x00&<>"'e]/ : /[\x00&<>"']/);


/**
 * Unescapes an HTML string.
 *
 * @param {string} str The string to unescape.
 * @return {string} An unescaped copy of {@code str}.
 */
goog.string.unescapeEntities = function(str) {
  if (goog.string.contains(str, '&')) {
    // We are careful not to use a DOM if we do not have one or we explicitly
    // requested non-DOM html unescaping.
    if (!goog.string.FORCE_NON_DOM_HTML_UNESCAPING &&
        'document' in goog.global) {
      return goog.string.unescapeEntitiesUsingDom_(str);
    } else {
      // Fall back on pure XML entities
      return goog.string.unescapePureXmlEntities_(str);
    }
  }
  return str;
};


/**
 * Unescapes a HTML string using the provided document.
 *
 * @param {string} str The string to unescape.
 * @param {!Document} document A document to use in escaping the string.
 * @return {string} An unescaped copy of {@code str}.
 */
goog.string.unescapeEntitiesWithDocument = function(str, document) {
  if (goog.string.contains(str, '&')) {
    return goog.string.unescapeEntitiesUsingDom_(str, document);
  }
  return str;
};


/**
 * Unescapes an HTML string using a DOM to resolve non-XML, non-numeric
 * entities. This function is XSS-safe and whitespace-preserving.
 * @private
 * @param {string} str The string to unescape.
 * @param {Document=} opt_document An optional document to use for creating
 *     elements. If this is not specified then the default window.document
 *     will be used.
 * @return {string} The unescaped {@code str} string.
 */
goog.string.unescapeEntitiesUsingDom_ = function(str, opt_document) {
  /** @type {!Object<string, string>} */
  var seen = {'&amp;': '&', '&lt;': '<', '&gt;': '>', '&quot;': '"'};
  var div;
  if (opt_document) {
    div = opt_document.createElement('div');
  } else {
    div = goog.global.document.createElement('div');
  }
  // Match as many valid entity characters as possible. If the actual entity
  // happens to be shorter, it will still work as innerHTML will return the
  // trailing characters unchanged. Since the entity characters do not include
  // open angle bracket, there is no chance of XSS from the innerHTML use.
  // Since no whitespace is passed to innerHTML, whitespace is preserved.
  return str.replace(goog.string.HTML_ENTITY_PATTERN_, function(s, entity) {
    // Check for cached entity.
    var value = seen[s];
    if (value) {
      return value;
    }
    // Check for numeric entity.
    if (entity.charAt(0) == '#') {
      // Prefix with 0 so that hex entities (e.g. &#x10) parse as hex numbers.
      var n = Number('0' + entity.substr(1));
      if (!isNaN(n)) {
        value = String.fromCharCode(n);
      }
    }
    // Fall back to innerHTML otherwise.
    if (!value) {
      // Append a non-entity character to avoid a bug in Webkit that parses
      // an invalid entity at the end of innerHTML text as the empty string.
      div.innerHTML = s + ' ';
      // Then remove the trailing character from the result.
      value = div.firstChild.nodeValue.slice(0, -1);
    }
    // Cache and return.
    return seen[s] = value;
  });
};


/**
 * Unescapes XML entities.
 * @private
 * @param {string} str The string to unescape.
 * @return {string} An unescaped copy of {@code str}.
 */
goog.string.unescapePureXmlEntities_ = function(str) {
  return str.replace(/&([^;]+);/g, function(s, entity) {
    switch (entity) {
      case 'amp':
        return '&';
      case 'lt':
        return '<';
      case 'gt':
        return '>';
      case 'quot':
        return '"';
      default:
        if (entity.charAt(0) == '#') {
          // Prefix with 0 so that hex entities (e.g. &#x10) parse as hex.
          var n = Number('0' + entity.substr(1));
          if (!isNaN(n)) {
            return String.fromCharCode(n);
          }
        }
        // For invalid entities we just return the entity
        return s;
    }
  });
};


/**
 * Regular expression that matches an HTML entity.
 * See also HTML5: Tokenization / Tokenizing character references.
 * @private
 * @type {!RegExp}
 */
goog.string.HTML_ENTITY_PATTERN_ = /&([^;\s<&]+);?/g;


/**
 * Do escaping of whitespace to preserve spatial formatting. We use character
 * entity #160 to make it safer for xml.
 * @param {string} str The string in which to escape whitespace.
 * @param {boolean=} opt_xml Whether to use XML compatible tags.
 * @return {string} An escaped copy of {@code str}.
 */
goog.string.whitespaceEscape = function(str, opt_xml) {
  // This doesn't use goog.string.preserveSpaces for backwards compatibility.
  return goog.string.newLineToBr(str.replace(/  /g, ' &#160;'), opt_xml);
};


/**
 * Preserve spaces that would be otherwise collapsed in HTML by replacing them
 * with non-breaking space Unicode characters.
 * @param {string} str The string in which to preserve whitespace.
 * @return {string} A copy of {@code str} with preserved whitespace.
 */
goog.string.preserveSpaces = function(str) {
  return str.replace(/(^|[\n ]) /g, '$1' + goog.string.Unicode.NBSP);
};


/**
 * Strip quote characters around a string.  The second argument is a string of
 * characters to treat as quotes.  This can be a single character or a string of
 * multiple character and in that case each of those are treated as possible
 * quote characters. For example:
 *
 * <pre>
 * goog.string.stripQuotes('"abc"', '"`') --> 'abc'
 * goog.string.stripQuotes('`abc`', '"`') --> 'abc'
 * </pre>
 *
 * @param {string} str The string to strip.
 * @param {string} quoteChars The quote characters to strip.
 * @return {string} A copy of {@code str} without the quotes.
 */
goog.string.stripQuotes = function(str, quoteChars) {
  var length = quoteChars.length;
  for (var i = 0; i < length; i++) {
    var quoteChar = length == 1 ? quoteChars : quoteChars.charAt(i);
    if (str.charAt(0) == quoteChar && str.charAt(str.length - 1) == quoteChar) {
      return str.substring(1, str.length - 1);
    }
  }
  return str;
};


/**
 * Truncates a string to a certain length and adds '...' if necessary.  The
 * length also accounts for the ellipsis, so a maximum length of 10 and a string
 * 'Hello World!' produces 'Hello W...'.
 * @param {string} str The string to truncate.
 * @param {number} chars Max number of characters.
 * @param {boolean=} opt_protectEscapedCharacters Whether to protect escaped
 *     characters from being cut off in the middle.
 * @return {string} The truncated {@code str} string.
 */
goog.string.truncate = function(str, chars, opt_protectEscapedCharacters) {
  if (opt_protectEscapedCharacters) {
    str = goog.string.unescapeEntities(str);
  }

  if (str.length > chars) {
    str = str.substring(0, chars - 3) + '...';
  }

  if (opt_protectEscapedCharacters) {
    str = goog.string.htmlEscape(str);
  }

  return str;
};


/**
 * Truncate a string in the middle, adding "..." if necessary,
 * and favoring the beginning of the string.
 * @param {string} str The string to truncate the middle of.
 * @param {number} chars Max number of characters.
 * @param {boolean=} opt_protectEscapedCharacters Whether to protect escaped
 *     characters from being cutoff in the middle.
 * @param {number=} opt_trailingChars Optional number of trailing characters to
 *     leave at the end of the string, instead of truncating as close to the
 *     middle as possible.
 * @return {string} A truncated copy of {@code str}.
 */
goog.string.truncateMiddle = function(
    str, chars, opt_protectEscapedCharacters, opt_trailingChars) {
  if (opt_protectEscapedCharacters) {
    str = goog.string.unescapeEntities(str);
  }

  if (opt_trailingChars && str.length > chars) {
    if (opt_trailingChars > chars) {
      opt_trailingChars = chars;
    }
    var endPoint = str.length - opt_trailingChars;
    var startPoint = chars - opt_trailingChars;
    str = str.substring(0, startPoint) + '...' + str.substring(endPoint);
  } else if (str.length > chars) {
    // Favor the beginning of the string:
    var half = Math.floor(chars / 2);
    var endPos = str.length - half;
    half += chars % 2;
    str = str.substring(0, half) + '...' + str.substring(endPos);
  }

  if (opt_protectEscapedCharacters) {
    str = goog.string.htmlEscape(str);
  }

  return str;
};


/**
 * Special chars that need to be escaped for goog.string.quote.
 * @private {!Object<string, string>}
 */
goog.string.specialEscapeChars_ = {
  '\0': '\\0',
  '\b': '\\b',
  '\f': '\\f',
  '\n': '\\n',
  '\r': '\\r',
  '\t': '\\t',
  '\x0B': '\\x0B',  // '\v' is not supported in JScript
  '"': '\\"',
  '\\': '\\\\',
  // To support the use case of embedding quoted strings inside of script
  // tags, we have to make sure HTML comments and opening/closing script tags do
  // not appear in the resulting string. The specific strings that must be
  // escaped are documented at:
  // http://www.w3.org/TR/html51/semantics.html#restrictions-for-contents-of-script-elements
  '<': '\x3c'
};


/**
 * Character mappings used internally for goog.string.escapeChar.
 * @private {!Object<string, string>}
 */
goog.string.jsEscapeCache_ = {
  '\'': '\\\''
};


/**
 * Encloses a string in double quotes and escapes characters so that the
 * string is a valid JS string. The resulting string is safe to embed in
 * `<script>` tags as "<" is escaped.
 * @param {string} s The string to quote.
 * @return {string} A copy of {@code s} surrounded by double quotes.
 */
goog.string.quote = function(s) {
  s = String(s);
  var sb = ['"'];
  for (var i = 0; i < s.length; i++) {
    var ch = s.charAt(i);
    var cc = ch.charCodeAt(0);
    sb[i + 1] = goog.string.specialEscapeChars_[ch] ||
        ((cc > 31 && cc < 127) ? ch : goog.string.escapeChar(ch));
  }
  sb.push('"');
  return sb.join('');
};


/**
 * Takes a string and returns the escaped string for that character.
 * @param {string} str The string to escape.
 * @return {string} An escaped string representing {@code str}.
 */
goog.string.escapeString = function(str) {
  var sb = [];
  for (var i = 0; i < str.length; i++) {
    sb[i] = goog.string.escapeChar(str.charAt(i));
  }
  return sb.join('');
};


/**
 * Takes a character and returns the escaped string for that character. For
 * example escapeChar(String.fromCharCode(15)) -> "\\x0E".
 * @param {string} c The character to escape.
 * @return {string} An escaped string representing {@code c}.
 */
goog.string.escapeChar = function(c) {
  if (c in goog.string.jsEscapeCache_) {
    return goog.string.jsEscapeCache_[c];
  }

  if (c in goog.string.specialEscapeChars_) {
    return goog.string.jsEscapeCache_[c] = goog.string.specialEscapeChars_[c];
  }

  var rv = c;
  var cc = c.charCodeAt(0);
  if (cc > 31 && cc < 127) {
    rv = c;
  } else {
    // tab is 9 but handled above
    if (cc < 256) {
      rv = '\\x';
      if (cc < 16 || cc > 256) {
        rv += '0';
      }
    } else {
      rv = '\\u';
      if (cc < 4096) {  // \u1000
        rv += '0';
      }
    }
    rv += cc.toString(16).toUpperCase();
  }

  return goog.string.jsEscapeCache_[c] = rv;
};


/**
 * Determines whether a string contains a substring.
 * @param {string} str The string to search.
 * @param {string} subString The substring to search for.
 * @return {boolean} Whether {@code str} contains {@code subString}.
 */
goog.string.contains = function(str, subString) {
  return str.indexOf(subString) != -1;
};


/**
 * Determines whether a string contains a substring, ignoring case.
 * @param {string} str The string to search.
 * @param {string} subString The substring to search for.
 * @return {boolean} Whether {@code str} contains {@code subString}.
 */
goog.string.caseInsensitiveContains = function(str, subString) {
  return goog.string.contains(str.toLowerCase(), subString.toLowerCase());
};


/**
 * Returns the non-overlapping occurrences of ss in s.
 * If either s or ss evalutes to false, then returns zero.
 * @param {string} s The string to look in.
 * @param {string} ss The string to look for.
 * @return {number} Number of occurrences of ss in s.
 */
goog.string.countOf = function(s, ss) {
  return s && ss ? s.split(ss).length - 1 : 0;
};


/**
 * Removes a substring of a specified length at a specific
 * index in a string.
 * @param {string} s The base string from which to remove.
 * @param {number} index The index at which to remove the substring.
 * @param {number} stringLength The length of the substring to remove.
 * @return {string} A copy of {@code s} with the substring removed or the full
 *     string if nothing is removed or the input is invalid.
 */
goog.string.removeAt = function(s, index, stringLength) {
  var resultStr = s;
  // If the index is greater or equal to 0 then remove substring
  if (index >= 0 && index < s.length && stringLength > 0) {
    resultStr = s.substr(0, index) +
        s.substr(index + stringLength, s.length - index - stringLength);
  }
  return resultStr;
};


/**
 *  Removes the first occurrence of a substring from a string.
 *  @param {string} s The base string from which to remove.
 *  @param {string} ss The string to remove.
 *  @return {string} A copy of {@code s} with {@code ss} removed or the full
 *      string if nothing is removed.
 */
goog.string.remove = function(s, ss) {
  var re = new RegExp(goog.string.regExpEscape(ss), '');
  return s.replace(re, '');
};


/**
 *  Removes all occurrences of a substring from a string.
 *  @param {string} s The base string from which to remove.
 *  @param {string} ss The string to remove.
 *  @return {string} A copy of {@code s} with {@code ss} removed or the full
 *      string if nothing is removed.
 */
goog.string.removeAll = function(s, ss) {
  var re = new RegExp(goog.string.regExpEscape(ss), 'g');
  return s.replace(re, '');
};


/**
 * Escapes characters in the string that are not safe to use in a RegExp.
 * @param {*} s The string to escape. If not a string, it will be casted
 *     to one.
 * @return {string} A RegExp safe, escaped copy of {@code s}.
 */
goog.string.regExpEscape = function(s) {
  return String(s)
      .replace(/([-()\[\]{}+?*.$\^|,:#<!\\])/g, '\\$1')
      .replace(/\x08/g, '\\x08');
};


/**
 * Repeats a string n times.
 * @param {string} string The string to repeat.
 * @param {number} length The number of times to repeat.
 * @return {string} A string containing {@code length} repetitions of
 *     {@code string}.
 */
goog.string.repeat = (String.prototype.repeat) ? function(string, length) {
  // The native method is over 100 times faster than the alternative.
  return string.repeat(length);
} : function(string, length) {
  return new Array(length + 1).join(string);
};


/**
 * Pads number to given length and optionally rounds it to a given precision.
 * For example:
 * <pre>padNumber(1.25, 2, 3) -> '01.250'
 * padNumber(1.25, 2) -> '01.25'
 * padNumber(1.25, 2, 1) -> '01.3'
 * padNumber(1.25, 0) -> '1.25'</pre>
 *
 * @param {number} num The number to pad.
 * @param {number} length The desired length.
 * @param {number=} opt_precision The desired precision.
 * @return {string} {@code num} as a string with the given options.
 */
goog.string.padNumber = function(num, length, opt_precision) {
  var s = goog.isDef(opt_precision) ? num.toFixed(opt_precision) : String(num);
  var index = s.indexOf('.');
  if (index == -1) {
    index = s.length;
  }
  return goog.string.repeat('0', Math.max(0, length - index)) + s;
};


/**
 * Returns a string representation of the given object, with
 * null and undefined being returned as the empty string.
 *
 * @param {*} obj The object to convert.
 * @return {string} A string representation of the {@code obj}.
 */
goog.string.makeSafe = function(obj) {
  return obj == null ? '' : String(obj);
};


/**
 * Concatenates string expressions. This is useful
 * since some browsers are very inefficient when it comes to using plus to
 * concat strings. Be careful when using null and undefined here since
 * these will not be included in the result. If you need to represent these
 * be sure to cast the argument to a String first.
 * For example:
 * <pre>buildString('a', 'b', 'c', 'd') -> 'abcd'
 * buildString(null, undefined) -> ''
 * </pre>
 * @param {...*} var_args A list of strings to concatenate. If not a string,
 *     it will be casted to one.
 * @return {string} The concatenation of {@code var_args}.
 */
goog.string.buildString = function(var_args) {
  return Array.prototype.join.call(arguments, '');
};


/**
 * Returns a string with at least 64-bits of randomness.
 *
 * Doesn't trust Javascript's random function entirely. Uses a combination of
 * random and current timestamp, and then encodes the string in base-36 to
 * make it shorter.
 *
 * @return {string} A random string, e.g. sn1s7vb4gcic.
 */
goog.string.getRandomString = function() {
  var x = 2147483648;
  return Math.floor(Math.random() * x).toString(36) +
      Math.abs(Math.floor(Math.random() * x) ^ goog.now()).toString(36);
};


/**
 * Compares two version numbers.
 *
 * @param {string|number} version1 Version of first item.
 * @param {string|number} version2 Version of second item.
 *
 * @return {number}  1 if {@code version1} is higher.
 *                   0 if arguments are equal.
 *                  -1 if {@code version2} is higher.
 */
goog.string.compareVersions = function(version1, version2) {
  var order = 0;
  // Trim leading and trailing whitespace and split the versions into
  // subversions.
  var v1Subs = goog.string.trim(String(version1)).split('.');
  var v2Subs = goog.string.trim(String(version2)).split('.');
  var subCount = Math.max(v1Subs.length, v2Subs.length);

  // Iterate over the subversions, as long as they appear to be equivalent.
  for (var subIdx = 0; order == 0 && subIdx < subCount; subIdx++) {
    var v1Sub = v1Subs[subIdx] || '';
    var v2Sub = v2Subs[subIdx] || '';

    // Split the subversions into pairs of numbers and qualifiers (like 'b').
    // Two different RegExp objects are needed because they are both using
    // the 'g' flag.
    var v1CompParser = new RegExp('(\\d*)(\\D*)', 'g');
    var v2CompParser = new RegExp('(\\d*)(\\D*)', 'g');
    do {
      var v1Comp = v1CompParser.exec(v1Sub) || ['', '', ''];
      var v2Comp = v2CompParser.exec(v2Sub) || ['', '', ''];
      // Break if there are no more matches.
      if (v1Comp[0].length == 0 && v2Comp[0].length == 0) {
        break;
      }

      // Parse the numeric part of the subversion. A missing number is
      // equivalent to 0.
      var v1CompNum = v1Comp[1].length == 0 ? 0 : parseInt(v1Comp[1], 10);
      var v2CompNum = v2Comp[1].length == 0 ? 0 : parseInt(v2Comp[1], 10);

      // Compare the subversion components. The number has the highest
      // precedence. Next, if the numbers are equal, a subversion without any
      // qualifier is always higher than a subversion with any qualifier. Next,
      // the qualifiers are compared as strings.
      order = goog.string.compareElements_(v1CompNum, v2CompNum) ||
          goog.string.compareElements_(
              v1Comp[2].length == 0, v2Comp[2].length == 0) ||
          goog.string.compareElements_(v1Comp[2], v2Comp[2]);
      // Stop as soon as an inequality is discovered.
    } while (order == 0);
  }

  return order;
};


/**
 * Compares elements of a version number.
 *
 * @param {string|number|boolean} left An element from a version number.
 * @param {string|number|boolean} right An element from a version number.
 *
 * @return {number}  1 if {@code left} is higher.
 *                   0 if arguments are equal.
 *                  -1 if {@code right} is higher.
 * @private
 */
goog.string.compareElements_ = function(left, right) {
  if (left < right) {
    return -1;
  } else if (left > right) {
    return 1;
  }
  return 0;
};


/**
 * String hash function similar to java.lang.String.hashCode().
 * The hash code for a string is computed as
 * s[0] * 31 ^ (n - 1) + s[1] * 31 ^ (n - 2) + ... + s[n - 1],
 * where s[i] is the ith character of the string and n is the length of
 * the string. We mod the result to make it between 0 (inclusive) and 2^32
 * (exclusive).
 * @param {string} str A string.
 * @return {number} Hash value for {@code str}, between 0 (inclusive) and 2^32
 *  (exclusive). The empty string returns 0.
 */
goog.string.hashCode = function(str) {
  var result = 0;
  for (var i = 0; i < str.length; ++i) {
    // Normalize to 4 byte range, 0 ... 2^32.
    result = (31 * result + str.charCodeAt(i)) >>> 0;
  }
  return result;
};


/**
 * The most recent unique ID. |0 is equivalent to Math.floor in this case.
 * @type {number}
 * @private
 */
goog.string.uniqueStringCounter_ = Math.random() * 0x80000000 | 0;


/**
 * Generates and returns a string which is unique in the current document.
 * This is useful, for example, to create unique IDs for DOM elements.
 * @return {string} A unique id.
 */
goog.string.createUniqueString = function() {
  return 'goog_' + goog.string.uniqueStringCounter_++;
};


/**
 * Converts the supplied string to a number, which may be Infinity or NaN.
 * This function strips whitespace: (toNumber(' 123') === 123)
 * This function accepts scientific notation: (toNumber('1e1') === 10)
 *
 * This is better than Javascript's built-in conversions because, sadly:
 *     (Number(' ') === 0) and (parseFloat('123a') === 123)
 *
 * @param {string} str The string to convert.
 * @return {number} The number the supplied string represents, or NaN.
 */
goog.string.toNumber = function(str) {
  var num = Number(str);
  if (num == 0 && goog.string.isEmptyOrWhitespace(str)) {
    return NaN;
  }
  return num;
};


/**
 * Returns whether the given string is lower camel case (e.g. "isFooBar").
 *
 * Note that this assumes the string is entirely letters.
 * @see http://en.wikipedia.org/wiki/CamelCase#Variations_and_synonyms
 *
 * @param {string} str String to test.
 * @return {boolean} Whether the string is lower camel case.
 */
goog.string.isLowerCamelCase = function(str) {
  return /^[a-z]+([A-Z][a-z]*)*$/.test(str);
};


/**
 * Returns whether the given string is upper camel case (e.g. "FooBarBaz").
 *
 * Note that this assumes the string is entirely letters.
 * @see http://en.wikipedia.org/wiki/CamelCase#Variations_and_synonyms
 *
 * @param {string} str String to test.
 * @return {boolean} Whether the string is upper camel case.
 */
goog.string.isUpperCamelCase = function(str) {
  return /^([A-Z][a-z]*)+$/.test(str);
};


/**
 * Converts a string from selector-case to camelCase (e.g. from
 * "multi-part-string" to "multiPartString"), useful for converting
 * CSS selectors and HTML dataset keys to their equivalent JS properties.
 * @param {string} str The string in selector-case form.
 * @return {string} The string in camelCase form.
 */
goog.string.toCamelCase = function(str) {
  return String(str).replace(
      /\-([a-z])/g, function(all, match) { return match.toUpperCase(); });
};


/**
 * Converts a string from camelCase to selector-case (e.g. from
 * "multiPartString" to "multi-part-string"), useful for converting JS
 * style and dataset properties to equivalent CSS selectors and HTML keys.
 * @param {string} str The string in camelCase form.
 * @return {string} The string in selector-case form.
 */
goog.string.toSelectorCase = function(str) {
  return String(str).replace(/([A-Z])/g, '-$1').toLowerCase();
};


/**
 * Converts a string into TitleCase. First character of the string is always
 * capitalized in addition to the first letter of every subsequent word.
 * Words are delimited by one or more whitespaces by default. Custom delimiters
 * can optionally be specified to replace the default, which doesn't preserve
 * whitespace delimiters and instead must be explicitly included if needed.
 *
 * Default delimiter => " ":
 *    goog.string.toTitleCase('oneTwoThree')    => 'OneTwoThree'
 *    goog.string.toTitleCase('one two three')  => 'One Two Three'
 *    goog.string.toTitleCase('  one   two   ') => '  One   Two   '
 *    goog.string.toTitleCase('one_two_three')  => 'One_two_three'
 *    goog.string.toTitleCase('one-two-three')  => 'One-two-three'
 *
 * Custom delimiter => "_-.":
 *    goog.string.toTitleCase('oneTwoThree', '_-.')       => 'OneTwoThree'
 *    goog.string.toTitleCase('one two three', '_-.')     => 'One two three'
 *    goog.string.toTitleCase('  one   two   ', '_-.')    => '  one   two   '
 *    goog.string.toTitleCase('one_two_three', '_-.')     => 'One_Two_Three'
 *    goog.string.toTitleCase('one-two-three', '_-.')     => 'One-Two-Three'
 *    goog.string.toTitleCase('one...two...three', '_-.') => 'One...Two...Three'
 *    goog.string.toTitleCase('one. two. three', '_-.')   => 'One. two. three'
 *    goog.string.toTitleCase('one-two.three', '_-.')     => 'One-Two.Three'
 *
 * @param {string} str String value in camelCase form.
 * @param {string=} opt_delimiters Custom delimiter character set used to
 *      distinguish words in the string value. Each character represents a
 *      single delimiter. When provided, default whitespace delimiter is
 *      overridden and must be explicitly included if needed.
 * @return {string} String value in TitleCase form.
 */
goog.string.toTitleCase = function(str, opt_delimiters) {
  var delimiters = goog.isString(opt_delimiters) ?
      goog.string.regExpEscape(opt_delimiters) :
      '\\s';

  // For IE8, we need to prevent using an empty character set. Otherwise,
  // incorrect matching will occur.
  delimiters = delimiters ? '|[' + delimiters + ']+' : '';

  var regexp = new RegExp('(^' + delimiters + ')([a-z])', 'g');
  return str.replace(
      regexp, function(all, p1, p2) { return p1 + p2.toUpperCase(); });
};


/**
 * Capitalizes a string, i.e. converts the first letter to uppercase
 * and all other letters to lowercase, e.g.:
 *
 * goog.string.capitalize('one')     => 'One'
 * goog.string.capitalize('ONE')     => 'One'
 * goog.string.capitalize('one two') => 'One two'
 *
 * Note that this function does not trim initial whitespace.
 *
 * @param {string} str String value to capitalize.
 * @return {string} String value with first letter in uppercase.
 */
goog.string.capitalize = function(str) {
  return String(str.charAt(0)).toUpperCase() +
      String(str.substr(1)).toLowerCase();
};


/**
 * Parse a string in decimal or hexidecimal ('0xFFFF') form.
 *
 * To parse a particular radix, please use parseInt(string, radix) directly. See
 * https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/parseInt
 *
 * This is a wrapper for the built-in parseInt function that will only parse
 * numbers as base 10 or base 16.  Some JS implementations assume strings
 * starting with "0" are intended to be octal. ES3 allowed but discouraged
 * this behavior. ES5 forbids it.  This function emulates the ES5 behavior.
 *
 * For more information, see Mozilla JS Reference: http://goo.gl/8RiFj
 *
 * @param {string|number|null|undefined} value The value to be parsed.
 * @return {number} The number, parsed. If the string failed to parse, this
 *     will be NaN.
 */
goog.string.parseInt = function(value) {
  // Force finite numbers to strings.
  if (isFinite(value)) {
    value = String(value);
  }

  if (goog.isString(value)) {
    // If the string starts with '0x' or '-0x', parse as hex.
    return /^\s*-?0x/i.test(value) ? parseInt(value, 16) : parseInt(value, 10);
  }

  return NaN;
};


/**
 * Splits a string on a separator a limited number of times.
 *
 * This implementation is more similar to Python or Java, where the limit
 * parameter specifies the maximum number of splits rather than truncating
 * the number of results.
 *
 * See http://docs.python.org/2/library/stdtypes.html#str.split
 * See JavaDoc: http://goo.gl/F2AsY
 * See Mozilla reference: http://goo.gl/dZdZs
 *
 * @param {string} str String to split.
 * @param {string} separator The separator.
 * @param {number} limit The limit to the number of splits. The resulting array
 *     will have a maximum length of limit+1.  Negative numbers are the same
 *     as zero.
 * @return {!Array<string>} The string, split.
 */

goog.string.splitLimit = function(str, separator, limit) {
  var parts = str.split(separator);
  var returnVal = [];

  // Only continue doing this while we haven't hit the limit and we have
  // parts left.
  while (limit > 0 && parts.length) {
    returnVal.push(parts.shift());
    limit--;
  }

  // If there are remaining parts, append them to the end.
  if (parts.length) {
    returnVal.push(parts.join(separator));
  }

  return returnVal;
};


/**
 * Computes the Levenshtein edit distance between two strings.
 * @param {string} a
 * @param {string} b
 * @return {number} The edit distance between the two strings.
 */
goog.string.editDistance = function(a, b) {
  var v0 = [];
  var v1 = [];

  if (a == b) {
    return 0;
  }

  if (!a.length || !b.length) {
    return Math.max(a.length, b.length);
  }

  for (var i = 0; i < b.length + 1; i++) {
    v0[i] = i;
  }

  for (var i = 0; i < a.length; i++) {
    v1[0] = i + 1;

    for (var j = 0; j < b.length; j++) {
      var cost = Number(a[i] != b[j]);
      // Cost for the substring is the minimum of adding one character, removing
      // one character, or a swap.
      v1[j + 1] = Math.min(v1[j] + 1, v0[j + 1] + 1, v0[j] + cost);
    }

    for (var j = 0; j < v0.length; j++) {
      v0[j] = v1[j];
    }
  }

  return v1[b.length];
};

// Copyright 2008 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Utilities to check the preconditions, postconditions and
 * invariants runtime.
 *
 * Methods in this package should be given special treatment by the compiler
 * for type-inference. For example, <code>goog.asserts.assert(foo)</code>
 * will restrict <code>foo</code> to a truthy value.
 *
 * The compiler has an option to disable asserts. So code like:
 * <code>
 * var x = goog.asserts.assert(foo()); goog.asserts.assert(bar());
 * </code>
 * will be transformed into:
 * <code>
 * var x = foo();
 * </code>
 * The compiler will leave in foo() (because its return value is used),
 * but it will remove bar() because it assumes it does not have side-effects.
 *
 * @author agrieve@google.com (Andrew Grieve)
 */

goog.provide('goog.asserts');
goog.provide('goog.asserts.AssertionError');

goog.require('goog.debug.Error');
goog.require('goog.dom.NodeType');
goog.require('goog.string');


/**
 * @define {boolean} Whether to strip out asserts or to leave them in.
 */
goog.define('goog.asserts.ENABLE_ASSERTS', goog.DEBUG);



/**
 * Error object for failed assertions.
 * @param {string} messagePattern The pattern that was used to form message.
 * @param {!Array<*>} messageArgs The items to substitute into the pattern.
 * @constructor
 * @extends {goog.debug.Error}
 * @final
 */
goog.asserts.AssertionError = function(messagePattern, messageArgs) {
  messageArgs.unshift(messagePattern);
  goog.debug.Error.call(this, goog.string.subs.apply(null, messageArgs));
  // Remove the messagePattern afterwards to avoid permanently modifying the
  // passed in array.
  messageArgs.shift();

  /**
   * The message pattern used to format the error message. Error handlers can
   * use this to uniquely identify the assertion.
   * @type {string}
   */
  this.messagePattern = messagePattern;
};
goog.inherits(goog.asserts.AssertionError, goog.debug.Error);


/** @override */
goog.asserts.AssertionError.prototype.name = 'AssertionError';


/**
 * The default error handler.
 * @param {!goog.asserts.AssertionError} e The exception to be handled.
 */
goog.asserts.DEFAULT_ERROR_HANDLER = function(e) {
  throw e;
};


/**
 * The handler responsible for throwing or logging assertion errors.
 * @private {function(!goog.asserts.AssertionError)}
 */
goog.asserts.errorHandler_ = goog.asserts.DEFAULT_ERROR_HANDLER;


/**
 * Throws an exception with the given message and "Assertion failed" prefixed
 * onto it.
 * @param {string} defaultMessage The message to use if givenMessage is empty.
 * @param {Array<*>} defaultArgs The substitution arguments for defaultMessage.
 * @param {string|undefined} givenMessage Message supplied by the caller.
 * @param {Array<*>} givenArgs The substitution arguments for givenMessage.
 * @throws {goog.asserts.AssertionError} When the value is not a number.
 * @private
 */
goog.asserts.doAssertFailure_ = function(
    defaultMessage, defaultArgs, givenMessage, givenArgs) {
  var message = 'Assertion failed';
  if (givenMessage) {
    message += ': ' + givenMessage;
    var args = givenArgs;
  } else if (defaultMessage) {
    message += ': ' + defaultMessage;
    args = defaultArgs;
  }
  // The '' + works around an Opera 10 bug in the unit tests. Without it,
  // a stack trace is added to var message above. With this, a stack trace is
  // not added until this line (it causes the extra garbage to be added after
  // the assertion message instead of in the middle of it).
  var e = new goog.asserts.AssertionError('' + message, args || []);
  goog.asserts.errorHandler_(e);
};


/**
 * Sets a custom error handler that can be used to customize the behavior of
 * assertion failures, for example by turning all assertion failures into log
 * messages.
 * @param {function(!goog.asserts.AssertionError)} errorHandler
 */
goog.asserts.setErrorHandler = function(errorHandler) {
  if (goog.asserts.ENABLE_ASSERTS) {
    goog.asserts.errorHandler_ = errorHandler;
  }
};


/**
 * Checks if the condition evaluates to true if goog.asserts.ENABLE_ASSERTS is
 * true.
 * @template T
 * @param {T} condition The condition to check.
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @return {T} The value of the condition.
 * @throws {goog.asserts.AssertionError} When the condition evaluates to false.
 */
goog.asserts.assert = function(condition, opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS && !condition) {
    goog.asserts.doAssertFailure_(
        '', null, opt_message, Array.prototype.slice.call(arguments, 2));
  }
  return condition;
};


/**
 * Fails if goog.asserts.ENABLE_ASSERTS is true. This function is useful in case
 * when we want to add a check in the unreachable area like switch-case
 * statement:
 *
 * <pre>
 *  switch(type) {
 *    case FOO: doSomething(); break;
 *    case BAR: doSomethingElse(); break;
 *    default: goog.assert.fail('Unrecognized type: ' + type);
 *      // We have only 2 types - "default:" section is unreachable code.
 *  }
 * </pre>
 *
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @throws {goog.asserts.AssertionError} Failure.
 */
goog.asserts.fail = function(opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS) {
    goog.asserts.errorHandler_(
        new goog.asserts.AssertionError(
            'Failure' + (opt_message ? ': ' + opt_message : ''),
            Array.prototype.slice.call(arguments, 1)));
  }
};


/**
 * Checks if the value is a number if goog.asserts.ENABLE_ASSERTS is true.
 * @param {*} value The value to check.
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @return {number} The value, guaranteed to be a number when asserts enabled.
 * @throws {goog.asserts.AssertionError} When the value is not a number.
 */
goog.asserts.assertNumber = function(value, opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS && !goog.isNumber(value)) {
    goog.asserts.doAssertFailure_(
        'Expected number but got %s: %s.', [goog.typeOf(value), value],
        opt_message, Array.prototype.slice.call(arguments, 2));
  }
  return /** @type {number} */ (value);
};


/**
 * Checks if the value is a string if goog.asserts.ENABLE_ASSERTS is true.
 * @param {*} value The value to check.
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @return {string} The value, guaranteed to be a string when asserts enabled.
 * @throws {goog.asserts.AssertionError} When the value is not a string.
 */
goog.asserts.assertString = function(value, opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS && !goog.isString(value)) {
    goog.asserts.doAssertFailure_(
        'Expected string but got %s: %s.', [goog.typeOf(value), value],
        opt_message, Array.prototype.slice.call(arguments, 2));
  }
  return /** @type {string} */ (value);
};


/**
 * Checks if the value is a function if goog.asserts.ENABLE_ASSERTS is true.
 * @param {*} value The value to check.
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @return {!Function} The value, guaranteed to be a function when asserts
 *     enabled.
 * @throws {goog.asserts.AssertionError} When the value is not a function.
 */
goog.asserts.assertFunction = function(value, opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS && !goog.isFunction(value)) {
    goog.asserts.doAssertFailure_(
        'Expected function but got %s: %s.', [goog.typeOf(value), value],
        opt_message, Array.prototype.slice.call(arguments, 2));
  }
  return /** @type {!Function} */ (value);
};


/**
 * Checks if the value is an Object if goog.asserts.ENABLE_ASSERTS is true.
 * @param {*} value The value to check.
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @return {!Object} The value, guaranteed to be a non-null object.
 * @throws {goog.asserts.AssertionError} When the value is not an object.
 */
goog.asserts.assertObject = function(value, opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS && !goog.isObject(value)) {
    goog.asserts.doAssertFailure_(
        'Expected object but got %s: %s.', [goog.typeOf(value), value],
        opt_message, Array.prototype.slice.call(arguments, 2));
  }
  return /** @type {!Object} */ (value);
};


/**
 * Checks if the value is an Array if goog.asserts.ENABLE_ASSERTS is true.
 * @param {*} value The value to check.
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @return {!Array<?>} The value, guaranteed to be a non-null array.
 * @throws {goog.asserts.AssertionError} When the value is not an array.
 */
goog.asserts.assertArray = function(value, opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS && !goog.isArray(value)) {
    goog.asserts.doAssertFailure_(
        'Expected array but got %s: %s.', [goog.typeOf(value), value],
        opt_message, Array.prototype.slice.call(arguments, 2));
  }
  return /** @type {!Array<?>} */ (value);
};


/**
 * Checks if the value is a boolean if goog.asserts.ENABLE_ASSERTS is true.
 * @param {*} value The value to check.
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @return {boolean} The value, guaranteed to be a boolean when asserts are
 *     enabled.
 * @throws {goog.asserts.AssertionError} When the value is not a boolean.
 */
goog.asserts.assertBoolean = function(value, opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS && !goog.isBoolean(value)) {
    goog.asserts.doAssertFailure_(
        'Expected boolean but got %s: %s.', [goog.typeOf(value), value],
        opt_message, Array.prototype.slice.call(arguments, 2));
  }
  return /** @type {boolean} */ (value);
};


/**
 * Checks if the value is a DOM Element if goog.asserts.ENABLE_ASSERTS is true.
 * @param {*} value The value to check.
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @return {!Element} The value, likely to be a DOM Element when asserts are
 *     enabled.
 * @throws {goog.asserts.AssertionError} When the value is not an Element.
 */
goog.asserts.assertElement = function(value, opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS &&
      (!goog.isObject(value) || value.nodeType != goog.dom.NodeType.ELEMENT)) {
    goog.asserts.doAssertFailure_(
        'Expected Element but got %s: %s.', [goog.typeOf(value), value],
        opt_message, Array.prototype.slice.call(arguments, 2));
  }
  return /** @type {!Element} */ (value);
};


/**
 * Checks if the value is an instance of the user-defined type if
 * goog.asserts.ENABLE_ASSERTS is true.
 *
 * The compiler may tighten the type returned by this function.
 *
 * @param {?} value The value to check.
 * @param {function(new: T, ...)} type A user-defined constructor.
 * @param {string=} opt_message Error message in case of failure.
 * @param {...*} var_args The items to substitute into the failure message.
 * @throws {goog.asserts.AssertionError} When the value is not an instance of
 *     type.
 * @return {T}
 * @template T
 */
goog.asserts.assertInstanceof = function(value, type, opt_message, var_args) {
  if (goog.asserts.ENABLE_ASSERTS && !(value instanceof type)) {
    goog.asserts.doAssertFailure_(
        'Expected instanceof %s but got %s.',
        [goog.asserts.getType_(type), goog.asserts.getType_(value)],
        opt_message, Array.prototype.slice.call(arguments, 3));
  }
  return value;
};


/**
 * Checks that no enumerable keys are present in Object.prototype. Such keys
 * would break most code that use {@code for (var ... in ...)} loops.
 */
goog.asserts.assertObjectPrototypeIsIntact = function() {
  for (var key in Object.prototype) {
    goog.asserts.fail(key + ' should not be enumerable in Object.prototype.');
  }
};


/**
 * Returns the type of a value. If a constructor is passed, and a suitable
 * string cannot be found, 'unknown type name' will be returned.
 * @param {*} value A constructor, object, or primitive.
 * @return {string} The best display name for the value, or 'unknown type name'.
 * @private
 */
goog.asserts.getType_ = function(value) {
  if (value instanceof Function) {
    return value.displayName || value.name || 'unknown type name';
  } else if (value instanceof Object) {
    return value.constructor.displayName || value.constructor.name ||
        Object.prototype.toString.call(value);
  } else {
    return value === null ? 'null' : typeof value;
  }
};

// Copyright 2006 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Utilities for manipulating arrays.
 *
 * @author arv@google.com (Erik Arvidsson)
 */


goog.provide('goog.array');
goog.provide('goog.array.ArrayLike');

goog.require('goog.asserts');


/**
 * @define {boolean} NATIVE_ARRAY_PROTOTYPES indicates whether the code should
 * rely on Array.prototype functions, if available.
 *
 * The Array.prototype functions can be defined by external libraries like
 * Prototype and setting this flag to false forces closure to use its own
 * goog.array implementation.
 *
 * If your javascript can be loaded by a third party site and you are wary about
 * relying on the prototype functions, specify
 * "--define goog.NATIVE_ARRAY_PROTOTYPES=false" to the JSCompiler.
 *
 * Setting goog.TRUSTED_SITE to false will automatically set
 * NATIVE_ARRAY_PROTOTYPES to false.
 */
goog.define('goog.NATIVE_ARRAY_PROTOTYPES', goog.TRUSTED_SITE);


/**
 * @define {boolean} If true, JSCompiler will use the native implementation of
 * array functions where appropriate (e.g., {@code Array#filter}) and remove the
 * unused pure JS implementation.
 */
goog.define('goog.array.ASSUME_NATIVE_FUNCTIONS', false);


/**
 * @typedef {Array|NodeList|Arguments|{length: number}}
 */
goog.array.ArrayLike;


/**
 * Returns the last element in an array without removing it.
 * Same as goog.array.last.
 * @param {Array<T>|goog.array.ArrayLike} array The array.
 * @return {T} Last item in array.
 * @template T
 */
goog.array.peek = function(array) {
  return array[array.length - 1];
};


/**
 * Returns the last element in an array without removing it.
 * Same as goog.array.peek.
 * @param {Array<T>|goog.array.ArrayLike} array The array.
 * @return {T} Last item in array.
 * @template T
 */
goog.array.last = goog.array.peek;

// NOTE(arv): Since most of the array functions are generic it allows you to
// pass an array-like object. Strings have a length and are considered array-
// like. However, the 'in' operator does not work on strings so we cannot just
// use the array path even if the browser supports indexing into strings. We
// therefore end up splitting the string.


/**
 * Returns the index of the first element of an array with a specified value, or
 * -1 if the element is not present in the array.
 *
 * See {@link http://tinyurl.com/developer-mozilla-org-array-indexof}
 *
 * @param {Array<T>|goog.array.ArrayLike} arr The array to be searched.
 * @param {T} obj The object for which we are searching.
 * @param {number=} opt_fromIndex The index at which to start the search. If
 *     omitted the search starts at index 0.
 * @return {number} The index of the first matching array element.
 * @template T
 */
goog.array.indexOf = goog.NATIVE_ARRAY_PROTOTYPES &&
        (goog.array.ASSUME_NATIVE_FUNCTIONS || Array.prototype.indexOf) ?
    function(arr, obj, opt_fromIndex) {
      goog.asserts.assert(arr.length != null);

      return Array.prototype.indexOf.call(arr, obj, opt_fromIndex);
    } :
    function(arr, obj, opt_fromIndex) {
      var fromIndex = opt_fromIndex == null ?
          0 :
          (opt_fromIndex < 0 ? Math.max(0, arr.length + opt_fromIndex) :
                               opt_fromIndex);

      if (goog.isString(arr)) {
        // Array.prototype.indexOf uses === so only strings should be found.
        if (!goog.isString(obj) || obj.length != 1) {
          return -1;
        }
        return arr.indexOf(obj, fromIndex);
      }

      for (var i = fromIndex; i < arr.length; i++) {
        if (i in arr && arr[i] === obj) return i;
      }
      return -1;
    };


/**
 * Returns the index of the last element of an array with a specified value, or
 * -1 if the element is not present in the array.
 *
 * See {@link http://tinyurl.com/developer-mozilla-org-array-lastindexof}
 *
 * @param {!Array<T>|!goog.array.ArrayLike} arr The array to be searched.
 * @param {T} obj The object for which we are searching.
 * @param {?number=} opt_fromIndex The index at which to start the search. If
 *     omitted the search starts at the end of the array.
 * @return {number} The index of the last matching array element.
 * @template T
 */
goog.array.lastIndexOf = goog.NATIVE_ARRAY_PROTOTYPES &&
        (goog.array.ASSUME_NATIVE_FUNCTIONS || Array.prototype.lastIndexOf) ?
    function(arr, obj, opt_fromIndex) {
      goog.asserts.assert(arr.length != null);

      // Firefox treats undefined and null as 0 in the fromIndex argument which
      // leads it to always return -1
      var fromIndex = opt_fromIndex == null ? arr.length - 1 : opt_fromIndex;
      return Array.prototype.lastIndexOf.call(arr, obj, fromIndex);
    } :
    function(arr, obj, opt_fromIndex) {
      var fromIndex = opt_fromIndex == null ? arr.length - 1 : opt_fromIndex;

      if (fromIndex < 0) {
        fromIndex = Math.max(0, arr.length + fromIndex);
      }

      if (goog.isString(arr)) {
        // Array.prototype.lastIndexOf uses === so only strings should be found.
        if (!goog.isString(obj) || obj.length != 1) {
          return -1;
        }
        return arr.lastIndexOf(obj, fromIndex);
      }

      for (var i = fromIndex; i >= 0; i--) {
        if (i in arr && arr[i] === obj) return i;
      }
      return -1;
    };


/**
 * Calls a function for each element in an array. Skips holes in the array.
 * See {@link http://tinyurl.com/developer-mozilla-org-array-foreach}
 *
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array like object over
 *     which to iterate.
 * @param {?function(this: S, T, number, ?): ?} f The function to call for every
 *     element. This function takes 3 arguments (the element, the index and the
 *     array). The return value is ignored.
 * @param {S=} opt_obj The object to be used as the value of 'this' within f.
 * @template T,S
 */
goog.array.forEach = goog.NATIVE_ARRAY_PROTOTYPES &&
        (goog.array.ASSUME_NATIVE_FUNCTIONS || Array.prototype.forEach) ?
    function(arr, f, opt_obj) {
      goog.asserts.assert(arr.length != null);

      Array.prototype.forEach.call(arr, f, opt_obj);
    } :
    function(arr, f, opt_obj) {
      var l = arr.length;  // must be fixed during loop... see docs
      var arr2 = goog.isString(arr) ? arr.split('') : arr;
      for (var i = 0; i < l; i++) {
        if (i in arr2) {
          f.call(/** @type {?} */ (opt_obj), arr2[i], i, arr);
        }
      }
    };


/**
 * Calls a function for each element in an array, starting from the last
 * element rather than the first.
 *
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this: S, T, number, ?): ?} f The function to call for every
 *     element. This function
 *     takes 3 arguments (the element, the index and the array). The return
 *     value is ignored.
 * @param {S=} opt_obj The object to be used as the value of 'this'
 *     within f.
 * @template T,S
 */
goog.array.forEachRight = function(arr, f, opt_obj) {
  var l = arr.length;  // must be fixed during loop... see docs
  var arr2 = goog.isString(arr) ? arr.split('') : arr;
  for (var i = l - 1; i >= 0; --i) {
    if (i in arr2) {
      f.call(/** @type {?} */ (opt_obj), arr2[i], i, arr);
    }
  }
};


/**
 * Calls a function for each element in an array, and if the function returns
 * true adds the element to a new array.
 *
 * See {@link http://tinyurl.com/developer-mozilla-org-array-filter}
 *
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, T, number, ?):boolean} f The function to call for
 *     every element. This function
 *     takes 3 arguments (the element, the index and the array) and must
 *     return a Boolean. If the return value is true the element is added to the
 *     result array. If it is false the element is not included.
 * @param {S=} opt_obj The object to be used as the value of 'this'
 *     within f.
 * @return {!Array<T>} a new array in which only elements that passed the test
 *     are present.
 * @template T,S
 */
goog.array.filter = goog.NATIVE_ARRAY_PROTOTYPES &&
        (goog.array.ASSUME_NATIVE_FUNCTIONS || Array.prototype.filter) ?
    function(arr, f, opt_obj) {
      goog.asserts.assert(arr.length != null);

      return Array.prototype.filter.call(arr, f, opt_obj);
    } :
    function(arr, f, opt_obj) {
      var l = arr.length;  // must be fixed during loop... see docs
      var res = [];
      var resLength = 0;
      var arr2 = goog.isString(arr) ? arr.split('') : arr;
      for (var i = 0; i < l; i++) {
        if (i in arr2) {
          var val = arr2[i];  // in case f mutates arr2
          if (f.call(/** @type {?} */ (opt_obj), val, i, arr)) {
            res[resLength++] = val;
          }
        }
      }
      return res;
    };


/**
 * Calls a function for each element in an array and inserts the result into a
 * new array.
 *
 * See {@link http://tinyurl.com/developer-mozilla-org-array-map}
 *
 * @param {Array<VALUE>|goog.array.ArrayLike} arr Array or array like object
 *     over which to iterate.
 * @param {function(this:THIS, VALUE, number, ?): RESULT} f The function to call
 *     for every element. This function takes 3 arguments (the element,
 *     the index and the array) and should return something. The result will be
 *     inserted into a new array.
 * @param {THIS=} opt_obj The object to be used as the value of 'this' within f.
 * @return {!Array<RESULT>} a new array with the results from f.
 * @template THIS, VALUE, RESULT
 */
goog.array.map = goog.NATIVE_ARRAY_PROTOTYPES &&
        (goog.array.ASSUME_NATIVE_FUNCTIONS || Array.prototype.map) ?
    function(arr, f, opt_obj) {
      goog.asserts.assert(arr.length != null);

      return Array.prototype.map.call(arr, f, opt_obj);
    } :
    function(arr, f, opt_obj) {
      var l = arr.length;  // must be fixed during loop... see docs
      var res = new Array(l);
      var arr2 = goog.isString(arr) ? arr.split('') : arr;
      for (var i = 0; i < l; i++) {
        if (i in arr2) {
          res[i] = f.call(/** @type {?} */ (opt_obj), arr2[i], i, arr);
        }
      }
      return res;
    };


/**
 * Passes every element of an array into a function and accumulates the result.
 *
 * See {@link http://tinyurl.com/developer-mozilla-org-array-reduce}
 *
 * For example:
 * var a = [1, 2, 3, 4];
 * goog.array.reduce(a, function(r, v, i, arr) {return r + v;}, 0);
 * returns 10
 *
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {function(this:S, R, T, number, ?) : R} f The function to call for
 *     every element. This function
 *     takes 4 arguments (the function's previous result or the initial value,
 *     the value of the current array element, the current array index, and the
 *     array itself)
 *     function(previousValue, currentValue, index, array).
 * @param {?} val The initial value to pass into the function on the first call.
 * @param {S=} opt_obj  The object to be used as the value of 'this'
 *     within f.
 * @return {R} Result of evaluating f repeatedly across the values of the array.
 * @template T,S,R
 */
goog.array.reduce = goog.NATIVE_ARRAY_PROTOTYPES &&
        (goog.array.ASSUME_NATIVE_FUNCTIONS || Array.prototype.reduce) ?
    function(arr, f, val, opt_obj) {
      goog.asserts.assert(arr.length != null);
      if (opt_obj) {
        f = goog.bind(f, opt_obj);
      }
      return Array.prototype.reduce.call(arr, f, val);
    } :
    function(arr, f, val, opt_obj) {
      var rval = val;
      goog.array.forEach(arr, function(val, index) {
        rval = f.call(/** @type {?} */ (opt_obj), rval, val, index, arr);
      });
      return rval;
    };


/**
 * Passes every element of an array into a function and accumulates the result,
 * starting from the last element and working towards the first.
 *
 * See {@link http://tinyurl.com/developer-mozilla-org-array-reduceright}
 *
 * For example:
 * var a = ['a', 'b', 'c'];
 * goog.array.reduceRight(a, function(r, v, i, arr) {return r + v;}, '');
 * returns 'cba'
 *
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, R, T, number, ?) : R} f The function to call for
 *     every element. This function
 *     takes 4 arguments (the function's previous result or the initial value,
 *     the value of the current array element, the current array index, and the
 *     array itself)
 *     function(previousValue, currentValue, index, array).
 * @param {?} val The initial value to pass into the function on the first call.
 * @param {S=} opt_obj The object to be used as the value of 'this'
 *     within f.
 * @return {R} Object returned as a result of evaluating f repeatedly across the
 *     values of the array.
 * @template T,S,R
 */
goog.array.reduceRight = goog.NATIVE_ARRAY_PROTOTYPES &&
        (goog.array.ASSUME_NATIVE_FUNCTIONS || Array.prototype.reduceRight) ?
    function(arr, f, val, opt_obj) {
      goog.asserts.assert(arr.length != null);
      goog.asserts.assert(f != null);
      if (opt_obj) {
        f = goog.bind(f, opt_obj);
      }
      return Array.prototype.reduceRight.call(arr, f, val);
    } :
    function(arr, f, val, opt_obj) {
      var rval = val;
      goog.array.forEachRight(arr, function(val, index) {
        rval = f.call(/** @type {?} */ (opt_obj), rval, val, index, arr);
      });
      return rval;
    };


/**
 * Calls f for each element of an array. If any call returns true, some()
 * returns true (without checking the remaining elements). If all calls
 * return false, some() returns false.
 *
 * See {@link http://tinyurl.com/developer-mozilla-org-array-some}
 *
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, T, number, ?) : boolean} f The function to call for
 *     for every element. This function takes 3 arguments (the element, the
 *     index and the array) and should return a boolean.
 * @param {S=} opt_obj  The object to be used as the value of 'this'
 *     within f.
 * @return {boolean} true if any element passes the test.
 * @template T,S
 */
goog.array.some = goog.NATIVE_ARRAY_PROTOTYPES &&
        (goog.array.ASSUME_NATIVE_FUNCTIONS || Array.prototype.some) ?
    function(arr, f, opt_obj) {
      goog.asserts.assert(arr.length != null);

      return Array.prototype.some.call(arr, f, opt_obj);
    } :
    function(arr, f, opt_obj) {
      var l = arr.length;  // must be fixed during loop... see docs
      var arr2 = goog.isString(arr) ? arr.split('') : arr;
      for (var i = 0; i < l; i++) {
        if (i in arr2 && f.call(/** @type {?} */ (opt_obj), arr2[i], i, arr)) {
          return true;
        }
      }
      return false;
    };


/**
 * Call f for each element of an array. If all calls return true, every()
 * returns true. If any call returns false, every() returns false and
 * does not continue to check the remaining elements.
 *
 * See {@link http://tinyurl.com/developer-mozilla-org-array-every}
 *
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, T, number, ?) : boolean} f The function to call for
 *     for every element. This function takes 3 arguments (the element, the
 *     index and the array) and should return a boolean.
 * @param {S=} opt_obj The object to be used as the value of 'this'
 *     within f.
 * @return {boolean} false if any element fails the test.
 * @template T,S
 */
goog.array.every = goog.NATIVE_ARRAY_PROTOTYPES &&
        (goog.array.ASSUME_NATIVE_FUNCTIONS || Array.prototype.every) ?
    function(arr, f, opt_obj) {
      goog.asserts.assert(arr.length != null);

      return Array.prototype.every.call(arr, f, opt_obj);
    } :
    function(arr, f, opt_obj) {
      var l = arr.length;  // must be fixed during loop... see docs
      var arr2 = goog.isString(arr) ? arr.split('') : arr;
      for (var i = 0; i < l; i++) {
        if (i in arr2 && !f.call(/** @type {?} */ (opt_obj), arr2[i], i, arr)) {
          return false;
        }
      }
      return true;
    };


/**
 * Counts the array elements that fulfill the predicate, i.e. for which the
 * callback function returns true. Skips holes in the array.
 *
 * @param {!(Array<T>|goog.array.ArrayLike)} arr Array or array like object
 *     over which to iterate.
 * @param {function(this: S, T, number, ?): boolean} f The function to call for
 *     every element. Takes 3 arguments (the element, the index and the array).
 * @param {S=} opt_obj The object to be used as the value of 'this' within f.
 * @return {number} The number of the matching elements.
 * @template T,S
 */
goog.array.count = function(arr, f, opt_obj) {
  var count = 0;
  goog.array.forEach(arr, function(element, index, arr) {
    if (f.call(/** @type {?} */ (opt_obj), element, index, arr)) {
      ++count;
    }
  }, opt_obj);
  return count;
};


/**
 * Search an array for the first element that satisfies a given condition and
 * return that element.
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, T, number, ?) : boolean} f The function to call
 *     for every element. This function takes 3 arguments (the element, the
 *     index and the array) and should return a boolean.
 * @param {S=} opt_obj An optional "this" context for the function.
 * @return {T|null} The first array element that passes the test, or null if no
 *     element is found.
 * @template T,S
 */
goog.array.find = function(arr, f, opt_obj) {
  var i = goog.array.findIndex(arr, f, opt_obj);
  return i < 0 ? null : goog.isString(arr) ? arr.charAt(i) : arr[i];
};


/**
 * Search an array for the first element that satisfies a given condition and
 * return its index.
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, T, number, ?) : boolean} f The function to call for
 *     every element. This function
 *     takes 3 arguments (the element, the index and the array) and should
 *     return a boolean.
 * @param {S=} opt_obj An optional "this" context for the function.
 * @return {number} The index of the first array element that passes the test,
 *     or -1 if no element is found.
 * @template T,S
 */
goog.array.findIndex = function(arr, f, opt_obj) {
  var l = arr.length;  // must be fixed during loop... see docs
  var arr2 = goog.isString(arr) ? arr.split('') : arr;
  for (var i = 0; i < l; i++) {
    if (i in arr2 && f.call(/** @type {?} */ (opt_obj), arr2[i], i, arr)) {
      return i;
    }
  }
  return -1;
};


/**
 * Search an array (in reverse order) for the last element that satisfies a
 * given condition and return that element.
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, T, number, ?) : boolean} f The function to call
 *     for every element. This function
 *     takes 3 arguments (the element, the index and the array) and should
 *     return a boolean.
 * @param {S=} opt_obj An optional "this" context for the function.
 * @return {T|null} The last array element that passes the test, or null if no
 *     element is found.
 * @template T,S
 */
goog.array.findRight = function(arr, f, opt_obj) {
  var i = goog.array.findIndexRight(arr, f, opt_obj);
  return i < 0 ? null : goog.isString(arr) ? arr.charAt(i) : arr[i];
};


/**
 * Search an array (in reverse order) for the last element that satisfies a
 * given condition and return its index.
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, T, number, ?) : boolean} f The function to call
 *     for every element. This function
 *     takes 3 arguments (the element, the index and the array) and should
 *     return a boolean.
 * @param {S=} opt_obj An optional "this" context for the function.
 * @return {number} The index of the last array element that passes the test,
 *     or -1 if no element is found.
 * @template T,S
 */
goog.array.findIndexRight = function(arr, f, opt_obj) {
  var l = arr.length;  // must be fixed during loop... see docs
  var arr2 = goog.isString(arr) ? arr.split('') : arr;
  for (var i = l - 1; i >= 0; i--) {
    if (i in arr2 && f.call(/** @type {?} */ (opt_obj), arr2[i], i, arr)) {
      return i;
    }
  }
  return -1;
};


/**
 * Whether the array contains the given object.
 * @param {goog.array.ArrayLike} arr The array to test for the presence of the
 *     element.
 * @param {*} obj The object for which to test.
 * @return {boolean} true if obj is present.
 */
goog.array.contains = function(arr, obj) {
  return goog.array.indexOf(arr, obj) >= 0;
};


/**
 * Whether the array is empty.
 * @param {goog.array.ArrayLike} arr The array to test.
 * @return {boolean} true if empty.
 */
goog.array.isEmpty = function(arr) {
  return arr.length == 0;
};


/**
 * Clears the array.
 * @param {goog.array.ArrayLike} arr Array or array like object to clear.
 */
goog.array.clear = function(arr) {
  // For non real arrays we don't have the magic length so we delete the
  // indices.
  if (!goog.isArray(arr)) {
    for (var i = arr.length - 1; i >= 0; i--) {
      delete arr[i];
    }
  }
  arr.length = 0;
};


/**
 * Pushes an item into an array, if it's not already in the array.
 * @param {Array<T>} arr Array into which to insert the item.
 * @param {T} obj Value to add.
 * @template T
 */
goog.array.insert = function(arr, obj) {
  if (!goog.array.contains(arr, obj)) {
    arr.push(obj);
  }
};


/**
 * Inserts an object at the given index of the array.
 * @param {goog.array.ArrayLike} arr The array to modify.
 * @param {*} obj The object to insert.
 * @param {number=} opt_i The index at which to insert the object. If omitted,
 *      treated as 0. A negative index is counted from the end of the array.
 */
goog.array.insertAt = function(arr, obj, opt_i) {
  goog.array.splice(arr, opt_i, 0, obj);
};


/**
 * Inserts at the given index of the array, all elements of another array.
 * @param {goog.array.ArrayLike} arr The array to modify.
 * @param {goog.array.ArrayLike} elementsToAdd The array of elements to add.
 * @param {number=} opt_i The index at which to insert the object. If omitted,
 *      treated as 0. A negative index is counted from the end of the array.
 */
goog.array.insertArrayAt = function(arr, elementsToAdd, opt_i) {
  goog.partial(goog.array.splice, arr, opt_i, 0).apply(null, elementsToAdd);
};


/**
 * Inserts an object into an array before a specified object.
 * @param {Array<T>} arr The array to modify.
 * @param {T} obj The object to insert.
 * @param {T=} opt_obj2 The object before which obj should be inserted. If obj2
 *     is omitted or not found, obj is inserted at the end of the array.
 * @template T
 */
goog.array.insertBefore = function(arr, obj, opt_obj2) {
  var i;
  if (arguments.length == 2 || (i = goog.array.indexOf(arr, opt_obj2)) < 0) {
    arr.push(obj);
  } else {
    goog.array.insertAt(arr, obj, i);
  }
};


/**
 * Removes the first occurrence of a particular value from an array.
 * @param {Array<T>|goog.array.ArrayLike} arr Array from which to remove
 *     value.
 * @param {T} obj Object to remove.
 * @return {boolean} True if an element was removed.
 * @template T
 */
goog.array.remove = function(arr, obj) {
  var i = goog.array.indexOf(arr, obj);
  var rv;
  if ((rv = i >= 0)) {
    goog.array.removeAt(arr, i);
  }
  return rv;
};


/**
 * Removes from an array the element at index i
 * @param {goog.array.ArrayLike} arr Array or array like object from which to
 *     remove value.
 * @param {number} i The index to remove.
 * @return {boolean} True if an element was removed.
 */
goog.array.removeAt = function(arr, i) {
  goog.asserts.assert(arr.length != null);

  // use generic form of splice
  // splice returns the removed items and if successful the length of that
  // will be 1
  return Array.prototype.splice.call(arr, i, 1).length == 1;
};


/**
 * Removes the first value that satisfies the given condition.
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, T, number, ?) : boolean} f The function to call
 *     for every element. This function
 *     takes 3 arguments (the element, the index and the array) and should
 *     return a boolean.
 * @param {S=} opt_obj An optional "this" context for the function.
 * @return {boolean} True if an element was removed.
 * @template T,S
 */
goog.array.removeIf = function(arr, f, opt_obj) {
  var i = goog.array.findIndex(arr, f, opt_obj);
  if (i >= 0) {
    goog.array.removeAt(arr, i);
    return true;
  }
  return false;
};


/**
 * Removes all values that satisfy the given condition.
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array
 *     like object over which to iterate.
 * @param {?function(this:S, T, number, ?) : boolean} f The function to call
 *     for every element. This function
 *     takes 3 arguments (the element, the index and the array) and should
 *     return a boolean.
 * @param {S=} opt_obj An optional "this" context for the function.
 * @return {number} The number of items removed
 * @template T,S
 */
goog.array.removeAllIf = function(arr, f, opt_obj) {
  var removedCount = 0;
  goog.array.forEachRight(arr, function(val, index) {
    if (f.call(/** @type {?} */ (opt_obj), val, index, arr)) {
      if (goog.array.removeAt(arr, index)) {
        removedCount++;
      }
    }
  });
  return removedCount;
};


/**
 * Returns a new array that is the result of joining the arguments.  If arrays
 * are passed then their items are added, however, if non-arrays are passed they
 * will be added to the return array as is.
 *
 * Note that ArrayLike objects will be added as is, rather than having their
 * items added.
 *
 * goog.array.concat([1, 2], [3, 4]) -> [1, 2, 3, 4]
 * goog.array.concat(0, [1, 2]) -> [0, 1, 2]
 * goog.array.concat([1, 2], null) -> [1, 2, null]
 *
 * There is bug in all current versions of IE (6, 7 and 8) where arrays created
 * in an iframe become corrupted soon (not immediately) after the iframe is
 * destroyed. This is common if loading data via goog.net.IframeIo, for example.
 * This corruption only affects the concat method which will start throwing
 * Catastrophic Errors (#-2147418113).
 *
 * See http://endoflow.com/scratch/corrupted-arrays.html for a test case.
 *
 * Internally goog.array should use this, so that all methods will continue to
 * work on these broken array objects.
 *
 * @param {...*} var_args Items to concatenate.  Arrays will have each item
 *     added, while primitives and objects will be added as is.
 * @return {!Array<?>} The new resultant array.
 */
goog.array.concat = function(var_args) {
  return Array.prototype.concat.apply(Array.prototype, arguments);
};


/**
 * Returns a new array that contains the contents of all the arrays passed.
 * @param {...!Array<T>} var_args
 * @return {!Array<T>}
 * @template T
 */
goog.array.join = function(var_args) {
  return Array.prototype.concat.apply(Array.prototype, arguments);
};


/**
 * Converts an object to an array.
 * @param {Array<T>|goog.array.ArrayLike} object  The object to convert to an
 *     array.
 * @return {!Array<T>} The object converted into an array. If object has a
 *     length property, every property indexed with a non-negative number
 *     less than length will be included in the result. If object does not
 *     have a length property, an empty array will be returned.
 * @template T
 */
goog.array.toArray = function(object) {
  var length = object.length;

  // If length is not a number the following it false. This case is kept for
  // backwards compatibility since there are callers that pass objects that are
  // not array like.
  if (length > 0) {
    var rv = new Array(length);
    for (var i = 0; i < length; i++) {
      rv[i] = object[i];
    }
    return rv;
  }
  return [];
};


/**
 * Does a shallow copy of an array.
 * @param {Array<T>|goog.array.ArrayLike} arr  Array or array-like object to
 *     clone.
 * @return {!Array<T>} Clone of the input array.
 * @template T
 */
goog.array.clone = goog.array.toArray;


/**
 * Extends an array with another array, element, or "array like" object.
 * This function operates 'in-place', it does not create a new Array.
 *
 * Example:
 * var a = [];
 * goog.array.extend(a, [0, 1]);
 * a; // [0, 1]
 * goog.array.extend(a, 2);
 * a; // [0, 1, 2]
 *
 * @param {Array<VALUE>} arr1  The array to modify.
 * @param {...(Array<VALUE>|VALUE)} var_args The elements or arrays of elements
 *     to add to arr1.
 * @template VALUE
 */
goog.array.extend = function(arr1, var_args) {
  for (var i = 1; i < arguments.length; i++) {
    var arr2 = arguments[i];
    if (goog.isArrayLike(arr2)) {
      var len1 = arr1.length || 0;
      var len2 = arr2.length || 0;
      arr1.length = len1 + len2;
      for (var j = 0; j < len2; j++) {
        arr1[len1 + j] = arr2[j];
      }
    } else {
      arr1.push(arr2);
    }
  }
};


/**
 * Adds or removes elements from an array. This is a generic version of Array
 * splice. This means that it might work on other objects similar to arrays,
 * such as the arguments object.
 *
 * @param {Array<T>|goog.array.ArrayLike} arr The array to modify.
 * @param {number|undefined} index The index at which to start changing the
 *     array. If not defined, treated as 0.
 * @param {number} howMany How many elements to remove (0 means no removal. A
 *     value below 0 is treated as zero and so is any other non number. Numbers
 *     are floored).
 * @param {...T} var_args Optional, additional elements to insert into the
 *     array.
 * @return {!Array<T>} the removed elements.
 * @template T
 */
goog.array.splice = function(arr, index, howMany, var_args) {
  goog.asserts.assert(arr.length != null);

  return Array.prototype.splice.apply(arr, goog.array.slice(arguments, 1));
};


/**
 * Returns a new array from a segment of an array. This is a generic version of
 * Array slice. This means that it might work on other objects similar to
 * arrays, such as the arguments object.
 *
 * @param {Array<T>|goog.array.ArrayLike} arr The array from
 * which to copy a segment.
 * @param {number} start The index of the first element to copy.
 * @param {number=} opt_end The index after the last element to copy.
 * @return {!Array<T>} A new array containing the specified segment of the
 *     original array.
 * @template T
 */
goog.array.slice = function(arr, start, opt_end) {
  goog.asserts.assert(arr.length != null);

  // passing 1 arg to slice is not the same as passing 2 where the second is
  // null or undefined (in that case the second argument is treated as 0).
  // we could use slice on the arguments object and then use apply instead of
  // testing the length
  if (arguments.length <= 2) {
    return Array.prototype.slice.call(arr, start);
  } else {
    return Array.prototype.slice.call(arr, start, opt_end);
  }
};


/**
 * Removes all duplicates from an array (retaining only the first
 * occurrence of each array element).  This function modifies the
 * array in place and doesn't change the order of the non-duplicate items.
 *
 * For objects, duplicates are identified as having the same unique ID as
 * defined by {@link goog.getUid}.
 *
 * Alternatively you can specify a custom hash function that returns a unique
 * value for each item in the array it should consider unique.
 *
 * Runtime: N,
 * Worstcase space: 2N (no dupes)
 *
 * @param {Array<T>|goog.array.ArrayLike} arr The array from which to remove
 *     duplicates.
 * @param {Array=} opt_rv An optional array in which to return the results,
 *     instead of performing the removal inplace.  If specified, the original
 *     array will remain unchanged.
 * @param {function(T):string=} opt_hashFn An optional function to use to
 *     apply to every item in the array. This function should return a unique
 *     value for each item in the array it should consider unique.
 * @template T
 */
goog.array.removeDuplicates = function(arr, opt_rv, opt_hashFn) {
  var returnArray = opt_rv || arr;
  var defaultHashFn = function(item) {
    // Prefix each type with a single character representing the type to
    // prevent conflicting keys (e.g. true and 'true').
    return goog.isObject(item) ? 'o' + goog.getUid(item) :
                                 (typeof item).charAt(0) + item;
  };
  var hashFn = opt_hashFn || defaultHashFn;

  var seen = {}, cursorInsert = 0, cursorRead = 0;
  while (cursorRead < arr.length) {
    var current = arr[cursorRead++];
    var key = hashFn(current);
    if (!Object.prototype.hasOwnProperty.call(seen, key)) {
      seen[key] = true;
      returnArray[cursorInsert++] = current;
    }
  }
  returnArray.length = cursorInsert;
};


/**
 * Searches the specified array for the specified target using the binary
 * search algorithm.  If no opt_compareFn is specified, elements are compared
 * using <code>goog.array.defaultCompare</code>, which compares the elements
 * using the built in < and > operators.  This will produce the expected
 * behavior for homogeneous arrays of String(s) and Number(s). The array
 * specified <b>must</b> be sorted in ascending order (as defined by the
 * comparison function).  If the array is not sorted, results are undefined.
 * If the array contains multiple instances of the specified target value, any
 * of these instances may be found.
 *
 * Runtime: O(log n)
 *
 * @param {Array<VALUE>|goog.array.ArrayLike} arr The array to be searched.
 * @param {TARGET} target The sought value.
 * @param {function(TARGET, VALUE): number=} opt_compareFn Optional comparison
 *     function by which the array is ordered. Should take 2 arguments to
 *     compare, and return a negative number, zero, or a positive number
 *     depending on whether the first argument is less than, equal to, or
 *     greater than the second.
 * @return {number} Lowest index of the target value if found, otherwise
 *     (-(insertion point) - 1). The insertion point is where the value should
 *     be inserted into arr to preserve the sorted property.  Return value >= 0
 *     iff target is found.
 * @template TARGET, VALUE
 */
goog.array.binarySearch = function(arr, target, opt_compareFn) {
  return goog.array.binarySearch_(
      arr, opt_compareFn || goog.array.defaultCompare, false /* isEvaluator */,
      target);
};


/**
 * Selects an index in the specified array using the binary search algorithm.
 * The evaluator receives an element and determines whether the desired index
 * is before, at, or after it.  The evaluator must be consistent (formally,
 * goog.array.map(goog.array.map(arr, evaluator, opt_obj), goog.math.sign)
 * must be monotonically non-increasing).
 *
 * Runtime: O(log n)
 *
 * @param {Array<VALUE>|goog.array.ArrayLike} arr The array to be searched.
 * @param {function(this:THIS, VALUE, number, ?): number} evaluator
 *     Evaluator function that receives 3 arguments (the element, the index and
 *     the array). Should return a negative number, zero, or a positive number
 *     depending on whether the desired index is before, at, or after the
 *     element passed to it.
 * @param {THIS=} opt_obj The object to be used as the value of 'this'
 *     within evaluator.
 * @return {number} Index of the leftmost element matched by the evaluator, if
 *     such exists; otherwise (-(insertion point) - 1). The insertion point is
 *     the index of the first element for which the evaluator returns negative,
 *     or arr.length if no such element exists. The return value is non-negative
 *     iff a match is found.
 * @template THIS, VALUE
 */
goog.array.binarySelect = function(arr, evaluator, opt_obj) {
  return goog.array.binarySearch_(
      arr, evaluator, true /* isEvaluator */, undefined /* opt_target */,
      opt_obj);
};


/**
 * Implementation of a binary search algorithm which knows how to use both
 * comparison functions and evaluators. If an evaluator is provided, will call
 * the evaluator with the given optional data object, conforming to the
 * interface defined in binarySelect. Otherwise, if a comparison function is
 * provided, will call the comparison function against the given data object.
 *
 * This implementation purposefully does not use goog.bind or goog.partial for
 * performance reasons.
 *
 * Runtime: O(log n)
 *
 * @param {Array<?>|goog.array.ArrayLike} arr The array to be searched.
 * @param {function(?, ?, ?): number | function(?, ?): number} compareFn
 *     Either an evaluator or a comparison function, as defined by binarySearch
 *     and binarySelect above.
 * @param {boolean} isEvaluator Whether the function is an evaluator or a
 *     comparison function.
 * @param {?=} opt_target If the function is a comparison function, then
 *     this is the target to binary search for.
 * @param {Object=} opt_selfObj If the function is an evaluator, this is an
 *     optional this object for the evaluator.
 * @return {number} Lowest index of the target value if found, otherwise
 *     (-(insertion point) - 1). The insertion point is where the value should
 *     be inserted into arr to preserve the sorted property.  Return value >= 0
 *     iff target is found.
 * @private
 */
goog.array.binarySearch_ = function(
    arr, compareFn, isEvaluator, opt_target, opt_selfObj) {
  var left = 0;            // inclusive
  var right = arr.length;  // exclusive
  var found;
  while (left < right) {
    var middle = (left + right) >> 1;
    var compareResult;
    if (isEvaluator) {
      compareResult = compareFn.call(opt_selfObj, arr[middle], middle, arr);
    } else {
      // NOTE(dimvar): To avoid this cast, we'd have to use function overloading
      // for the type of binarySearch_, which the type system can't express yet.
      compareResult = /** @type {function(?, ?): number} */ (compareFn)(
          opt_target, arr[middle]);
    }
    if (compareResult > 0) {
      left = middle + 1;
    } else {
      right = middle;
      // We are looking for the lowest index so we can't return immediately.
      found = !compareResult;
    }
  }
  // left is the index if found, or the insertion point otherwise.
  // ~left is a shorthand for -left - 1.
  return found ? left : ~left;
};


/**
 * Sorts the specified array into ascending order.  If no opt_compareFn is
 * specified, elements are compared using
 * <code>goog.array.defaultCompare</code>, which compares the elements using
 * the built in < and > operators.  This will produce the expected behavior
 * for homogeneous arrays of String(s) and Number(s), unlike the native sort,
 * but will give unpredictable results for heterogenous lists of strings and
 * numbers with different numbers of digits.
 *
 * This sort is not guaranteed to be stable.
 *
 * Runtime: Same as <code>Array.prototype.sort</code>
 *
 * @param {Array<T>} arr The array to be sorted.
 * @param {?function(T,T):number=} opt_compareFn Optional comparison
 *     function by which the
 *     array is to be ordered. Should take 2 arguments to compare, and return a
 *     negative number, zero, or a positive number depending on whether the
 *     first argument is less than, equal to, or greater than the second.
 * @template T
 */
goog.array.sort = function(arr, opt_compareFn) {
  // TODO(arv): Update type annotation since null is not accepted.
  arr.sort(opt_compareFn || goog.array.defaultCompare);
};


/**
 * Sorts the specified array into ascending order in a stable way.  If no
 * opt_compareFn is specified, elements are compared using
 * <code>goog.array.defaultCompare</code>, which compares the elements using
 * the built in < and > operators.  This will produce the expected behavior
 * for homogeneous arrays of String(s) and Number(s).
 *
 * Runtime: Same as <code>Array.prototype.sort</code>, plus an additional
 * O(n) overhead of copying the array twice.
 *
 * @param {Array<T>} arr The array to be sorted.
 * @param {?function(T, T): number=} opt_compareFn Optional comparison function
 *     by which the array is to be ordered. Should take 2 arguments to compare,
 *     and return a negative number, zero, or a positive number depending on
 *     whether the first argument is less than, equal to, or greater than the
 *     second.
 * @template T
 */
goog.array.stableSort = function(arr, opt_compareFn) {
  for (var i = 0; i < arr.length; i++) {
    arr[i] = {index: i, value: arr[i]};
  }
  var valueCompareFn = opt_compareFn || goog.array.defaultCompare;
  function stableCompareFn(obj1, obj2) {
    return valueCompareFn(obj1.value, obj2.value) || obj1.index - obj2.index;
  }
  goog.array.sort(arr, stableCompareFn);
  for (var i = 0; i < arr.length; i++) {
    arr[i] = arr[i].value;
  }
};


/**
 * Sort the specified array into ascending order based on item keys
 * returned by the specified key function.
 * If no opt_compareFn is specified, the keys are compared in ascending order
 * using <code>goog.array.defaultCompare</code>.
 *
 * Runtime: O(S(f(n)), where S is runtime of <code>goog.array.sort</code>
 * and f(n) is runtime of the key function.
 *
 * @param {Array<T>} arr The array to be sorted.
 * @param {function(T): K} keyFn Function taking array element and returning
 *     a key used for sorting this element.
 * @param {?function(K, K): number=} opt_compareFn Optional comparison function
 *     by which the keys are to be ordered. Should take 2 arguments to compare,
 *     and return a negative number, zero, or a positive number depending on
 *     whether the first argument is less than, equal to, or greater than the
 *     second.
 * @template T,K
 */
goog.array.sortByKey = function(arr, keyFn, opt_compareFn) {
  var keyCompareFn = opt_compareFn || goog.array.defaultCompare;
  goog.array.sort(
      arr, function(a, b) { return keyCompareFn(keyFn(a), keyFn(b)); });
};


/**
 * Sorts an array of objects by the specified object key and compare
 * function. If no compare function is provided, the key values are
 * compared in ascending order using <code>goog.array.defaultCompare</code>.
 * This won't work for keys that get renamed by the compiler. So use
 * {'foo': 1, 'bar': 2} rather than {foo: 1, bar: 2}.
 * @param {Array<Object>} arr An array of objects to sort.
 * @param {string} key The object key to sort by.
 * @param {Function=} opt_compareFn The function to use to compare key
 *     values.
 */
goog.array.sortObjectsByKey = function(arr, key, opt_compareFn) {
  goog.array.sortByKey(arr, function(obj) { return obj[key]; }, opt_compareFn);
};


/**
 * Tells if the array is sorted.
 * @param {!Array<T>} arr The array.
 * @param {?function(T,T):number=} opt_compareFn Function to compare the
 *     array elements.
 *     Should take 2 arguments to compare, and return a negative number, zero,
 *     or a positive number depending on whether the first argument is less
 *     than, equal to, or greater than the second.
 * @param {boolean=} opt_strict If true no equal elements are allowed.
 * @return {boolean} Whether the array is sorted.
 * @template T
 */
goog.array.isSorted = function(arr, opt_compareFn, opt_strict) {
  var compare = opt_compareFn || goog.array.defaultCompare;
  for (var i = 1; i < arr.length; i++) {
    var compareResult = compare(arr[i - 1], arr[i]);
    if (compareResult > 0 || compareResult == 0 && opt_strict) {
      return false;
    }
  }
  return true;
};


/**
 * Compares two arrays for equality. Two arrays are considered equal if they
 * have the same length and their corresponding elements are equal according to
 * the comparison function.
 *
 * @param {goog.array.ArrayLike} arr1 The first array to compare.
 * @param {goog.array.ArrayLike} arr2 The second array to compare.
 * @param {Function=} opt_equalsFn Optional comparison function.
 *     Should take 2 arguments to compare, and return true if the arguments
 *     are equal. Defaults to {@link goog.array.defaultCompareEquality} which
 *     compares the elements using the built-in '===' operator.
 * @return {boolean} Whether the two arrays are equal.
 */
goog.array.equals = function(arr1, arr2, opt_equalsFn) {
  if (!goog.isArrayLike(arr1) || !goog.isArrayLike(arr2) ||
      arr1.length != arr2.length) {
    return false;
  }
  var l = arr1.length;
  var equalsFn = opt_equalsFn || goog.array.defaultCompareEquality;
  for (var i = 0; i < l; i++) {
    if (!equalsFn(arr1[i], arr2[i])) {
      return false;
    }
  }
  return true;
};


/**
 * 3-way array compare function.
 * @param {!Array<VALUE>|!goog.array.ArrayLike} arr1 The first array to
 *     compare.
 * @param {!Array<VALUE>|!goog.array.ArrayLike} arr2 The second array to
 *     compare.
 * @param {function(VALUE, VALUE): number=} opt_compareFn Optional comparison
 *     function by which the array is to be ordered. Should take 2 arguments to
 *     compare, and return a negative number, zero, or a positive number
 *     depending on whether the first argument is less than, equal to, or
 *     greater than the second.
 * @return {number} Negative number, zero, or a positive number depending on
 *     whether the first argument is less than, equal to, or greater than the
 *     second.
 * @template VALUE
 */
goog.array.compare3 = function(arr1, arr2, opt_compareFn) {
  var compare = opt_compareFn || goog.array.defaultCompare;
  var l = Math.min(arr1.length, arr2.length);
  for (var i = 0; i < l; i++) {
    var result = compare(arr1[i], arr2[i]);
    if (result != 0) {
      return result;
    }
  }
  return goog.array.defaultCompare(arr1.length, arr2.length);
};


/**
 * Compares its two arguments for order, using the built in < and >
 * operators.
 * @param {VALUE} a The first object to be compared.
 * @param {VALUE} b The second object to be compared.
 * @return {number} A negative number, zero, or a positive number as the first
 *     argument is less than, equal to, or greater than the second,
 *     respectively.
 * @template VALUE
 */
goog.array.defaultCompare = function(a, b) {
  return a > b ? 1 : a < b ? -1 : 0;
};


/**
 * Compares its two arguments for inverse order, using the built in < and >
 * operators.
 * @param {VALUE} a The first object to be compared.
 * @param {VALUE} b The second object to be compared.
 * @return {number} A negative number, zero, or a positive number as the first
 *     argument is greater than, equal to, or less than the second,
 *     respectively.
 * @template VALUE
 */
goog.array.inverseDefaultCompare = function(a, b) {
  return -goog.array.defaultCompare(a, b);
};


/**
 * Compares its two arguments for equality, using the built in === operator.
 * @param {*} a The first object to compare.
 * @param {*} b The second object to compare.
 * @return {boolean} True if the two arguments are equal, false otherwise.
 */
goog.array.defaultCompareEquality = function(a, b) {
  return a === b;
};


/**
 * Inserts a value into a sorted array. The array is not modified if the
 * value is already present.
 * @param {Array<VALUE>|goog.array.ArrayLike} array The array to modify.
 * @param {VALUE} value The object to insert.
 * @param {function(VALUE, VALUE): number=} opt_compareFn Optional comparison
 *     function by which the array is ordered. Should take 2 arguments to
 *     compare, and return a negative number, zero, or a positive number
 *     depending on whether the first argument is less than, equal to, or
 *     greater than the second.
 * @return {boolean} True if an element was inserted.
 * @template VALUE
 */
goog.array.binaryInsert = function(array, value, opt_compareFn) {
  var index = goog.array.binarySearch(array, value, opt_compareFn);
  if (index < 0) {
    goog.array.insertAt(array, value, -(index + 1));
    return true;
  }
  return false;
};


/**
 * Removes a value from a sorted array.
 * @param {!Array<VALUE>|!goog.array.ArrayLike} array The array to modify.
 * @param {VALUE} value The object to remove.
 * @param {function(VALUE, VALUE): number=} opt_compareFn Optional comparison
 *     function by which the array is ordered. Should take 2 arguments to
 *     compare, and return a negative number, zero, or a positive number
 *     depending on whether the first argument is less than, equal to, or
 *     greater than the second.
 * @return {boolean} True if an element was removed.
 * @template VALUE
 */
goog.array.binaryRemove = function(array, value, opt_compareFn) {
  var index = goog.array.binarySearch(array, value, opt_compareFn);
  return (index >= 0) ? goog.array.removeAt(array, index) : false;
};


/**
 * Splits an array into disjoint buckets according to a splitting function.
 * @param {Array<T>} array The array.
 * @param {function(this:S, T,number,Array<T>):?} sorter Function to call for
 *     every element.  This takes 3 arguments (the element, the index and the
 *     array) and must return a valid object key (a string, number, etc), or
 *     undefined, if that object should not be placed in a bucket.
 * @param {S=} opt_obj The object to be used as the value of 'this' within
 *     sorter.
 * @return {!Object} An object, with keys being all of the unique return values
 *     of sorter, and values being arrays containing the items for
 *     which the splitter returned that key.
 * @template T,S
 */
goog.array.bucket = function(array, sorter, opt_obj) {
  var buckets = {};

  for (var i = 0; i < array.length; i++) {
    var value = array[i];
    var key = sorter.call(/** @type {?} */ (opt_obj), value, i, array);
    if (goog.isDef(key)) {
      // Push the value to the right bucket, creating it if necessary.
      var bucket = buckets[key] || (buckets[key] = []);
      bucket.push(value);
    }
  }

  return buckets;
};


/**
 * Creates a new object built from the provided array and the key-generation
 * function.
 * @param {Array<T>|goog.array.ArrayLike} arr Array or array like object over
 *     which to iterate whose elements will be the values in the new object.
 * @param {?function(this:S, T, number, ?) : string} keyFunc The function to
 *     call for every element. This function takes 3 arguments (the element, the
 *     index and the array) and should return a string that will be used as the
 *     key for the element in the new object. If the function returns the same
 *     key for more than one element, the value for that key is
 *     implementation-defined.
 * @param {S=} opt_obj The object to be used as the value of 'this'
 *     within keyFunc.
 * @return {!Object<T>} The new object.
 * @template T,S
 */
goog.array.toObject = function(arr, keyFunc, opt_obj) {
  var ret = {};
  goog.array.forEach(arr, function(element, index) {
    ret[keyFunc.call(/** @type {?} */ (opt_obj), element, index, arr)] =
        element;
  });
  return ret;
};


/**
 * Creates a range of numbers in an arithmetic progression.
 *
 * Range takes 1, 2, or 3 arguments:
 * <pre>
 * range(5) is the same as range(0, 5, 1) and produces [0, 1, 2, 3, 4]
 * range(2, 5) is the same as range(2, 5, 1) and produces [2, 3, 4]
 * range(-2, -5, -1) produces [-2, -3, -4]
 * range(-2, -5, 1) produces [], since stepping by 1 wouldn't ever reach -5.
 * </pre>
 *
 * @param {number} startOrEnd The starting value of the range if an end argument
 *     is provided. Otherwise, the start value is 0, and this is the end value.
 * @param {number=} opt_end The optional end value of the range.
 * @param {number=} opt_step The step size between range values. Defaults to 1
 *     if opt_step is undefined or 0.
 * @return {!Array<number>} An array of numbers for the requested range. May be
 *     an empty array if adding the step would not converge toward the end
 *     value.
 */
goog.array.range = function(startOrEnd, opt_end, opt_step) {
  var array = [];
  var start = 0;
  var end = startOrEnd;
  var step = opt_step || 1;
  if (opt_end !== undefined) {
    start = startOrEnd;
    end = opt_end;
  }

  if (step * (end - start) < 0) {
    // Sign mismatch: start + step will never reach the end value.
    return [];
  }

  if (step > 0) {
    for (var i = start; i < end; i += step) {
      array.push(i);
    }
  } else {
    for (var i = start; i > end; i += step) {
      array.push(i);
    }
  }
  return array;
};


/**
 * Returns an array consisting of the given value repeated N times.
 *
 * @param {VALUE} value The value to repeat.
 * @param {number} n The repeat count.
 * @return {!Array<VALUE>} An array with the repeated value.
 * @template VALUE
 */
goog.array.repeat = function(value, n) {
  var array = [];
  for (var i = 0; i < n; i++) {
    array[i] = value;
  }
  return array;
};


/**
 * Returns an array consisting of every argument with all arrays
 * expanded in-place recursively.
 *
 * @param {...*} var_args The values to flatten.
 * @return {!Array<?>} An array containing the flattened values.
 */
goog.array.flatten = function(var_args) {
  var CHUNK_SIZE = 8192;

  var result = [];
  for (var i = 0; i < arguments.length; i++) {
    var element = arguments[i];
    if (goog.isArray(element)) {
      for (var c = 0; c < element.length; c += CHUNK_SIZE) {
        var chunk = goog.array.slice(element, c, c + CHUNK_SIZE);
        var recurseResult = goog.array.flatten.apply(null, chunk);
        for (var r = 0; r < recurseResult.length; r++) {
          result.push(recurseResult[r]);
        }
      }
    } else {
      result.push(element);
    }
  }
  return result;
};


/**
 * Rotates an array in-place. After calling this method, the element at
 * index i will be the element previously at index (i - n) %
 * array.length, for all values of i between 0 and array.length - 1,
 * inclusive.
 *
 * For example, suppose list comprises [t, a, n, k, s]. After invoking
 * rotate(array, 1) (or rotate(array, -4)), array will comprise [s, t, a, n, k].
 *
 * @param {!Array<T>} array The array to rotate.
 * @param {number} n The amount to rotate.
 * @return {!Array<T>} The array.
 * @template T
 */
goog.array.rotate = function(array, n) {
  goog.asserts.assert(array.length != null);

  if (array.length) {
    n %= array.length;
    if (n > 0) {
      Array.prototype.unshift.apply(array, array.splice(-n, n));
    } else if (n < 0) {
      Array.prototype.push.apply(array, array.splice(0, -n));
    }
  }
  return array;
};


/**
 * Moves one item of an array to a new position keeping the order of the rest
 * of the items. Example use case: keeping a list of JavaScript objects
 * synchronized with the corresponding list of DOM elements after one of the
 * elements has been dragged to a new position.
 * @param {!(Array|Arguments|{length:number})} arr The array to modify.
 * @param {number} fromIndex Index of the item to move between 0 and
 *     {@code arr.length - 1}.
 * @param {number} toIndex Target index between 0 and {@code arr.length - 1}.
 */
goog.array.moveItem = function(arr, fromIndex, toIndex) {
  goog.asserts.assert(fromIndex >= 0 && fromIndex < arr.length);
  goog.asserts.assert(toIndex >= 0 && toIndex < arr.length);
  // Remove 1 item at fromIndex.
  var removedItems = Array.prototype.splice.call(arr, fromIndex, 1);
  // Insert the removed item at toIndex.
  Array.prototype.splice.call(arr, toIndex, 0, removedItems[0]);
  // We don't use goog.array.insertAt and goog.array.removeAt, because they're
  // significantly slower than splice.
};


/**
 * Creates a new array for which the element at position i is an array of the
 * ith element of the provided arrays.  The returned array will only be as long
 * as the shortest array provided; additional values are ignored.  For example,
 * the result of zipping [1, 2] and [3, 4, 5] is [[1,3], [2, 4]].
 *
 * This is similar to the zip() function in Python.  See {@link
 * http://docs.python.org/library/functions.html#zip}
 *
 * @param {...!goog.array.ArrayLike} var_args Arrays to be combined.
 * @return {!Array<!Array<?>>} A new array of arrays created from
 *     provided arrays.
 */
goog.array.zip = function(var_args) {
  if (!arguments.length) {
    return [];
  }
  var result = [];
  var minLen = arguments[0].length;
  for (var i = 1; i < arguments.length; i++) {
    if (arguments[i].length < minLen) {
      minLen = arguments[i].length;
    }
  }
  for (var i = 0; i < minLen; i++) {
    var value = [];
    for (var j = 0; j < arguments.length; j++) {
      value.push(arguments[j][i]);
    }
    result.push(value);
  }
  return result;
};


/**
 * Shuffles the values in the specified array using the Fisher-Yates in-place
 * shuffle (also known as the Knuth Shuffle). By default, calls Math.random()
 * and so resets the state of that random number generator. Similarly, may reset
 * the state of the any other specified random number generator.
 *
 * Runtime: O(n)
 *
 * @param {!Array<?>} arr The array to be shuffled.
 * @param {function():number=} opt_randFn Optional random function to use for
 *     shuffling.
 *     Takes no arguments, and returns a random number on the interval [0, 1).
 *     Defaults to Math.random() using JavaScript's built-in Math library.
 */
goog.array.shuffle = function(arr, opt_randFn) {
  var randFn = opt_randFn || Math.random;

  for (var i = arr.length - 1; i > 0; i--) {
    // Choose a random array index in [0, i] (inclusive with i).
    var j = Math.floor(randFn() * (i + 1));

    var tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }
};


/**
 * Returns a new array of elements from arr, based on the indexes of elements
 * provided by index_arr. For example, the result of index copying
 * ['a', 'b', 'c'] with index_arr [1,0,0,2] is ['b', 'a', 'a', 'c'].
 *
 * @param {!Array<T>} arr The array to get a indexed copy from.
 * @param {!Array<number>} index_arr An array of indexes to get from arr.
 * @return {!Array<T>} A new array of elements from arr in index_arr order.
 * @template T
 */
goog.array.copyByIndex = function(arr, index_arr) {
  var result = [];
  goog.array.forEach(index_arr, function(index) { result.push(arr[index]); });
  return result;
};

// Copyright 2008 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Namespace with crypto related helper functions.
 */

goog.provide('goog.crypt');

goog.require('goog.array');
goog.require('goog.asserts');


/**
 * Turns a string into an array of bytes; a "byte" being a JS number in the
 * range 0-255.
 * @param {string} str String value to arrify.
 * @return {!Array<number>} Array of numbers corresponding to the
 *     UCS character codes of each character in str.
 */
goog.crypt.stringToByteArray = function(str) {
  var output = [], p = 0;
  for (var i = 0; i < str.length; i++) {
    var c = str.charCodeAt(i);
    while (c > 0xff) {
      output[p++] = c & 0xff;
      c >>= 8;
    }
    output[p++] = c;
  }
  return output;
};


/**
 * Turns an array of numbers into the string given by the concatenation of the
 * characters to which the numbers correspond.
 * @param {!Uint8Array|!Array<number>} bytes Array of numbers representing
 *     characters.
 * @return {string} Stringification of the array.
 */
goog.crypt.byteArrayToString = function(bytes) {
  var CHUNK_SIZE = 8192;

  // Special-case the simple case for speed's sake.
  if (bytes.length <= CHUNK_SIZE) {
    return String.fromCharCode.apply(null, bytes);
  }

  // The remaining logic splits conversion by chunks since
  // Function#apply() has a maximum parameter count.
  // See discussion: http://goo.gl/LrWmZ9

  var str = '';
  for (var i = 0; i < bytes.length; i += CHUNK_SIZE) {
    var chunk = goog.array.slice(bytes, i, i + CHUNK_SIZE);
    str += String.fromCharCode.apply(null, chunk);
  }
  return str;
};


/**
 * Turns an array of numbers into the hex string given by the concatenation of
 * the hex values to which the numbers correspond.
 * @param {Uint8Array|Array<number>} array Array of numbers representing
 *     characters.
 * @return {string} Hex string.
 */
goog.crypt.byteArrayToHex = function(array) {
  return goog.array
      .map(
          array,
          function(numByte) {
            var hexByte = numByte.toString(16);
            return hexByte.length > 1 ? hexByte : '0' + hexByte;
          })
      .join('');
};


/**
 * Converts a hex string into an integer array.
 * @param {string} hexString Hex string of 16-bit integers (two characters
 *     per integer).
 * @return {!Array<number>} Array of {0,255} integers for the given string.
 */
goog.crypt.hexToByteArray = function(hexString) {
  goog.asserts.assert(
      hexString.length % 2 == 0, 'Key string length must be multiple of 2');
  var arr = [];
  for (var i = 0; i < hexString.length; i += 2) {
    arr.push(parseInt(hexString.substring(i, i + 2), 16));
  }
  return arr;
};


/**
 * Converts a JS string to a UTF-8 "byte" array.
 * @param {string} str 16-bit unicode string.
 * @return {!Array<number>} UTF-8 byte array.
 */
goog.crypt.stringToUtf8ByteArray = function(str) {
  // TODO(user): Use native implementations if/when available
  var out = [], p = 0;
  for (var i = 0; i < str.length; i++) {
    var c = str.charCodeAt(i);
    if (c < 128) {
      out[p++] = c;
    } else if (c < 2048) {
      out[p++] = (c >> 6) | 192;
      out[p++] = (c & 63) | 128;
    } else if (
        ((c & 0xFC00) == 0xD800) && (i + 1) < str.length &&
        ((str.charCodeAt(i + 1) & 0xFC00) == 0xDC00)) {
      // Surrogate Pair
      c = 0x10000 + ((c & 0x03FF) << 10) + (str.charCodeAt(++i) & 0x03FF);
      out[p++] = (c >> 18) | 240;
      out[p++] = ((c >> 12) & 63) | 128;
      out[p++] = ((c >> 6) & 63) | 128;
      out[p++] = (c & 63) | 128;
    } else {
      out[p++] = (c >> 12) | 224;
      out[p++] = ((c >> 6) & 63) | 128;
      out[p++] = (c & 63) | 128;
    }
  }
  return out;
};


/**
 * Converts a UTF-8 byte array to JavaScript's 16-bit Unicode.
 * @param {Uint8Array|Array<number>} bytes UTF-8 byte array.
 * @return {string} 16-bit Unicode string.
 */
goog.crypt.utf8ByteArrayToString = function(bytes) {
  // TODO(user): Use native implementations if/when available
  var out = [], pos = 0, c = 0;
  while (pos < bytes.length) {
    var c1 = bytes[pos++];
    if (c1 < 128) {
      out[c++] = String.fromCharCode(c1);
    } else if (c1 > 191 && c1 < 224) {
      var c2 = bytes[pos++];
      out[c++] = String.fromCharCode((c1 & 31) << 6 | c2 & 63);
    } else if (c1 > 239 && c1 < 365) {
      // Surrogate Pair
      var c2 = bytes[pos++];
      var c3 = bytes[pos++];
      var c4 = bytes[pos++];
      var u = ((c1 & 7) << 18 | (c2 & 63) << 12 | (c3 & 63) << 6 | c4 & 63) -
          0x10000;
      out[c++] = String.fromCharCode(0xD800 + (u >> 10));
      out[c++] = String.fromCharCode(0xDC00 + (u & 1023));
    } else {
      var c2 = bytes[pos++];
      var c3 = bytes[pos++];
      out[c++] =
          String.fromCharCode((c1 & 15) << 12 | (c2 & 63) << 6 | c3 & 63);
    }
  }
  return out.join('');
};


/**
 * XOR two byte arrays.
 * @param {!Uint8Array|!Int8Array|!Array<number>} bytes1 Byte array 1.
 * @param {!Uint8Array|!Int8Array|!Array<number>} bytes2 Byte array 2.
 * @return {!Array<number>} Resulting XOR of the two byte arrays.
 */
goog.crypt.xorByteArray = function(bytes1, bytes2) {
  goog.asserts.assert(
      bytes1.length == bytes2.length, 'XOR array lengths must match');

  var result = [];
  for (var i = 0; i < bytes1.length; i++) {
    result.push(bytes1[i] ^ bytes2[i]);
  }
  return result;
};

// Copyright 2013 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Utilities used by goog.labs.userAgent tools. These functions
 * should not be used outside of goog.labs.userAgent.*.
 *
 *
 * @author nnaze@google.com (Nathan Naze)
 */

goog.provide('goog.labs.userAgent.util');

goog.require('goog.string');


/**
 * Gets the native userAgent string from navigator if it exists.
 * If navigator or navigator.userAgent string is missing, returns an empty
 * string.
 * @return {string}
 * @private
 */
goog.labs.userAgent.util.getNativeUserAgentString_ = function() {
  var navigator = goog.labs.userAgent.util.getNavigator_();
  if (navigator) {
    var userAgent = navigator.userAgent;
    if (userAgent) {
      return userAgent;
    }
  }
  return '';
};


/**
 * Getter for the native navigator.
 * This is a separate function so it can be stubbed out in testing.
 * @return {Navigator}
 * @private
 */
goog.labs.userAgent.util.getNavigator_ = function() {
  return goog.global.navigator;
};


/**
 * A possible override for applications which wish to not check
 * navigator.userAgent but use a specified value for detection instead.
 * @private {string}
 */
goog.labs.userAgent.util.userAgent_ =
    goog.labs.userAgent.util.getNativeUserAgentString_();


/**
 * Applications may override browser detection on the built in
 * navigator.userAgent object by setting this string. Set to null to use the
 * browser object instead.
 * @param {?string=} opt_userAgent The User-Agent override.
 */
goog.labs.userAgent.util.setUserAgent = function(opt_userAgent) {
  goog.labs.userAgent.util.userAgent_ = opt_userAgent ||
      goog.labs.userAgent.util.getNativeUserAgentString_();
};


/**
 * @return {string} The user agent string.
 */
goog.labs.userAgent.util.getUserAgent = function() {
  return goog.labs.userAgent.util.userAgent_;
};


/**
 * @param {string} str
 * @return {boolean} Whether the user agent contains the given string, ignoring
 *     case.
 */
goog.labs.userAgent.util.matchUserAgent = function(str) {
  var userAgent = goog.labs.userAgent.util.getUserAgent();
  return goog.string.contains(userAgent, str);
};


/**
 * @param {string} str
 * @return {boolean} Whether the user agent contains the given string.
 */
goog.labs.userAgent.util.matchUserAgentIgnoreCase = function(str) {
  var userAgent = goog.labs.userAgent.util.getUserAgent();
  return goog.string.caseInsensitiveContains(userAgent, str);
};


/**
 * Parses the user agent into tuples for each section.
 * @param {string} userAgent
 * @return {!Array<!Array<string>>} Tuples of key, version, and the contents
 *     of the parenthetical.
 */
goog.labs.userAgent.util.extractVersionTuples = function(userAgent) {
  // Matches each section of a user agent string.
  // Example UA:
  // Mozilla/5.0 (iPad; U; CPU OS 3_2_1 like Mac OS X; en-us)
  // AppleWebKit/531.21.10 (KHTML, like Gecko) Mobile/7B405
  // This has three version tuples: Mozilla, AppleWebKit, and Mobile.

  var versionRegExp = new RegExp(
      // Key. Note that a key may have a space.
      // (i.e. 'Mobile Safari' in 'Mobile Safari/5.0')
      '(\\w[\\w ]+)' +

      '/' +                // slash
      '([^\\s]+)' +        // version (i.e. '5.0b')
      '\\s*' +             // whitespace
      '(?:\\((.*?)\\))?',  // parenthetical info. parentheses not matched.
      'g');

  var data = [];
  var match;

  // Iterate and collect the version tuples.  Each iteration will be the
  // next regex match.
  while (match = versionRegExp.exec(userAgent)) {
    data.push([
      match[1],  // key
      match[2],  // value
      // || undefined as this is not undefined in IE7 and IE8
      match[3] || undefined  // info
    ]);
  }

  return data;
};


// Copyright 2013 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Closure user agent platform detection.
 * @see <a href="http://www.useragentstring.com/">User agent strings</a>
 * For more information on browser brand, rendering engine, or device see the
 * other sub-namespaces in goog.labs.userAgent (browser, engine, and device
 * respectively).
 *
 */

goog.provide('goog.labs.userAgent.platform');

goog.require('goog.labs.userAgent.util');
goog.require('goog.string');


/**
 * @return {boolean} Whether the platform is Android.
 */
goog.labs.userAgent.platform.isAndroid = function() {
  return goog.labs.userAgent.util.matchUserAgent('Android');
};


/**
 * @return {boolean} Whether the platform is iPod.
 */
goog.labs.userAgent.platform.isIpod = function() {
  return goog.labs.userAgent.util.matchUserAgent('iPod');
};


/**
 * @return {boolean} Whether the platform is iPhone.
 */
goog.labs.userAgent.platform.isIphone = function() {
  return goog.labs.userAgent.util.matchUserAgent('iPhone') &&
      !goog.labs.userAgent.util.matchUserAgent('iPod') &&
      !goog.labs.userAgent.util.matchUserAgent('iPad');
};


/**
 * @return {boolean} Whether the platform is iPad.
 */
goog.labs.userAgent.platform.isIpad = function() {
  return goog.labs.userAgent.util.matchUserAgent('iPad');
};


/**
 * @return {boolean} Whether the platform is iOS.
 */
goog.labs.userAgent.platform.isIos = function() {
  return goog.labs.userAgent.platform.isIphone() ||
      goog.labs.userAgent.platform.isIpad() ||
      goog.labs.userAgent.platform.isIpod();
};


/**
 * @return {boolean} Whether the platform is Mac.
 */
goog.labs.userAgent.platform.isMacintosh = function() {
  return goog.labs.userAgent.util.matchUserAgent('Macintosh');
};


/**
 * Note: ChromeOS is not considered to be Linux as it does not report itself
 * as Linux in the user agent string.
 * @return {boolean} Whether the platform is Linux.
 */
goog.labs.userAgent.platform.isLinux = function() {
  return goog.labs.userAgent.util.matchUserAgent('Linux');
};


/**
 * @return {boolean} Whether the platform is Windows.
 */
goog.labs.userAgent.platform.isWindows = function() {
  return goog.labs.userAgent.util.matchUserAgent('Windows');
};


/**
 * @return {boolean} Whether the platform is ChromeOS.
 */
goog.labs.userAgent.platform.isChromeOS = function() {
  return goog.labs.userAgent.util.matchUserAgent('CrOS');
};


/**
 * The version of the platform. We only determine the version for Windows,
 * Mac, and Chrome OS. It doesn't make much sense on Linux. For Windows, we only
 * look at the NT version. Non-NT-based versions (e.g. 95, 98, etc.) are given
 * version 0.0.
 *
 * @return {string} The platform version or empty string if version cannot be
 *     determined.
 */
goog.labs.userAgent.platform.getVersion = function() {
  var userAgentString = goog.labs.userAgent.util.getUserAgent();
  var version = '', re;
  if (goog.labs.userAgent.platform.isWindows()) {
    re = /Windows (?:NT|Phone) ([0-9.]+)/;
    var match = re.exec(userAgentString);
    if (match) {
      version = match[1];
    } else {
      version = '0.0';
    }
  } else if (goog.labs.userAgent.platform.isIos()) {
    re = /(?:iPhone|iPod|iPad|CPU)\s+OS\s+(\S+)/;
    var match = re.exec(userAgentString);
    // Report the version as x.y.z and not x_y_z
    version = match && match[1].replace(/_/g, '.');
  } else if (goog.labs.userAgent.platform.isMacintosh()) {
    re = /Mac OS X ([0-9_.]+)/;
    var match = re.exec(userAgentString);
    // Note: some old versions of Camino do not report an OSX version.
    // Default to 10.
    version = match ? match[1].replace(/_/g, '.') : '10';
  } else if (goog.labs.userAgent.platform.isAndroid()) {
    re = /Android\s+([^\);]+)(\)|;)/;
    var match = re.exec(userAgentString);
    version = match && match[1];
  } else if (goog.labs.userAgent.platform.isChromeOS()) {
    re = /(?:CrOS\s+(?:i686|x86_64)\s+([0-9.]+))/;
    var match = re.exec(userAgentString);
    version = match && match[1];
  }
  return version || '';
};


/**
 * @param {string|number} version The version to check.
 * @return {boolean} Whether the browser version is higher or the same as the
 *     given version.
 */
goog.labs.userAgent.platform.isVersionOrHigher = function(version) {
  return goog.string.compareVersions(goog.labs.userAgent.platform.getVersion(),
                                     version) >= 0;
};

// Copyright 2006 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Utilities for manipulating objects/maps/hashes.
 * @author arv@google.com (Erik Arvidsson)
 */

goog.provide('goog.object');


/**
 * Calls a function for each element in an object/map/hash.
 *
 * @param {Object<K,V>} obj The object over which to iterate.
 * @param {function(this:T,V,?,Object<K,V>):?} f The function to call
 *     for every element. This function takes 3 arguments (the value, the
 *     key and the object) and the return value is ignored.
 * @param {T=} opt_obj This is used as the 'this' object within f.
 * @template T,K,V
 */
goog.object.forEach = function(obj, f, opt_obj) {
  for (var key in obj) {
    f.call(/** @type {?} */ (opt_obj), obj[key], key, obj);
  }
};


/**
 * Calls a function for each element in an object/map/hash. If that call returns
 * true, adds the element to a new object.
 *
 * @param {Object<K,V>} obj The object over which to iterate.
 * @param {function(this:T,V,?,Object<K,V>):boolean} f The function to call
 *     for every element. This
 *     function takes 3 arguments (the value, the key and the object)
 *     and should return a boolean. If the return value is true the
 *     element is added to the result object. If it is false the
 *     element is not included.
 * @param {T=} opt_obj This is used as the 'this' object within f.
 * @return {!Object<K,V>} a new object in which only elements that passed the
 *     test are present.
 * @template T,K,V
 */
goog.object.filter = function(obj, f, opt_obj) {
  var res = {};
  for (var key in obj) {
    if (f.call(/** @type {?} */ (opt_obj), obj[key], key, obj)) {
      res[key] = obj[key];
    }
  }
  return res;
};


/**
 * For every element in an object/map/hash calls a function and inserts the
 * result into a new object.
 *
 * @param {Object<K,V>} obj The object over which to iterate.
 * @param {function(this:T,V,?,Object<K,V>):R} f The function to call
 *     for every element. This function
 *     takes 3 arguments (the value, the key and the object)
 *     and should return something. The result will be inserted
 *     into a new object.
 * @param {T=} opt_obj This is used as the 'this' object within f.
 * @return {!Object<K,R>} a new object with the results from f.
 * @template T,K,V,R
 */
goog.object.map = function(obj, f, opt_obj) {
  var res = {};
  for (var key in obj) {
    res[key] = f.call(/** @type {?} */ (opt_obj), obj[key], key, obj);
  }
  return res;
};


/**
 * Calls a function for each element in an object/map/hash. If any
 * call returns true, returns true (without checking the rest). If
 * all calls return false, returns false.
 *
 * @param {Object<K,V>} obj The object to check.
 * @param {function(this:T,V,?,Object<K,V>):boolean} f The function to
 *     call for every element. This function
 *     takes 3 arguments (the value, the key and the object) and should
 *     return a boolean.
 * @param {T=} opt_obj This is used as the 'this' object within f.
 * @return {boolean} true if any element passes the test.
 * @template T,K,V
 */
goog.object.some = function(obj, f, opt_obj) {
  for (var key in obj) {
    if (f.call(/** @type {?} */ (opt_obj), obj[key], key, obj)) {
      return true;
    }
  }
  return false;
};


/**
 * Calls a function for each element in an object/map/hash. If
 * all calls return true, returns true. If any call returns false, returns
 * false at this point and does not continue to check the remaining elements.
 *
 * @param {Object<K,V>} obj The object to check.
 * @param {?function(this:T,V,?,Object<K,V>):boolean} f The function to
 *     call for every element. This function
 *     takes 3 arguments (the value, the key and the object) and should
 *     return a boolean.
 * @param {T=} opt_obj This is used as the 'this' object within f.
 * @return {boolean} false if any element fails the test.
 * @template T,K,V
 */
goog.object.every = function(obj, f, opt_obj) {
  for (var key in obj) {
    if (!f.call(/** @type {?} */ (opt_obj), obj[key], key, obj)) {
      return false;
    }
  }
  return true;
};


/**
 * Returns the number of key-value pairs in the object map.
 *
 * @param {Object} obj The object for which to get the number of key-value
 *     pairs.
 * @return {number} The number of key-value pairs in the object map.
 */
goog.object.getCount = function(obj) {
  // JS1.5 has __count__ but it has been deprecated so it raises a warning...
  // in other words do not use. Also __count__ only includes the fields on the
  // actual object and not in the prototype chain.
  var rv = 0;
  for (var key in obj) {
    rv++;
  }
  return rv;
};


/**
 * Returns one key from the object map, if any exists.
 * For map literals the returned key will be the first one in most of the
 * browsers (a know exception is Konqueror).
 *
 * @param {Object} obj The object to pick a key from.
 * @return {string|undefined} The key or undefined if the object is empty.
 */
goog.object.getAnyKey = function(obj) {
  for (var key in obj) {
    return key;
  }
};


/**
 * Returns one value from the object map, if any exists.
 * For map literals the returned value will be the first one in most of the
 * browsers (a know exception is Konqueror).
 *
 * @param {Object<K,V>} obj The object to pick a value from.
 * @return {V|undefined} The value or undefined if the object is empty.
 * @template K,V
 */
goog.object.getAnyValue = function(obj) {
  for (var key in obj) {
    return obj[key];
  }
};


/**
 * Whether the object/hash/map contains the given object as a value.
 * An alias for goog.object.containsValue(obj, val).
 *
 * @param {Object<K,V>} obj The object in which to look for val.
 * @param {V} val The object for which to check.
 * @return {boolean} true if val is present.
 * @template K,V
 */
goog.object.contains = function(obj, val) {
  return goog.object.containsValue(obj, val);
};


/**
 * Returns the values of the object/map/hash.
 *
 * @param {Object<K,V>} obj The object from which to get the values.
 * @return {!Array<V>} The values in the object/map/hash.
 * @template K,V
 */
goog.object.getValues = function(obj) {
  var res = [];
  var i = 0;
  for (var key in obj) {
    res[i++] = obj[key];
  }
  return res;
};


/**
 * Returns the keys of the object/map/hash.
 *
 * @param {Object} obj The object from which to get the keys.
 * @return {!Array<string>} Array of property keys.
 */
goog.object.getKeys = function(obj) {
  var res = [];
  var i = 0;
  for (var key in obj) {
    res[i++] = key;
  }
  return res;
};


/**
 * Get a value from an object multiple levels deep.  This is useful for
 * pulling values from deeply nested objects, such as JSON responses.
 * Example usage: getValueByKeys(jsonObj, 'foo', 'entries', 3)
 *
 * @param {!Object} obj An object to get the value from.  Can be array-like.
 * @param {...(string|number|!Array<number|string>|!IArrayLike<number|string>)}
 *     var_args A number of keys
 *     (as strings, or numbers, for array-like objects).  Can also be
 *     specified as a single array of keys.
 * @return {*} The resulting value.  If, at any point, the value for a key
 *     is undefined, returns undefined.
 */
goog.object.getValueByKeys = function(obj, var_args) {
  var isArrayLike = goog.isArrayLike(var_args);
  var keys = isArrayLike ? var_args : arguments;

  // Start with the 2nd parameter for the variable parameters syntax.
  for (var i = isArrayLike ? 0 : 1; i < keys.length; i++) {
    obj = obj[keys[i]];
    if (!goog.isDef(obj)) {
      break;
    }
  }

  return obj;
};


/**
 * Whether the object/map/hash contains the given key.
 *
 * @param {Object} obj The object in which to look for key.
 * @param {?} key The key for which to check.
 * @return {boolean} true If the map contains the key.
 */
goog.object.containsKey = function(obj, key) {
  return obj !== null && key in obj;
};


/**
 * Whether the object/map/hash contains the given value. This is O(n).
 *
 * @param {Object<K,V>} obj The object in which to look for val.
 * @param {V} val The value for which to check.
 * @return {boolean} true If the map contains the value.
 * @template K,V
 */
goog.object.containsValue = function(obj, val) {
  for (var key in obj) {
    if (obj[key] == val) {
      return true;
    }
  }
  return false;
};


/**
 * Searches an object for an element that satisfies the given condition and
 * returns its key.
 * @param {Object<K,V>} obj The object to search in.
 * @param {function(this:T,V,string,Object<K,V>):boolean} f The
 *      function to call for every element. Takes 3 arguments (the value,
 *     the key and the object) and should return a boolean.
 * @param {T=} opt_this An optional "this" context for the function.
 * @return {string|undefined} The key of an element for which the function
 *     returns true or undefined if no such element is found.
 * @template T,K,V
 */
goog.object.findKey = function(obj, f, opt_this) {
  for (var key in obj) {
    if (f.call(/** @type {?} */ (opt_this), obj[key], key, obj)) {
      return key;
    }
  }
  return undefined;
};


/**
 * Searches an object for an element that satisfies the given condition and
 * returns its value.
 * @param {Object<K,V>} obj The object to search in.
 * @param {function(this:T,V,string,Object<K,V>):boolean} f The function
 *     to call for every element. Takes 3 arguments (the value, the key
 *     and the object) and should return a boolean.
 * @param {T=} opt_this An optional "this" context for the function.
 * @return {V} The value of an element for which the function returns true or
 *     undefined if no such element is found.
 * @template T,K,V
 */
goog.object.findValue = function(obj, f, opt_this) {
  var key = goog.object.findKey(obj, f, opt_this);
  return key && obj[key];
};


/**
 * Whether the object/map/hash is empty.
 *
 * @param {Object} obj The object to test.
 * @return {boolean} true if obj is empty.
 */
goog.object.isEmpty = function(obj) {
  for (var key in obj) {
    return false;
  }
  return true;
};


/**
 * Removes all key value pairs from the object/map/hash.
 *
 * @param {Object} obj The object to clear.
 */
goog.object.clear = function(obj) {
  for (var i in obj) {
    delete obj[i];
  }
};


/**
 * Removes a key-value pair based on the key.
 *
 * @param {Object} obj The object from which to remove the key.
 * @param {?} key The key to remove.
 * @return {boolean} Whether an element was removed.
 */
goog.object.remove = function(obj, key) {
  var rv;
  if (rv = key in /** @type {!Object} */ (obj)) {
    delete obj[key];
  }
  return rv;
};


/**
 * Adds a key-value pair to the object. Throws an exception if the key is
 * already in use. Use set if you want to change an existing pair.
 *
 * @param {Object<K,V>} obj The object to which to add the key-value pair.
 * @param {string} key The key to add.
 * @param {V} val The value to add.
 * @template K,V
 */
goog.object.add = function(obj, key, val) {
  if (obj !== null && key in obj) {
    throw Error('The object already contains the key "' + key + '"');
  }
  goog.object.set(obj, key, val);
};


/**
 * Returns the value for the given key.
 *
 * @param {Object<K,V>} obj The object from which to get the value.
 * @param {string} key The key for which to get the value.
 * @param {R=} opt_val The value to return if no item is found for the given
 *     key (default is undefined).
 * @return {V|R|undefined} The value for the given key.
 * @template K,V,R
 */
goog.object.get = function(obj, key, opt_val) {
  if (obj !== null && key in obj) {
    return obj[key];
  }
  return opt_val;
};


/**
 * Adds a key-value pair to the object/map/hash.
 *
 * @param {Object<K,V>} obj The object to which to add the key-value pair.
 * @param {string} key The key to add.
 * @param {V} value The value to add.
 * @template K,V
 */
goog.object.set = function(obj, key, value) {
  obj[key] = value;
};


/**
 * Adds a key-value pair to the object/map/hash if it doesn't exist yet.
 *
 * @param {Object<K,V>} obj The object to which to add the key-value pair.
 * @param {string} key The key to add.
 * @param {V} value The value to add if the key wasn't present.
 * @return {V} The value of the entry at the end of the function.
 * @template K,V
 */
goog.object.setIfUndefined = function(obj, key, value) {
  return key in /** @type {!Object} */ (obj) ? obj[key] : (obj[key] = value);
};


/**
 * Sets a key and value to an object if the key is not set. The value will be
 * the return value of the given function. If the key already exists, the
 * object will not be changed and the function will not be called (the function
 * will be lazily evaluated -- only called if necessary).
 *
 * This function is particularly useful for use with a map used a as a cache.
 *
 * @param {!Object<K,V>} obj The object to which to add the key-value pair.
 * @param {string} key The key to add.
 * @param {function():V} f The value to add if the key wasn't present.
 * @return {V} The value of the entry at the end of the function.
 * @template K,V
 */
goog.object.setWithReturnValueIfNotSet = function(obj, key, f) {
  if (key in obj) {
    return obj[key];
  }

  var val = f();
  obj[key] = val;
  return val;
};


/**
 * Compares two objects for equality using === on the values.
 *
 * @param {!Object<K,V>} a
 * @param {!Object<K,V>} b
 * @return {boolean}
 * @template K,V
 */
goog.object.equals = function(a, b) {
  for (var k in a) {
    if (!(k in b) || a[k] !== b[k]) {
      return false;
    }
  }
  for (var k in b) {
    if (!(k in a)) {
      return false;
    }
  }
  return true;
};


/**
 * Does a flat clone of the object.
 *
 * @param {Object<K,V>} obj Object to clone.
 * @return {!Object<K,V>} Clone of the input object.
 * @template K,V
 */
goog.object.clone = function(obj) {
  // We cannot use the prototype trick because a lot of methods depend on where
  // the actual key is set.

  var res = {};
  for (var key in obj) {
    res[key] = obj[key];
  }
  return res;
  // We could also use goog.mixin but I wanted this to be independent from that.
};


/**
 * Clones a value. The input may be an Object, Array, or basic type. Objects and
 * arrays will be cloned recursively.
 *
 * WARNINGS:
 * <code>goog.object.unsafeClone</code> does not detect reference loops. Objects
 * that refer to themselves will cause infinite recursion.
 *
 * <code>goog.object.unsafeClone</code> is unaware of unique identifiers, and
 * copies UIDs created by <code>getUid</code> into cloned results.
 *
 * @param {*} obj The value to clone.
 * @return {*} A clone of the input value.
 */
goog.object.unsafeClone = function(obj) {
  var type = goog.typeOf(obj);
  if (type == 'object' || type == 'array') {
    if (goog.isFunction(obj.clone)) {
      return obj.clone();
    }
    var clone = type == 'array' ? [] : {};
    for (var key in obj) {
      clone[key] = goog.object.unsafeClone(obj[key]);
    }
    return clone;
  }

  return obj;
};


/**
 * Returns a new object in which all the keys and values are interchanged
 * (keys become values and values become keys). If multiple keys map to the
 * same value, the chosen transposed value is implementation-dependent.
 *
 * @param {Object} obj The object to transpose.
 * @return {!Object} The transposed object.
 */
goog.object.transpose = function(obj) {
  var transposed = {};
  for (var key in obj) {
    transposed[obj[key]] = key;
  }
  return transposed;
};


/**
 * The names of the fields that are defined on Object.prototype.
 * @type {Array<string>}
 * @private
 */
goog.object.PROTOTYPE_FIELDS_ = [
  'constructor', 'hasOwnProperty', 'isPrototypeOf', 'propertyIsEnumerable',
  'toLocaleString', 'toString', 'valueOf'
];


/**
 * Extends an object with another object.
 * This operates 'in-place'; it does not create a new Object.
 *
 * Example:
 * var o = {};
 * goog.object.extend(o, {a: 0, b: 1});
 * o; // {a: 0, b: 1}
 * goog.object.extend(o, {b: 2, c: 3});
 * o; // {a: 0, b: 2, c: 3}
 *
 * @param {Object} target The object to modify. Existing properties will be
 *     overwritten if they are also present in one of the objects in
 *     {@code var_args}.
 * @param {...Object} var_args The objects from which values will be copied.
 */
goog.object.extend = function(target, var_args) {
  var key, source;
  for (var i = 1; i < arguments.length; i++) {
    source = arguments[i];
    for (key in source) {
      target[key] = source[key];
    }

    // For IE the for-in-loop does not contain any properties that are not
    // enumerable on the prototype object (for example isPrototypeOf from
    // Object.prototype) and it will also not include 'replace' on objects that
    // extend String and change 'replace' (not that it is common for anyone to
    // extend anything except Object).

    for (var j = 0; j < goog.object.PROTOTYPE_FIELDS_.length; j++) {
      key = goog.object.PROTOTYPE_FIELDS_[j];
      if (Object.prototype.hasOwnProperty.call(source, key)) {
        target[key] = source[key];
      }
    }
  }
};


/**
 * Creates a new object built from the key-value pairs provided as arguments.
 * @param {...*} var_args If only one argument is provided and it is an array
 *     then this is used as the arguments,  otherwise even arguments are used as
 *     the property names and odd arguments are used as the property values.
 * @return {!Object} The new object.
 * @throws {Error} If there are uneven number of arguments or there is only one
 *     non array argument.
 */
goog.object.create = function(var_args) {
  var argLength = arguments.length;
  if (argLength == 1 && goog.isArray(arguments[0])) {
    return goog.object.create.apply(null, arguments[0]);
  }

  if (argLength % 2) {
    throw Error('Uneven number of arguments');
  }

  var rv = {};
  for (var i = 0; i < argLength; i += 2) {
    rv[arguments[i]] = arguments[i + 1];
  }
  return rv;
};


/**
 * Creates a new object where the property names come from the arguments but
 * the value is always set to true
 * @param {...*} var_args If only one argument is provided and it is an array
 *     then this is used as the arguments,  otherwise the arguments are used
 *     as the property names.
 * @return {!Object} The new object.
 */
goog.object.createSet = function(var_args) {
  var argLength = arguments.length;
  if (argLength == 1 && goog.isArray(arguments[0])) {
    return goog.object.createSet.apply(null, arguments[0]);
  }

  var rv = {};
  for (var i = 0; i < argLength; i++) {
    rv[arguments[i]] = true;
  }
  return rv;
};


/**
 * Creates an immutable view of the underlying object, if the browser
 * supports immutable objects.
 *
 * In default mode, writes to this view will fail silently. In strict mode,
 * they will throw an error.
 *
 * @param {!Object<K,V>} obj An object.
 * @return {!Object<K,V>} An immutable view of that object, or the
 *     original object if this browser does not support immutables.
 * @template K,V
 */
goog.object.createImmutableView = function(obj) {
  var result = obj;
  if (Object.isFrozen && !Object.isFrozen(obj)) {
    result = Object.create(obj);
    Object.freeze(result);
  }
  return result;
};


/**
 * @param {!Object} obj An object.
 * @return {boolean} Whether this is an immutable view of the object.
 */
goog.object.isImmutableView = function(obj) {
  return !!Object.isFrozen && Object.isFrozen(obj);
};

// Copyright 2013 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Closure user agent detection (Browser).
 * @see <a href="http://www.useragentstring.com/">User agent strings</a>
 * For more information on rendering engine, platform, or device see the other
 * sub-namespaces in goog.labs.userAgent, goog.labs.userAgent.platform,
 * goog.labs.userAgent.device respectively.)
 *
 * @author martone@google.com (Andy Martone)
 */

goog.provide('goog.labs.userAgent.browser');

goog.require('goog.array');
goog.require('goog.labs.userAgent.util');
goog.require('goog.object');
goog.require('goog.string');


// TODO(nnaze): Refactor to remove excessive exclusion logic in matching
// functions.


/**
 * @return {boolean} Whether the user's browser is Opera.
 * @private
 */
goog.labs.userAgent.browser.matchOpera_ = function() {
  return goog.labs.userAgent.util.matchUserAgent('Opera') ||
      goog.labs.userAgent.util.matchUserAgent('OPR');
};


/**
 * @return {boolean} Whether the user's browser is IE.
 * @private
 */
goog.labs.userAgent.browser.matchIE_ = function() {
  return goog.labs.userAgent.util.matchUserAgent('Trident') ||
      goog.labs.userAgent.util.matchUserAgent('MSIE');
};


/**
 * @return {boolean} Whether the user's browser is Edge.
 * @private
 */
goog.labs.userAgent.browser.matchEdge_ = function() {
  return goog.labs.userAgent.util.matchUserAgent('Edge');
};


/**
 * @return {boolean} Whether the user's browser is Firefox.
 * @private
 */
goog.labs.userAgent.browser.matchFirefox_ = function() {
  return goog.labs.userAgent.util.matchUserAgent('Firefox');
};


/**
 * @return {boolean} Whether the user's browser is Safari.
 * @private
 */
goog.labs.userAgent.browser.matchSafari_ = function() {
  return goog.labs.userAgent.util.matchUserAgent('Safari') &&
      !(goog.labs.userAgent.browser.matchChrome_() ||
        goog.labs.userAgent.browser.matchCoast_() ||
        goog.labs.userAgent.browser.matchOpera_() ||
        goog.labs.userAgent.browser.matchEdge_() ||
        goog.labs.userAgent.browser.isSilk() ||
        goog.labs.userAgent.util.matchUserAgent('Android'));
};


/**
 * @return {boolean} Whether the user's browser is Coast (Opera's Webkit-based
 *     iOS browser).
 * @private
 */
goog.labs.userAgent.browser.matchCoast_ = function() {
  return goog.labs.userAgent.util.matchUserAgent('Coast');
};


/**
 * @return {boolean} Whether the user's browser is iOS Webview.
 * @private
 */
goog.labs.userAgent.browser.matchIosWebview_ = function() {
  // iOS Webview does not show up as Chrome or Safari. Also check for Opera's
  // WebKit-based iOS browser, Coast.
  return (goog.labs.userAgent.util.matchUserAgent('iPad') ||
          goog.labs.userAgent.util.matchUserAgent('iPhone')) &&
      !goog.labs.userAgent.browser.matchSafari_() &&
      !goog.labs.userAgent.browser.matchChrome_() &&
      !goog.labs.userAgent.browser.matchCoast_() &&
      goog.labs.userAgent.util.matchUserAgent('AppleWebKit');
};


/**
 * @return {boolean} Whether the user's browser is Chrome.
 * @private
 */
goog.labs.userAgent.browser.matchChrome_ = function() {
  return (goog.labs.userAgent.util.matchUserAgent('Chrome') ||
      goog.labs.userAgent.util.matchUserAgent('CriOS')) &&
      !goog.labs.userAgent.browser.matchOpera_() &&
      !goog.labs.userAgent.browser.matchEdge_();
};


/**
 * @return {boolean} Whether the user's browser is the Android browser.
 * @private
 */
goog.labs.userAgent.browser.matchAndroidBrowser_ = function() {
  // Android can appear in the user agent string for Chrome on Android.
  // This is not the Android standalone browser if it does.
  return goog.labs.userAgent.util.matchUserAgent('Android') &&
      !(goog.labs.userAgent.browser.isChrome() ||
        goog.labs.userAgent.browser.isFirefox() ||
        goog.labs.userAgent.browser.isOpera() ||
        goog.labs.userAgent.browser.isSilk());
};


/**
 * @return {boolean} Whether the user's browser is Opera.
 */
goog.labs.userAgent.browser.isOpera = goog.labs.userAgent.browser.matchOpera_;


/**
 * @return {boolean} Whether the user's browser is IE.
 */
goog.labs.userAgent.browser.isIE = goog.labs.userAgent.browser.matchIE_;


/**
 * @return {boolean} Whether the user's browser is Edge.
 */
goog.labs.userAgent.browser.isEdge = goog.labs.userAgent.browser.matchEdge_;


/**
 * @return {boolean} Whether the user's browser is Firefox.
 */
goog.labs.userAgent.browser.isFirefox =
    goog.labs.userAgent.browser.matchFirefox_;


/**
 * @return {boolean} Whether the user's browser is Safari.
 */
goog.labs.userAgent.browser.isSafari =
    goog.labs.userAgent.browser.matchSafari_;


/**
 * @return {boolean} Whether the user's browser is Coast (Opera's Webkit-based
 *     iOS browser).
 */
goog.labs.userAgent.browser.isCoast =
    goog.labs.userAgent.browser.matchCoast_;


/**
 * @return {boolean} Whether the user's browser is iOS Webview.
 */
goog.labs.userAgent.browser.isIosWebview =
    goog.labs.userAgent.browser.matchIosWebview_;


/**
 * @return {boolean} Whether the user's browser is Chrome.
 */
goog.labs.userAgent.browser.isChrome =
    goog.labs.userAgent.browser.matchChrome_;


/**
 * @return {boolean} Whether the user's browser is the Android browser.
 */
goog.labs.userAgent.browser.isAndroidBrowser =
    goog.labs.userAgent.browser.matchAndroidBrowser_;


/**
 * For more information, see:
 * http://docs.aws.amazon.com/silk/latest/developerguide/user-agent.html
 * @return {boolean} Whether the user's browser is Silk.
 */
goog.labs.userAgent.browser.isSilk = function() {
  return goog.labs.userAgent.util.matchUserAgent('Silk');
};


/**
 * @return {string} The browser version or empty string if version cannot be
 *     determined. Note that for Internet Explorer, this returns the version of
 *     the browser, not the version of the rendering engine. (IE 8 in
 *     compatibility mode will return 8.0 rather than 7.0. To determine the
 *     rendering engine version, look at document.documentMode instead. See
 *     http://msdn.microsoft.com/en-us/library/cc196988(v=vs.85).aspx for more
 *     details.)
 */
goog.labs.userAgent.browser.getVersion = function() {
  var userAgentString = goog.labs.userAgent.util.getUserAgent();
  // Special case IE since IE's version is inside the parenthesis and
  // without the '/'.
  if (goog.labs.userAgent.browser.isIE()) {
    return goog.labs.userAgent.browser.getIEVersion_(userAgentString);
  }

  var versionTuples = goog.labs.userAgent.util.extractVersionTuples(
      userAgentString);

  // Construct a map for easy lookup.
  var versionMap = {};
  goog.array.forEach(versionTuples, function(tuple) {
    // Note that the tuple is of length three, but we only care about the
    // first two.
    var key = tuple[0];
    var value = tuple[1];
    versionMap[key] = value;
  });

  var versionMapHasKey = goog.partial(goog.object.containsKey, versionMap);

  // Gives the value with the first key it finds, otherwise empty string.
  function lookUpValueWithKeys(keys) {
    var key = goog.array.find(keys, versionMapHasKey);
    return versionMap[key] || '';
  }

  // Check Opera before Chrome since Opera 15+ has "Chrome" in the string.
  // See
  // http://my.opera.com/ODIN/blog/2013/07/15/opera-user-agent-strings-opera-15-and-beyond
  if (goog.labs.userAgent.browser.isOpera()) {
    // Opera 10 has Version/10.0 but Opera/9.8, so look for "Version" first.
    // Opera uses 'OPR' for more recent UAs.
    return lookUpValueWithKeys(['Version', 'Opera', 'OPR']);
  }

  // Check Edge before Chrome since it has Chrome in the string.
  if (goog.labs.userAgent.browser.isEdge()) {
    return lookUpValueWithKeys(['Edge']);
  }

  if (goog.labs.userAgent.browser.isChrome()) {
    return lookUpValueWithKeys(['Chrome', 'CriOS']);
  }

  // Usually products browser versions are in the third tuple after "Mozilla"
  // and the engine.
  var tuple = versionTuples[2];
  return tuple && tuple[1] || '';
};


/**
 * @param {string|number} version The version to check.
 * @return {boolean} Whether the browser version is higher or the same as the
 *     given version.
 */
goog.labs.userAgent.browser.isVersionOrHigher = function(version) {
  return goog.string.compareVersions(goog.labs.userAgent.browser.getVersion(),
                                     version) >= 0;
};


/**
 * Determines IE version. More information:
 * http://msdn.microsoft.com/en-us/library/ie/bg182625(v=vs.85).aspx#uaString
 * http://msdn.microsoft.com/en-us/library/hh869301(v=vs.85).aspx
 * http://blogs.msdn.com/b/ie/archive/2010/03/23/introducing-ie9-s-user-agent-string.aspx
 * http://blogs.msdn.com/b/ie/archive/2009/01/09/the-internet-explorer-8-user-agent-string-updated-edition.aspx
 *
 * @param {string} userAgent the User-Agent.
 * @return {string}
 * @private
 */
goog.labs.userAgent.browser.getIEVersion_ = function(userAgent) {
  // IE11 may identify itself as MSIE 9.0 or MSIE 10.0 due to an IE 11 upgrade
  // bug. Example UA:
  // Mozilla/5.0 (MSIE 9.0; Windows NT 6.1; WOW64; Trident/7.0; rv:11.0)
  // like Gecko.
  // See http://www.whatismybrowser.com/developers/unknown-user-agent-fragments.
  var rv = /rv: *([\d\.]*)/.exec(userAgent);
  if (rv && rv[1]) {
    return rv[1];
  }

  var version = '';
  var msie = /MSIE +([\d\.]+)/.exec(userAgent);
  if (msie && msie[1]) {
    // IE in compatibility mode usually identifies itself as MSIE 7.0; in this
    // case, use the Trident version to determine the version of IE. For more
    // details, see the links above.
    var tridentVersion = /Trident\/(\d.\d)/.exec(userAgent);
    if (msie[1] == '7.0') {
      if (tridentVersion && tridentVersion[1]) {
        switch (tridentVersion[1]) {
          case '4.0':
            version = '8.0';
            break;
          case '5.0':
            version = '9.0';
            break;
          case '6.0':
            version = '10.0';
            break;
          case '7.0':
            version = '11.0';
            break;
        }
      } else {
        version = '7.0';
      }
    } else {
      version = msie[1];
    }
  }
  return version;
};

// Copyright 2013 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Closure user agent detection.
 * @see http://en.wikipedia.org/wiki/User_agent
 * For more information on browser brand, platform, or device see the other
 * sub-namespaces in goog.labs.userAgent (browser, platform, and device).
 *
 */

goog.provide('goog.labs.userAgent.engine');

goog.require('goog.array');
goog.require('goog.labs.userAgent.util');
goog.require('goog.string');


/**
 * @return {boolean} Whether the rendering engine is Presto.
 */
goog.labs.userAgent.engine.isPresto = function() {
  return goog.labs.userAgent.util.matchUserAgent('Presto');
};


/**
 * @return {boolean} Whether the rendering engine is Trident.
 */
goog.labs.userAgent.engine.isTrident = function() {
  // IE only started including the Trident token in IE8.
  return goog.labs.userAgent.util.matchUserAgent('Trident') ||
      goog.labs.userAgent.util.matchUserAgent('MSIE');
};


/**
 * @return {boolean} Whether the rendering engine is Edge.
 */
goog.labs.userAgent.engine.isEdge = function() {
  return goog.labs.userAgent.util.matchUserAgent('Edge');
};


/**
 * @return {boolean} Whether the rendering engine is WebKit.
 */
goog.labs.userAgent.engine.isWebKit = function() {
  return goog.labs.userAgent.util.matchUserAgentIgnoreCase('WebKit') &&
      !goog.labs.userAgent.engine.isEdge();
};


/**
 * @return {boolean} Whether the rendering engine is Gecko.
 */
goog.labs.userAgent.engine.isGecko = function() {
  return goog.labs.userAgent.util.matchUserAgent('Gecko') &&
      !goog.labs.userAgent.engine.isWebKit() &&
      !goog.labs.userAgent.engine.isTrident() &&
      !goog.labs.userAgent.engine.isEdge();
};


/**
 * @return {string} The rendering engine's version or empty string if version
 *     can't be determined.
 */
goog.labs.userAgent.engine.getVersion = function() {
  var userAgentString = goog.labs.userAgent.util.getUserAgent();
  if (userAgentString) {
    var tuples = goog.labs.userAgent.util.extractVersionTuples(
        userAgentString);

    var engineTuple = goog.labs.userAgent.engine.getEngineTuple_(tuples);
    if (engineTuple) {
      // In Gecko, the version string is either in the browser info or the
      // Firefox version.  See Gecko user agent string reference:
      // http://goo.gl/mULqa
      if (engineTuple[0] == 'Gecko') {
        return goog.labs.userAgent.engine.getVersionForKey_(
            tuples, 'Firefox');
      }

      return engineTuple[1];
    }

    // MSIE has only one version identifier, and the Trident version is
    // specified in the parenthetical. IE Edge is covered in the engine tuple
    // detection.
    var browserTuple = tuples[0];
    var info;
    if (browserTuple && (info = browserTuple[2])) {
      var match = /Trident\/([^\s;]+)/.exec(info);
      if (match) {
        return match[1];
      }
    }
  }
  return '';
};


/**
 * @param {!Array<!Array<string>>} tuples Extracted version tuples.
 * @return {!Array<string>|undefined} The engine tuple or undefined if not
 *     found.
 * @private
 */
goog.labs.userAgent.engine.getEngineTuple_ = function(tuples) {
  if (!goog.labs.userAgent.engine.isEdge()) {
    return tuples[1];
  }
  for (var i = 0; i < tuples.length; i++) {
    var tuple = tuples[i];
    if (tuple[0] == 'Edge') {
      return tuple;
    }
  }
};


/**
 * @param {string|number} version The version to check.
 * @return {boolean} Whether the rendering engine version is higher or the same
 *     as the given version.
 */
goog.labs.userAgent.engine.isVersionOrHigher = function(version) {
  return goog.string.compareVersions(goog.labs.userAgent.engine.getVersion(),
                                     version) >= 0;
};


/**
 * @param {!Array<!Array<string>>} tuples Version tuples.
 * @param {string} key The key to look for.
 * @return {string} The version string of the given key, if present.
 *     Otherwise, the empty string.
 * @private
 */
goog.labs.userAgent.engine.getVersionForKey_ = function(tuples, key) {
  // TODO(nnaze): Move to util if useful elsewhere.

  var pair = goog.array.find(tuples, function(pair) {
    return key == pair[0];
  });

  return pair && pair[1] || '';
};

// Copyright 2006 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Rendering engine detection.
 * @see <a href="http://www.useragentstring.com/">User agent strings</a>
 * For information on the browser brand (such as Safari versus Chrome), see
 * goog.userAgent.product.
 * @author arv@google.com (Erik Arvidsson)
 * @see ../demos/useragent.html
 */

goog.provide('goog.userAgent');

goog.require('goog.labs.userAgent.browser');
goog.require('goog.labs.userAgent.engine');
goog.require('goog.labs.userAgent.platform');
goog.require('goog.labs.userAgent.util');
goog.require('goog.string');


/**
 * @define {boolean} Whether we know at compile-time that the browser is IE.
 */
goog.define('goog.userAgent.ASSUME_IE', false);


/**
 * @define {boolean} Whether we know at compile-time that the browser is EDGE.
 */
goog.define('goog.userAgent.ASSUME_EDGE', false);


/**
 * @define {boolean} Whether we know at compile-time that the browser is GECKO.
 */
goog.define('goog.userAgent.ASSUME_GECKO', false);


/**
 * @define {boolean} Whether we know at compile-time that the browser is WEBKIT.
 */
goog.define('goog.userAgent.ASSUME_WEBKIT', false);


/**
 * @define {boolean} Whether we know at compile-time that the browser is a
 *     mobile device running WebKit e.g. iPhone or Android.
 */
goog.define('goog.userAgent.ASSUME_MOBILE_WEBKIT', false);


/**
 * @define {boolean} Whether we know at compile-time that the browser is OPERA.
 */
goog.define('goog.userAgent.ASSUME_OPERA', false);


/**
 * @define {boolean} Whether the
 *     {@code goog.userAgent.isVersionOrHigher}
 *     function will return true for any version.
 */
goog.define('goog.userAgent.ASSUME_ANY_VERSION', false);


/**
 * Whether we know the browser engine at compile-time.
 * @type {boolean}
 * @private
 */
goog.userAgent.BROWSER_KNOWN_ = goog.userAgent.ASSUME_IE ||
    goog.userAgent.ASSUME_EDGE || goog.userAgent.ASSUME_GECKO ||
    goog.userAgent.ASSUME_MOBILE_WEBKIT || goog.userAgent.ASSUME_WEBKIT ||
    goog.userAgent.ASSUME_OPERA;


/**
 * Returns the userAgent string for the current browser.
 *
 * @return {string} The userAgent string.
 */
goog.userAgent.getUserAgentString = function() {
  return goog.labs.userAgent.util.getUserAgent();
};


/**
 * TODO(nnaze): Change type to "Navigator" and update compilation targets.
 * @return {Object} The native navigator object.
 */
goog.userAgent.getNavigator = function() {
  // Need a local navigator reference instead of using the global one,
  // to avoid the rare case where they reference different objects.
  // (in a WorkerPool, for example).
  return goog.global['navigator'] || null;
};


/**
 * Whether the user agent is Opera.
 * @type {boolean}
 */
goog.userAgent.OPERA = goog.userAgent.BROWSER_KNOWN_ ?
    goog.userAgent.ASSUME_OPERA :
    goog.labs.userAgent.browser.isOpera();


/**
 * Whether the user agent is Internet Explorer.
 * @type {boolean}
 */
goog.userAgent.IE = goog.userAgent.BROWSER_KNOWN_ ?
    goog.userAgent.ASSUME_IE :
    goog.labs.userAgent.browser.isIE();


/**
 * Whether the user agent is Microsoft Edge.
 * @type {boolean}
 */
goog.userAgent.EDGE = goog.userAgent.BROWSER_KNOWN_ ?
    goog.userAgent.ASSUME_EDGE :
    goog.labs.userAgent.engine.isEdge();


/**
 * Whether the user agent is MS Internet Explorer or MS Edge.
 * @type {boolean}
 */
goog.userAgent.EDGE_OR_IE = goog.userAgent.EDGE || goog.userAgent.IE;


/**
 * Whether the user agent is Gecko. Gecko is the rendering engine used by
 * Mozilla, Firefox, and others.
 * @type {boolean}
 */
goog.userAgent.GECKO = goog.userAgent.BROWSER_KNOWN_ ?
    goog.userAgent.ASSUME_GECKO :
    goog.labs.userAgent.engine.isGecko();


/**
 * Whether the user agent is WebKit. WebKit is the rendering engine that
 * Safari, Android and others use.
 * @type {boolean}
 */
goog.userAgent.WEBKIT = goog.userAgent.BROWSER_KNOWN_ ?
    goog.userAgent.ASSUME_WEBKIT || goog.userAgent.ASSUME_MOBILE_WEBKIT :
    goog.labs.userAgent.engine.isWebKit();


/**
 * Whether the user agent is running on a mobile device.
 *
 * This is a separate function so that the logic can be tested.
 *
 * TODO(nnaze): Investigate swapping in goog.labs.userAgent.device.isMobile().
 *
 * @return {boolean} Whether the user agent is running on a mobile device.
 * @private
 */
goog.userAgent.isMobile_ = function() {
  return goog.userAgent.WEBKIT &&
      goog.labs.userAgent.util.matchUserAgent('Mobile');
};


/**
 * Whether the user agent is running on a mobile device.
 *
 * TODO(nnaze): Consider deprecating MOBILE when labs.userAgent
 *   is promoted as the gecko/webkit logic is likely inaccurate.
 *
 * @type {boolean}
 */
goog.userAgent.MOBILE =
    goog.userAgent.ASSUME_MOBILE_WEBKIT || goog.userAgent.isMobile_();


/**
 * Used while transitioning code to use WEBKIT instead.
 * @type {boolean}
 * @deprecated Use {@link goog.userAgent.product.SAFARI} instead.
 * TODO(nicksantos): Delete this from goog.userAgent.
 */
goog.userAgent.SAFARI = goog.userAgent.WEBKIT;


/**
 * @return {string} the platform (operating system) the user agent is running
 *     on. Default to empty string because navigator.platform may not be defined
 *     (on Rhino, for example).
 * @private
 */
goog.userAgent.determinePlatform_ = function() {
  var navigator = goog.userAgent.getNavigator();
  return navigator && navigator.platform || '';
};


/**
 * The platform (operating system) the user agent is running on. Default to
 * empty string because navigator.platform may not be defined (on Rhino, for
 * example).
 * @type {string}
 */
goog.userAgent.PLATFORM = goog.userAgent.determinePlatform_();


/**
 * @define {boolean} Whether the user agent is running on a Macintosh operating
 *     system.
 */
goog.define('goog.userAgent.ASSUME_MAC', false);


/**
 * @define {boolean} Whether the user agent is running on a Windows operating
 *     system.
 */
goog.define('goog.userAgent.ASSUME_WINDOWS', false);


/**
 * @define {boolean} Whether the user agent is running on a Linux operating
 *     system.
 */
goog.define('goog.userAgent.ASSUME_LINUX', false);


/**
 * @define {boolean} Whether the user agent is running on a X11 windowing
 *     system.
 */
goog.define('goog.userAgent.ASSUME_X11', false);


/**
 * @define {boolean} Whether the user agent is running on Android.
 */
goog.define('goog.userAgent.ASSUME_ANDROID', false);


/**
 * @define {boolean} Whether the user agent is running on an iPhone.
 */
goog.define('goog.userAgent.ASSUME_IPHONE', false);


/**
 * @define {boolean} Whether the user agent is running on an iPad.
 */
goog.define('goog.userAgent.ASSUME_IPAD', false);


/**
 * @type {boolean}
 * @private
 */
goog.userAgent.PLATFORM_KNOWN_ = goog.userAgent.ASSUME_MAC ||
    goog.userAgent.ASSUME_WINDOWS || goog.userAgent.ASSUME_LINUX ||
    goog.userAgent.ASSUME_X11 || goog.userAgent.ASSUME_ANDROID ||
    goog.userAgent.ASSUME_IPHONE || goog.userAgent.ASSUME_IPAD;


/**
 * Whether the user agent is running on a Macintosh operating system.
 * @type {boolean}
 */
goog.userAgent.MAC = goog.userAgent.PLATFORM_KNOWN_ ?
    goog.userAgent.ASSUME_MAC :
    goog.labs.userAgent.platform.isMacintosh();


/**
 * Whether the user agent is running on a Windows operating system.
 * @type {boolean}
 */
goog.userAgent.WINDOWS = goog.userAgent.PLATFORM_KNOWN_ ?
    goog.userAgent.ASSUME_WINDOWS :
    goog.labs.userAgent.platform.isWindows();


/**
 * Whether the user agent is Linux per the legacy behavior of
 * goog.userAgent.LINUX, which considered ChromeOS to also be
 * Linux.
 * @return {boolean}
 * @private
 */
goog.userAgent.isLegacyLinux_ = function() {
  return goog.labs.userAgent.platform.isLinux() ||
      goog.labs.userAgent.platform.isChromeOS();
};


/**
 * Whether the user agent is running on a Linux operating system.
 *
 * Note that goog.userAgent.LINUX considers ChromeOS to be Linux,
 * while goog.labs.userAgent.platform considers ChromeOS and
 * Linux to be different OSes.
 *
 * @type {boolean}
 */
goog.userAgent.LINUX = goog.userAgent.PLATFORM_KNOWN_ ?
    goog.userAgent.ASSUME_LINUX :
    goog.userAgent.isLegacyLinux_();


/**
 * @return {boolean} Whether the user agent is an X11 windowing system.
 * @private
 */
goog.userAgent.isX11_ = function() {
  var navigator = goog.userAgent.getNavigator();
  return !!navigator &&
      goog.string.contains(navigator['appVersion'] || '', 'X11');
};


/**
 * Whether the user agent is running on a X11 windowing system.
 * @type {boolean}
 */
goog.userAgent.X11 = goog.userAgent.PLATFORM_KNOWN_ ?
    goog.userAgent.ASSUME_X11 :
    goog.userAgent.isX11_();


/**
 * Whether the user agent is running on Android.
 * @type {boolean}
 */
goog.userAgent.ANDROID = goog.userAgent.PLATFORM_KNOWN_ ?
    goog.userAgent.ASSUME_ANDROID :
    goog.labs.userAgent.platform.isAndroid();


/**
 * Whether the user agent is running on an iPhone.
 * @type {boolean}
 */
goog.userAgent.IPHONE = goog.userAgent.PLATFORM_KNOWN_ ?
    goog.userAgent.ASSUME_IPHONE :
    goog.labs.userAgent.platform.isIphone();


/**
 * Whether the user agent is running on an iPad.
 * @type {boolean}
 */
goog.userAgent.IPAD = goog.userAgent.PLATFORM_KNOWN_ ?
    goog.userAgent.ASSUME_IPAD :
    goog.labs.userAgent.platform.isIpad();


/**
 * @return {string} The string that describes the version number of the user
 *     agent.
 * Assumes user agent is opera.
 * @private
 */
goog.userAgent.operaVersion_ = function() {
  var version = goog.global.opera.version;
  try {
    return version();
  } catch (e) {
    return version;
  }
};


/**
 * @return {string} The string that describes the version number of the user
 *     agent.
 * @private
 */
goog.userAgent.determineVersion_ = function() {
  // All browsers have different ways to detect the version and they all have
  // different naming schemes.

  if (goog.userAgent.OPERA && goog.global['opera']) {
    return goog.userAgent.operaVersion_();
  }

  // version is a string rather than a number because it may contain 'b', 'a',
  // and so on.
  var version = '';
  var arr = goog.userAgent.getVersionRegexResult_();
  if (arr) {
    version = arr ? arr[1] : '';
  }

  if (goog.userAgent.IE) {
    // IE9 can be in document mode 9 but be reporting an inconsistent user agent
    // version.  If it is identifying as a version lower than 9 we take the
    // documentMode as the version instead.  IE8 has similar behavior.
    // It is recommended to set the X-UA-Compatible header to ensure that IE9
    // uses documentMode 9.
    var docMode = goog.userAgent.getDocumentMode_();
    if (docMode > parseFloat(version)) {
      return String(docMode);
    }
  }

  return version;
};


/**
 * @return {Array|undefined} The version regex matches from parsing the user
 *     agent string. These regex statements must be executed inline so they can
 *     be compiled out by the closure compiler with the rest of the useragent
 *     detection logic when ASSUME_* is specified.
 * @private
 */
goog.userAgent.getVersionRegexResult_ = function() {
  var userAgent = goog.userAgent.getUserAgentString();
  if (goog.userAgent.GECKO) {
    return /rv\:([^\);]+)(\)|;)/.exec(userAgent);
  }
  if (goog.userAgent.EDGE) {
    return /Edge\/([\d\.]+)/.exec(userAgent);
  }
  if (goog.userAgent.IE) {
    return /\b(?:MSIE|rv)[: ]([^\);]+)(\)|;)/.exec(userAgent);
  }
  if (goog.userAgent.WEBKIT) {
    // WebKit/125.4
    return /WebKit\/(\S+)/.exec(userAgent);
  }
};


/**
 * @return {number|undefined} Returns the document mode (for testing).
 * @private
 */
goog.userAgent.getDocumentMode_ = function() {
  // NOTE(user): goog.userAgent may be used in context where there is no DOM.
  var doc = goog.global['document'];
  return doc ? doc['documentMode'] : undefined;
};


/**
 * The version of the user agent. This is a string because it might contain
 * 'b' (as in beta) as well as multiple dots.
 * @type {string}
 */
goog.userAgent.VERSION = goog.userAgent.determineVersion_();


/**
 * Compares two version numbers.
 *
 * @param {string} v1 Version of first item.
 * @param {string} v2 Version of second item.
 *
 * @return {number}  1 if first argument is higher
 *                   0 if arguments are equal
 *                  -1 if second argument is higher.
 * @deprecated Use goog.string.compareVersions.
 */
goog.userAgent.compare = function(v1, v2) {
  return goog.string.compareVersions(v1, v2);
};


/**
 * Cache for {@link goog.userAgent.isVersionOrHigher}.
 * Calls to compareVersions are surprisingly expensive and, as a browser's
 * version number is unlikely to change during a session, we cache the results.
 * @const
 * @private
 */
goog.userAgent.isVersionOrHigherCache_ = {};


/**
 * Whether the user agent version is higher or the same as the given version.
 * NOTE: When checking the version numbers for Firefox and Safari, be sure to
 * use the engine's version, not the browser's version number.  For example,
 * Firefox 3.0 corresponds to Gecko 1.9 and Safari 3.0 to Webkit 522.11.
 * Opera and Internet Explorer versions match the product release number.<br>
 * @see <a href="http://en.wikipedia.org/wiki/Safari_version_history">
 *     Webkit</a>
 * @see <a href="http://en.wikipedia.org/wiki/Gecko_engine">Gecko</a>
 *
 * @param {string|number} version The version to check.
 * @return {boolean} Whether the user agent version is higher or the same as
 *     the given version.
 */
goog.userAgent.isVersionOrHigher = function(version) {
  return goog.userAgent.ASSUME_ANY_VERSION ||
      goog.userAgent.isVersionOrHigherCache_[version] ||
      (goog.userAgent.isVersionOrHigherCache_[version] =
           goog.string.compareVersions(goog.userAgent.VERSION, version) >= 0);
};


/**
 * Deprecated alias to {@code goog.userAgent.isVersionOrHigher}.
 * @param {string|number} version The version to check.
 * @return {boolean} Whether the user agent version is higher or the same as
 *     the given version.
 * @deprecated Use goog.userAgent.isVersionOrHigher().
 */
goog.userAgent.isVersion = goog.userAgent.isVersionOrHigher;


/**
 * Whether the IE effective document mode is higher or the same as the given
 * document mode version.
 * NOTE: Only for IE, return false for another browser.
 *
 * @param {number} documentMode The document mode version to check.
 * @return {boolean} Whether the IE effective document mode is higher or the
 *     same as the given version.
 */
goog.userAgent.isDocumentModeOrHigher = function(documentMode) {
  return Number(goog.userAgent.DOCUMENT_MODE) >= documentMode;
};


/**
 * Deprecated alias to {@code goog.userAgent.isDocumentModeOrHigher}.
 * @param {number} version The version to check.
 * @return {boolean} Whether the IE effective document mode is higher or the
 *      same as the given version.
 * @deprecated Use goog.userAgent.isDocumentModeOrHigher().
 */
goog.userAgent.isDocumentMode = goog.userAgent.isDocumentModeOrHigher;


/**
 * For IE version < 7, documentMode is undefined, so attempt to use the
 * CSS1Compat property to see if we are in standards mode. If we are in
 * standards mode, treat the browser version as the document mode. Otherwise,
 * IE is emulating version 5.
 * @type {number|undefined}
 * @const
 */
goog.userAgent.DOCUMENT_MODE = (function() {
  var doc = goog.global['document'];
  var mode = goog.userAgent.getDocumentMode_();
  if (!doc || !goog.userAgent.IE) {
    return undefined;
  }
  return mode || (doc['compatMode'] == 'CSS1Compat' ?
                      parseInt(goog.userAgent.VERSION, 10) :
                      5);
})();

// Copyright 2008 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Detects the specific browser and not just the rendering engine.
 *
 */

goog.provide('goog.userAgent.product');

goog.require('goog.labs.userAgent.browser');
goog.require('goog.labs.userAgent.platform');
goog.require('goog.userAgent');


/**
 * @define {boolean} Whether the code is running on the Firefox web browser.
 */
goog.define('goog.userAgent.product.ASSUME_FIREFOX', false);


/**
 * @define {boolean} Whether we know at compile-time that the product is an
 *     iPhone.
 */
goog.define('goog.userAgent.product.ASSUME_IPHONE', false);


/**
 * @define {boolean} Whether we know at compile-time that the product is an
 *     iPad.
 */
goog.define('goog.userAgent.product.ASSUME_IPAD', false);


/**
 * @define {boolean} Whether we know at compile-time that the product is an
 *     AOSP browser or WebView inside a pre KitKat Android phone or tablet.
 */
goog.define('goog.userAgent.product.ASSUME_ANDROID', false);


/**
 * @define {boolean} Whether the code is running on the Chrome web browser on
 * any platform or AOSP browser or WebView in a KitKat+ Android phone or tablet.
 */
goog.define('goog.userAgent.product.ASSUME_CHROME', false);


/**
 * @define {boolean} Whether the code is running on the Safari web browser.
 */
goog.define('goog.userAgent.product.ASSUME_SAFARI', false);


/**
 * Whether we know the product type at compile-time.
 * @type {boolean}
 * @private
 */
goog.userAgent.product.PRODUCT_KNOWN_ = goog.userAgent.ASSUME_IE ||
    goog.userAgent.ASSUME_EDGE || goog.userAgent.ASSUME_OPERA ||
    goog.userAgent.product.ASSUME_FIREFOX ||
    goog.userAgent.product.ASSUME_IPHONE ||
    goog.userAgent.product.ASSUME_IPAD ||
    goog.userAgent.product.ASSUME_ANDROID ||
    goog.userAgent.product.ASSUME_CHROME ||
    goog.userAgent.product.ASSUME_SAFARI;


/**
 * Whether the code is running on the Opera web browser.
 * @type {boolean}
 */
goog.userAgent.product.OPERA = goog.userAgent.OPERA;


/**
 * Whether the code is running on an IE web browser.
 * @type {boolean}
 */
goog.userAgent.product.IE = goog.userAgent.IE;


/**
 * Whether the code is running on an Edge web browser.
 * @type {boolean}
 */
goog.userAgent.product.EDGE = goog.userAgent.EDGE;


/**
 * Whether the code is running on the Firefox web browser.
 * @type {boolean}
 */
goog.userAgent.product.FIREFOX = goog.userAgent.product.PRODUCT_KNOWN_ ?
    goog.userAgent.product.ASSUME_FIREFOX :
    goog.labs.userAgent.browser.isFirefox();


/**
 * Whether the user agent is an iPhone or iPod (as in iPod touch).
 * @return {boolean}
 * @private
 */
goog.userAgent.product.isIphoneOrIpod_ = function() {
  return goog.labs.userAgent.platform.isIphone() ||
      goog.labs.userAgent.platform.isIpod();
};


/**
 * Whether the code is running on an iPhone or iPod touch.
 *
 * iPod touch is considered an iPhone for legacy reasons.
 * @type {boolean}
 */
goog.userAgent.product.IPHONE = goog.userAgent.product.PRODUCT_KNOWN_ ?
    goog.userAgent.product.ASSUME_IPHONE :
    goog.userAgent.product.isIphoneOrIpod_();


/**
 * Whether the code is running on an iPad.
 * @type {boolean}
 */
goog.userAgent.product.IPAD = goog.userAgent.product.PRODUCT_KNOWN_ ?
    goog.userAgent.product.ASSUME_IPAD :
    goog.labs.userAgent.platform.isIpad();


/**
 * Whether the code is running on AOSP browser or WebView inside
 * a pre KitKat Android phone or tablet.
 * @type {boolean}
 */
goog.userAgent.product.ANDROID = goog.userAgent.product.PRODUCT_KNOWN_ ?
    goog.userAgent.product.ASSUME_ANDROID :
    goog.labs.userAgent.browser.isAndroidBrowser();


/**
 * Whether the code is running on the Chrome web browser on any platform
 * or AOSP browser or WebView in a KitKat+ Android phone or tablet.
 * @type {boolean}
 */
goog.userAgent.product.CHROME = goog.userAgent.product.PRODUCT_KNOWN_ ?
    goog.userAgent.product.ASSUME_CHROME :
    goog.labs.userAgent.browser.isChrome();


/**
 * @return {boolean} Whether the browser is Safari on desktop.
 * @private
 */
goog.userAgent.product.isSafariDesktop_ = function() {
  return goog.labs.userAgent.browser.isSafari() &&
      !goog.labs.userAgent.platform.isIos();
};


/**
 * Whether the code is running on the desktop Safari web browser.
 * Note: the legacy behavior here is only true for Safari not running
 * on iOS.
 * @type {boolean}
 */
goog.userAgent.product.SAFARI = goog.userAgent.product.PRODUCT_KNOWN_ ?
    goog.userAgent.product.ASSUME_SAFARI :
    goog.userAgent.product.isSafariDesktop_();

// Copyright 2007 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Base64 en/decoding. Not much to say here except that we
 * work with decoded values in arrays of bytes. By "byte" I mean a number
 * in [0, 255].
 *
 * @author doughtie@google.com (Gavin Doughtie)
 */

goog.provide('goog.crypt.base64');

goog.require('goog.asserts');
goog.require('goog.crypt');
goog.require('goog.string');
goog.require('goog.userAgent');
goog.require('goog.userAgent.product');

// Static lookup maps, lazily populated by init_()


/**
 * Maps bytes to characters.
 * @type {Object}
 * @private
 */
goog.crypt.base64.byteToCharMap_ = null;


/**
 * Maps characters to bytes. Used for normal and websafe characters.
 * @type {Object}
 * @private
 */
goog.crypt.base64.charToByteMap_ = null;


/**
 * Maps bytes to websafe characters.
 * @type {Object}
 * @private
 */
goog.crypt.base64.byteToCharMapWebSafe_ = null;


/**
 * Our default alphabet, shared between
 * ENCODED_VALS and ENCODED_VALS_WEBSAFE
 * @type {string}
 */
goog.crypt.base64.ENCODED_VALS_BASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    'abcdefghijklmnopqrstuvwxyz' +
    '0123456789';


/**
 * Our default alphabet. Value 64 (=) is special; it means "nothing."
 * @type {string}
 */
goog.crypt.base64.ENCODED_VALS = goog.crypt.base64.ENCODED_VALS_BASE + '+/=';


/**
 * Our websafe alphabet.
 * @type {string}
 */
goog.crypt.base64.ENCODED_VALS_WEBSAFE =
    goog.crypt.base64.ENCODED_VALS_BASE + '-_.';


/**
 * White list of implementations with known-good native atob and btoa functions.
 * Listing these explicitly (via the ASSUME_* wrappers) benefits dead-code
 * removal in per-browser compilations.
 * @private {boolean}
 */
goog.crypt.base64.ASSUME_NATIVE_SUPPORT_ = goog.userAgent.GECKO ||
    (goog.userAgent.WEBKIT && !goog.userAgent.product.SAFARI) ||
    goog.userAgent.OPERA;


/**
 * Does this browser have a working btoa function?
 * @private {boolean}
 */
goog.crypt.base64.HAS_NATIVE_ENCODE_ =
    goog.crypt.base64.ASSUME_NATIVE_SUPPORT_ ||
    typeof(goog.global.btoa) == 'function';


/**
 * Does this browser have a working atob function?
 * We blacklist known-bad implementations:
 *  - IE (10+) added atob() but it does not tolerate whitespace on the input.
 * @private {boolean}
 */
goog.crypt.base64.HAS_NATIVE_DECODE_ =
    goog.crypt.base64.ASSUME_NATIVE_SUPPORT_ ||
    (!goog.userAgent.product.SAFARI && !goog.userAgent.IE &&
     typeof(goog.global.atob) == 'function');


/**
 * Base64-encode an array of bytes.
 *
 * @param {Array<number>|Uint8Array} input An array of bytes (numbers with
 *     value in [0, 255]) to encode.
 * @param {boolean=} opt_webSafe True indicates we should use the alternative
 *     alphabet, which does not require escaping for use in URLs.
 * @return {string} The base64 encoded string.
 */
goog.crypt.base64.encodeByteArray = function(input, opt_webSafe) {
  // Assert avoids runtime dependency on goog.isArrayLike, which helps reduce
  // size of jscompiler output, and which yields slight performance increase.
  goog.asserts.assert(
      goog.isArrayLike(input), 'encodeByteArray takes an array as a parameter');

  goog.crypt.base64.init_();

  var byteToCharMap = opt_webSafe ? goog.crypt.base64.byteToCharMapWebSafe_ :
                                    goog.crypt.base64.byteToCharMap_;

  var output = [];

  for (var i = 0; i < input.length; i += 3) {
    var byte1 = input[i];
    var haveByte2 = i + 1 < input.length;
    var byte2 = haveByte2 ? input[i + 1] : 0;
    var haveByte3 = i + 2 < input.length;
    var byte3 = haveByte3 ? input[i + 2] : 0;

    var outByte1 = byte1 >> 2;
    var outByte2 = ((byte1 & 0x03) << 4) | (byte2 >> 4);
    var outByte3 = ((byte2 & 0x0F) << 2) | (byte3 >> 6);
    var outByte4 = byte3 & 0x3F;

    if (!haveByte3) {
      outByte4 = 64;

      if (!haveByte2) {
        outByte3 = 64;
      }
    }

    output.push(
        byteToCharMap[outByte1], byteToCharMap[outByte2],
        byteToCharMap[outByte3], byteToCharMap[outByte4]);
  }

  return output.join('');
};


/**
 * Base64-encode a string.
 *
 * @param {string} input A string to encode.
 * @param {boolean=} opt_webSafe True indicates we should use the alternative
 *     alphabet, which does not require escaping for use in URLs.
 * @return {string} The base64 encoded string.
 */
goog.crypt.base64.encodeString = function(input, opt_webSafe) {
  // Shortcut for browsers that implement
  // a native base64 encoder in the form of "btoa/atob"
  if (goog.crypt.base64.HAS_NATIVE_ENCODE_ && !opt_webSafe) {
    return goog.global.btoa(input);
  }
  return goog.crypt.base64.encodeByteArray(
      goog.crypt.stringToByteArray(input), opt_webSafe);
};


/**
 * Base64-decode a string.
 *
 * @param {string} input Input to decode. Any whitespace is ignored, and the
 *     input maybe encoded with either supported alphabet (or a mix thereof).
 * @param {boolean=} opt_webSafe True indicates we should use the alternative
 *     alphabet, which does not require escaping for use in URLs. Note that
 *     passing false may also still allow webSafe input decoding, when the
 *     fallback decoder is used on browsers without native support.
 * @return {string} string representing the decoded value.
 */
goog.crypt.base64.decodeString = function(input, opt_webSafe) {
  // Shortcut for browsers that implement
  // a native base64 encoder in the form of "btoa/atob"
  if (goog.crypt.base64.HAS_NATIVE_DECODE_ && !opt_webSafe) {
    return goog.global.atob(input);
  }
  var output = '';
  function pushByte(b) { output += String.fromCharCode(b); }

  goog.crypt.base64.decodeStringInternal_(input, pushByte);

  return output;
};


/**
 * Base64-decode a string to an Array of numbers.
 *
 * In base-64 decoding, groups of four characters are converted into three
 * bytes.  If the encoder did not apply padding, the input length may not
 * be a multiple of 4.
 *
 * In this case, the last group will have fewer than 4 characters, and
 * padding will be inferred.  If the group has one or two characters, it decodes
 * to one byte.  If the group has three characters, it decodes to two bytes.
 *
 * @param {string} input Input to decode. Any whitespace is ignored, and the
 *     input maybe encoded with either supported alphabet (or a mix thereof).
 * @param {boolean=} opt_ignored Unused parameter, retained for compatibility.
 * @return {!Array<number>} bytes representing the decoded value.
 */
goog.crypt.base64.decodeStringToByteArray = function(input, opt_ignored) {
  var output = [];
  function pushByte(b) { output.push(b); }

  goog.crypt.base64.decodeStringInternal_(input, pushByte);

  return output;
};


/**
 * Base64-decode a string to a Uint8Array.
 *
 * Note that Uint8Array is not supported on older browsers, e.g. IE < 10.
 * @see http://caniuse.com/uint8array
 *
 * In base-64 decoding, groups of four characters are converted into three
 * bytes.  If the encoder did not apply padding, the input length may not
 * be a multiple of 4.
 *
 * In this case, the last group will have fewer than 4 characters, and
 * padding will be inferred.  If the group has one or two characters, it decodes
 * to one byte.  If the group has three characters, it decodes to two bytes.
 *
 * @param {string} input Input to decode. Any whitespace is ignored, and the
 *     input maybe encoded with either supported alphabet (or a mix thereof).
 * @return {!Uint8Array} bytes representing the decoded value.
 */
goog.crypt.base64.decodeStringToUint8Array = function(input) {
  goog.asserts.assert(
      !goog.userAgent.IE || goog.userAgent.isVersionOrHigher('10'),
      'Browser does not support typed arrays');
  var output = new Uint8Array(Math.ceil(input.length * 3 / 4));
  var outLen = 0;
  function pushByte(b) { output[outLen++] = b; }

  goog.crypt.base64.decodeStringInternal_(input, pushByte);

  return output.subarray(0, outLen);
};


/**
 * @param {string} input Input to decode.
 * @param {function(number):void} pushByte result accumulator.
 * @private
 */
goog.crypt.base64.decodeStringInternal_ = function(input, pushByte) {
  goog.crypt.base64.init_();

  var nextCharIndex = 0;
  /**
   * @param {number} default_val Used for end-of-input.
   * @return {number} The next 6-bit value, or the default for end-of-input.
   */
  function getByte(default_val) {
    while (nextCharIndex < input.length) {
      var ch = input.charAt(nextCharIndex++);
      var b = goog.crypt.base64.charToByteMap_[ch];
      if (b != null) {
        return b;  // Common case: decoded the char.
      }
      if (!goog.string.isEmptyOrWhitespace(ch)) {
        throw Error('Unknown base64 encoding at char: ' + ch);
      }
      // We encountered whitespace: loop around to the next input char.
    }
    return default_val;  // No more input remaining.
  }

  while (true) {
    var byte1 = getByte(-1);
    var byte2 = getByte(0);
    var byte3 = getByte(64);
    var byte4 = getByte(64);

    // The common case is that all four bytes are present, so if we have byte4
    // we can skip over the truncated input special case handling.
    if (byte4 === 64) {
      if (byte1 === -1) {
        return;  // Terminal case: no input left to decode.
      }
      // Here we know an intermediate number of bytes are missing.
      // The defaults for byte2, byte3 and byte4 apply the inferred padding
      // rules per the public API documentation. i.e: 1 byte
      // missing should yield 2 bytes of output, but 2 or 3 missing bytes yield
      // a single byte of output. (Recall that 64 corresponds the padding char).
    }

    var outByte1 = (byte1 << 2) | (byte2 >> 4);
    pushByte(outByte1);

    if (byte3 != 64) {
      var outByte2 = ((byte2 << 4) & 0xF0) | (byte3 >> 2);
      pushByte(outByte2);

      if (byte4 != 64) {
        var outByte3 = ((byte3 << 6) & 0xC0) | byte4;
        pushByte(outByte3);
      }
    }
  }
};


/**
 * Lazy static initialization function. Called before
 * accessing any of the static map variables.
 * @private
 */
goog.crypt.base64.init_ = function() {
  if (!goog.crypt.base64.byteToCharMap_) {
    goog.crypt.base64.byteToCharMap_ = {};
    goog.crypt.base64.charToByteMap_ = {};
    goog.crypt.base64.byteToCharMapWebSafe_ = {};

    // We want quick mappings back and forth, so we precompute two maps.
    for (var i = 0; i < goog.crypt.base64.ENCODED_VALS.length; i++) {
      goog.crypt.base64.byteToCharMap_[i] =
          goog.crypt.base64.ENCODED_VALS.charAt(i);
      goog.crypt.base64.charToByteMap_[goog.crypt.base64.byteToCharMap_[i]] = i;
      goog.crypt.base64.byteToCharMapWebSafe_[i] =
          goog.crypt.base64.ENCODED_VALS_WEBSAFE.charAt(i);

      // Be forgiving when decoding and correctly decode both encodings.
      if (i >= goog.crypt.base64.ENCODED_VALS_BASE.length) {
        goog.crypt.base64
            .charToByteMap_[goog.crypt.base64.ENCODED_VALS_WEBSAFE.charAt(i)] =
            i;
      }
    }
  }
};

// Protocol Buffers - Google's data interchange format
// Copyright 2008 Google Inc.  All rights reserved.
// https://developers.google.com/protocol-buffers/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/**
 * @fileoverview This file contains helper code used by jspb.BinaryReader
 * and BinaryWriter.
 *
 * @author aappleby@google.com (Austin Appleby)
 */

goog.provide('jspb.utils');

goog.require('goog.asserts');
goog.require('goog.crypt.base64');
goog.require('goog.string');
goog.require('jspb.BinaryConstants');


/**
 * Javascript can't natively handle 64-bit data types, so to manipulate them we
 * have to split them into two 32-bit halves and do the math manually.
 *
 * Instead of instantiating and passing small structures around to do this, we
 * instead just use two global temporary values. This one stores the low 32
 * bits of a split value - for example, if the original value was a 64-bit
 * integer, this temporary value will contain the low 32 bits of that integer.
 * If the original value was a double, this temporary value will contain the
 * low 32 bits of the binary representation of that double, etcetera.
 * @type {number}
 */
jspb.utils.split64Low = 0;


/**
 * And correspondingly, this temporary variable will contain the high 32 bits
 * of whatever value was split.
 * @type {number}
 */
jspb.utils.split64High = 0;


/**
 * Splits an unsigned Javascript integer into two 32-bit halves and stores it
 * in the temp values above.
 * @param {number} value The number to split.
 */
jspb.utils.splitUint64 = function(value) {
  // Extract low 32 bits and high 32 bits as unsigned integers.
  var lowBits = value >>> 0;
  var highBits = Math.floor((value - lowBits) /
                            jspb.BinaryConstants.TWO_TO_32) >>> 0;

  jspb.utils.split64Low = lowBits;
  jspb.utils.split64High = highBits;
};


/**
 * Splits a signed Javascript integer into two 32-bit halves and stores it in
 * the temp values above.
 * @param {number} value The number to split.
 */
jspb.utils.splitInt64 = function(value) {
  // Convert to sign-magnitude representation.
  var sign = (value < 0);
  value = Math.abs(value);

  // Extract low 32 bits and high 32 bits as unsigned integers.
  var lowBits = value >>> 0;
  var highBits = Math.floor((value - lowBits) /
                            jspb.BinaryConstants.TWO_TO_32);
  highBits = highBits >>> 0;

  // Perform two's complement conversion if the sign bit was set.
  if (sign) {
    highBits = ~highBits >>> 0;
    lowBits = ~lowBits >>> 0;
    lowBits += 1;
    if (lowBits > 0xFFFFFFFF) {
      lowBits = 0;
      highBits++;
      if (highBits > 0xFFFFFFFF) highBits = 0;
    }
  }

  jspb.utils.split64Low = lowBits;
  jspb.utils.split64High = highBits;
};


/**
 * Convers a signed Javascript integer into zigzag format, splits it into two
 * 32-bit halves, and stores it in the temp values above.
 * @param {number} value The number to split.
 */
jspb.utils.splitZigzag64 = function(value) {
  // Convert to sign-magnitude and scale by 2 before we split the value.
  var sign = (value < 0);
  value = Math.abs(value) * 2;

  jspb.utils.splitUint64(value);
  var lowBits = jspb.utils.split64Low;
  var highBits = jspb.utils.split64High;

  // If the value is negative, subtract 1 from the split representation so we
  // don't lose the sign bit due to precision issues.
  if (sign) {
    if (lowBits == 0) {
      if (highBits == 0) {
        lowBits = 0xFFFFFFFF;
        highBits = 0xFFFFFFFF;
      } else {
        highBits--;
        lowBits = 0xFFFFFFFF;
      }
    } else {
      lowBits--;
    }
  }

  jspb.utils.split64Low = lowBits;
  jspb.utils.split64High = highBits;
};


/**
 * Converts a floating-point number into 32-bit IEEE representation and stores
 * it in the temp values above.
 * @param {number} value
 */
jspb.utils.splitFloat32 = function(value) {
  var sign = (value < 0) ? 1 : 0;
  value = sign ? -value : value;
  var exp;
  var mant;

  // Handle zeros.
  if (value === 0) {
    if ((1 / value) > 0) {
      // Positive zero.
      jspb.utils.split64High = 0;
      jspb.utils.split64Low = 0x00000000;
    } else {
      // Negative zero.
      jspb.utils.split64High = 0;
      jspb.utils.split64Low = 0x80000000;
    }
    return;
  }

  // Handle nans.
  if (isNaN(value)) {
    jspb.utils.split64High = 0;
    jspb.utils.split64Low = 0x7FFFFFFF;
    return;
  }

  // Handle infinities.
  if (value > jspb.BinaryConstants.FLOAT32_MAX) {
    jspb.utils.split64High = 0;
    jspb.utils.split64Low = ((sign << 31) | (0x7F800000)) >>> 0;
    return;
  }

  // Handle denormals.
  if (value < jspb.BinaryConstants.FLOAT32_MIN) {
    // Number is a denormal.
    mant = Math.round(value / Math.pow(2, -149));
    jspb.utils.split64High = 0;
    jspb.utils.split64Low = ((sign << 31) | mant) >>> 0;
    return;
  }

  exp = Math.floor(Math.log(value) / Math.LN2);
  mant = value * Math.pow(2, -exp);
  mant = Math.round(mant * jspb.BinaryConstants.TWO_TO_23) & 0x7FFFFF;

  jspb.utils.split64High = 0;
  jspb.utils.split64Low = ((sign << 31) | ((exp + 127) << 23) | mant) >>> 0;
};


/**
 * Converts a floating-point number into 64-bit IEEE representation and stores
 * it in the temp values above.
 * @param {number} value
 */
jspb.utils.splitFloat64 = function(value) {
  var sign = (value < 0) ? 1 : 0;
  value = sign ? -value : value;

  // Handle zeros.
  if (value === 0) {
    if ((1 / value) > 0) {
      // Positive zero.
      jspb.utils.split64High = 0x00000000;
      jspb.utils.split64Low = 0x00000000;
    } else {
      // Negative zero.
      jspb.utils.split64High = 0x80000000;
      jspb.utils.split64Low = 0x00000000;
    }
    return;
  }

  // Handle nans.
  if (isNaN(value)) {
    jspb.utils.split64High = 0x7FFFFFFF;
    jspb.utils.split64Low = 0xFFFFFFFF;
    return;
  }

  // Handle infinities.
  if (value > jspb.BinaryConstants.FLOAT64_MAX) {
    jspb.utils.split64High = ((sign << 31) | (0x7FF00000)) >>> 0;
    jspb.utils.split64Low = 0;
    return;
  }

  // Handle denormals.
  if (value < jspb.BinaryConstants.FLOAT64_MIN) {
    // Number is a denormal.
    var mant = value / Math.pow(2, -1074);
    var mantHigh = (mant / jspb.BinaryConstants.TWO_TO_32);
    jspb.utils.split64High = ((sign << 31) | mantHigh) >>> 0;
    jspb.utils.split64Low = (mant >>> 0);
    return;
  }

  var exp = Math.floor(Math.log(value) / Math.LN2);
  if (exp == 1024) exp = 1023;
  var mant = value * Math.pow(2, -exp);

  var mantHigh = (mant * jspb.BinaryConstants.TWO_TO_20) & 0xFFFFF;
  var mantLow = (mant * jspb.BinaryConstants.TWO_TO_52) >>> 0;

  jspb.utils.split64High =
      ((sign << 31) | ((exp + 1023) << 20) | mantHigh) >>> 0;
  jspb.utils.split64Low = mantLow;
};


/**
 * Converts an 8-character hash string into two 32-bit numbers and stores them
 * in the temp values above.
 * @param {string} hash
 */
jspb.utils.splitHash64 = function(hash) {
  var a = hash.charCodeAt(0);
  var b = hash.charCodeAt(1);
  var c = hash.charCodeAt(2);
  var d = hash.charCodeAt(3);
  var e = hash.charCodeAt(4);
  var f = hash.charCodeAt(5);
  var g = hash.charCodeAt(6);
  var h = hash.charCodeAt(7);

  jspb.utils.split64Low = (a + (b << 8) + (c << 16) + (d << 24)) >>> 0;
  jspb.utils.split64High = (e + (f << 8) + (g << 16) + (h << 24)) >>> 0;
};


/**
 * Joins two 32-bit values into a 64-bit unsigned integer. Precision will be
 * lost if the result is greater than 2^52.
 * @param {number} bitsLow
 * @param {number} bitsHigh
 * @return {number}
 */
jspb.utils.joinUint64 = function(bitsLow, bitsHigh) {
  return bitsHigh * jspb.BinaryConstants.TWO_TO_32 + bitsLow;
};


/**
 * Joins two 32-bit values into a 64-bit signed integer. Precision will be lost
 * if the result is greater than 2^52.
 * @param {number} bitsLow
 * @param {number} bitsHigh
 * @return {number}
 */
jspb.utils.joinInt64 = function(bitsLow, bitsHigh) {
  // If the high bit is set, do a manual two's complement conversion.
  var sign = (bitsHigh & 0x80000000);
  if (sign) {
    bitsLow = (~bitsLow + 1) >>> 0;
    bitsHigh = ~bitsHigh >>> 0;
    if (bitsLow == 0) {
      bitsHigh = (bitsHigh + 1) >>> 0;
    }
  }

  var result = jspb.utils.joinUint64(bitsLow, bitsHigh);
  return sign ? -result : result;
};


/**
 * Joins two 32-bit values into a 64-bit unsigned integer and applies zigzag
 * decoding. Precision will be lost if the result is greater than 2^52.
 * @param {number} bitsLow
 * @param {number} bitsHigh
 * @return {number}
 */
jspb.utils.joinZigzag64 = function(bitsLow, bitsHigh) {
  // Extract the sign bit and shift right by one.
  var sign = bitsLow & 1;
  bitsLow = ((bitsLow >>> 1) | (bitsHigh << 31)) >>> 0;
  bitsHigh = bitsHigh >>> 1;

  // Increment the split value if the sign bit was set.
  if (sign) {
    bitsLow = (bitsLow + 1) >>> 0;
    if (bitsLow == 0) {
      bitsHigh = (bitsHigh + 1) >>> 0;
    }
  }

  var result = jspb.utils.joinUint64(bitsLow, bitsHigh);
  return sign ? -result : result;
};


/**
 * Joins two 32-bit values into a 32-bit IEEE floating point number and
 * converts it back into a Javascript number.
 * @param {number} bitsLow The low 32 bits of the binary number;
 * @param {number} bitsHigh The high 32 bits of the binary number.
 * @return {number}
 */
jspb.utils.joinFloat32 = function(bitsLow, bitsHigh) {
  var sign = ((bitsLow >> 31) * 2 + 1);
  var exp = (bitsLow >>> 23) & 0xFF;
  var mant = bitsLow & 0x7FFFFF;

  if (exp == 0xFF) {
    if (mant) {
      return NaN;
    } else {
      return sign * Infinity;
    }
  }

  if (exp == 0) {
    // Denormal.
    return sign * Math.pow(2, -149) * mant;
  } else {
    return sign * Math.pow(2, exp - 150) *
           (mant + Math.pow(2, 23));
  }
};


/**
 * Joins two 32-bit values into a 64-bit IEEE floating point number and
 * converts it back into a Javascript number.
 * @param {number} bitsLow The low 32 bits of the binary number;
 * @param {number} bitsHigh The high 32 bits of the binary number.
 * @return {number}
 */
jspb.utils.joinFloat64 = function(bitsLow, bitsHigh) {
  var sign = ((bitsHigh >> 31) * 2 + 1);
  var exp = (bitsHigh >>> 20) & 0x7FF;
  var mant = jspb.BinaryConstants.TWO_TO_32 * (bitsHigh & 0xFFFFF) + bitsLow;

  if (exp == 0x7FF) {
    if (mant) {
      return NaN;
    } else {
      return sign * Infinity;
    }
  }

  if (exp == 0) {
    // Denormal.
    return sign * Math.pow(2, -1074) * mant;
  } else {
    return sign * Math.pow(2, exp - 1075) *
           (mant + jspb.BinaryConstants.TWO_TO_52);
  }
};


/**
 * Joins two 32-bit values into an 8-character hash string.
 * @param {number} bitsLow
 * @param {number} bitsHigh
 * @return {string}
 */
jspb.utils.joinHash64 = function(bitsLow, bitsHigh) {
  var a = (bitsLow >>> 0) & 0xFF;
  var b = (bitsLow >>> 8) & 0xFF;
  var c = (bitsLow >>> 16) & 0xFF;
  var d = (bitsLow >>> 24) & 0xFF;
  var e = (bitsHigh >>> 0) & 0xFF;
  var f = (bitsHigh >>> 8) & 0xFF;
  var g = (bitsHigh >>> 16) & 0xFF;
  var h = (bitsHigh >>> 24) & 0xFF;

  return String.fromCharCode(a, b, c, d, e, f, g, h);
};


/**
 * Individual digits for number->string conversion.
 * @const {!Array.<number>}
 */
jspb.utils.DIGITS = [
  '0', '1', '2', '3', '4', '5', '6', '7',
  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
];


/**
 * Losslessly converts a 64-bit unsigned integer in 32:32 split representation
 * into a decimal string.
 * @param {number} bitsLow The low 32 bits of the binary number;
 * @param {number} bitsHigh The high 32 bits of the binary number.
 * @return {string} The binary number represented as a string.
 */
jspb.utils.joinUnsignedDecimalString = function(bitsLow, bitsHigh) {
  // Skip the expensive conversion if the number is small enough to use the
  // built-in conversions.
  if (bitsHigh <= 0x1FFFFF) {
    return '' + (jspb.BinaryConstants.TWO_TO_32 * bitsHigh + bitsLow);
  }

  // What this code is doing is essentially converting the input number from
  // base-2 to base-1e7, which allows us to represent the 64-bit range with
  // only 3 (very large) digits. Those digits are then trivial to convert to
  // a base-10 string.

  // The magic numbers used here are -
  // 2^24 = 16777216 = (1,6777216) in base-1e7.
  // 2^48 = 281474976710656 = (2,8147497,6710656) in base-1e7.

  // Split 32:32 representation into 16:24:24 representation so our
  // intermediate digits don't overflow.
  var low = bitsLow & 0xFFFFFF;
  var mid = (((bitsLow >>> 24) | (bitsHigh << 8)) >>> 0) & 0xFFFFFF;
  var high = (bitsHigh >> 16) & 0xFFFF;

  // Assemble our three base-1e7 digits, ignoring carries. The maximum
  // value in a digit at this step is representable as a 48-bit integer, which
  // can be stored in a 64-bit floating point number.
  var digitA = low + (mid * 6777216) + (high * 6710656);
  var digitB = mid + (high * 8147497);
  var digitC = (high * 2);

  // Apply carries from A to B and from B to C.
  var base = 10000000;
  if (digitA >= base) {
    digitB += Math.floor(digitA / base);
    digitA %= base;
  }

  if (digitB >= base) {
    digitC += Math.floor(digitB / base);
    digitB %= base;
  }

  // Convert base-1e7 digits to base-10, omitting leading zeroes.
  var table = jspb.utils.DIGITS;
  var start = false;
  var result = '';

  function emit(digit) {
    var temp = base;
    for (var i = 0; i < 7; i++) {
      temp /= 10;
      var decimalDigit = ((digit / temp) % 10) >>> 0;
      if ((decimalDigit == 0) && !start) continue;
      start = true;
      result += table[decimalDigit];
    }
  }

  if (digitC || start) emit(digitC);
  if (digitB || start) emit(digitB);
  if (digitA || start) emit(digitA);

  return result;
};


/**
 * Losslessly converts a 64-bit signed integer in 32:32 split representation
 * into a decimal string.
 * @param {number} bitsLow The low 32 bits of the binary number;
 * @param {number} bitsHigh The high 32 bits of the binary number.
 * @return {string} The binary number represented as a string.
 */
jspb.utils.joinSignedDecimalString = function(bitsLow, bitsHigh) {
  // If we're treating the input as a signed value and the high bit is set, do
  // a manual two's complement conversion before the decimal conversion.
  var negative = (bitsHigh & 0x80000000);
  if (negative) {
    bitsLow = (~bitsLow + 1) >>> 0;
    var carry = (bitsLow == 0) ? 1 : 0;
    bitsHigh = (~bitsHigh + carry) >>> 0;
  }

  var result = jspb.utils.joinUnsignedDecimalString(bitsLow, bitsHigh);
  return negative ? '-' + result : result;
};


/**
 * Convert an 8-character hash string representing either a signed or unsigned
 * 64-bit integer into its decimal representation without losing accuracy.
 * @param {string} hash The hash string to convert.
 * @param {boolean} signed True if we should treat the hash string as encoding
 *     a signed integer.
 * @return {string}
 */
jspb.utils.hash64ToDecimalString = function(hash, signed) {
  jspb.utils.splitHash64(hash);
  var bitsLow = jspb.utils.split64Low;
  var bitsHigh = jspb.utils.split64High;
  return signed ?
      jspb.utils.joinSignedDecimalString(bitsLow, bitsHigh) :
      jspb.utils.joinUnsignedDecimalString(bitsLow, bitsHigh);
};


/**
 * Converts an array of 8-character hash strings into their decimal
 * representations.
 * @param {!Array.<string>} hashes The array of hash strings to convert.
 * @param {boolean} signed True if we should treat the hash string as encoding
 *     a signed integer.
 * @return {!Array.<string>}
 */
jspb.utils.hash64ArrayToDecimalStrings = function(hashes, signed) {
  var result = new Array(hashes.length);
  for (var i = 0; i < hashes.length; i++) {
    result[i] = jspb.utils.hash64ToDecimalString(hashes[i], signed);
  }
  return result;
};


/**
 * Converts an 8-character hash string into its hexadecimal representation.
 * @param {string} hash
 * @return {string}
 */
jspb.utils.hash64ToHexString = function(hash) {
  var temp = new Array(18);
  temp[0] = '0';
  temp[1] = 'x';

  for (var i = 0; i < 8; i++) {
    var c = hash.charCodeAt(7 - i);
    temp[i * 2 + 2] = jspb.utils.DIGITS[c >> 4];
    temp[i * 2 + 3] = jspb.utils.DIGITS[c & 0xF];
  }

  var result = temp.join('');
  return result;
};


/**
 * Converts a '0x<16 digits>' hex string into its hash string representation.
 * @param {string} hex
 * @return {string}
 */
jspb.utils.hexStringToHash64 = function(hex) {
  hex = hex.toLowerCase();
  goog.asserts.assert(hex.length == 18);
  goog.asserts.assert(hex[0] == '0');
  goog.asserts.assert(hex[1] == 'x');

  var result = '';
  for (var i = 0; i < 8; i++) {
    var hi = jspb.utils.DIGITS.indexOf(hex[i * 2 + 2]);
    var lo = jspb.utils.DIGITS.indexOf(hex[i * 2 + 3]);
    result = String.fromCharCode(hi * 16 + lo) + result;
  }

  return result;
};


/**
 * Convert an 8-character hash string representing either a signed or unsigned
 * 64-bit integer into a Javascript number. Will lose accuracy if the result is
 * larger than 2^52.
 * @param {string} hash The hash string to convert.
 * @param {boolean} signed True if the has should be interpreted as a signed
 *     number.
 * @return {number}
 */
jspb.utils.hash64ToNumber = function(hash, signed) {
  jspb.utils.splitHash64(hash);
  var bitsLow = jspb.utils.split64Low;
  var bitsHigh = jspb.utils.split64High;
  return signed ? jspb.utils.joinInt64(bitsLow, bitsHigh) :
                  jspb.utils.joinUint64(bitsLow, bitsHigh);
};


/**
 * Convert a Javascript number into an 8-character hash string. Will lose
 * precision if the value is non-integral or greater than 2^64.
 * @param {number} value The integer to convert.
 * @return {string}
 */
jspb.utils.numberToHash64 = function(value) {
  jspb.utils.splitInt64(value);
  return jspb.utils.joinHash64(jspb.utils.split64Low,
                                  jspb.utils.split64High);
};


/**
 * Counts the number of contiguous varints in a buffer.
 * @param {!Uint8Array} buffer The buffer to scan.
 * @param {number} start The starting point in the buffer to scan.
 * @param {number} end The end point in the buffer to scan.
 * @return {number} The number of varints in the buffer.
 */
jspb.utils.countVarints = function(buffer, start, end) {
  // Count how many high bits of each byte were set in the buffer.
  var count = 0;
  for (var i = start; i < end; i++) {
    count += buffer[i] >> 7;
  }

  // The number of varints in the buffer equals the size of the buffer minus
  // the number of non-terminal bytes in the buffer (those with the high bit
  // set).
  return (end - start) - count;
};


/**
 * Counts the number of contiguous varint fields with the given field number in
 * the buffer.
 * @param {!Uint8Array} buffer The buffer to scan.
 * @param {number} start The starting point in the buffer to scan.
 * @param {number} end The end point in the buffer to scan.
 * @param {number} field The field number to count.
 * @return {number} The number of matching fields in the buffer.
 */
jspb.utils.countVarintFields = function(buffer, start, end, field) {
  var count = 0;
  var cursor = start;
  var tag = field * 8 + jspb.BinaryConstants.WireType.VARINT;

  if (tag < 128) {
    // Single-byte field tag, we can use a slightly quicker count.
    while (cursor < end) {
      // Skip the field tag, or exit if we find a non-matching tag.
      if (buffer[cursor++] != tag) return count;

      // Field tag matches, we've found a valid field.
      count++;

      // Skip the varint.
      while (1) {
        var x = buffer[cursor++];
        if ((x & 0x80) == 0) break;
      }
    }
  } else {
    while (cursor < end) {
      // Skip the field tag, or exit if we find a non-matching tag.
      var temp = tag;
      while (temp > 128) {
        if (buffer[cursor] != ((temp & 0x7F) | 0x80)) return count;
        cursor++;
        temp >>= 7;
      }
      if (buffer[cursor++] != temp) return count;

      // Field tag matches, we've found a valid field.
      count++;

      // Skip the varint.
      while (1) {
        var x = buffer[cursor++];
        if ((x & 0x80) == 0) break;
      }
    }
  }
  return count;
};


/**
 * Counts the number of contiguous fixed32 fields with the given tag in the
 * buffer.
 * @param {!Uint8Array} buffer The buffer to scan.
 * @param {number} start The starting point in the buffer to scan.
 * @param {number} end The end point in the buffer to scan.
 * @param {number} tag The tag value to count.
 * @param {number} stride The number of bytes to skip per field.
 * @return {number} The number of fields with a matching tag in the buffer.
 * @private
 */
jspb.utils.countFixedFields_ =
    function(buffer, start, end, tag, stride) {
  var count = 0;
  var cursor = start;

  if (tag < 128) {
    // Single-byte field tag, we can use a slightly quicker count.
    while (cursor < end) {
      // Skip the field tag, or exit if we find a non-matching tag.
      if (buffer[cursor++] != tag) return count;

      // Field tag matches, we've found a valid field.
      count++;

      // Skip the value.
      cursor += stride;
    }
  } else {
    while (cursor < end) {
      // Skip the field tag, or exit if we find a non-matching tag.
      var temp = tag;
      while (temp > 128) {
        if (buffer[cursor++] != ((temp & 0x7F) | 0x80)) return count;
        temp >>= 7;
      }
      if (buffer[cursor++] != temp) return count;

      // Field tag matches, we've found a valid field.
      count++;

      // Skip the value.
      cursor += stride;
    }
  }
  return count;
};


/**
 * Counts the number of contiguous fixed32 fields with the given field number
 * in the buffer.
 * @param {!Uint8Array} buffer The buffer to scan.
 * @param {number} start The starting point in the buffer to scan.
 * @param {number} end The end point in the buffer to scan.
 * @param {number} field The field number to count.
 * @return {number} The number of matching fields in the buffer.
 */
jspb.utils.countFixed32Fields = function(buffer, start, end, field) {
  var tag = field * 8 + jspb.BinaryConstants.WireType.FIXED32;
  return jspb.utils.countFixedFields_(buffer, start, end, tag, 4);
};


/**
 * Counts the number of contiguous fixed64 fields with the given field number
 * in the buffer.
 * @param {!Uint8Array} buffer The buffer to scan.
 * @param {number} start The starting point in the buffer to scan.
 * @param {number} end The end point in the buffer to scan.
 * @param {number} field The field number to count
 * @return {number} The number of matching fields in the buffer.
 */
jspb.utils.countFixed64Fields = function(buffer, start, end, field) {
  var tag = field * 8 + jspb.BinaryConstants.WireType.FIXED64;
  return jspb.utils.countFixedFields_(buffer, start, end, tag, 8);
};


/**
 * Counts the number of contiguous delimited fields with the given field number
 * in the buffer.
 * @param {!Uint8Array} buffer The buffer to scan.
 * @param {number} start The starting point in the buffer to scan.
 * @param {number} end The end point in the buffer to scan.
 * @param {number} field The field number to count.
 * @return {number} The number of matching fields in the buffer.
 */
jspb.utils.countDelimitedFields = function(buffer, start, end, field) {
  var count = 0;
  var cursor = start;
  var tag = field * 8 + jspb.BinaryConstants.WireType.DELIMITED;

  while (cursor < end) {
    // Skip the field tag, or exit if we find a non-matching tag.
    var temp = tag;
    while (temp > 128) {
      if (buffer[cursor++] != ((temp & 0x7F) | 0x80)) return count;
      temp >>= 7;
    }
    if (buffer[cursor++] != temp) return count;

    // Field tag matches, we've found a valid field.
    count++;

    // Decode the length prefix.
    var length = 0;
    var shift = 1;
    while (1) {
      temp = buffer[cursor++];
      length += (temp & 0x7f) * shift;
      shift *= 128;
      if ((temp & 0x80) == 0) break;
    }

    // Advance the cursor past the blob.
    cursor += length;
  }
  return count;
};


/**
 * Clones a scalar field. Pulling this out to a helper method saves us a few
 * bytes of generated code.
 * @param {Array} array
 * @return {Array}
 */
jspb.utils.cloneRepeatedScalarField = function(array) {
  return array ? array.slice() : null;
};


/**
 * Clones an array of messages using the provided cloner function.
 * @param {Array.<jspb.BinaryMessage>} messages
 * @param {jspb.ClonerFunction} cloner
 * @return {Array.<jspb.BinaryMessage>}
 */
jspb.utils.cloneRepeatedMessageField = function(messages, cloner) {
  if (messages === null) return null;
  var result = [];
  for (var i = 0; i < messages.length; i++) {
    result.push(cloner(messages[i]));
  }
  return result;
};


/**
 * Clones an array of byte blobs.
 * @param {Array.<Uint8Array>} blobs
 * @return {Array.<Uint8Array>}
 */
jspb.utils.cloneRepeatedBlobField = function(blobs) {
  if (blobs === null) return null;
  var result = [];
  for (var i = 0; i < blobs.length; i++) {
    result.push(new Uint8Array(blobs[i]));
  }
  return result;
};


/**
 * String-ify bytes for text format. Should be optimized away in non-debug.
 * The returned string uses \xXX escapes for all values and is itself quoted.
 * [1, 31] serializes to '"\x01\x1f"'.
 * @param {jspb.ByteSource} byteSource The bytes to serialize.
 * @param {boolean=} opt_stringIsRawBytes The string is interpreted as a series
 * of raw bytes rather than base64 data.
 * @return {string} Stringified bytes for text format.
 */
jspb.utils.debugBytesToTextFormat = function(byteSource,
                                                opt_stringIsRawBytes) {
  var s = '"';
  if (byteSource) {
    var bytes =
        jspb.utils.byteSourceToUint8Array(byteSource, opt_stringIsRawBytes);
    for (var i = 0; i < bytes.length; i++) {
      s += '\\x';
      if (bytes[i] < 16) s += '0';
      s += bytes[i].toString(16);
    }
  }
  return s + '"';
};


/**
 * String-ify a scalar for text format. Should be optimized away in non-debug.
 * @param {string|number|boolean} scalar The scalar to stringify.
 * @return {string} Stringified scalar for text format.
 */
jspb.utils.debugScalarToTextFormat = function(scalar) {
  if (goog.isString(scalar)) {
    return goog.string.quote(scalar);
  } else {
    return scalar.toString();
  }
};


/**
 * Utility function: convert a string with codepoints 0--255 inclusive to a
 * Uint8Array. If any codepoints greater than 255 exist in the string, throws an
 * exception.
 * @param {string} str
 * @return {!Uint8Array}
 * @private
 */
jspb.utils.stringToByteArray_ = function(str) {
  var arr = new Uint8Array(str.length);
  for (var i = 0; i < str.length; i++) {
    var codepoint = str.charCodeAt(i);
    if (codepoint > 255) {
      throw new Error('Conversion error: string contains codepoint ' +
                      'outside of byte range');
    }
    arr[i] = codepoint;
  }
  return arr;
};


/**
 * Converts any type defined in jspb.ByteSource into a Uint8Array.
 * @param {!jspb.ByteSource} data
 * @param {boolean=} opt_stringIsRawBytes Interpret a string as a series of raw
 * bytes (encoded as codepoints 0--255 inclusive) rather than base64 data
 * (default behavior).
 * @return {!Uint8Array}
 * @suppress {invalidCasts}
 */
jspb.utils.byteSourceToUint8Array = function(data, opt_stringIsRawBytes) {
  if (data.constructor === Uint8Array) {
    return /** @type {!Uint8Array} */(data);
  }

  if (data.constructor === ArrayBuffer) {
    data = /** @type {!ArrayBuffer} */(data);
    return /** @type {!Uint8Array} */(new Uint8Array(data));
  }

  if (data.constructor === Array) {
    data = /** @type {!Array.<number>} */(data);
    return /** @type {!Uint8Array} */(new Uint8Array(data));
  }

  if (data.constructor === String) {
    data = /** @type {string} */(data);
    if (opt_stringIsRawBytes) {
      return jspb.utils.stringToByteArray_(data);
    } else {
      return goog.crypt.base64.decodeStringToUint8Array(data);
    }
  }

  goog.asserts.fail('Type not convertible to Uint8Array.');
  return /** @type {!Uint8Array} */(new Uint8Array(0));
};

// Protocol Buffers - Google's data interchange format
// Copyright 2008 Google Inc.  All rights reserved.
// https://developers.google.com/protocol-buffers/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/**
 * @fileoverview This file contains utilities for decoding primitive values
 * (signed and unsigned integers, varints, booleans, enums, hashes, strings,
 * and raw bytes) embedded in Uint8Arrays into their corresponding Javascript
 * types.
 *
 * Major caveat - Javascript is unable to accurately represent integers larger
 * than 2^53 due to its use of a double-precision floating point format or all
 * numbers. If you need to guarantee that 64-bit values survive with all bits
 * intact, you _must_ read them using one of the Hash64 methods, which return
 * an 8-character string.
 *
 * @author aappleby@google.com (Austin Appleby)
 */

goog.provide('jspb.BinaryDecoder');
goog.provide('jspb.BinaryIterator');

goog.require('goog.asserts');
goog.require('jspb.utils');



/**
 * Simple helper class for traversing the contents of repeated scalar fields.
 * that may or may not have been packed into a wire-format blob.
 * @param {?jspb.BinaryDecoder=} opt_decoder
 * @param {?function(this:jspb.BinaryDecoder):(number|boolean|string)=}
 *     opt_next The decoder method to use for next().
 * @param {?Array.<number|boolean|string>=} opt_elements
 * @constructor
 * @struct
 */
jspb.BinaryIterator = function(opt_decoder, opt_next, opt_elements) {
  /** @private {jspb.BinaryDecoder} */
  this.decoder_ = null;

  /**
   * The BinaryDecoder member function used when iterating over packed data.
   * @private {?function(this:jspb.BinaryDecoder):(number|boolean|string)}
   */
  this.nextMethod_ = null;

  /** @private {Array.<number>} */
  this.elements_ = null;

  /** @private {number} */
  this.cursor_ = 0;

  /** @private {number|boolean|string|null} */
  this.nextValue_ = null;

  /** @private {boolean} */
  this.atEnd_ = true;

  this.init_(opt_decoder, opt_next, opt_elements);
};


/**
 * @param {?jspb.BinaryDecoder=} opt_decoder
 * @param {?function(this:jspb.BinaryDecoder):(number|boolean|string)=}
 *     opt_next The decoder method to use for next().
 * @param {?Array.<number|boolean|string>=} opt_elements
 * @private
 */
jspb.BinaryIterator.prototype.init_ =
    function(opt_decoder, opt_next, opt_elements) {
  if (opt_decoder && opt_next) {
    this.decoder_ = opt_decoder;
    this.nextMethod_ = opt_next;
  }
  this.elements_ = opt_elements ? opt_elements : null;
  this.cursor_ = 0;
  this.nextValue_ = null;
  this.atEnd_ = !this.decoder_ && !this.elements_;

  this.next();
};


/**
 * Global pool of BinaryIterator instances.
 * @private {!Array.<!jspb.BinaryIterator>}
 */
jspb.BinaryIterator.instanceCache_ = [];


/**
 * Allocates a BinaryIterator from the cache, creating a new one if the cache
 * is empty.
 * @param {?jspb.BinaryDecoder=} opt_decoder
 * @param {?function(this:jspb.BinaryDecoder):(number|boolean|string)=}
 *     opt_next The decoder method to use for next().
 * @param {?Array.<number|boolean|string>=} opt_elements
 * @return {!jspb.BinaryIterator}
 */
jspb.BinaryIterator.alloc = function(opt_decoder, opt_next, opt_elements) {
  if (jspb.BinaryIterator.instanceCache_.length) {
    var iterator = jspb.BinaryIterator.instanceCache_.pop();
    iterator.init_(opt_decoder, opt_next, opt_elements);
    return iterator;
  } else {
    return new jspb.BinaryIterator(opt_decoder, opt_next, opt_elements);
  }
};


/**
 * Puts this instance back in the instance cache.
 */
jspb.BinaryIterator.prototype.free = function() {
  this.clear();
  if (jspb.BinaryIterator.instanceCache_.length < 100) {
    jspb.BinaryIterator.instanceCache_.push(this);
  }
};


/**
 * Clears the iterator.
 */
jspb.BinaryIterator.prototype.clear = function() {
  if (this.decoder_) {
    this.decoder_.free();
  }
  this.decoder_ = null;
  this.nextMethod_ = null;
  this.elements_ = null;
  this.cursor_ = 0;
  this.nextValue_ = null;
  this.atEnd_ = true;
};


/**
 * Returns the element at the iterator, or null if the iterator is invalid or
 * past the end of the decoder/array.
 * @return {number|boolean|string|null}
 */
jspb.BinaryIterator.prototype.get = function() {
  return this.nextValue_;
};


/**
 * Returns true if the iterator is at the end of the decoder/array.
 * @return {boolean}
 */
jspb.BinaryIterator.prototype.atEnd = function() {
  return this.atEnd_;
};


/**
 * Returns the element at the iterator and steps to the next element,
 * equivalent to '*pointer++' in C.
 * @return {number|boolean|string|null}
 */
jspb.BinaryIterator.prototype.next = function() {
  var lastValue = this.nextValue_;
  if (this.decoder_) {
    if (this.decoder_.atEnd()) {
      this.nextValue_ = null;
      this.atEnd_ = true;
    } else {
      this.nextValue_ = this.nextMethod_.call(this.decoder_);
    }
  } else if (this.elements_) {
    if (this.cursor_ == this.elements_.length) {
      this.nextValue_ = null;
      this.atEnd_ = true;
    } else {
      this.nextValue_ = this.elements_[this.cursor_++];
    }
  }
  return lastValue;
};



/**
 * BinaryDecoder implements the decoders for all the wire types specified in
 * https://developers.google.com/protocol-buffers/docs/encoding.
 *
 * @param {jspb.ByteSource=} opt_bytes The bytes we're reading from.
 * @param {number=} opt_start The optional offset to start reading at.
 * @param {number=} opt_length The optional length of the block to read -
 *     we'll throw an assertion if we go off the end of the block.
 * @constructor
 * @struct
 */
jspb.BinaryDecoder = function(opt_bytes, opt_start, opt_length) {
  /**
   * Typed byte-wise view of the source buffer.
   * @private {Uint8Array}
   */
  this.bytes_ = null;

  /**
   * Start point of the block to read.
   * @private {number}
   */
  this.start_ = 0;

  /**
   * End point of the block to read.
   * @private {number}
   */
  this.end_ = 0;

  /**
   * Current read location in bytes_.
   * @private {number}
   */
  this.cursor_ = 0;

  /**
   * Temporary storage for the low 32 bits of 64-bit data types that we're
   * decoding.
   * @private {number}
   */
  this.tempLow_ = 0;

  /**
   * Temporary storage for the high 32 bits of 64-bit data types that we're
   * decoding.
   * @private {number}
   */
  this.tempHigh_ = 0;

  /**
   * Set to true if this decoder encountered an error due to corrupt data.
   * @private {boolean}
   */
  this.error_ = false;

  if (opt_bytes) {
    this.setBlock(opt_bytes, opt_start, opt_length);
  }
};


/**
 * Global pool of BinaryDecoder instances.
 * @private {!Array.<!jspb.BinaryDecoder>}
 */
jspb.BinaryDecoder.instanceCache_ = [];


/**
 * Pops an instance off the instance cache, or creates one if the cache is
 * empty.
 * @param {jspb.ByteSource=} opt_bytes The bytes we're reading from.
 * @param {number=} opt_start The optional offset to start reading at.
 * @param {number=} opt_length The optional length of the block to read -
 *     we'll throw an assertion if we go off the end of the block.
 * @return {!jspb.BinaryDecoder}
 */
jspb.BinaryDecoder.alloc = function(opt_bytes, opt_start, opt_length) {
  if (jspb.BinaryDecoder.instanceCache_.length) {
    var newDecoder = jspb.BinaryDecoder.instanceCache_.pop();
    if (opt_bytes) {
      newDecoder.setBlock(opt_bytes, opt_start, opt_length);
    }
    return newDecoder;
  } else {
    return new jspb.BinaryDecoder(opt_bytes, opt_start, opt_length);
  }
};


/**
 * Puts this instance back in the instance cache.
 */
jspb.BinaryDecoder.prototype.free = function() {
  this.clear();
  if (jspb.BinaryDecoder.instanceCache_.length < 100) {
    jspb.BinaryDecoder.instanceCache_.push(this);
  }
};


/**
 * Makes a copy of this decoder.
 * @return {!jspb.BinaryDecoder}
 */
jspb.BinaryDecoder.prototype.clone = function() {
  return jspb.BinaryDecoder.alloc(this.bytes_,
      this.start_, this.end_ - this.start_);
};


/**
 * Clears the decoder.
 */
jspb.BinaryDecoder.prototype.clear = function() {
  this.bytes_ = null;
  this.start_ = 0;
  this.end_ = 0;
  this.cursor_ = 0;
  this.error_ = false;
};


/**
 * Returns the raw buffer.
 * @return {Uint8Array} The raw buffer.
 */
jspb.BinaryDecoder.prototype.getBuffer = function() {
  return this.bytes_;
};


/**
 * Changes the block of bytes we're decoding.
 * @param {!jspb.ByteSource} data The bytes we're reading from.
 * @param {number=} opt_start The optional offset to start reading at.
 * @param {number=} opt_length The optional length of the block to read -
 *     we'll throw an assertion if we go off the end of the block.
 */
jspb.BinaryDecoder.prototype.setBlock =
    function(data, opt_start, opt_length) {
  this.bytes_ = jspb.utils.byteSourceToUint8Array(data);
  this.start_ = goog.isDef(opt_start) ? opt_start : 0;
  this.end_ =
      goog.isDef(opt_length) ? this.start_ + opt_length : this.bytes_.length;
  this.cursor_ = this.start_;
};


/**
 * @return {number}
 */
jspb.BinaryDecoder.prototype.getEnd = function() {
  return this.end_;
};


/**
 * @param {number} end
 */
jspb.BinaryDecoder.prototype.setEnd = function(end) {
  this.end_ = end;
};


/**
 * Moves the read cursor back to the start of the block.
 */
jspb.BinaryDecoder.prototype.reset = function() {
  this.cursor_ = this.start_;
};


/**
 * Returns the internal read cursor.
 * @return {number} The internal read cursor.
 */
jspb.BinaryDecoder.prototype.getCursor = function() {
  return this.cursor_;
};


/**
 * Returns the internal read cursor.
 * @param {number} cursor The new cursor.
 */
jspb.BinaryDecoder.prototype.setCursor = function(cursor) {
  this.cursor_ = cursor;
};


/**
 * Advances the stream cursor by the given number of bytes.
 * @param {number} count The number of bytes to advance by.
 */
jspb.BinaryDecoder.prototype.advance = function(count) {
  this.cursor_ += count;
  goog.asserts.assert(this.cursor_ <= this.end_);
};


/**
 * Returns true if this decoder is at the end of the block.
 * @return {boolean}
 */
jspb.BinaryDecoder.prototype.atEnd = function() {
  return this.cursor_ == this.end_;
};


/**
 * Returns true if this decoder is at the end of the block.
 * @return {boolean}
 */
jspb.BinaryDecoder.prototype.pastEnd = function() {
  return this.cursor_ > this.end_;
};


/**
 * Returns true if this decoder encountered an error due to corrupt data.
 * @return {boolean}
 */
jspb.BinaryDecoder.prototype.getError = function() {
  return this.error_ ||
         (this.cursor_ < 0) ||
         (this.cursor_ > this.end_);
};


/**
 * Reads an unsigned varint from the binary stream and stores it as a split
 * 64-bit integer. Since this does not convert the value to a number, no
 * precision is lost.
 *
 * It's possible for an unsigned varint to be incorrectly encoded - more than
 * 64 bits' worth of data could be present. If this happens, this method will
 * throw an error.
 *
 * Decoding varints requires doing some funny base-128 math - for more
 * details on the format, see
 * https://developers.google.com/protocol-buffers/docs/encoding
 *
 * @private
 */
jspb.BinaryDecoder.prototype.readSplitVarint64_ = function() {
  var temp;
  var lowBits = 0;
  var highBits = 0;

  // Read the first four bytes of the varint, stopping at the terminator if we
  // see it.
  for (var i = 0; i < 4; i++) {
    temp = this.bytes_[this.cursor_++];
    lowBits |= (temp & 0x7F) << (i * 7);
    if (temp < 128) {
      this.tempLow_ = lowBits >>> 0;
      this.tempHigh_ = 0;
      return;
    }
  }

  // Read the fifth byte, which straddles the low and high dwords.
  temp = this.bytes_[this.cursor_++];
  lowBits |= (temp & 0x7F) << 28;
  highBits |= (temp & 0x7F) >> 4;
  if (temp < 128) {
    this.tempLow_ = lowBits >>> 0;
    this.tempHigh_ = highBits >>> 0;
    return;
  }

  // Read the sixth through tenth byte.
  for (var i = 0; i < 5; i++) {
    temp = this.bytes_[this.cursor_++];
    highBits |= (temp & 0x7F) << (i * 7 + 3);
    if (temp < 128) {
      this.tempLow_ = lowBits >>> 0;
      this.tempHigh_ = highBits >>> 0;
      return;
    }
  }

  // If we did not see the terminator, the encoding was invalid.
  goog.asserts.fail('Failed to read varint, encoding is invalid.');
  this.error_ = true;
};


/**
 * Skips over a varint in the block without decoding it.
 */
jspb.BinaryDecoder.prototype.skipVarint = function() {
  while (this.bytes_[this.cursor_] & 0x80) {
    this.cursor_++;
  }
  this.cursor_++;
};


/**
 * Skips backwards over a varint in the block - to do this correctly, we have
 * to know the value we're skipping backwards over or things are ambiguous.
 * @param {number} value The varint value to unskip.
 */
jspb.BinaryDecoder.prototype.unskipVarint = function(value) {
  while (value > 128) {
    this.cursor_--;
    value = value >>> 7;
  }
  this.cursor_--;
};


/**
 * Reads a 32-bit varint from the binary stream. Due to a quirk of the encoding
 * format and Javascript's handling of bitwise math, this actually works
 * correctly for both signed and unsigned 32-bit varints.
 *
 * This function is called vastly more frequently than any other in
 * BinaryDecoder, so it has been unrolled and tweaked for performance.
 *
 * If there are more than 32 bits of data in the varint, it _must_ be due to
 * sign-extension. If we're in debug mode and the high 32 bits don't match the
 * expected sign extension, this method will throw an error.
 *
 * Decoding varints requires doing some funny base-128 math - for more
 * details on the format, see
 * https://developers.google.com/protocol-buffers/docs/encoding
 *
 * @return {number} The decoded unsigned 32-bit varint.
 */
jspb.BinaryDecoder.prototype.readUnsignedVarint32 = function() {
  var temp;
  var bytes = this.bytes_;

  temp = bytes[this.cursor_ + 0];
  var x = (temp & 0x7F);
  if (temp < 128) {
    this.cursor_ += 1;
    goog.asserts.assert(this.cursor_ <= this.end_);
    return x;
  }

  temp = bytes[this.cursor_ + 1];
  x |= (temp & 0x7F) << 7;
  if (temp < 128) {
    this.cursor_ += 2;
    goog.asserts.assert(this.cursor_ <= this.end_);
    return x;
  }

  temp = bytes[this.cursor_ + 2];
  x |= (temp & 0x7F) << 14;
  if (temp < 128) {
    this.cursor_ += 3;
    goog.asserts.assert(this.cursor_ <= this.end_);
    return x;
  }

  temp = bytes[this.cursor_ + 3];
  x |= (temp & 0x7F) << 21;
  if (temp < 128) {
    this.cursor_ += 4;
    goog.asserts.assert(this.cursor_ <= this.end_);
    return x;
  }

  temp = bytes[this.cursor_ + 4];
  x |= (temp & 0x0F) << 28;
  if (temp < 128) {
    // We're reading the high bits of an unsigned varint. The byte we just read
    // also contains bits 33 through 35, which we're going to discard. Those
    // bits _must_ be zero, or the encoding is invalid.
    goog.asserts.assert((temp & 0xF0) == 0);
    this.cursor_ += 5;
    goog.asserts.assert(this.cursor_ <= this.end_);
    return x >>> 0;
  }

  // If we get here, we're reading the sign extension of a negative 32-bit int.
  // We can skip these bytes, as we know in advance that they have to be all
  // 1's if the varint is correctly encoded. Since we also know the value is
  // negative, we don't have to coerce it to unsigned before we return it.

  goog.asserts.assert((temp & 0xF0) == 0xF0);
  goog.asserts.assert(bytes[this.cursor_ + 5] == 0xFF);
  goog.asserts.assert(bytes[this.cursor_ + 6] == 0xFF);
  goog.asserts.assert(bytes[this.cursor_ + 7] == 0xFF);
  goog.asserts.assert(bytes[this.cursor_ + 8] == 0xFF);
  goog.asserts.assert(bytes[this.cursor_ + 9] == 0x01);

  this.cursor_ += 10;
  goog.asserts.assert(this.cursor_ <= this.end_);
  return x;
};


/**
 * The readUnsignedVarint32 above deals with signed 32-bit varints correctly,
 * so this is just an alias.
 *
 * @return {number} The decoded signed 32-bit varint.
 */
jspb.BinaryDecoder.prototype.readSignedVarint32 =
    jspb.BinaryDecoder.prototype.readUnsignedVarint32;


/**
 * Reads a 32-bit unsigned variant and returns its value as a string.
 *
 * @return {string} The decoded unsigned 32-bit varint as a string.
 */
jspb.BinaryDecoder.prototype.readUnsignedVarint32String = function() {
  // 32-bit integers fit in JavaScript numbers without loss of precision, so
  // string variants of 32-bit varint readers can simply delegate then convert
  // to string.
  var value = this.readUnsignedVarint32();
  return value.toString();
};

/**
 * Reads a 32-bit signed variant and returns its value as a string.
 *
 * @return {string} The decoded signed 32-bit varint as a string.
 */
jspb.BinaryDecoder.prototype.readSignedVarint32String = function() {
  // 32-bit integers fit in JavaScript numbers without loss of precision, so
  // string variants of 32-bit varint readers can simply delegate then convert
  // to string.
  var value = this.readSignedVarint32();
  return value.toString();
};


/**
 * Reads a signed, zigzag-encoded 32-bit varint from the binary stream.
 *
 * Zigzag encoding is a modification of varint encoding that reduces the
 * storage overhead for small negative integers - for more details on the
 * format, see https://developers.google.com/protocol-buffers/docs/encoding
 *
 * @return {number} The decoded signed, zigzag-encoded 32-bit varint.
 */
jspb.BinaryDecoder.prototype.readZigzagVarint32 = function() {
  var result = this.readUnsignedVarint32();
  return (result >>> 1) ^ - (result & 1);
};


/**
 * Reads an unsigned 64-bit varint from the binary stream. Note that since
 * Javascript represents all numbers as double-precision floats, there will be
 * precision lost if the absolute value of the varint is larger than 2^53.
 *
 * @return {number} The decoded unsigned varint. Precision will be lost if the
 *     integer exceeds 2^53.
 */
jspb.BinaryDecoder.prototype.readUnsignedVarint64 = function() {
  this.readSplitVarint64_();
  return jspb.utils.joinUint64(this.tempLow_, this.tempHigh_);
};


/**
 * Reads an unsigned 64-bit varint from the binary stream and returns the value
 * as a decimal string.
 *
 * @return {string} The decoded unsigned varint as a decimal string.
 */
jspb.BinaryDecoder.prototype.readUnsignedVarint64String = function() {
  this.readSplitVarint64_();
  return jspb.utils.joinUnsignedDecimalString(this.tempLow_, this.tempHigh_);
};


/**
 * Reads a signed 64-bit varint from the binary stream. Note that since
 * Javascript represents all numbers as double-precision floats, there will be
 * precision lost if the absolute value of the varint is larger than 2^53.
 *
 * @return {number} The decoded signed varint. Precision will be lost if the
 *     integer exceeds 2^53.
 */
jspb.BinaryDecoder.prototype.readSignedVarint64 = function() {
  this.readSplitVarint64_();
  return jspb.utils.joinInt64(this.tempLow_, this.tempHigh_);
};


/**
 * Reads an signed 64-bit varint from the binary stream and returns the value
 * as a decimal string.
 *
 * @return {string} The decoded signed varint as a decimal string.
 */
jspb.BinaryDecoder.prototype.readSignedVarint64String = function() {
  this.readSplitVarint64_();
  return jspb.utils.joinSignedDecimalString(this.tempLow_, this.tempHigh_);
};


/**
 * Reads a signed, zigzag-encoded 64-bit varint from the binary stream. Note
 * that since Javascript represents all numbers as double-precision floats,
 * there will be precision lost if the absolute value of the varint is larger
 * than 2^53.
 *
 * Zigzag encoding is a modification of varint encoding that reduces the
 * storage overhead for small negative integers - for more details on the
 * format, see https://developers.google.com/protocol-buffers/docs/encoding
 *
 * @return {number} The decoded zigzag varint. Precision will be lost if the
 *     integer exceeds 2^53.
 */
jspb.BinaryDecoder.prototype.readZigzagVarint64 = function() {
  this.readSplitVarint64_();
  return jspb.utils.joinZigzag64(this.tempLow_, this.tempHigh_);
};


/**
 * Reads a raw unsigned 8-bit integer from the binary stream.
 *
 * @return {number} The unsigned 8-bit integer read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readUint8 = function() {
  var a = this.bytes_[this.cursor_ + 0];
  this.cursor_ += 1;
  goog.asserts.assert(this.cursor_ <= this.end_);
  return a;
};


/**
 * Reads a raw unsigned 16-bit integer from the binary stream.
 *
 * @return {number} The unsigned 16-bit integer read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readUint16 = function() {
  var a = this.bytes_[this.cursor_ + 0];
  var b = this.bytes_[this.cursor_ + 1];
  this.cursor_ += 2;
  goog.asserts.assert(this.cursor_ <= this.end_);
  return (a << 0) | (b << 8);
};


/**
 * Reads a raw unsigned 32-bit integer from the binary stream.
 *
 * @return {number} The unsigned 32-bit integer read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readUint32 = function() {
  var a = this.bytes_[this.cursor_ + 0];
  var b = this.bytes_[this.cursor_ + 1];
  var c = this.bytes_[this.cursor_ + 2];
  var d = this.bytes_[this.cursor_ + 3];
  this.cursor_ += 4;
  goog.asserts.assert(this.cursor_ <= this.end_);
  return ((a << 0) | (b << 8) | (c << 16) | (d << 24)) >>> 0;
};


/**
 * Reads a raw unsigned 64-bit integer from the binary stream. Note that since
 * Javascript represents all numbers as double-precision floats, there will be
 * precision lost if the absolute value of the integer is larger than 2^53.
 *
 * @return {number} The unsigned 64-bit integer read from the binary stream.
 *     Precision will be lost if the integer exceeds 2^53.
 */
jspb.BinaryDecoder.prototype.readUint64 = function() {
  var bitsLow = this.readUint32();
  var bitsHigh = this.readUint32();
  return jspb.utils.joinUint64(bitsLow, bitsHigh);
};


/**
 * Reads a raw signed 8-bit integer from the binary stream.
 *
 * @return {number} The signed 8-bit integer read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readInt8 = function() {
  var a = this.bytes_[this.cursor_ + 0];
  this.cursor_ += 1;
  goog.asserts.assert(this.cursor_ <= this.end_);
  return (a << 24) >> 24;
};


/**
 * Reads a raw signed 16-bit integer from the binary stream.
 *
 * @return {number} The signed 16-bit integer read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readInt16 = function() {
  var a = this.bytes_[this.cursor_ + 0];
  var b = this.bytes_[this.cursor_ + 1];
  this.cursor_ += 2;
  goog.asserts.assert(this.cursor_ <= this.end_);
  return (((a << 0) | (b << 8)) << 16) >> 16;
};


/**
 * Reads a raw signed 32-bit integer from the binary stream.
 *
 * @return {number} The signed 32-bit integer read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readInt32 = function() {
  var a = this.bytes_[this.cursor_ + 0];
  var b = this.bytes_[this.cursor_ + 1];
  var c = this.bytes_[this.cursor_ + 2];
  var d = this.bytes_[this.cursor_ + 3];
  this.cursor_ += 4;
  goog.asserts.assert(this.cursor_ <= this.end_);
  return (a << 0) | (b << 8) | (c << 16) | (d << 24);
};


/**
 * Reads a raw signed 64-bit integer from the binary stream. Note that since
 * Javascript represents all numbers as double-precision floats, there will be
 * precision lost if the absolute vlaue of the integer is larger than 2^53.
 *
 * @return {number} The signed 64-bit integer read from the binary stream.
 *     Precision will be lost if the integer exceeds 2^53.
 */
jspb.BinaryDecoder.prototype.readInt64 = function() {
  var bitsLow = this.readUint32();
  var bitsHigh = this.readUint32();
  return jspb.utils.joinInt64(bitsLow, bitsHigh);
};


/**
 * Reads a 32-bit floating-point number from the binary stream, using the
 * temporary buffer to realign the data.
 *
 * @return {number} The float read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readFloat = function() {
  var bitsLow = this.readUint32();
  var bitsHigh = 0;
  return jspb.utils.joinFloat32(bitsLow, bitsHigh);
};


/**
 * Reads a 64-bit floating-point number from the binary stream, using the
 * temporary buffer to realign the data.
 *
 * @return {number} The double read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readDouble = function() {
  var bitsLow = this.readUint32();
  var bitsHigh = this.readUint32();
  return jspb.utils.joinFloat64(bitsLow, bitsHigh);
};


/**
 * Reads a boolean value from the binary stream.
 * @return {boolean} The boolean read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readBool = function() {
  return !!this.bytes_[this.cursor_++];
};


/**
 * Reads an enum value from the binary stream, which are always encoded as
 * signed varints.
 * @return {number} The enum value read from the binary stream.
 */
jspb.BinaryDecoder.prototype.readEnum = function() {
  return this.readSignedVarint32();
};


/**
 * Reads and parses a UTF-8 encoded unicode string from the stream.
 * The code is inspired by maps.vectortown.parse.StreamedDataViewReader, with
 * the exception that the implementation here does not get confused if it
 * encounters characters longer than three bytes. These characters are ignored
 * though, as they are extremely rare: three UTF-8 bytes cover virtually all
 * characters in common use (http://en.wikipedia.org/wiki/UTF-8).
 * @param {number} length The length of the string to read.
 * @return {string} The decoded string.
 */
jspb.BinaryDecoder.prototype.readString = function(length) {
  var bytes = this.bytes_;
  var cursor = this.cursor_;
  var end = cursor + length;
  var chars = [];

  while (cursor < end) {
    var c = bytes[cursor++];
    if (c < 128) { // Regular 7-bit ASCII.
      chars.push(c);
    } else if (c < 192) {
      // UTF-8 continuation mark. We are out of sync. This
      // might happen if we attempted to read a character
      // with more than three bytes.
      continue;
    } else if (c < 224) { // UTF-8 with two bytes.
      var c2 = bytes[cursor++];
      chars.push(((c & 31) << 6) | (c2 & 63));
    } else if (c < 240) { // UTF-8 with three bytes.
      var c2 = bytes[cursor++];
      var c3 = bytes[cursor++];
      chars.push(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
    }
  }

  // String.fromCharCode.apply is faster than manually appending characters on
  // Chrome 25+, and generates no additional cons string garbage.
  var result = String.fromCharCode.apply(null, chars);
  this.cursor_ = cursor;
  return result;
};


/**
 * Reads and parses a UTF-8 encoded unicode string (with length prefix) from
 * the stream.
 * @return {string} The decoded string.
 */
jspb.BinaryDecoder.prototype.readStringWithLength = function() {
  var length = this.readUnsignedVarint32();
  return this.readString(length);
};


/**
 * Reads a block of raw bytes from the binary stream.
 *
 * @param {number} length The number of bytes to read.
 * @return {Uint8Array} The decoded block of bytes, or null if the length was
 *     invalid.
 */
jspb.BinaryDecoder.prototype.readBytes = function(length) {
  if (length < 0 ||
      this.cursor_ + length > this.bytes_.length) {
    this.error_ = true;
    return null;
  }

  var result = this.bytes_.subarray(this.cursor_, this.cursor_ + length);

  this.cursor_ += length;
  goog.asserts.assert(this.cursor_ <= this.end_);
  return result;
};


/**
 * Reads a 64-bit varint from the stream and returns it as an 8-character
 * Unicode string for use as a hash table key.
 *
 * @return {string} The hash value.
 */
jspb.BinaryDecoder.prototype.readVarintHash64 = function() {
  this.readSplitVarint64_();
  return jspb.utils.joinHash64(this.tempLow_, this.tempHigh_);
};


/**
 * Reads a 64-bit fixed-width value from the stream and returns it as an
 * 8-character Unicode string for use as a hash table key.
 *
 * @return {string} The hash value.
 */
jspb.BinaryDecoder.prototype.readFixedHash64 = function() {
  var bytes = this.bytes_;
  var cursor = this.cursor_;

  var a = bytes[cursor + 0];
  var b = bytes[cursor + 1];
  var c = bytes[cursor + 2];
  var d = bytes[cursor + 3];
  var e = bytes[cursor + 4];
  var f = bytes[cursor + 5];
  var g = bytes[cursor + 6];
  var h = bytes[cursor + 7];

  this.cursor_ += 8;

  return String.fromCharCode(a, b, c, d, e, f, g, h);
};

// Protocol Buffers - Google's data interchange format
// Copyright 2008 Google Inc.  All rights reserved.
// https://developers.google.com/protocol-buffers/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/**
 * @fileoverview This file contains utilities for converting binary,
 * wire-format protocol buffers into Javascript data structures.
 *
 * jspb's BinaryReader class wraps the BinaryDecoder class to add methods
 * that understand the protocol buffer syntax and can do the type checking and
 * bookkeeping necessary to parse trees of nested messages.
 *
 * Major caveat - Users of this library _must_ keep their Javascript proto
 * parsing code in sync with the original .proto file - presumably you'll be
 * using the typed jspb code generator, but if you bypass that you'll need
 * to keep things in sync by hand.
 *
 * @author aappleby@google.com (Austin Appleby)
 */

goog.provide('jspb.BinaryReader');

goog.require('goog.asserts');
goog.require('jspb.BinaryConstants');
goog.require('jspb.BinaryDecoder');



/**
 * BinaryReader implements the decoders for all the wire types specified in
 * https://developers.google.com/protocol-buffers/docs/encoding.
 *
 * @param {jspb.ByteSource=} opt_bytes The bytes we're reading from.
 * @param {number=} opt_start The optional offset to start reading at.
 * @param {number=} opt_length The optional length of the block to read -
 *     we'll throw an assertion if we go off the end of the block.
 * @constructor
 * @struct
 */
jspb.BinaryReader = function(opt_bytes, opt_start, opt_length) {
  /**
   * Wire-format decoder.
   * @private {!jspb.BinaryDecoder}
   */
  this.decoder_ = jspb.BinaryDecoder.alloc(opt_bytes, opt_start, opt_length);

  /**
   * Cursor immediately before the field tag.
   * @private {number}
   */
  this.fieldCursor_ = this.decoder_.getCursor();

  /**
   * Field number of the next field in the buffer, filled in by nextField().
   * @private {number}
   */
  this.nextField_ = jspb.BinaryConstants.INVALID_FIELD_NUMBER;

  /**
   * Wire type of the next proto field in the buffer, filled in by
   * nextField().
   * @private {jspb.BinaryConstants.WireType}
   */
  this.nextWireType_ = jspb.BinaryConstants.WireType.INVALID;

  /**
   * Set to true if this reader encountered an error due to corrupt data.
   * @private {boolean}
   */
  this.error_ = false;

  /**
   * User-defined reader callbacks.
   * @private {Object.<string, function(!jspb.BinaryReader):*>}
   */
  this.readCallbacks_ = null;
};


/**
 * Global pool of BinaryReader instances.
 * @private {!Array.<!jspb.BinaryReader>}
 */
jspb.BinaryReader.instanceCache_ = [];


/**
 * Pops an instance off the instance cache, or creates one if the cache is
 * empty.
 * @param {jspb.ByteSource=} opt_bytes The bytes we're reading from.
 * @param {number=} opt_start The optional offset to start reading at.
 * @param {number=} opt_length The optional length of the block to read -
 *     we'll throw an assertion if we go off the end of the block.
 * @return {!jspb.BinaryReader}
 */
jspb.BinaryReader.alloc =
    function(opt_bytes, opt_start, opt_length) {
  if (jspb.BinaryReader.instanceCache_.length) {
    var newReader = jspb.BinaryReader.instanceCache_.pop();
    if (opt_bytes) {
      newReader.decoder_.setBlock(opt_bytes, opt_start, opt_length);
    }
    return newReader;
  } else {
    return new jspb.BinaryReader(opt_bytes, opt_start, opt_length);
  }
};


/**
 * Alias for the above method.
 * @param {jspb.ByteSource=} opt_bytes The bytes we're reading from.
 * @param {number=} opt_start The optional offset to start reading at.
 * @param {number=} opt_length The optional length of the block to read -
 *     we'll throw an assertion if we go off the end of the block.
 * @return {!jspb.BinaryReader}
 */
jspb.BinaryReader.prototype.alloc = jspb.BinaryReader.alloc;


/**
 * Puts this instance back in the instance cache.
 */
jspb.BinaryReader.prototype.free = function() {
  this.decoder_.clear();
  this.nextField_ = jspb.BinaryConstants.INVALID_FIELD_NUMBER;
  this.nextWireType_ = jspb.BinaryConstants.WireType.INVALID;
  this.error_ = false;
  this.readCallbacks_ = null;

  if (jspb.BinaryReader.instanceCache_.length < 100) {
    jspb.BinaryReader.instanceCache_.push(this);
  }
};


/**
 * Returns the cursor immediately before the current field's tag.
 * @return {number} The internal read cursor.
 */
jspb.BinaryReader.prototype.getFieldCursor = function() {
  return this.fieldCursor_;
};


/**
 * Returns the internal read cursor.
 * @return {number} The internal read cursor.
 */
jspb.BinaryReader.prototype.getCursor = function() {
  return this.decoder_.getCursor();
};


/**
 * Returns the raw buffer.
 * @return {Uint8Array} The raw buffer.
 */
jspb.BinaryReader.prototype.getBuffer = function() {
  return this.decoder_.getBuffer();
};


/**
 * @return {number} The field number of the next field in the buffer, or
 *     INVALID_FIELD_NUMBER if there is no next field.
 */
jspb.BinaryReader.prototype.getFieldNumber = function() {
  return this.nextField_;
};


/**
 * @return {jspb.BinaryConstants.WireType} The wire type of the next field
 *     in the stream, or WireType.INVALID if there is no next field.
 */
jspb.BinaryReader.prototype.getWireType = function() {
  return this.nextWireType_;
};


/**
 * @return {boolean} Whether the current wire type is an end-group tag. Used as
 * an exit condition in decoder loops in generated code.
 */
jspb.BinaryReader.prototype.isEndGroup = function() {
  return this.nextWireType_ == jspb.BinaryConstants.WireType.END_GROUP;
};


/**
 * Returns true if this reader hit an error due to corrupt data.
 * @return {boolean}
 */
jspb.BinaryReader.prototype.getError = function() {
  return this.error_ || this.decoder_.getError();
};


/**
 * Points this reader at a new block of bytes.
 * @param {!Uint8Array} bytes The block of bytes we're reading from.
 * @param {number} start The offset to start reading at.
 * @param {number} length The length of the block to read.
 */
jspb.BinaryReader.prototype.setBlock = function(bytes, start, length) {
  this.decoder_.setBlock(bytes, start, length);
  this.nextField_ = jspb.BinaryConstants.INVALID_FIELD_NUMBER;
  this.nextWireType_ = jspb.BinaryConstants.WireType.INVALID;
};


/**
 * Rewinds the stream cursor to the beginning of the buffer and resets all
 * internal state.
 */
jspb.BinaryReader.prototype.reset = function() {
  this.decoder_.reset();
  this.nextField_ = jspb.BinaryConstants.INVALID_FIELD_NUMBER;
  this.nextWireType_ = jspb.BinaryConstants.WireType.INVALID;
};


/**
 * Advances the stream cursor by the given number of bytes.
 * @param {number} count The number of bytes to advance by.
 */
jspb.BinaryReader.prototype.advance = function(count) {
  this.decoder_.advance(count);
};


/**
 * Reads the next field header in the stream if there is one, returns true if
 * we saw a valid field header or false if we've read the whole stream.
 * Throws an error if we encountered a deprecated START_GROUP/END_GROUP field.
 * @return {boolean} True if the stream contains more fields.
 */
jspb.BinaryReader.prototype.nextField = function() {
  // If we're at the end of the block, there are no more fields.
  if (this.decoder_.atEnd()) {
    return false;
  }

  // If we hit an error decoding the previous field, stop now before we
  // try to decode anything else
  if (this.getError()) {
    goog.asserts.fail('Decoder hit an error');
    return false;
  }

  // Otherwise just read the header of the next field.
  this.fieldCursor_ = this.decoder_.getCursor();
  var header = this.decoder_.readUnsignedVarint32();

  var nextField = header >>> 3;
  var nextWireType = /** @type {jspb.BinaryConstants.WireType} */
      (header & 0x7);

  // If the wire type isn't one of the valid ones, something's broken.
  if (nextWireType != jspb.BinaryConstants.WireType.VARINT &&
      nextWireType != jspb.BinaryConstants.WireType.FIXED32 &&
      nextWireType != jspb.BinaryConstants.WireType.FIXED64 &&
      nextWireType != jspb.BinaryConstants.WireType.DELIMITED &&
      nextWireType != jspb.BinaryConstants.WireType.START_GROUP &&
      nextWireType != jspb.BinaryConstants.WireType.END_GROUP) {
    goog.asserts.fail('Invalid wire type');
    this.error_ = true;
    return false;
  }

  this.nextField_ = nextField;
  this.nextWireType_ = nextWireType;

  return true;
};


/**
 * Winds the reader back to just before this field's header.
 */
jspb.BinaryReader.prototype.unskipHeader = function() {
  this.decoder_.unskipVarint((this.nextField_ << 3) | this.nextWireType_);
};


/**
 * Skips all contiguous fields whose header matches the one we just read.
 */
jspb.BinaryReader.prototype.skipMatchingFields = function() {
  var field = this.nextField_;
  this.unskipHeader();

  while (this.nextField() && (this.getFieldNumber() == field)) {
    this.skipField();
  }

  if (!this.decoder_.atEnd()) {
    this.unskipHeader();
  }
};


/**
 * Skips over the next varint field in the binary stream.
 */
jspb.BinaryReader.prototype.skipVarintField = function() {
  if (this.nextWireType_ != jspb.BinaryConstants.WireType.VARINT) {
    goog.asserts.fail('Invalid wire type for skipVarintField');
    this.skipField();
    return;
  }

  this.decoder_.skipVarint();
};


/**
 * Skips over the next delimited field in the binary stream.
 */
jspb.BinaryReader.prototype.skipDelimitedField = function() {
  if (this.nextWireType_ != jspb.BinaryConstants.WireType.DELIMITED) {
    goog.asserts.fail('Invalid wire type for skipDelimitedField');
    this.skipField();
    return;
  }

  var length = this.decoder_.readUnsignedVarint32();
  this.decoder_.advance(length);
};


/**
 * Skips over the next fixed32 field in the binary stream.
 */
jspb.BinaryReader.prototype.skipFixed32Field = function() {
  if (this.nextWireType_ != jspb.BinaryConstants.WireType.FIXED32) {
    goog.asserts.fail('Invalid wire type for skipFixed32Field');
    this.skipField();
    return;
  }

  this.decoder_.advance(4);
};


/**
 * Skips over the next fixed64 field in the binary stream.
 */
jspb.BinaryReader.prototype.skipFixed64Field = function() {
  if (this.nextWireType_ != jspb.BinaryConstants.WireType.FIXED64) {
    goog.asserts.fail('Invalid wire type for skipFixed64Field');
    this.skipField();
    return;
  }

  this.decoder_.advance(8);
};


/**
 * Skips over the next group field in the binary stream.
 */
jspb.BinaryReader.prototype.skipGroup = function() {
  // Keep a stack of start-group tags that must be matched by end-group tags.
  var nestedGroups = [this.nextField_];
  do {
    if (!this.nextField()) {
      goog.asserts.fail('Unmatched start-group tag: stream EOF');
      this.error_ = true;
      return;
    }
    if (this.nextWireType_ ==
        jspb.BinaryConstants.WireType.START_GROUP) {
      // Nested group start.
      nestedGroups.push(this.nextField_);
    } else if (this.nextWireType_ ==
               jspb.BinaryConstants.WireType.END_GROUP) {
      // Group end: check that it matches top-of-stack.
      if (this.nextField_ != nestedGroups.pop()) {
        goog.asserts.fail('Unmatched end-group tag');
        this.error_ = true;
        return;
      }
    }
  } while (nestedGroups.length > 0);
};


/**
 * Skips over the next field in the binary stream - this is useful if we're
 * decoding a message that contain unknown fields.
 */
jspb.BinaryReader.prototype.skipField = function() {
  switch (this.nextWireType_) {
    case jspb.BinaryConstants.WireType.VARINT:
      this.skipVarintField();
      break;
    case jspb.BinaryConstants.WireType.FIXED64:
      this.skipFixed64Field();
      break;
    case jspb.BinaryConstants.WireType.DELIMITED:
      this.skipDelimitedField();
      break;
    case jspb.BinaryConstants.WireType.FIXED32:
      this.skipFixed32Field();
      break;
    case jspb.BinaryConstants.WireType.START_GROUP:
      this.skipGroup();
      break;
    default:
      goog.asserts.fail('Invalid wire encoding for field.');
  }
};


/**
 * Registers a user-defined read callback.
 * @param {string} callbackName
 * @param {function(!jspb.BinaryReader):*} callback
 */
jspb.BinaryReader.prototype.registerReadCallback =
    function(callbackName, callback) {
  if (goog.isNull(this.readCallbacks_)) {
    this.readCallbacks_ = {};
  }
  goog.asserts.assert(!this.readCallbacks_[callbackName]);
  this.readCallbacks_[callbackName] = callback;
};


/**
 * Runs a registered read callback.
 * @param {string} callbackName The name the callback is registered under.
 * @return {*} The value returned by the callback.
 */
jspb.BinaryReader.prototype.runReadCallback = function(callbackName) {
  goog.asserts.assert(!goog.isNull(this.readCallbacks_));
  var callback = this.readCallbacks_[callbackName];
  goog.asserts.assert(callback);
  return callback(this);
};


/**
 * Reads a field of any valid non-message type from the binary stream.
 * @param {jspb.BinaryConstants.FieldType} fieldType
 * @return {jspb.AnyFieldType}
 */
jspb.BinaryReader.prototype.readAny = function(fieldType) {
  this.nextWireType_ = jspb.BinaryConstants.FieldTypeToWireType(fieldType);
  var fieldTypes = jspb.BinaryConstants.FieldType;
  switch (fieldType) {
    case fieldTypes.DOUBLE:
      return this.readDouble();
    case fieldTypes.FLOAT:
      return this.readFloat();
    case fieldTypes.INT64:
      return this.readInt64();
    case fieldTypes.UINT64:
      return this.readUint64();
    case fieldTypes.INT32:
      return this.readInt32();
    case fieldTypes.FIXED64:
      return this.readFixed64();
    case fieldTypes.FIXED32:
      return this.readFixed32();
    case fieldTypes.BOOL:
      return this.readBool();
    case fieldTypes.STRING:
      return this.readString();
    case fieldTypes.GROUP:
      goog.asserts.fail('Group field type not supported in readAny()');
    case fieldTypes.MESSAGE:
      goog.asserts.fail('Message field type not supported in readAny()');
    case fieldTypes.BYTES:
      return this.readBytes();
    case fieldTypes.UINT32:
      return this.readUint32();
    case fieldTypes.ENUM:
      return this.readEnum();
    case fieldTypes.SFIXED32:
      return this.readSfixed32();
    case fieldTypes.SFIXED64:
      return this.readSfixed64();
    case fieldTypes.SINT32:
      return this.readSint32();
    case fieldTypes.SINT64:
      return this.readSint64();
    case fieldTypes.FHASH64:
      return this.readFixedHash64();
    case fieldTypes.VHASH64:
      return this.readVarintHash64();
    default:
      goog.asserts.fail('Invalid field type in readAny()');
  }
  return 0;
};


/**
 * Deserialize a proto into the provided message object using the provided
 * reader function. This function is templated as we currently have one client
 * who is using manual deserialization instead of the code-generated versions.
 * @template T
 * @param {T} message
 * @param {function(T, !jspb.BinaryReader)} reader
 */
jspb.BinaryReader.prototype.readMessage = function(message, reader) {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.DELIMITED);

  // Save the current endpoint of the decoder and move it to the end of the
  // embedded message.
  var oldEnd = this.decoder_.getEnd();
  var length = this.decoder_.readUnsignedVarint32();
  var newEnd = this.decoder_.getCursor() + length;
  this.decoder_.setEnd(newEnd);

  // Deserialize the embedded message.
  reader(message, this);

  // Advance the decoder past the embedded message and restore the endpoint.
  this.decoder_.setCursor(newEnd);
  this.decoder_.setEnd(oldEnd);
};


/**
 * Deserialize a proto into the provided message object using the provided
 * reader function, assuming that the message is serialized as a group
 * with the given tag.
 * @template T
 * @param {number} field
 * @param {T} message
 * @param {function(T, !jspb.BinaryReader)} reader
 */
jspb.BinaryReader.prototype.readGroup =
    function(field, message, reader) {
  // Ensure that the wire type is correct.
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.START_GROUP);
  // Ensure that the field number is correct.
  goog.asserts.assert(this.nextField_ == field);

  // Deserialize the message. The deserialization will stop at an END_GROUP tag.
  reader(message, this);

  if (!this.error_ &&
      this.nextWireType_ != jspb.BinaryConstants.WireType.END_GROUP) {
    goog.asserts.fail('Group submessage did not end with an END_GROUP tag');
    this.error_ = true;
  }
};


/**
 * Return a decoder that wraps the current delimited field.
 * @return {!jspb.BinaryDecoder}
 */
jspb.BinaryReader.prototype.getFieldDecoder = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.DELIMITED);

  var length = this.decoder_.readUnsignedVarint32();
  var start = this.decoder_.getCursor();
  var end = start + length;

  var innerDecoder = jspb.BinaryDecoder.alloc(this.decoder_.getBuffer(),
                                                 start, length);
  this.decoder_.setCursor(end);
  return innerDecoder;
};


/**
 * Reads a signed 32-bit integer field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * @return {number} The value of the signed 32-bit integer field.
 */
jspb.BinaryReader.prototype.readInt32 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readSignedVarint32();
};


/**
 * Reads a signed 32-bit integer field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * Returns the value as a string.
 *
 * @return {string} The value of the signed 32-bit integer field as a decimal
 * string.
 */
jspb.BinaryReader.prototype.readInt32String = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readSignedVarint32String();
};


/**
 * Reads a signed 64-bit integer field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * @return {number} The value of the signed 64-bit integer field.
 */
jspb.BinaryReader.prototype.readInt64 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readSignedVarint64();
};


/**
 * Reads a signed 64-bit integer field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * Returns the value as a string.
 *
 * @return {string} The value of the signed 64-bit integer field as a decimal
 * string.
 */
jspb.BinaryReader.prototype.readInt64String = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readSignedVarint64String();
};


/**
 * Reads an unsigned 32-bit integer field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * @return {number} The value of the unsigned 32-bit integer field.
 */
jspb.BinaryReader.prototype.readUint32 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readUnsignedVarint32();
};


/**
 * Reads an unsigned 32-bit integer field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * Returns the value as a string.
 *
 * @return {string} The value of the unsigned 32-bit integer field as a decimal
 * string.
 */
jspb.BinaryReader.prototype.readUint32String = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readUnsignedVarint32String();
};


/**
 * Reads an unsigned 64-bit integer field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * @return {number} The value of the unsigned 64-bit integer field.
 */
jspb.BinaryReader.prototype.readUint64 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readUnsignedVarint64();
};


/**
 * Reads an unsigned 64-bit integer field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * Returns the value as a string.
 *
 * @return {string} The value of the unsigned 64-bit integer field as a decimal
 * string.
 */
jspb.BinaryReader.prototype.readUint64String = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readUnsignedVarint64String();
};


/**
 * Reads a signed zigzag-encoded 32-bit integer field from the binary stream,
 * or throws an error if the next field in the stream is not of the correct
 * wire type.
 *
 * @return {number} The value of the signed 32-bit integer field.
 */
jspb.BinaryReader.prototype.readSint32 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readZigzagVarint32();
};


/**
 * Reads a signed zigzag-encoded 64-bit integer field from the binary stream,
 * or throws an error if the next field in the stream is not of the correct
 * wire type.
 *
 * @return {number} The value of the signed 64-bit integer field.
 */
jspb.BinaryReader.prototype.readSint64 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readZigzagVarint64();
};


/**
 * Reads an unsigned 32-bit fixed-length integer fiield from the binary stream,
 * or throws an error if the next field in the stream is not of the correct
 * wire type.
 *
 * @return {number} The value of the double field.
 */
jspb.BinaryReader.prototype.readFixed32 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.FIXED32);
  return this.decoder_.readUint32();
};


/**
 * Reads an unsigned 64-bit fixed-length integer fiield from the binary stream,
 * or throws an error if the next field in the stream is not of the correct
 * wire type.
 *
 * @return {number} The value of the float field.
 */
jspb.BinaryReader.prototype.readFixed64 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.FIXED64);
  return this.decoder_.readUint64();
};


/**
 * Reads a signed 32-bit fixed-length integer fiield from the binary stream, or
 * throws an error if the next field in the stream is not of the correct wire
 * type.
 *
 * @return {number} The value of the double field.
 */
jspb.BinaryReader.prototype.readSfixed32 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.FIXED32);
  return this.decoder_.readInt32();
};


/**
 * Reads a signed 64-bit fixed-length integer fiield from the binary stream, or
 * throws an error if the next field in the stream is not of the correct wire
 * type.
 *
 * @return {number} The value of the float field.
 */
jspb.BinaryReader.prototype.readSfixed64 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.FIXED64);
  return this.decoder_.readInt64();
};


/**
 * Reads a 32-bit floating-point field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * @return {number} The value of the float field.
 */
jspb.BinaryReader.prototype.readFloat = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.FIXED32);
  return this.decoder_.readFloat();
};


/**
 * Reads a 64-bit floating-point field from the binary stream, or throws an
 * error if the next field in the stream is not of the correct wire type.
 *
 * @return {number} The value of the double field.
 */
jspb.BinaryReader.prototype.readDouble = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.FIXED64);
  return this.decoder_.readDouble();
};


/**
 * Reads a boolean field from the binary stream, or throws an error if the next
 * field in the stream is not of the correct wire type.
 *
 * @return {boolean} The value of the boolean field.
 */
jspb.BinaryReader.prototype.readBool = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return !!this.decoder_.readUnsignedVarint32();
};


/**
 * Reads an enum field from the binary stream, or throws an error if the next
 * field in the stream is not of the correct wire type.
 *
 * @return {number} The value of the enum field.
 */
jspb.BinaryReader.prototype.readEnum = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readSignedVarint64();
};


/**
 * Reads a string field from the binary stream, or throws an error if the next
 * field in the stream is not of the correct wire type.
 *
 * @return {string} The value of the string field.
 */
jspb.BinaryReader.prototype.readString = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.DELIMITED);
  var length = this.decoder_.readUnsignedVarint32();
  return this.decoder_.readString(length);
};


/**
 * Reads a length-prefixed block of bytes from the binary stream, or returns
 * null if the next field in the stream has an invalid length value.
 *
 * @return {Uint8Array} The block of bytes.
 */
jspb.BinaryReader.prototype.readBytes = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.DELIMITED);
  var length = this.decoder_.readUnsignedVarint32();
  return this.decoder_.readBytes(length);
};


/**
 * Reads a 64-bit varint or fixed64 field from the stream and returns it as a
 * 8-character Unicode string for use as a hash table key, or throws an error
 * if the next field in the stream is not of the correct wire type.
 *
 * @return {string} The hash value.
 */
jspb.BinaryReader.prototype.readVarintHash64 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.VARINT);
  return this.decoder_.readVarintHash64();
};


/**
 * Reads a 64-bit varint or fixed64 field from the stream and returns it as a
 * 8-character Unicode string for use as a hash table key, or throws an error
 * if the next field in the stream is not of the correct wire type.
 *
 * @return {string} The hash value.
 */
jspb.BinaryReader.prototype.readFixedHash64 = function() {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.FIXED64);
  return this.decoder_.readFixedHash64();
};


/**
 * Reads a packed scalar field using the supplied raw reader function.
 * @param {function()} decodeMethod
 * @return {!Array}
 * @private
 */
jspb.BinaryReader.prototype.readPackedField_ = function(decodeMethod) {
  goog.asserts.assert(
      this.nextWireType_ == jspb.BinaryConstants.WireType.DELIMITED);
  var length = this.decoder_.readUnsignedVarint32();
  var end = this.decoder_.getCursor() + length;
  var result = [];
  while (this.decoder_.getCursor() < end) {
    // TODO(aappleby): .call is slow
    result.push(decodeMethod.call(this.decoder_));
  }
  return result;
};


/**
 * Reads a packed int32 field, which consists of a length header and a list of
 * signed varints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedInt32 = function() {
  return this.readPackedField_(this.decoder_.readSignedVarint32);
};


/**
 * Reads a packed int32 field, which consists of a length header and a list of
 * signed varints. Returns a list of strings.
 * @return {!Array.<string>}
 */
jspb.BinaryReader.prototype.readPackedInt32String = function() {
  return this.readPackedField_(this.decoder_.readSignedVarint32String);
};


/**
 * Reads a packed int64 field, which consists of a length header and a list of
 * signed varints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedInt64 = function() {
  return this.readPackedField_(this.decoder_.readSignedVarint64);
};


/**
 * Reads a packed int64 field, which consists of a length header and a list of
 * signed varints. Returns a list of strings.
 * @return {!Array.<string>}
 */
jspb.BinaryReader.prototype.readPackedInt64String = function() {
  return this.readPackedField_(this.decoder_.readSignedVarint64String);
};


/**
 * Reads a packed uint32 field, which consists of a length header and a list of
 * unsigned varints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedUint32 = function() {
  return this.readPackedField_(this.decoder_.readUnsignedVarint32);
};


/**
 * Reads a packed uint32 field, which consists of a length header and a list of
 * unsigned varints. Returns a list of strings.
 * @return {!Array.<string>}
 */
jspb.BinaryReader.prototype.readPackedUint32String = function() {
  return this.readPackedField_(this.decoder_.readUnsignedVarint32String);
};


/**
 * Reads a packed uint64 field, which consists of a length header and a list of
 * unsigned varints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedUint64 = function() {
  return this.readPackedField_(this.decoder_.readUnsignedVarint64);
};


/**
 * Reads a packed uint64 field, which consists of a length header and a list of
 * unsigned varints. Returns a list of strings.
 * @return {!Array.<string>}
 */
jspb.BinaryReader.prototype.readPackedUint64String = function() {
  return this.readPackedField_(this.decoder_.readUnsignedVarint64String);
};


/**
 * Reads a packed sint32 field, which consists of a length header and a list of
 * zigzag varints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedSint32 = function() {
  return this.readPackedField_(this.decoder_.readZigzagVarint32);
};


/**
 * Reads a packed sint64 field, which consists of a length header and a list of
 * zigzag varints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedSint64 = function() {
  return this.readPackedField_(this.decoder_.readZigzagVarint64);
};


/**
 * Reads a packed fixed32 field, which consists of a length header and a list
 * of unsigned 32-bit ints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedFixed32 = function() {
  return this.readPackedField_(this.decoder_.readUint32);
};


/**
 * Reads a packed fixed64 field, which consists of a length header and a list
 * of unsigned 64-bit ints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedFixed64 = function() {
  return this.readPackedField_(this.decoder_.readUint64);
};


/**
 * Reads a packed sfixed32 field, which consists of a length header and a list
 * of 32-bit ints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedSfixed32 = function() {
  return this.readPackedField_(this.decoder_.readInt32);
};


/**
 * Reads a packed sfixed64 field, which consists of a length header and a list
 * of 64-bit ints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedSfixed64 = function() {
  return this.readPackedField_(this.decoder_.readInt64);
};


/**
 * Reads a packed float field, which consists of a length header and a list of
 * floats.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedFloat = function() {
  return this.readPackedField_(this.decoder_.readFloat);
};


/**
 * Reads a packed double field, which consists of a length header and a list of
 * doubles.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedDouble = function() {
  return this.readPackedField_(this.decoder_.readDouble);
};


/**
 * Reads a packed bool field, which consists of a length header and a list of
 * unsigned varints.
 * @return {!Array.<boolean>}
 */
jspb.BinaryReader.prototype.readPackedBool = function() {
  return this.readPackedField_(this.decoder_.readBool);
};


/**
 * Reads a packed enum field, which consists of a length header and a list of
 * unsigned varints.
 * @return {!Array.<number>}
 */
jspb.BinaryReader.prototype.readPackedEnum = function() {
  return this.readPackedField_(this.decoder_.readEnum);
};


/**
 * Reads a packed varint hash64 field, which consists of a length header and a
 * list of varint hash64s.
 * @return {!Array.<string>}
 */
jspb.BinaryReader.prototype.readPackedVarintHash64 = function() {
  return this.readPackedField_(this.decoder_.readVarintHash64);
};


/**
 * Reads a packed fixed hash64 field, which consists of a length header and a
 * list of fixed hash64s.
 * @return {!Array.<string>}
 */
jspb.BinaryReader.prototype.readPackedFixedHash64 = function() {
  return this.readPackedField_(this.decoder_.readFixedHash64);
};

// Protocol Buffers - Google's data interchange format
// Copyright 2008 Google Inc.  All rights reserved.
// https://developers.google.com/protocol-buffers/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/**
 * @fileoverview This file contains helper code used by jspb.utils to
 * handle 64-bit integer conversion to/from strings.
 *
 * @author cfallin@google.com (Chris Fallin)
 *
 * TODO(haberman): move this to javascript/closure/math?
 */

goog.provide('jspb.arith.Int64');
goog.provide('jspb.arith.UInt64');

/**
 * UInt64 implements some 64-bit arithmetic routines necessary for properly
 * handling 64-bit integer fields. It implements lossless integer arithmetic on
 * top of JavaScript's number type, which has only 53 bits of precision, by
 * representing 64-bit integers as two 32-bit halves.
 *
 * @param {number} lo The low 32 bits.
 * @param {number} hi The high 32 bits.
 * @constructor
 */
jspb.arith.UInt64 = function(lo, hi) {
  /**
   * The low 32 bits.
   * @public {number}
   */
  this.lo = lo;
  /**
   * The high 32 bits.
   * @public {number}
   */
  this.hi = hi;
};


/**
 * Compare two 64-bit numbers. Returns -1 if the first is
 * less, +1 if the first is greater, or 0 if both are equal.
 * @param {!jspb.arith.UInt64} other
 * @return {number}
 */
jspb.arith.UInt64.prototype.cmp = function(other) {
  if (this.hi < other.hi || (this.hi == other.hi && this.lo < other.lo)) {
    return -1;
  } else if (this.hi == other.hi && this.lo == other.lo) {
    return 0;
  } else {
    return 1;
  }
};


/**
 * Right-shift this number by one bit.
 * @return {!jspb.arith.UInt64}
 */
jspb.arith.UInt64.prototype.rightShift = function() {
  var hi = this.hi >>> 1;
  var lo = (this.lo >>> 1) | ((this.hi & 1) << 31);
  return new jspb.arith.UInt64(lo >>> 0, hi >>> 0);
};


/**
 * Left-shift this number by one bit.
 * @return {!jspb.arith.UInt64}
 */
jspb.arith.UInt64.prototype.leftShift = function() {
  var lo = this.lo << 1;
  var hi = (this.hi << 1) | (this.lo >>> 31);
  return new jspb.arith.UInt64(lo >>> 0, hi >>> 0);
};


/**
 * Test the MSB.
 * @return {boolean}
 */
jspb.arith.UInt64.prototype.msb = function() {
  return !!(this.hi & 0x80000000);
};


/**
 * Test the LSB.
 * @return {boolean}
 */
jspb.arith.UInt64.prototype.lsb = function() {
  return !!(this.lo & 1);
};


/**
 * Test whether this number is zero.
 * @return {boolean}
 */
jspb.arith.UInt64.prototype.zero = function() {
  return this.lo == 0 && this.hi == 0;
};


/**
 * Add two 64-bit numbers to produce a 64-bit number.
 * @param {!jspb.arith.UInt64} other
 * @return {!jspb.arith.UInt64}
 */
jspb.arith.UInt64.prototype.add = function(other) {
  var lo = ((this.lo + other.lo) & 0xffffffff) >>> 0;
  var hi =
      (((this.hi + other.hi) & 0xffffffff) >>> 0) +
      (((this.lo + other.lo) >= 0x100000000) ? 1 : 0);
  return new jspb.arith.UInt64(lo >>> 0, hi >>> 0);
};


/**
 * Subtract two 64-bit numbers to produce a 64-bit number.
 * @param {!jspb.arith.UInt64} other
 * @return {!jspb.arith.UInt64}
 */
jspb.arith.UInt64.prototype.sub = function(other) {
  var lo = ((this.lo - other.lo) & 0xffffffff) >>> 0;
  var hi =
      (((this.hi - other.hi) & 0xffffffff) >>> 0) -
      (((this.lo - other.lo) < 0) ? 1 : 0);
  return new jspb.arith.UInt64(lo >>> 0, hi >>> 0);
};


/**
 * Multiply two 32-bit numbers to produce a 64-bit number.
 * @param {number} a The first integer:  must be in [0, 2^32-1).
 * @param {number} b The second integer: must be in [0, 2^32-1).
 * @return {!jspb.arith.UInt64}
 */
jspb.arith.UInt64.mul32x32 = function(a, b) {
  // Directly multiplying two 32-bit numbers may produce up to 64 bits of
  // precision, thus losing precision because of the 53-bit mantissa of
  // JavaScript numbers. So we multiply with 16-bit digits (radix 65536)
  // instead.
  var aLow = (a & 0xffff);
  var aHigh = (a >>> 16);
  var bLow = (b & 0xffff);
  var bHigh = (b >>> 16);
  var productLow =
      // 32-bit result, result bits 0-31, take all 32 bits
      (aLow * bLow) +
      // 32-bit result, result bits 16-47, take bottom 16 as our top 16
      ((aLow * bHigh) & 0xffff) * 0x10000 +
      // 32-bit result, result bits 16-47, take bottom 16 as our top 16
      ((aHigh * bLow) & 0xffff) * 0x10000;
  var productHigh =
      // 32-bit result, result bits 32-63, take all 32 bits
      (aHigh * bHigh) +
      // 32-bit result, result bits 16-47, take top 16 as our bottom 16
      ((aLow * bHigh) >>> 16) +
      // 32-bit result, result bits 16-47, take top 16 as our bottom 16
      ((aHigh * bLow) >>> 16);

  // Carry. Note that we actually have up to *two* carries due to addition of
  // three terms.
  while (productLow >= 0x100000000) {
    productLow -= 0x100000000;
    productHigh += 1;
  }

  return new jspb.arith.UInt64(productLow >>> 0, productHigh >>> 0);
};


/**
 * Multiply this number by a 32-bit number, producing a 96-bit number, then
 * truncate the top 32 bits.
 * @param {number} a The multiplier.
 * @return {!jspb.arith.UInt64}
 */
jspb.arith.UInt64.prototype.mul = function(a) {
  // Produce two parts: at bits 0-63, and 32-95.
  var lo = jspb.arith.UInt64.mul32x32(this.lo, a);
  var hi = jspb.arith.UInt64.mul32x32(this.hi, a);
  // Left-shift hi by 32 bits, truncating its top bits. The parts will then be
  // aligned for addition.
  hi.hi = hi.lo;
  hi.lo = 0;
  return lo.add(hi);
};


/**
 * Divide a 64-bit number by a 32-bit number to produce a
 * 64-bit quotient and a 32-bit remainder.
 * @param {number} _divisor
 * @return {Array.<jspb.arith.UInt64>} array of [quotient, remainder],
 * unless divisor is 0, in which case an empty array is returned.
 */
jspb.arith.UInt64.prototype.div = function(_divisor) {
  if (_divisor == 0) {
    return [];
  }

  // We perform long division using a radix-2 algorithm, for simplicity (i.e.,
  // one bit at a time). TODO: optimize to a radix-2^32 algorithm, taking care
  // to get the variable shifts right.
  var quotient = new jspb.arith.UInt64(0, 0);
  var remainder = new jspb.arith.UInt64(this.lo, this.hi);
  var divisor = new jspb.arith.UInt64(_divisor, 0);
  var unit = new jspb.arith.UInt64(1, 0);

  // Left-shift the divisor and unit until the high bit of divisor is set.
  while (!divisor.msb()) {
    divisor = divisor.leftShift();
    unit = unit.leftShift();
  }

  // Perform long division one bit at a time.
  while (!unit.zero()) {
    // If divisor < remainder, add unit to quotient and subtract divisor from
    // remainder.
    if (divisor.cmp(remainder) <= 0) {
      quotient = quotient.add(unit);
      remainder = remainder.sub(divisor);
    }
    // Right-shift the divisor and unit.
    divisor = divisor.rightShift();
    unit = unit.rightShift();
  }

  return [quotient, remainder];
};


/**
 * Convert a 64-bit number to a string.
 * @return {string}
 * @override
 */
jspb.arith.UInt64.prototype.toString = function() {
  var result = '';
  var num = this;
  while (!num.zero()) {
    var divResult = num.div(10);
    var quotient = divResult[0], remainder = divResult[1];
    result = remainder.lo + result;
    num = quotient;
  }
  if (result == '') {
    result = '0';
  }
  return result;
};


/**
 * Parse a string into a 64-bit number. Returns `null` on a parse error.
 * @param {string} s
 * @return {?jspb.arith.UInt64}
 */
jspb.arith.UInt64.fromString = function(s) {
  var result = new jspb.arith.UInt64(0, 0);
  // optimization: reuse this instance for each digit.
  var digit64 = new jspb.arith.UInt64(0, 0);
  for (var i = 0; i < s.length; i++) {
    if (s[i] < '0' || s[i] > '9') {
      return null;
    }
    var digit = parseInt(s[i], 10);
    digit64.lo = digit;
    result = result.mul(10).add(digit64);
  }
  return result;
};


/**
 * Make a copy of the uint64.
 * @return {!jspb.arith.UInt64}
 */
jspb.arith.UInt64.prototype.clone = function() {
  return new jspb.arith.UInt64(this.lo, this.hi);
};


/**
 * Int64 is like UInt64, but modifies string conversions to interpret the stored
 * 64-bit value as a twos-complement-signed integer. It does *not* support the
 * full range of operations that UInt64 does: only add, subtract, and string
 * conversions.
 *
 * N.B. that multiply and divide routines are *NOT* supported. They will throw
 * exceptions. (They are not necessary to implement string conversions, which
 * are the only operations we really need in jspb.)
 *
 * @param {number} lo The low 32 bits.
 * @param {number} hi The high 32 bits.
 * @constructor
 */
jspb.arith.Int64 = function(lo, hi) {
  /**
   * The low 32 bits.
   * @public {number}
   */
  this.lo = lo;
  /**
   * The high 32 bits.
   * @public {number}
   */
  this.hi = hi;
};


/**
 * Add two 64-bit numbers to produce a 64-bit number.
 * @param {!jspb.arith.Int64} other
 * @return {!jspb.arith.Int64}
 */
jspb.arith.Int64.prototype.add = function(other) {
  var lo = ((this.lo + other.lo) & 0xffffffff) >>> 0;
  var hi =
      (((this.hi + other.hi) & 0xffffffff) >>> 0) +
      (((this.lo + other.lo) >= 0x100000000) ? 1 : 0);
  return new jspb.arith.Int64(lo >>> 0, hi >>> 0);
};


/**
 * Subtract two 64-bit numbers to produce a 64-bit number.
 * @param {!jspb.arith.Int64} other
 * @return {!jspb.arith.Int64}
 */
jspb.arith.Int64.prototype.sub = function(other) {
  var lo = ((this.lo - other.lo) & 0xffffffff) >>> 0;
  var hi =
      (((this.hi - other.hi) & 0xffffffff) >>> 0) -
      (((this.lo - other.lo) < 0) ? 1 : 0);
  return new jspb.arith.Int64(lo >>> 0, hi >>> 0);
};


/**
 * Make a copy of the int64.
 * @return {!jspb.arith.Int64}
 */
jspb.arith.Int64.prototype.clone = function() {
  return new jspb.arith.Int64(this.lo, this.hi);
};


/**
 * Convert a 64-bit number to a string.
 * @return {string}
 * @override
 */
jspb.arith.Int64.prototype.toString = function() {
  // If the number is negative, find its twos-complement inverse.
  var sign = (this.hi & 0x80000000) != 0;
  var num = new jspb.arith.UInt64(this.lo, this.hi);
  if (sign) {
    num = new jspb.arith.UInt64(0, 0).sub(num);
  }
  return (sign ? '-' : '') + num.toString();
};


/**
 * Parse a string into a 64-bit number. Returns `null` on a parse error.
 * @param {string} s
 * @return {?jspb.arith.Int64}
 */
jspb.arith.Int64.fromString = function(s) {
  var hasNegative = (s.length > 0 && s[0] == '-');
  if (hasNegative) {
    s = s.substring(1);
  }
  var num = jspb.arith.UInt64.fromString(s);
  if (num === null) {
    return null;
  }
  if (hasNegative) {
    num = new jspb.arith.UInt64(0, 0).sub(num);
  }
  return new jspb.arith.Int64(num.lo, num.hi);
};

// Protocol Buffers - Google's data interchange format
// Copyright 2008 Google Inc.  All rights reserved.
// https://developers.google.com/protocol-buffers/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/**
 * @fileoverview This file contains utilities for encoding Javascript objects
 * into binary, wire-format protocol buffers (in the form of Uint8Arrays) that
 * a server can consume directly.
 *
 * jspb's BinaryWriter class defines methods for efficiently encoding
 * Javascript objects into binary, wire-format protocol buffers and supports
 * all the fundamental field types used in protocol buffers.
 *
 * Major caveat 1 - Users of this library _must_ keep their Javascript proto
 * parsing code in sync with the original .proto file - presumably you'll be
 * using the typed jspb code generator, but if you bypass that you'll need
 * to keep things in sync by hand.
 *
 * Major caveat 2 - Javascript is unable to accurately represent integers
 * larger than 2^53 due to its use of a double-precision floating point format
 * for all numbers. BinaryWriter does not make any special effort to preserve
 * precision for values above this limit - if you need to pass 64-bit integers
 * (hash codes, for example) between the client and server without precision
 * loss, do _not_ use this library.
 *
 * Major caveat 3 - This class uses typed arrays and must not be used on older
 * browsers that do not support them.
 *
 * @author aappleby@google.com (Austin Appleby)
 */

goog.provide('jspb.BinaryWriter');

goog.require('goog.asserts');
goog.require('goog.crypt.base64');
goog.require('jspb.BinaryConstants');
goog.require('jspb.arith.Int64');
goog.require('jspb.arith.UInt64');
goog.require('jspb.utils');

goog.forwardDeclare('jspb.Message');



/**
 * BinaryWriter implements encoders for all the wire types specified in
 * https://developers.google.com/protocol-buffers/docs/encoding.
 *
 * @constructor
 * @struct
 */
jspb.BinaryWriter = function() {
  /**
   * Blocks of serialized data that will be concatenated once all messages have
   * been written.
   * @private {!Array<!Uint8Array|!Array<number>>}
   */
  this.blocks_ = [];

  /**
   * Total number of bytes in the blocks_ array. Does _not_ include the temp
   * buffer.
   * @private {number}
   */
  this.totalLength_ = 0;

  /**
   * Temporary buffer holding a message that we're still serializing. When we
   * get to a stopping point (either the start of a new submessage, or when we
   * need to append a raw Uint8Array), the temp buffer will be added to the
   * block array above and a new temp buffer will be created.
   * @private {!Array.<number>}
   */
  this.temp_ = [];

  /**
   * A stack of bookmarks containing the parent blocks for each message started
   * via beginSubMessage(), needed as bookkeeping for endSubMessage().
   * TODO(aappleby): Deprecated, users should be calling writeMessage().
   * @private {!Array.<!jspb.BinaryWriter.Bookmark_>}
   */
  this.bookmarks_ = [];
};


/**
 * @typedef {{block: !Array.<number>, length: number}}
 * @private
 */
jspb.BinaryWriter.Bookmark_;


/**
 * Saves the current temp buffer in the blocks_ array and starts a new one.
 * @return {!Array.<number>} Returns a reference to the old temp buffer.
 * @private
 */
jspb.BinaryWriter.prototype.saveTempBuffer_ = function() {
  var oldTemp = this.temp_;
  this.blocks_.push(this.temp_);
  this.totalLength_ += this.temp_.length;
  this.temp_ = [];
  return oldTemp;
};


/**
 * Append a typed array of bytes onto the buffer.
 *
 * @param {!Uint8Array} arr The byte array to append.
 * @private
 */
jspb.BinaryWriter.prototype.appendUint8Array_ = function(arr) {
  if (this.temp_.length) {
    this.saveTempBuffer_();
  }
  this.blocks_.push(arr);
  this.totalLength_ += arr.length;
};


/**
 * Append an untyped array of bytes onto the buffer.
 *
 * @param {!Array.<number>} arr The byte array to append.
 * @private
 */
jspb.BinaryWriter.prototype.appendArray_ = function(arr) {
  if (this.temp_.length) {
    this.saveTempBuffer_();
  }
  this.temp_ = arr;
};


/**
 * Begins a length-delimited section by writing the field header to the current
 * temp buffer and then saving it in the block array. Returns the saved block,
 * which we will append the length to in endDelimited_ below.
 * @param {number} field
 * @return {!jspb.BinaryWriter.Bookmark_}
 * @private
 */
jspb.BinaryWriter.prototype.beginDelimited_ = function(field) {
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);
  return {block: this.saveTempBuffer_(), length: this.totalLength_};
};


/**
 * Ends a length-delimited block by encoding the _change_ in length of the
 * buffer to the parent block and adds the number of bytes needed to encode
 * that length to the total byte length. Note that 'parentLength' _must_ be the
 * total length _after_ the field header was written in beginDelimited_ above.
 * @param {!jspb.BinaryWriter.Bookmark_} bookmark
 * @private
 */
jspb.BinaryWriter.prototype.endDelimited_ = function(bookmark) {
  var messageLength = this.totalLength_ + this.temp_.length - bookmark.length;
  goog.asserts.assert(messageLength >= 0);

  var bytes = 1;
  while (messageLength > 127) {
    bookmark.block.push((messageLength & 0x7f) | 0x80);
    messageLength = messageLength >>> 7;
    bytes++;
  }

  bookmark.block.push(messageLength);
  this.totalLength_ += bytes;
};


/**
 * Resets the writer, throwing away any accumulated buffers.
 */
jspb.BinaryWriter.prototype.reset = function() {
  this.blocks_ = [];
  this.temp_ = [];
  this.totalLength_ = 0;
  this.bookmarks_ = [];
};


/**
 * Converts the encoded data into a Uint8Array.
 * @return {!Uint8Array}
 */
jspb.BinaryWriter.prototype.getResultBuffer = function() {
  goog.asserts.assert(this.bookmarks_.length == 0);

  var flat = new Uint8Array(this.totalLength_ + this.temp_.length);

  var blocks = this.blocks_;
  var blockCount = blocks.length;
  var offset = 0;

  for (var i = 0; i < blockCount; i++) {
    var block = blocks[i];
    flat.set(block, offset);
    offset += block.length;
  }

  flat.set(this.temp_, offset);
  offset += this.temp_.length;

  // Post condition: `flattened` must have had every byte written.
  goog.asserts.assert(offset == flat.length);

  // Replace our block list with the flattened block, which lets GC reclaim
  // the temp blocks sooner.
  this.blocks_ = [flat];
  this.temp_ = [];

  return flat;
};


/**
 * Converts the encoded data into a bas64-encoded string.
 * @return {string}
 */
jspb.BinaryWriter.prototype.getResultBase64String = function() {
  return goog.crypt.base64.encodeByteArray(this.getResultBuffer());
};


/**
 * Begins a new sub-message. The client must call endSubMessage() when they're
 * done.
 * TODO(aappleby): Deprecated. Move callers to writeMessage().
 * @param {number} field The field number of the sub-message.
 */
jspb.BinaryWriter.prototype.beginSubMessage = function(field) {
  this.bookmarks_.push(this.beginDelimited_(field));
};


/**
 * Finishes a sub-message and packs it into the parent messages' buffer.
 * TODO(aappleby): Deprecated. Move callers to writeMessage().
 */
jspb.BinaryWriter.prototype.endSubMessage = function() {
  goog.asserts.assert(this.bookmarks_.length >= 0);
  this.endDelimited_(this.bookmarks_.pop());
};


/**
 * Encodes a 32-bit unsigned integer into its wire-format varint representation
 * and stores it in the buffer.
 * @param {number} value The integer to convert.
 */
jspb.BinaryWriter.prototype.rawWriteUnsignedVarint32 = function(value) {
  goog.asserts.assert(value == Math.floor(value));

  while (value > 127) {
    this.temp_.push((value & 0x7f) | 0x80);
    value = value >>> 7;
  }

  this.temp_.push(value);
};


/**
 * Encodes a 32-bit signed integer into its wire-format varint representation
 * and stores it in the buffer.
 * @param {number} value The integer to convert.
 */
jspb.BinaryWriter.prototype.rawWriteSignedVarint32 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  if (value >= 0) {
    this.rawWriteUnsignedVarint32(value);
    return;
  }

  // Write nine bytes with a _signed_ right shift so we preserve the sign bit.
  for (var i = 0; i < 9; i++) {
    this.temp_.push((value & 0x7f) | 0x80);
    value = value >> 7;
  }

  // The above loop writes out 63 bits, so the last byte is always the sign bit
  // which is always set for negative numbers.
  this.temp_.push(1);
};


/**
 * Encodes an unsigned 64-bit integer in 32:32 split representation into its
 * wire-format varint representation and stores it in the buffer.
 * @param {number} lowBits The low 32 bits of the int.
 * @param {number} highBits The high 32 bits of the int.
 */
jspb.BinaryWriter.prototype.rawWriteSplitVarint =
    function(lowBits, highBits) {
  // Break the binary representation into chunks of 7 bits, set the 8th bit
  // in each chunk if it's not the final chunk, and append to the result.
  while (highBits > 0 || lowBits > 127) {
    this.temp_.push((lowBits & 0x7f) | 0x80);
    lowBits = ((lowBits >>> 7) | (highBits << 25)) >>> 0;
    highBits = highBits >>> 7;
  }
  this.temp_.push(lowBits);
};


/**
 * Encodes a JavaScript integer into its wire-format varint representation and
 * stores it in the buffer. Due to the way the varint encoding works this
 * behaves correctly for both signed and unsigned integers, though integers
 * that are not representable in 64 bits will still be truncated.
 * @param {number} value The integer to convert.
 */
jspb.BinaryWriter.prototype.rawWriteVarint = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  jspb.utils.splitInt64(value);
  this.rawWriteSplitVarint(jspb.utils.split64Low,
                           jspb.utils.split64High);
};


/**
 * Encodes a jspb.arith.{Int64,UInt64} instance into its wire-format
 * varint representation and stores it in the buffer. Due to the way the varint
 * encoding works this behaves correctly for both signed and unsigned integers,
 * though integers that are not representable in 64 bits will still be
 * truncated.
 * @param {jspb.arith.Int64|jspb.arith.UInt64} value
 */
jspb.BinaryWriter.prototype.rawWriteVarintFromNum = function(value) {
  this.rawWriteSplitVarint(value.lo, value.hi);
};


/**
 * Encodes a JavaScript integer into its wire-format, zigzag-encoded varint
 * representation and stores it in the buffer.
 * @param {number} value The integer to convert.
 */
jspb.BinaryWriter.prototype.rawWriteZigzagVarint32 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  this.rawWriteUnsignedVarint32(((value << 1) ^ (value >> 31)) >>> 0);
};


/**
 * Encodes a JavaScript integer into its wire-format, zigzag-encoded varint
 * representation and stores it in the buffer. Integers not representable in 64
 * bits will be truncated.
 * @param {number} value The integer to convert.
 */
jspb.BinaryWriter.prototype.rawWriteZigzagVarint = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  jspb.utils.splitZigzag64(value);
  this.rawWriteSplitVarint(jspb.utils.split64Low,
                           jspb.utils.split64High);
};


/**
 * Writes a raw 8-bit unsigned integer to the buffer. Numbers outside the range
 * [0,2^8) will be truncated.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteUint8 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  goog.asserts.assert((value >= 0) && (value < 256));
  this.temp_.push((value >>> 0) & 0xFF);
};


/**
 * Writes a raw 16-bit unsigned integer to the buffer. Numbers outside the
 * range [0,2^16) will be truncated.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteUint16 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  goog.asserts.assert((value >= 0) && (value < 65536));
  this.temp_.push((value >>> 0) & 0xFF);
  this.temp_.push((value >>> 8) & 0xFF);
};


/**
 * Writes a raw 32-bit unsigned integer to the buffer. Numbers outside the
 * range [0,2^32) will be truncated.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteUint32 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  goog.asserts.assert((value >= 0) &&
                      (value < jspb.BinaryConstants.TWO_TO_32));
  this.temp_.push((value >>> 0) & 0xFF);
  this.temp_.push((value >>> 8) & 0xFF);
  this.temp_.push((value >>> 16) & 0xFF);
  this.temp_.push((value >>> 24) & 0xFF);
};


/**
 * Writes a raw 64-bit unsigned integer to the buffer. Numbers outside the
 * range [0,2^64) will be truncated.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteUint64 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  goog.asserts.assert((value >= 0) &&
                      (value < jspb.BinaryConstants.TWO_TO_64));
  jspb.utils.splitUint64(value);
  this.rawWriteUint32(jspb.utils.split64Low);
  this.rawWriteUint32(jspb.utils.split64High);
};


/**
 * Writes a raw 8-bit integer to the buffer. Numbers outside the range
 * [-2^7,2^7) will be truncated.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteInt8 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  goog.asserts.assert((value >= -128) && (value < 128));
  this.temp_.push((value >>> 0) & 0xFF);
};


/**
 * Writes a raw 16-bit integer to the buffer. Numbers outside the range
 * [-2^15,2^15) will be truncated.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteInt16 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  goog.asserts.assert((value >= -32768) && (value < 32768));
  this.temp_.push((value >>> 0) & 0xFF);
  this.temp_.push((value >>> 8) & 0xFF);
};


/**
 * Writes a raw 32-bit integer to the buffer. Numbers outside the range
 * [-2^31,2^31) will be truncated.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteInt32 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_31) &&
                      (value < jspb.BinaryConstants.TWO_TO_31));
  this.temp_.push((value >>> 0) & 0xFF);
  this.temp_.push((value >>> 8) & 0xFF);
  this.temp_.push((value >>> 16) & 0xFF);
  this.temp_.push((value >>> 24) & 0xFF);
};


/**
 * Writes a raw 64-bit integer to the buffer. Numbers outside the range
 * [-2^63,2^63) will be truncated.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteInt64 = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_63) &&
                      (value < jspb.BinaryConstants.TWO_TO_63));
  jspb.utils.splitInt64(value);
  this.rawWriteUint32(jspb.utils.split64Low);
  this.rawWriteUint32(jspb.utils.split64High);
};


/**
 * Writes a raw single-precision floating point value to the buffer. Numbers
 * requiring more than 32 bits of precision will be truncated.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteFloat = function(value) {
  jspb.utils.splitFloat32(value);
  this.rawWriteUint32(jspb.utils.split64Low);
};


/**
 * Writes a raw double-precision floating point value to the buffer. As this is
 * the native format used by JavaScript, no precision will be lost.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteDouble = function(value) {
  jspb.utils.splitFloat64(value);
  this.rawWriteUint32(jspb.utils.split64Low);
  this.rawWriteUint32(jspb.utils.split64High);
};


/**
 * Writes a raw boolean value to the buffer as a varint.
 * @param {boolean} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteBool = function(value) {
  goog.asserts.assert(goog.isBoolean(value));
  this.temp_.push(~~value);
};


/**
 * Writes an raw enum value to the buffer as a varint.
 * @param {number} value The value to write.
 */
jspb.BinaryWriter.prototype.rawWriteEnum = function(value) {
  goog.asserts.assert(value == Math.floor(value));
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_31) &&
                      (value < jspb.BinaryConstants.TWO_TO_31));
  this.rawWriteSignedVarint32(value);
};


/**
 * Writes a raw string value to the buffer.
 * @param {string} string The string to write.
 */
jspb.BinaryWriter.prototype.rawWriteUtf8String = function(string) {
  for (var i = 0; i < string.length; i++) {
    this.temp_.push(string.charCodeAt(i));
  }
};


/**
 * Writes an arbitrary raw byte array to the buffer.
 * @param {!Uint8Array} bytes The array of bytes to write.
 */
jspb.BinaryWriter.prototype.rawWriteBytes = function(bytes) {
  this.appendUint8Array_(bytes);
};


/**
 * Writes an arbitrary raw byte array to the buffer.
 * @param {!Uint8Array} bytes The array of bytes to write.
 * @param {number} start The start of the range to write.
 * @param {number} end The end of the range to write.
 */
jspb.BinaryWriter.prototype.rawWriteByteRange = function(bytes, start, end) {
  this.appendUint8Array_(bytes.subarray(start, end));
};


/**
 * Writes a 64-bit hash string (8 characters @ 8 bits of data each) to the
 * buffer as a varint.
 * @param {string} hash The hash to write.
 */
jspb.BinaryWriter.prototype.rawWriteVarintHash64 = function(hash) {
  jspb.utils.splitHash64(hash);
  this.rawWriteSplitVarint(jspb.utils.split64Low,
                           jspb.utils.split64High);
};


/**
 * Writes a 64-bit hash string (8 characters @ 8 bits of data each) to the
 * buffer as a fixed64.
 * @param {string} hash The hash to write.
 */
jspb.BinaryWriter.prototype.rawWriteFixedHash64 = function(hash) {
  jspb.utils.splitHash64(hash);
  this.rawWriteUint32(jspb.utils.split64Low);
  this.rawWriteUint32(jspb.utils.split64High);
};


/**
 * Encodes a (field number, wire type) tuple into a wire-format field header
 * and stores it in the buffer as a varint.
 * @param {number} field The field number.
 * @param {number} wireType The wire-type of the field, as specified in the
 *     protocol buffer documentation.
 * @private
 */
jspb.BinaryWriter.prototype.rawWriteFieldHeader_ =
    function(field, wireType) {
  goog.asserts.assert(field >= 1 && field == Math.floor(field));
  var x = field * 8 + wireType;
  this.rawWriteUnsignedVarint32(x);
};


/**
 * Writes a field of any valid scalar type to the binary stream.
 * @param {jspb.BinaryConstants.FieldType} fieldType
 * @param {number} field
 * @param {jspb.AnyFieldType} value
 */
jspb.BinaryWriter.prototype.writeAny = function(fieldType, field, value) {
  var fieldTypes = jspb.BinaryConstants.FieldType;
  switch (fieldType) {
    case fieldTypes.DOUBLE:
      this.writeDouble(field, /** @type {number} */(value));
      return;
    case fieldTypes.FLOAT:
      this.writeFloat(field, /** @type {number} */(value));
      return;
    case fieldTypes.INT64:
      this.writeInt64(field, /** @type {number} */(value));
      return;
    case fieldTypes.UINT64:
      this.writeUint64(field, /** @type {number} */(value));
      return;
    case fieldTypes.INT32:
      this.writeInt32(field, /** @type {number} */(value));
      return;
    case fieldTypes.FIXED64:
      this.writeFixed64(field, /** @type {number} */(value));
      return;
    case fieldTypes.FIXED32:
      this.writeFixed32(field, /** @type {number} */(value));
      return;
    case fieldTypes.BOOL:
      this.writeBool(field, /** @type {boolean} */(value));
      return;
    case fieldTypes.STRING:
      this.writeString(field, /** @type {string} */(value));
      return;
    case fieldTypes.GROUP:
      goog.asserts.fail('Group field type not supported in writeAny()');
      return;
    case fieldTypes.MESSAGE:
      goog.asserts.fail('Message field type not supported in writeAny()');
      return;
    case fieldTypes.BYTES:
      this.writeBytes(field, /** @type {?Uint8Array} */(value));
      return;
    case fieldTypes.UINT32:
      this.writeUint32(field, /** @type {number} */(value));
      return;
    case fieldTypes.ENUM:
      this.writeEnum(field, /** @type {number} */(value));
      return;
    case fieldTypes.SFIXED32:
      this.writeSfixed32(field, /** @type {number} */(value));
      return;
    case fieldTypes.SFIXED64:
      this.writeSfixed64(field, /** @type {number} */(value));
      return;
    case fieldTypes.SINT32:
      this.writeSint32(field, /** @type {number} */(value));
      return;
    case fieldTypes.SINT64:
      this.writeSint64(field, /** @type {number} */(value));
      return;
    case fieldTypes.FHASH64:
      this.writeFixedHash64(field, /** @type {string} */(value));
      return;
    case fieldTypes.VHASH64:
      this.writeVarintHash64(field, /** @type {string} */(value));
      return;
    default:
      goog.asserts.fail('Invalid field type in writeAny()');
      return;
  }
};


/**
 * Writes a varint field to the buffer without range checking.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeUnsignedVarint32_ = function(field, value) {
  if (value == null) return;
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.rawWriteSignedVarint32(value);
};


/**
 * Writes a varint field to the buffer without range checking.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeSignedVarint32_ = function(field, value) {
  if (value == null) return;
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.rawWriteSignedVarint32(value);
};


/**
 * Writes a varint field to the buffer without range checking.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeVarint_ = function(field, value) {
  if (value == null) return;
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.rawWriteVarint(value);
};


/**
 * Writes a zigzag varint field to the buffer without range checking.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeZigzagVarint32_ = function(field, value) {
  if (value == null) return;
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.rawWriteZigzagVarint32(value);
};


/**
 * Writes a zigzag varint field to the buffer without range checking.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeZigzagVarint_ = function(field, value) {
  if (value == null) return;
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.rawWriteZigzagVarint(value);
};


/**
 * Writes an int32 field to the buffer. Numbers outside the range [-2^31,2^31)
 * will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeInt32 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_31) &&
                      (value < jspb.BinaryConstants.TWO_TO_31));
  this.writeSignedVarint32_(field, value);
};


/**
 * Writes an int32 field represented as a string to the buffer. Numbers outside
 * the range [-2^31,2^31) will be truncated.
 * @param {number} field The field number.
 * @param {string?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeInt32String = function(field, value) {
  if (value == null) return;
  var intValue = /** {number} */ parseInt(value, 10);
  goog.asserts.assert((intValue >= -jspb.BinaryConstants.TWO_TO_31) &&
                      (intValue < jspb.BinaryConstants.TWO_TO_31));
  this.writeSignedVarint32_(field, intValue);
};


/**
 * Writes an int64 field to the buffer. Numbers outside the range [-2^63,2^63)
 * will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeInt64 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_63) &&
                      (value < jspb.BinaryConstants.TWO_TO_63));
  this.writeVarint_(field, value);
};


/**
 * Writes a int64 field (with value as a string) to the buffer.
 * @param {number} field The field number.
 * @param {string?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeInt64String = function(field, value) {
  if (value == null) return;
  var num = jspb.arith.Int64.fromString(value);
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.rawWriteVarintFromNum(num);
};


/**
 * Writes a uint32 field to the buffer. Numbers outside the range [0,2^32)
 * will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeUint32 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= 0) &&
                      (value < jspb.BinaryConstants.TWO_TO_32));
  this.writeUnsignedVarint32_(field, value);
};


/**
 * Writes a uint32 field represented as a string to the buffer. Numbers outside
 * the range [0,2^32) will be truncated.
 * @param {number} field The field number.
 * @param {string?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeUint32String = function(field, value) {
  if (value == null) return;
  var intValue = /** {number} */ parseInt(value, 10);
  goog.asserts.assert((intValue >= 0) &&
                      (intValue < jspb.BinaryConstants.TWO_TO_32));
  this.writeUnsignedVarint32_(field, intValue);
};


/**
 * Writes a uint64 field to the buffer. Numbers outside the range [0,2^64)
 * will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeUint64 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= 0) &&
                      (value < jspb.BinaryConstants.TWO_TO_64));
  this.writeVarint_(field, value);
};


/**
 * Writes a uint64 field (with value as a string) to the buffer.
 * @param {number} field The field number.
 * @param {string?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeUint64String = function(field, value) {
  if (value == null) return;
  var num = jspb.arith.UInt64.fromString(value);
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.rawWriteVarintFromNum(num);
};


/**
 * Writes a sint32 field to the buffer. Numbers outside the range [-2^31,2^31)
 * will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeSint32 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_31) &&
                      (value < jspb.BinaryConstants.TWO_TO_31));
  this.writeZigzagVarint32_(field, value);
};


/**
 * Writes a sint64 field to the buffer. Numbers outside the range [-2^63,2^63)
 * will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeSint64 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_63) &&
                      (value < jspb.BinaryConstants.TWO_TO_63));
  this.writeZigzagVarint_(field, value);
};


/**
 * Writes a fixed32 field to the buffer. Numbers outside the range [0,2^32)
 * will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeFixed32 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= 0) &&
                      (value < jspb.BinaryConstants.TWO_TO_32));
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.FIXED32);
  this.rawWriteUint32(value);
};


/**
 * Writes a fixed64 field to the buffer. Numbers outside the range [0,2^64)
 * will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeFixed64 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= 0) &&
                      (value < jspb.BinaryConstants.TWO_TO_64));
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.FIXED64);
  this.rawWriteUint64(value);
};


/**
 * Writes a sfixed32 field to the buffer. Numbers outside the range
 * [-2^31,2^31) will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeSfixed32 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_31) &&
                      (value < jspb.BinaryConstants.TWO_TO_31));
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.FIXED32);
  this.rawWriteInt32(value);
};


/**
 * Writes a sfixed64 field to the buffer. Numbers outside the range
 * [-2^63,2^63) will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeSfixed64 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_63) &&
                      (value < jspb.BinaryConstants.TWO_TO_63));
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.FIXED64);
  this.rawWriteInt64(value);
};


/**
 * Writes a single-precision floating point field to the buffer. Numbers
 * requiring more than 32 bits of precision will be truncated.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeFloat = function(field, value) {
  if (value == null) return;
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.FIXED32);
  this.rawWriteFloat(value);
};


/**
 * Writes a double-precision floating point field to the buffer. As this is the
 * native format used by JavaScript, no precision will be lost.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeDouble = function(field, value) {
  if (value == null) return;
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.FIXED64);
  this.rawWriteDouble(value);
};


/**
 * Writes a boolean field to the buffer.
 * @param {number} field The field number.
 * @param {boolean?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeBool = function(field, value) {
  if (value == null) return;
  goog.asserts.assert(goog.isBoolean(value));
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.temp_.push(~~value);
};


/**
 * Writes an enum field to the buffer.
 * @param {number} field The field number.
 * @param {number?} value The value to write.
 */
jspb.BinaryWriter.prototype.writeEnum = function(field, value) {
  if (value == null) return;
  goog.asserts.assert((value >= -jspb.BinaryConstants.TWO_TO_31) &&
                      (value < jspb.BinaryConstants.TWO_TO_31));
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.rawWriteSignedVarint32(value);
};


/**
 * Writes a string field to the buffer.
 * @param {number} field The field number.
 * @param {string?} value The string to write.
 */
jspb.BinaryWriter.prototype.writeString = function(field, value) {
  if (value == null) return;
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);

  // Conversion loop swiped from goog.crypt.stringToUtf8ByteArray. Note that
  // 'bytes' will be at least as long as 'value', but could be longer if we
  // need to unpack unicode characters.
  var bytes = [];
  for (var i = 0; i < value.length; i++) {
    var c = value.charCodeAt(i);
    if (c < 128) {
      bytes.push(c);
    } else if (c < 2048) {
      bytes.push((c >> 6) | 192);
      bytes.push((c & 63) | 128);
    } else {
      bytes.push((c >> 12) | 224);
      bytes.push(((c >> 6) & 63) | 128);
      bytes.push((c & 63) | 128);
    }
  }

  this.rawWriteUnsignedVarint32(bytes.length);
  this.appendArray_(bytes);
};


/**
 * Writes an arbitrary byte field to the buffer. Note - to match the behavior
 * of the C++ implementation, empty byte arrays _are_ serialized.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {jspb.ByteSource} value The array of bytes to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 * @param {boolean=} opt_stringIsRawBytes If `value` is a string, interpret it
 * as a series of raw bytes (codepoints 0--255 inclusive) rather than base64
 * data.
 */
jspb.BinaryWriter.prototype.writeBytes =
    function(field, value, opt_buffer, opt_start, opt_end,
             opt_stringIsRawBytes) {
  if (value != null) {
    this.rawWriteFieldHeader_(field,
        jspb.BinaryConstants.WireType.DELIMITED);
    this.rawWriteUnsignedVarint32(value.length);
    this.rawWriteBytes(
        jspb.utils.byteSourceToUint8Array(value, opt_stringIsRawBytes));
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an arbitrary byte field to the buffer, with `opt_stringIsRawBytes`
 * flag implicitly true.
 * @param {number} field
 * @param {jspb.ByteSource} value The array of bytes to write.
 */
jspb.BinaryWriter.prototype.writeBytesRawString = function(field, value) {
  this.writeBytes(field, value, null, null, null, true);
};


/**
 * Writes a message to the buffer.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @template MessageType
 * @param {number} field The field number.
 * @param {?MessageType} value The message to write.
 * @param {!jspb.WriterFunction} writerCallback Will be invoked with the value
 *     to write and the writer to write it with.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writeMessage =
    function(field, value, writerCallback, opt_buffer, opt_start, opt_end) {
  if (value !== null) {
    var bookmark = this.beginDelimited_(field);

    writerCallback(value, this);

    this.endDelimited_(bookmark);
  } else if (opt_buffer && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes a group message to the buffer.
 *
 * @template MessageType
 * @param {number} field The field number.
 * @param {?MessageType} value The message to write, wrapped with START_GROUP /
 *     END_GROUP tags. Will be a no-op if 'value' is null.
 * @param {!jspb.WriterFunction} writerCallback Will be invoked with the value
 *     to write and the writer to write it with.
 */
jspb.BinaryWriter.prototype.writeGroup =
    function(field, value, writerCallback) {
  if (value) {
    this.rawWriteFieldHeader_(
        field, jspb.BinaryConstants.WireType.START_GROUP);
    writerCallback(value, this);
    this.rawWriteFieldHeader_(
        field, jspb.BinaryConstants.WireType.END_GROUP);
  }
};


/**
 * Writes a 64-bit hash string field (8 characters @ 8 bits of data each) to
 * the buffer.
 * @param {number} field The field number.
 * @param {string?} value The hash string.
 */
jspb.BinaryWriter.prototype.writeFixedHash64 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert(value.length == 8);
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.FIXED64);
  this.rawWriteFixedHash64(value);
};


/**
 * Writes a 64-bit hash string field (8 characters @ 8 bits of data each) to
 * the buffer.
 * @param {number} field The field number.
 * @param {string?} value The hash string.
 */
jspb.BinaryWriter.prototype.writeVarintHash64 = function(field, value) {
  if (value == null) return;
  goog.asserts.assert(value.length == 8);
  this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.VARINT);
  this.rawWriteVarintHash64(value);
};


/**
 * Writes an array of numbers to the buffer as a repeated varint field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeRepeatedUnsignedVarint32_ =
    function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeUnsignedVarint32_(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated varint field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeRepeatedSignedVarint32_ =
    function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeSignedVarint32_(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated varint field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeRepeatedVarint_ = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeVarint_(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated zigzag field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeRepeatedZigzag32_ = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeZigzagVarint32_(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated zigzag field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @private
 */
jspb.BinaryWriter.prototype.writeRepeatedZigzag_ = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeZigzagVarint_(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated 32-bit int field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedInt32 =
    jspb.BinaryWriter.prototype.writeRepeatedSignedVarint32_;


/**
 * Writes an array of numbers formatted as strings to the buffer as a repeated
 * 32-bit int field.
 * @param {number} field The field number.
 * @param {?Array.<string>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedInt32String =
    function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeInt32String(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated 64-bit int field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedInt64 =
    jspb.BinaryWriter.prototype.writeRepeatedVarint_;


/**
 * Writes an array of numbers formatted as strings to the buffer as a repeated
 * 64-bit int field.
 * @param {number} field The field number.
 * @param {?Array.<string>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedInt64String =
    function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeInt64String(field, value[i]);
  }
};


/**
 * Writes an array numbers to the buffer as a repeated unsigned 32-bit int
 *     field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedUint32 =
    jspb.BinaryWriter.prototype.writeRepeatedUnsignedVarint32_;


/**
 * Writes an array of numbers formatted as strings to the buffer as a repeated
 * unsigned 32-bit int field.
 * @param {number} field The field number.
 * @param {?Array.<string>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedUint32String =
    function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeUint32String(field, value[i]);
  }
};


/**
 * Writes an array numbers to the buffer as a repeated unsigned 64-bit int
 *     field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedUint64 =
    jspb.BinaryWriter.prototype.writeRepeatedVarint_;


/**
 * Writes an array of numbers formatted as strings to the buffer as a repeated
 * unsigned 64-bit int field.
 * @param {number} field The field number.
 * @param {?Array.<string>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedUint64String =
    function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeUint64String(field, value[i]);
  }
};


/**
 * Writes an array numbers to the buffer as a repeated signed 32-bit int field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedSint32 =
    jspb.BinaryWriter.prototype.writeRepeatedZigzag32_;


/**
 * Writes an array numbers to the buffer as a repeated signed 64-bit int field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedSint64 =
    jspb.BinaryWriter.prototype.writeRepeatedZigzag_;


/**
 * Writes an array of numbers to the buffer as a repeated fixed32 field. This
 * works for both signed and unsigned fixed32s.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedFixed32 = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeFixed32(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated fixed64 field. This
 * works for both signed and unsigned fixed64s.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedFixed64 = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeFixed64(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated sfixed32 field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedSfixed32 = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeSfixed32(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated sfixed64 field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedSfixed64 = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeSfixed64(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated float field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedFloat = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeFloat(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a repeated double field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedDouble = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeDouble(field, value[i]);
  }
};


/**
 * Writes an array of booleans to the buffer as a repeated bool field.
 * @param {number} field The field number.
 * @param {?Array.<boolean>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedBool = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeBool(field, value[i]);
  }
};


/**
 * Writes an array of enums to the buffer as a repeated enum field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedEnum = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeEnum(field, value[i]);
  }
};


/**
 * Writes an array of strings to the buffer as a repeated string field.
 * @param {number} field The field number.
 * @param {?Array.<string>} value The array of strings to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedString = function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeString(field, value[i]);
  }
};


/**
 * Writes an array of arbitrary byte fields to the buffer.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<!Uint8Array|string>} value
 *     The arrays of arrays of bytes to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 * @param {boolean=} opt_stringIsRawBytes Any values that are strings are
 * interpreted as raw bytes rather than base64 data.
 */
jspb.BinaryWriter.prototype.writeRepeatedBytes =
    function(field, value, opt_buffer, opt_start, opt_end,
             opt_stringIsRawBytes) {
  if (value != null) {
    for (var i = 0; i < value.length; i++) {
      this.writeBytes(field, value[i], null, null, null, opt_stringIsRawBytes);
    }
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of arbitrary byte fields to the buffer, with
 * `opt_stringIsRawBytes` implicitly true.
 * @param {number} field
 * @param {?Array.<string>} value
 */
jspb.BinaryWriter.prototype.writeRepeatedBytesRawString =
    function(field, value) {
  this.writeRepeatedBytes(field, value, null, null, null, true);
};


/**
 * Writes an array of messages to the buffer.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @template MessageType
 * @param {number} field The field number.
 * @param {?Array.<!MessageType>} value The array of messages to
 *    write.
 * @param {!jspb.WriterFunction} writerCallback Will be invoked with the value
 *     to write and the writer to write it with.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writeRepeatedMessage =
    function(field, value, writerCallback, opt_buffer, opt_start, opt_end) {
  if (value) {
    for (var i = 0; i < value.length; i++) {
      var bookmark = this.beginDelimited_(field);

      writerCallback(value[i], this);

      this.endDelimited_(bookmark);
    }
  } else if (opt_buffer && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of group messages to the buffer.
 *
 * @template MessageType
 * @param {number} field The field number.
 * @param {?Array.<!MessageType>} value The array of messages to
 *    write.
 * @param {!jspb.WriterFunction} writerCallback Will be invoked with the value
 *     to write and the writer to write it with.
 */
jspb.BinaryWriter.prototype.writeRepeatedGroup =
    function(field, value, writerCallback) {
  if (value) {
    for (var i = 0; i < value.length; i++) {
      this.rawWriteFieldHeader_(
          field, jspb.BinaryConstants.WireType.START_GROUP);
      writerCallback(value[i], this);
      this.rawWriteFieldHeader_(
          field, jspb.BinaryConstants.WireType.END_GROUP);
    }
  }
};


/**
 * Writes a 64-bit hash string field (8 characters @ 8 bits of data each) to
 * the buffer.
 * @param {number} field The field number.
 * @param {?Array.<string>} value The array of hashes to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedFixedHash64 =
    function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeFixedHash64(field, value[i]);
  }
};


/**
 * Writes a repeated 64-bit hash string field (8 characters @ 8 bits of data
 * each) to the buffer.
 * @param {number} field The field number.
 * @param {?Array.<string>} value The array of hashes to write.
 */
jspb.BinaryWriter.prototype.writeRepeatedVarintHash64 =
    function(field, value) {
  if (value == null) return;
  for (var i = 0; i < value.length; i++) {
    this.writeVarintHash64(field, value[i]);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed varint field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 * @private
 */
jspb.BinaryWriter.prototype.writePackedUnsignedVarint32_ =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    var bookmark = this.beginDelimited_(field);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteUnsignedVarint32(value[i]);
    }
    this.endDelimited_(bookmark);
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed varint field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 * @private
 */
jspb.BinaryWriter.prototype.writePackedSignedVarint32_ =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    var bookmark = this.beginDelimited_(field);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteSignedVarint32(value[i]);
    }
    this.endDelimited_(bookmark);
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed varint field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 * @private
 */
jspb.BinaryWriter.prototype.writePackedVarint_ =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    var bookmark = this.beginDelimited_(field);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteVarint(value[i]);
    }
    this.endDelimited_(bookmark);
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed zigzag field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 * @private
 */
jspb.BinaryWriter.prototype.writePackedZigzag_ =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    var bookmark = this.beginDelimited_(field);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteZigzagVarint(value[i]);
    }
    this.endDelimited_(bookmark);
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed 32-bit int field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedInt32 =
    jspb.BinaryWriter.prototype.writePackedSignedVarint32_;


/**
 * Writes an array of numbers represented as strings to the buffer as a packed
 * 32-bit int field.
 * @param {number} field
 * @param {?Array.<string>} value
 */
jspb.BinaryWriter.prototype.writePackedInt32String = function(field, value) {
  if (value == null || !value.length) return;
  var bookmark = this.beginDelimited_(field);
  for (var i = 0; i < value.length; i++) {
    this.rawWriteSignedVarint32(parseInt(value[i], 10));
  }
  this.endDelimited_(bookmark);
};


/**
 * Writes an array of numbers to the buffer as a packed 64-bit int field.
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedInt64 =
    jspb.BinaryWriter.prototype.writePackedVarint_;


/**
 * Writes an array of numbers represented as strings to the buffer as a packed
 * 64-bit int field.
 * @param {number} field
 * @param {?Array.<string>} value
 */
jspb.BinaryWriter.prototype.writePackedInt64String =
    function(field, value) {
  if (value == null || !value.length) return;
  var bookmark = this.beginDelimited_(field);
  for (var i = 0; i < value.length; i++) {
    var num = jspb.arith.Int64.fromString(value[i]);
    this.rawWriteVarintFromNum(num);
  }
  this.endDelimited_(bookmark);
};


/**
 * Writes an array numbers to the buffer as a packed unsigned 32-bit int field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedUint32 =
    jspb.BinaryWriter.prototype.writePackedUnsignedVarint32_;


/**
 * Writes an array of numbers represented as strings to the buffer as a packed
 * unsigned 32-bit int field.
 * @param {number} field
 * @param {?Array.<string>} value
 */
jspb.BinaryWriter.prototype.writePackedUint32String =
    function(field, value) {
  if (value == null || !value.length) return;
  var bookmark = this.beginDelimited_(field);
  for (var i = 0; i < value.length; i++) {
    this.rawWriteUnsignedVarint32(parseInt(value[i], 10));
  }
  this.endDelimited_(bookmark);
};


/**
 * Writes an array numbers to the buffer as a packed unsigned 64-bit int field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedUint64 =
    jspb.BinaryWriter.prototype.writePackedVarint_;


/**
 * Writes an array of numbers represented as strings to the buffer as a packed
 * unsigned 64-bit int field.
 * @param {number} field
 * @param {?Array.<string>} value
 */
jspb.BinaryWriter.prototype.writePackedUint64String =
    function(field, value) {
  if (value == null || !value.length) return;
  var bookmark = this.beginDelimited_(field);
  for (var i = 0; i < value.length; i++) {
    var num = jspb.arith.UInt64.fromString(value[i]);
    this.rawWriteVarintFromNum(num);
  }
  this.endDelimited_(bookmark);
};


/**
 * Writes an array numbers to the buffer as a packed signed 32-bit int field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedSint32 =
    jspb.BinaryWriter.prototype.writePackedZigzag_;


/**
 * Writes an array numbers to the buffer as a packed signed 64-bit int field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedSint64 =
    jspb.BinaryWriter.prototype.writePackedZigzag_;


/**
 * Writes an array of numbers to the buffer as a packed fixed32 field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedFixed32 =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);
    this.rawWriteUnsignedVarint32(value.length * 4);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteUint32(value[i]);
    }
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed fixed64 field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedFixed64 =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);
    this.rawWriteUnsignedVarint32(value.length * 8);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteUint64(value[i]);
    }
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed sfixed32 field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedSfixed32 =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);
    this.rawWriteUnsignedVarint32(value.length * 4);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteInt32(value[i]);
    }
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed sfixed64 field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedSfixed64 =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null) {
    this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);
    this.rawWriteUnsignedVarint32(value.length * 8);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteInt64(value[i]);
    }
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed float field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedFloat =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);
    this.rawWriteUnsignedVarint32(value.length * 4);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteFloat(value[i]);
    }
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of numbers to the buffer as a packed double field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedDouble =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);
    this.rawWriteUnsignedVarint32(value.length * 8);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteDouble(value[i]);
    }
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of booleans to the buffer as a packed bool field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<boolean>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedBool =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);
    this.rawWriteUnsignedVarint32(value.length);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteBool(value[i]);
    }
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes an array of enums to the buffer as a packed enum field.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<number>} value The array of ints to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedEnum =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    var bookmark = this.beginDelimited_(field);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteEnum(value[i]);
    }
    this.endDelimited_(bookmark);
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes a 64-bit hash string field (8 characters @ 8 bits of data each) to
 * the buffer.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<string>} value The array of hashes to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedFixedHash64 =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    this.rawWriteFieldHeader_(field, jspb.BinaryConstants.WireType.DELIMITED);
    this.rawWriteUnsignedVarint32(value.length * 8);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteFixedHash64(value[i]);
    }
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};


/**
 * Writes a 64-bit hash string field (8 characters @ 8 bits of data each) to
 * the buffer.
 *
 * If 'value' is null, this method will try and copy the pre-serialized value
 * in 'opt_buffer' if present.
 *
 * @param {number} field The field number.
 * @param {?Array.<string>} value The array of hashes to write.
 * @param {?Uint8Array=} opt_buffer A buffer containing pre-packed values.
 * @param {?number=} opt_start The starting point in the above buffer.
 * @param {?number=} opt_end The ending point in the above buffer.
 */
jspb.BinaryWriter.prototype.writePackedVarintHash64 =
    function(field, value, opt_buffer, opt_start, opt_end) {
  if (value != null && value.length) {
    var bookmark = this.beginDelimited_(field);
    for (var i = 0; i < value.length; i++) {
      this.rawWriteVarintHash64(value[i]);
    }
    this.endDelimited_(bookmark);
  } else if ((opt_buffer != null) && (opt_start != null) && (opt_end != null)) {
    this.rawWriteByteRange(opt_buffer, opt_start, opt_end);
  }
};

// Copyright 2006 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview JSON utility functions.
 * @author arv@google.com (Erik Arvidsson)
 */


goog.provide('goog.json');
goog.provide('goog.json.Replacer');
goog.provide('goog.json.Reviver');
goog.provide('goog.json.Serializer');


/**
 * @define {boolean} If true, use the native JSON parsing API.
 * NOTE(ruilopes): EXPERIMENTAL, handle with care.  Setting this to true might
 * break your code.  The default {@code goog.json.parse} implementation is able
 * to handle invalid JSON, such as JSPB.
 */
goog.define('goog.json.USE_NATIVE_JSON', false);


/**
 * Tests if a string is an invalid JSON string. This only ensures that we are
 * not using any invalid characters
 * @param {string} s The string to test.
 * @return {boolean} True if the input is a valid JSON string.
 */
goog.json.isValid = function(s) {
  // All empty whitespace is not valid.
  if (/^\s*$/.test(s)) {
    return false;
  }

  // This is taken from http://www.json.org/json2.js which is released to the
  // public domain.
  // Changes: We dissallow \u2028 Line separator and \u2029 Paragraph separator
  // inside strings.  We also treat \u2028 and \u2029 as whitespace which they
  // are in the RFC but IE and Safari does not match \s to these so we need to
  // include them in the reg exps in all places where whitespace is allowed.
  // We allowed \x7f inside strings because some tools don't escape it,
  // e.g. http://www.json.org/java/org/json/JSONObject.java

  // Parsing happens in three stages. In the first stage, we run the text
  // against regular expressions that look for non-JSON patterns. We are
  // especially concerned with '()' and 'new' because they can cause invocation,
  // and '=' because it can cause mutation. But just to be safe, we want to
  // reject all unexpected forms.

  // We split the first stage into 4 regexp operations in order to work around
  // crippling inefficiencies in IE's and Safari's regexp engines. First we
  // replace all backslash pairs with '@' (a non-JSON character). Second, we
  // replace all simple value tokens with ']' characters, but only when followed
  // by a colon, comma, closing bracket or end of string. Third, we delete all
  // open brackets that follow a colon or comma or that begin the text. Finally,
  // we look to see that the remaining characters are only whitespace or ']' or
  // ',' or ':' or '{' or '}'. If that is so, then the text is safe for eval.

  // Don't make these static since they have the global flag.
  var backslashesRe = /\\["\\\/bfnrtu]/g;
  var simpleValuesRe =
      /(?:"[^"\\\n\r\u2028\u2029\x00-\x08\x0a-\x1f]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?)[\s\u2028\u2029]*(?=:|,|]|}|$)/g;
  var openBracketsRe = /(?:^|:|,)(?:[\s\u2028\u2029]*\[)+/g;
  var remainderRe = /^[\],:{}\s\u2028\u2029]*$/;

  return remainderRe.test(
      s.replace(backslashesRe, '@')
          .replace(simpleValuesRe, ']')
          .replace(openBracketsRe, ''));
};


/**
 * Parses a JSON string and returns the result. This throws an exception if
 * the string is an invalid JSON string.
 *
 * Note that this is very slow on large strings. If you trust the source of
 * the string then you should use unsafeParse instead.
 *
 * @param {*} s The JSON string to parse.
 * @throws Error if s is invalid JSON.
 * @return {Object} The object generated from the JSON string, or null.
 */
goog.json.parse = goog.json.USE_NATIVE_JSON ?
    /** @type {function(*):Object} */ (goog.global['JSON']['parse']) :
                                      function(s) {
                                        var o = String(s);
                                        if (goog.json.isValid(o)) {
                                          /** @preserveTry */
                                          try {
                                            return /** @type {Object} */ (
                                                eval('(' + o + ')'));
                                          } catch (ex) {
                                          }
                                        }
                                        throw Error(
                                            'Invalid JSON string: ' + o);
                                      };


/**
 * Parses a JSON string and returns the result. This uses eval so it is open
 * to security issues and it should only be used if you trust the source.
 *
 * @param {string} s The JSON string to parse.
 * @return {Object} The object generated from the JSON string.
 */
goog.json.unsafeParse = goog.json.USE_NATIVE_JSON ?
    /** @type {function(string):Object} */ (goog.global['JSON']['parse']) :
                                           function(s) {
                                             return /** @type {Object} */ (
                                                 eval('(' + s + ')'));
                                           };


/**
 * JSON replacer, as defined in Section 15.12.3 of the ES5 spec.
 * @see http://ecma-international.org/ecma-262/5.1/#sec-15.12.3
 *
 * TODO(nicksantos): Array should also be a valid replacer.
 *
 * @typedef {function(this:Object, string, *): *}
 */
goog.json.Replacer;


/**
 * JSON reviver, as defined in Section 15.12.2 of the ES5 spec.
 * @see http://ecma-international.org/ecma-262/5.1/#sec-15.12.3
 *
 * @typedef {function(this:Object, string, *): *}
 */
goog.json.Reviver;


/**
 * Serializes an object or a value to a JSON string.
 *
 * @param {*} object The object to serialize.
 * @param {?goog.json.Replacer=} opt_replacer A replacer function
 *     called for each (key, value) pair that determines how the value
 *     should be serialized. By defult, this just returns the value
 *     and allows default serialization to kick in.
 * @throws Error if there are loops in the object graph.
 * @return {string} A JSON string representation of the input.
 */
goog.json.serialize = goog.json.USE_NATIVE_JSON ?
    /** @type {function(*, ?goog.json.Replacer=):string} */
    (goog.global['JSON']['stringify']) :
    function(object, opt_replacer) {
      // NOTE(nicksantos): Currently, we never use JSON.stringify.
      //
      // The last time I evaluated this, JSON.stringify had subtle bugs and
      // behavior differences on all browsers, and the performance win was not
      // large enough to justify all the issues. This may change in the future
      // as browser implementations get better.
      //
      // assertSerialize in json_test contains if branches for the cases
      // that fail.
      return new goog.json.Serializer(opt_replacer).serialize(object);
    };



/**
 * Class that is used to serialize JSON objects to a string.
 * @param {?goog.json.Replacer=} opt_replacer Replacer.
 * @constructor
 */
goog.json.Serializer = function(opt_replacer) {
  /**
   * @type {goog.json.Replacer|null|undefined}
   * @private
   */
  this.replacer_ = opt_replacer;
};


/**
 * Serializes an object or a value to a JSON string.
 *
 * @param {*} object The object to serialize.
 * @throws Error if there are loops in the object graph.
 * @return {string} A JSON string representation of the input.
 */
goog.json.Serializer.prototype.serialize = function(object) {
  var sb = [];
  this.serializeInternal(object, sb);
  return sb.join('');
};


/**
 * Serializes a generic value to a JSON string
 * @protected
 * @param {*} object The object to serialize.
 * @param {Array<string>} sb Array used as a string builder.
 * @throws Error if there are loops in the object graph.
 */
goog.json.Serializer.prototype.serializeInternal = function(object, sb) {
  if (object == null) {
    // undefined == null so this branch covers undefined as well as null
    sb.push('null');
    return;
  }

  if (typeof object == 'object') {
    if (goog.isArray(object)) {
      this.serializeArray(object, sb);
      return;
    } else if (
        object instanceof String || object instanceof Number ||
        object instanceof Boolean) {
      object = object.valueOf();
      // Fall through to switch below.
    } else {
      this.serializeObject_(/** @type {Object} */ (object), sb);
      return;
    }
  }

  switch (typeof object) {
    case 'string':
      this.serializeString_(object, sb);
      break;
    case 'number':
      this.serializeNumber_(object, sb);
      break;
    case 'boolean':
      sb.push(String(object));
      break;
    case 'function':
      sb.push('null');
      break;
    default:
      throw Error('Unknown type: ' + typeof object);
  }
};


/**
 * Character mappings used internally for goog.string.quote
 * @private
 * @type {!Object}
 */
goog.json.Serializer.charToJsonCharCache_ = {
  '\"': '\\"',
  '\\': '\\\\',
  '/': '\\/',
  '\b': '\\b',
  '\f': '\\f',
  '\n': '\\n',
  '\r': '\\r',
  '\t': '\\t',

  '\x0B': '\\u000b'  // '\v' is not supported in JScript
};


/**
 * Regular expression used to match characters that need to be replaced.
 * The S60 browser has a bug where unicode characters are not matched by
 * regular expressions. The condition below detects such behaviour and
 * adjusts the regular expression accordingly.
 * @private
 * @type {!RegExp}
 */
goog.json.Serializer.charsToReplace_ = /\uffff/.test('\uffff') ?
    /[\\\"\x00-\x1f\x7f-\uffff]/g :
    /[\\\"\x00-\x1f\x7f-\xff]/g;


/**
 * Serializes a string to a JSON string
 * @private
 * @param {string} s The string to serialize.
 * @param {Array<string>} sb Array used as a string builder.
 */
goog.json.Serializer.prototype.serializeString_ = function(s, sb) {
  // The official JSON implementation does not work with international
  // characters.
  sb.push('"', s.replace(goog.json.Serializer.charsToReplace_, function(c) {
    // caching the result improves performance by a factor 2-3
    var rv = goog.json.Serializer.charToJsonCharCache_[c];
    if (!rv) {
      rv = '\\u' + (c.charCodeAt(0) | 0x10000).toString(16).substr(1);
      goog.json.Serializer.charToJsonCharCache_[c] = rv;
    }
    return rv;
  }), '"');
};


/**
 * Serializes a number to a JSON string
 * @private
 * @param {number} n The number to serialize.
 * @param {Array<string>} sb Array used as a string builder.
 */
goog.json.Serializer.prototype.serializeNumber_ = function(n, sb) {
  sb.push(isFinite(n) && !isNaN(n) ? String(n) : 'null');
};


/**
 * Serializes an array to a JSON string
 * @param {Array<string>} arr The array to serialize.
 * @param {Array<string>} sb Array used as a string builder.
 * @protected
 */
goog.json.Serializer.prototype.serializeArray = function(arr, sb) {
  var l = arr.length;
  sb.push('[');
  var sep = '';
  for (var i = 0; i < l; i++) {
    sb.push(sep);

    var value = arr[i];
    this.serializeInternal(
        this.replacer_ ? this.replacer_.call(arr, String(i), value) : value,
        sb);

    sep = ',';
  }
  sb.push(']');
};


/**
 * Serializes an object to a JSON string
 * @private
 * @param {Object} obj The object to serialize.
 * @param {Array<string>} sb Array used as a string builder.
 */
goog.json.Serializer.prototype.serializeObject_ = function(obj, sb) {
  sb.push('{');
  var sep = '';
  for (var key in obj) {
    if (Object.prototype.hasOwnProperty.call(obj, key)) {
      var value = obj[key];
      // Skip functions.
      if (typeof value != 'function') {
        sb.push(sep);
        this.serializeString_(key, sb);
        sb.push(':');

        this.serializeInternal(
            this.replacer_ ? this.replacer_.call(obj, key, value) : value, sb);

        sep = ',';
      }
    }
  }
  sb.push('}');
};

// Protocol Buffers - Google's data interchange format
// Copyright 2008 Google Inc.  All rights reserved.
// https://developers.google.com/protocol-buffers/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/**
 * @fileoverview Definition of jspb.Message.
 *
 * @author mwr@google.com (Mark Rawling)
 */

goog.provide('jspb.ExtensionFieldInfo');
goog.provide('jspb.Message');

goog.require('goog.array');
goog.require('goog.asserts');
goog.require('goog.json');
goog.require('goog.object');

// Not needed in compilation units that have no protos with xids.
goog.forwardDeclare('xid.String');



/**
 * Stores information for a single extension field.
 *
 * For example, an extension field defined like so:
 *
 *     extend BaseMessage {
 *       optional MyMessage my_field = 123;
 *     }
 *
 * will result in an ExtensionFieldInfo object with these properties:
 *
 *     {
 *       fieldIndex: 123,
 *       fieldName: {my_field_renamed: 0},
 *       ctor: proto.example.MyMessage,
 *       toObjectFn: proto.example.MyMessage.toObject,
 *       isRepeated: 0
 *     }
 *
 * We include `toObjectFn` to allow the JSCompiler to perform dead-code removal
 * on unused toObject() methods.
 *
 * If an extension field is primitive, ctor and toObjectFn will be null.
 * isRepeated should be 0 or 1.
 *
 * binary{Reader,Writer}Fn and (if message type) binaryMessageSerializeFn are
 * always provided. binaryReaderFn and binaryWriterFn are references to the
 * appropriate methods on BinaryReader/BinaryWriter to read/write the value of
 * this extension, and binaryMessageSerializeFn is a reference to the message
 * class's .serializeBinary method, if available.
 *
 * @param {number} fieldNumber
 * @param {Object} fieldName This has the extension field name as a property.
 * @param {?function(new: jspb.Message, Array=)} ctor
 * @param {?function((boolean|undefined),!jspb.Message):!Object} toObjectFn
 * @param {number} isRepeated
 * @param {?function(number,?)=} opt_binaryReaderFn
 * @param {?function(number,?)|function(number,?,?,?,?,?)=} opt_binaryWriterFn
 * @param {?function(?,?)=} opt_binaryMessageSerializeFn
 * @param {?function(?,?)=} opt_binaryMessageDeserializeFn
 * @param {?boolean=} opt_isPacked
 * @constructor
 * @struct
 * @template T
 */
jspb.ExtensionFieldInfo = function(fieldNumber, fieldName, ctor, toObjectFn,
    isRepeated, opt_binaryReaderFn, opt_binaryWriterFn,
    opt_binaryMessageSerializeFn, opt_binaryMessageDeserializeFn,
    opt_isPacked) {
  /** @const */
  this.fieldIndex = fieldNumber;
  /** @const */
  this.fieldName = fieldName;
  /** @const */
  this.ctor = ctor;
  /** @const */
  this.toObjectFn = toObjectFn;
  /** @const */
  this.binaryReaderFn = opt_binaryReaderFn;
  /** @const */
  this.binaryWriterFn = opt_binaryWriterFn;
  /** @const */
  this.binaryMessageSerializeFn = opt_binaryMessageSerializeFn;
  /** @const */
  this.binaryMessageDeserializeFn = opt_binaryMessageDeserializeFn;
  /** @const */
  this.isRepeated = isRepeated;
  /** @const */
  this.isPacked = opt_isPacked;
};


/**
 * Base class for all JsPb messages.
 * @constructor
 * @struct
 */
jspb.Message = function() {
};


/**
 * @define {boolean} Whether to generate toObject methods for objects. Turn
 *     this off, if you do not want toObject to be ever used in your project.
 *     When turning off this flag, consider adding a conformance test that bans
 *     calling toObject. Enabling this will disable the JSCompiler's ability to
 *     dead code eliminate fields used in protocol buffers that are never used
 *     in an application.
 */
goog.define('jspb.Message.GENERATE_TO_OBJECT', true);


/**
 * @define {boolean} Whether to generate fromObject methods for objects. Turn
 *     this off, if you do not want fromObject to be ever used in your project.
 *     When turning off this flag, consider adding a conformance test that bans
 *     calling fromObject. Enabling this might disable the JSCompiler's ability
 *     to dead code eliminate fields used in protocol buffers that are never
 *     used in an application.
 *     NOTE: By default no protos actually have a fromObject method. You need to
 *     add the jspb.generate_from_object options to the proto definition to
 *     activate the feature.
 *     By default this is enabled for test code only.
 */
goog.define('jspb.Message.GENERATE_FROM_OBJECT', !goog.DISALLOW_TEST_ONLY_CODE);


/**
 * @define {boolean} Turning on this flag does NOT change the behavior of JSPB
 *     and only affects private internal state. It may, however, break some
 *     tests that use naive deeply-equals algorithms, because using a proto
 *     mutates its internal state.
 *     Projects are advised to turn this flag always on.
 */
goog.define('jspb.Message.MINIMIZE_MEMORY_ALLOCATIONS', COMPILED);
// TODO(b/19419436) Turn this on by default.


/**
 * The internal data array.
 * @type {!Array}
 * @protected
 */
jspb.Message.prototype.array;


/**
 * Wrappers are the constructed instances of message-type fields. They are built
 * on demand from the raw array data. Includes message fields, repeated message
 * fields and extension message fields. Indexed by field number.
 * @type {Object}
 * @private
 */
jspb.Message.prototype.wrappers_;


/**
 * The object that contains extension fields, if any. This is an object that
 * maps from a proto field number to the field's value.
 * @type {Object}
 * @private
 */
jspb.Message.prototype.extensionObject_;


/**
 * Non-extension fields with a field number at or above the pivot are
 * stored in the extension object (in addition to all extension fields).
 * @type {number}
 * @private
 */
jspb.Message.prototype.pivot_;


/**
 * The JsPb message_id of this proto.
 * @type {string|undefined} the message id or undefined if this message
 *     has no id.
 * @private
 */
jspb.Message.prototype.messageId_;


/**
 * The xid of this proto type (The same for all instances of a proto). Provides
 * a way to identify a proto by stable obfuscated name.
 * @see {xid}.
 * Available if {@link jspb.generate_xid} is added as a Message option to
 * a protocol buffer.
 * @const {!xid.String|undefined} The xid or undefined if message is
 *     annotated to generate the xid.
 */
jspb.Message.prototype.messageXid;



/**
 * Returns the JsPb message_id of this proto.
 * @return {string|undefined} the message id or undefined if this message
 *     has no id.
 */
jspb.Message.prototype.getJsPbMessageId = function() {
  return this.messageId_;
};


/**
 * An offset applied to lookups into this.array to account for the presence or
 * absence of a messageId at position 0. For response messages, this will be 0.
 * Otherwise, it will be -1 so that the first array position is not wasted.
 * @type {number}
 * @private
 */
jspb.Message.prototype.arrayIndexOffset_;


/**
 * Returns the index into msg.array at which the proto field with tag number
 * fieldNumber will be located.
 * @param {!jspb.Message} msg Message for which we're calculating an index.
 * @param {number} fieldNumber The field number.
 * @return {number} The index.
 * @private
 */
jspb.Message.getIndex_ = function(msg, fieldNumber) {
  return fieldNumber + msg.arrayIndexOffset_;
};


/**
 * Initializes a JsPb Message.
 * @param {!jspb.Message} msg The JsPb proto to modify.
 * @param {Array|undefined} data An initial data array.
 * @param {string|number} messageId For response messages, the message id or ''
 *     if no message id is specified. For non-response messages, 0.
 * @param {number} suggestedPivot The field number at which to start putting
 *     fields into the extension object. This is only used if data does not
 *     contain an extension object already. -1 if no extension object is
 *     required for this message type.
 * @param {Array<number>} repeatedFields The message's repeated fields.
 * @param {Array<!Array<number>>=} opt_oneofFields The fields belonging to
 *     each of the message's oneof unions.
 * @protected
 */
jspb.Message.initialize = function(
    msg, data, messageId, suggestedPivot, repeatedFields, opt_oneofFields) {
  msg.wrappers_ = jspb.Message.MINIMIZE_MEMORY_ALLOCATIONS ? null : {};
  if (!data) {
    data = messageId ? [messageId] : [];
  }
  msg.messageId_ = messageId ? String(messageId) : undefined;
  // If the messageId is 0, this message is not a response message, so we shift
  // array indices down by 1 so as not to waste the first position in the array,
  // which would otherwise go unused.
  msg.arrayIndexOffset_ = messageId === 0 ? -1 : 0;
  msg.array = data;
  jspb.Message.materializeExtensionObject_(msg, suggestedPivot);
  if (repeatedFields) {
    for (var i = 0; i < repeatedFields.length; i++) {
      var fieldNumber = repeatedFields[i];
      if (fieldNumber < msg.pivot_) {
        var index = jspb.Message.getIndex_(msg, fieldNumber);
        msg.array[index] = msg.array[index] ||
            (jspb.Message.MINIMIZE_MEMORY_ALLOCATIONS ?
                jspb.Message.EMPTY_LIST_SENTINEL_ :
                []);
      } else {
        msg.extensionObject_[fieldNumber] =
            msg.extensionObject_[fieldNumber] ||
            (jspb.Message.MINIMIZE_MEMORY_ALLOCATIONS ?
                jspb.Message.EMPTY_LIST_SENTINEL_ :
                []);
      }
    }
  }

  if (opt_oneofFields && opt_oneofFields.length) {
    // Compute the oneof case for each union. This ensures only one value is
    // set in the union.
    goog.array.forEach(
        opt_oneofFields, goog.partial(jspb.Message.computeOneofCase, msg));
  }
};


/**
 * Used to mark empty repeated fields. Serializes to null when serialized
 * to JSON.
 * When reading a repeated field readers must check the return value against
 * this value and return and replace it with a new empty array if it is
 * present.
 * @private @const {!Object}
 */
jspb.Message.EMPTY_LIST_SENTINEL_ = goog.DEBUG && Object.freeze ?
    Object.freeze([]) :
    [];


/**
 * Ensures that the array contains an extension object if necessary.
 * If the array contains an extension object in its last position, then the
 * object is kept in place and its position is used as the pivot.  If not, then
 * create an extension object using suggestedPivot.  If suggestedPivot is -1,
 * we don't have an extension object at all, in which case all fields are stored
 * in the array.
 * @param {!jspb.Message} msg The JsPb proto to modify.
 * @param {number} suggestedPivot See description for initialize().
 * @private
 */
jspb.Message.materializeExtensionObject_ = function(msg, suggestedPivot) {
  if (msg.array.length) {
    var foundIndex = msg.array.length - 1;
    var obj = msg.array[foundIndex];
    // Normal fields are never objects, so we can be sure that if we find an
    // object here, then it's the extension object. However, we must ensure that
    // the object is not an array, since arrays are valid field values.
    // NOTE(lukestebbing): We avoid looking at .length to avoid a JIT bug
    // in Safari on iOS 8. See the description of CL/86511464 for details.
    if (obj && typeof obj == 'object' && !goog.isArray(obj)) {
      msg.pivot_ = foundIndex - msg.arrayIndexOffset_;
      msg.extensionObject_ = obj;
      return;
    }
  }
  // This complexity exists because we keep all extension fields in the
  // extensionObject_ regardless of proto field number. Changing this would
  // simplify the code here, but it would require changing the serialization
  // format from the server, which is not backwards compatible.
  // TODO(jshneier): Should we just treat extension fields the same as
  // non-extension fields, and select whether they appear in the object or in
  // the array purely based on tag number? This would allow simplifying all the
  // get/setExtension logic, but it would require the breaking change described
  // above.
  if (suggestedPivot > -1) {
    msg.pivot_ = suggestedPivot;
    var pivotIndex = jspb.Message.getIndex_(msg, suggestedPivot);
    if (!jspb.Message.MINIMIZE_MEMORY_ALLOCATIONS) {
      msg.extensionObject_ = msg.array[pivotIndex] = {};
    } else {
      // Initialize to null to avoid changing the shape of the proto when it
      // gets eventually set.
      msg.extensionObject_ = null;
    }
  } else {
    msg.pivot_ = Number.MAX_VALUE;
  }
};


/**
 * Creates an empty extensionObject_ if non exists.
 * @param {!jspb.Message} msg The JsPb proto to modify.
 * @private
 */
jspb.Message.maybeInitEmptyExtensionObject_ = function(msg) {
  var pivotIndex = jspb.Message.getIndex_(msg, msg.pivot_);
  if (!msg.array[pivotIndex]) {
    msg.extensionObject_ = msg.array[pivotIndex] = {};
  }
};


/**
 * Converts a JsPb repeated message field into an object list.
 * @param {!Array<T>} field The repeated message field to be
 *     converted.
 * @param {?function(boolean=): Object|
 *     function((boolean|undefined),T): Object} toObjectFn The toObject
 *     function for this field.  We need to pass this for effective dead code
 *     removal.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Array<Object>} An array of converted message objects.
 * @template T
 */
jspb.Message.toObjectList = function(field, toObjectFn, opt_includeInstance) {
  // Not using goog.array.map in the generated code to keep it small.
  // And not using it here to avoid a function call.
  var result = [];
  for (var i = 0; i < field.length; i++) {
    result[i] = toObjectFn.call(field[i], opt_includeInstance,
      /** @type {!jspb.Message} */ (field[i]));
  }
  return result;
};


/**
 * Adds a proto's extension data to a Soy rendering object.
 * @param {!jspb.Message} proto The proto whose extensions to convert.
 * @param {!Object} obj The Soy object to add converted extension data to.
 * @param {!Object} extensions The proto class' registered extensions.
 * @param {function(jspb.ExtensionFieldInfo) : *} getExtensionFn The proto
 *     class' getExtension function. Passed for effective dead code removal.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 */
jspb.Message.toObjectExtension = function(proto, obj, extensions,
    getExtensionFn, opt_includeInstance) {
  for (var fieldNumber in extensions) {
    var fieldInfo = extensions[fieldNumber];
    var value = getExtensionFn.call(proto, fieldInfo);
    if (value) {
      for (var name in fieldInfo.fieldName) {
        if (fieldInfo.fieldName.hasOwnProperty(name)) {
          break; // the compiled field name
        }
      }
      if (!fieldInfo.toObjectFn) {
        obj[name] = value;
      } else {
        if (fieldInfo.isRepeated) {
          obj[name] = jspb.Message.toObjectList(
              /** @type {!Array<jspb.Message>} */ (value),
              fieldInfo.toObjectFn, opt_includeInstance);
        } else {
          obj[name] = fieldInfo.toObjectFn(opt_includeInstance, value);
        }
      }
    }
  }
};


/**
 * Writes a proto's extension data to a binary-format output stream.
 * @param {!jspb.Message} proto The proto whose extensions to convert.
 * @param {*} writer The binary-format writer to write to.
 * @param {!Object} extensions The proto class' registered extensions.
 * @param {function(jspb.ExtensionFieldInfo) : *} getExtensionFn The proto
 *     class' getExtension function. Passed for effective dead code removal.
 */
jspb.Message.serializeBinaryExtensions = function(proto, writer, extensions,
    getExtensionFn) {
  for (var fieldNumber in extensions) {
    var fieldInfo = extensions[fieldNumber];
    // The old codegen doesn't add the extra fields to ExtensionFieldInfo, so we
    // need to gracefully error-out here rather than produce a null dereference
    // below.
    if (!fieldInfo.binaryWriterFn) {
      throw new Error('Message extension present that was generated ' +
                      'without binary serialization support');
    }
    var value = getExtensionFn.call(proto, fieldInfo);
    if (value) {
      if (fieldInfo.ctor) {  // is this a message type?
        // If the message type of the extension was generated without binary
        // support, there may not be a binary message serializer function, and
        // we can't know when we codegen the extending message that the extended
        // message may require binary support, so we can *only* catch this error
        // here, at runtime (and this decoupled codegen is the whole point of
        // extensions!).
        if (fieldInfo.binaryMessageSerializeFn) {
          fieldInfo.binaryWriterFn.call(writer, fieldInfo.fieldIndex,
              value, fieldInfo.binaryMessageSerializeFn);
        } else {
          throw new Error('Message extension present holding submessage ' +
                          'without binary support enabled, and message is ' +
                          'being serialized to binary format');
        }
      } else {
        fieldInfo.binaryWriterFn.call(writer, fieldInfo.fieldIndex, value);
      }
    }
  }
};


/**
 * Reads an extension field from the given reader and, if a valid extension,
 * sets the extension value.
 * @param {!jspb.Message} msg A jspb proto.
 * @param {{skipField:function(),getFieldNumber:function():number}} reader
 * @param {!Object} extensions The extensions object.
 * @param {function(jspb.ExtensionFieldInfo)} getExtensionFn
 * @param {function(jspb.ExtensionFieldInfo, ?)} setExtensionFn
 */
jspb.Message.readBinaryExtension = function(msg, reader, extensions,
    getExtensionFn, setExtensionFn) {
  var fieldInfo = extensions[reader.getFieldNumber()];
  if (!fieldInfo) {
    reader.skipField();
    return;
  }
  if (!fieldInfo.binaryReaderFn) {
    throw new Error('Deserializing extension whose generated code does not ' +
                    'support binary format');
  }

  var value;
  if (fieldInfo.ctor) {
    // Message type.
    value = new fieldInfo.ctor();
    fieldInfo.binaryReaderFn.call(
        reader, value, fieldInfo.binaryMessageDeserializeFn);
  } else {
    // All other types.
    value = fieldInfo.binaryReaderFn.call(reader);
  }

  if (fieldInfo.isRepeated && !fieldInfo.isPacked) {
    var currentList = getExtensionFn.call(msg, fieldInfo);
    if (!currentList) {
      setExtensionFn.call(msg, fieldInfo, [value]);
    } else {
      currentList.push(value);
    }
  } else {
    setExtensionFn.call(msg, fieldInfo, value);
  }
};


/**
 * Gets the value of a non-extension field.
 * @param {!jspb.Message} msg A jspb proto.
 * @param {number} fieldNumber The field number.
 * @return {string|number|boolean|Uint8Array|Array|null|undefined}
 * The field's value.
 * @protected
 */
jspb.Message.getField = function(msg, fieldNumber) {
  if (fieldNumber < msg.pivot_) {
    var index = jspb.Message.getIndex_(msg, fieldNumber);
    var val = msg.array[index];
    if (val === jspb.Message.EMPTY_LIST_SENTINEL_) {
      return msg.array[index] = [];
    }
    return val;
  } else {
    var val = msg.extensionObject_[fieldNumber];
    if (val === jspb.Message.EMPTY_LIST_SENTINEL_) {
      return msg.extensionObject_[fieldNumber] = [];
    }
    return val;
  }
};


/**
 * Gets the value of a non-extension primitive field, with proto3 (non-nullable
 * primitives) semantics. Returns `defaultValue` if the field is not otherwise
 * set.
 * @template T
 * @param {!jspb.Message} msg A jspb proto.
 * @param {number} fieldNumber The field number.
 * @param {T} defaultValue The default value.
 * @return {T} The field's value.
 * @protected
 */
jspb.Message.getFieldProto3 = function(msg, fieldNumber, defaultValue) {
  var value = jspb.Message.getField(msg, fieldNumber);
  if (value == null) {
    return defaultValue;
  } else {
    return value;
  }
};


/**
 * Sets the value of a non-extension field.
 * @param {!jspb.Message} msg A jspb proto.
 * @param {number} fieldNumber The field number.
 * @param {string|number|boolean|Uint8Array|Array|undefined} value New value
 * @protected
 */
jspb.Message.setField = function(msg, fieldNumber, value) {
  if (fieldNumber < msg.pivot_) {
    msg.array[jspb.Message.getIndex_(msg, fieldNumber)] = value;
  } else {
    msg.extensionObject_[fieldNumber] = value;
  }
};


/**
 * Sets the value of a field in a oneof union and clears all other fields in
 * the union.
 * @param {!jspb.Message} msg A jspb proto.
 * @param {number} fieldNumber The field number.
 * @param {!Array<number>} oneof The fields belonging to the union.
 * @param {string|number|boolean|Uint8Array|Array|undefined} value New value
 * @protected
 */
jspb.Message.setOneofField = function(msg, fieldNumber, oneof, value) {
  var currentCase = jspb.Message.computeOneofCase(msg, oneof);
  if (currentCase && currentCase !== fieldNumber && value !== undefined) {
    if (msg.wrappers_ && currentCase in msg.wrappers_) {
      msg.wrappers_[currentCase] = undefined;
    }
    jspb.Message.setField(msg, currentCase, undefined);
  }
  jspb.Message.setField(msg, fieldNumber, value);
};


/**
 * Computes the selection in a oneof group for the given message, ensuring
 * only one field is set in the process.
 *
 * According to the protobuf language guide (
 * https://developers.google.com/protocol-buffers/docs/proto#oneof), "if the
 * parser encounters multiple members of the same oneof on the wire, only the
 * last member seen is used in the parsed message." Since JSPB serializes
 * messages to a JSON array, the "last member seen" will always be the field
 * with the greatest field number (directly corresponding to the greatest
 * array index).
 *
 * @param {!jspb.Message} msg A jspb proto.
 * @param {!Array<number>} oneof The field numbers belonging to the union.
 * @return {number} The field number currently set in the union, or 0 if none.
 * @protected
 */
jspb.Message.computeOneofCase = function(msg, oneof) {
  var oneofField;
  var oneofValue;

  goog.array.forEach(oneof, function(fieldNumber) {
    var value = jspb.Message.getField(msg, fieldNumber);
    if (goog.isDefAndNotNull(value)) {
      oneofField = fieldNumber;
      oneofValue = value;
      jspb.Message.setField(msg, fieldNumber, undefined);
    }
  });

  if (oneofField) {
    // NB: We know the value is unique, so we can call jspb.Message.setField
    // directly instead of jpsb.Message.setOneofField. Also, setOneofField
    // calls this function.
    jspb.Message.setField(msg, oneofField, oneofValue);
    return oneofField;
  }

  return 0;
};


/**
 * Gets and wraps a proto field on access.
 * @param {!jspb.Message} msg A jspb proto.
 * @param {function(new:jspb.Message, Array)} ctor Constructor for the field.
 * @param {number} fieldNumber The field number.
 * @param {number=} opt_required True (1) if this is a required field.
 * @return {jspb.Message} The field as a jspb proto.
 * @protected
 */
jspb.Message.getWrapperField = function(msg, ctor, fieldNumber, opt_required) {
  // TODO(mwr): Consider copying data and/or arrays.
  if (!msg.wrappers_) {
    msg.wrappers_ = {};
  }
  if (!msg.wrappers_[fieldNumber]) {
    var data = /** @type {Array} */ (jspb.Message.getField(msg, fieldNumber));
    if (opt_required || data) {
      // TODO(mwr): Remove existence test for always valid default protos.
      msg.wrappers_[fieldNumber] = new ctor(data);
    }
  }
  return /** @type {jspb.Message} */ (msg.wrappers_[fieldNumber]);
};


/**
 * Gets and wraps a repeated proto field on access.
 * @param {!jspb.Message} msg A jspb proto.
 * @param {function(new:jspb.Message, Array)} ctor Constructor for the field.
 * @param {number} fieldNumber The field number.
 * @return {Array<!jspb.Message>} The repeated field as an array of protos.
 * @protected
 */
jspb.Message.getRepeatedWrapperField = function(msg, ctor, fieldNumber) {
  if (!msg.wrappers_) {
    msg.wrappers_ = {};
  }
  if (!msg.wrappers_[fieldNumber]) {
    var data = jspb.Message.getField(msg, fieldNumber);
    for (var wrappers = [], i = 0; i < data.length; i++) {
      wrappers[i] = new ctor(data[i]);
    }
    msg.wrappers_[fieldNumber] = wrappers;
  }
  var val = msg.wrappers_[fieldNumber];
  if (val == jspb.Message.EMPTY_LIST_SENTINEL_) {
    val = msg.wrappers_[fieldNumber] = [];
  }
  return /** @type {Array<!jspb.Message>} */ (val);
};


/**
 * Sets a proto field and syncs it to the backing array.
 * @param {!jspb.Message} msg A jspb proto.
 * @param {number} fieldNumber The field number.
 * @param {jspb.Message|undefined} value A new value for this proto field.
 * @protected
 */
jspb.Message.setWrapperField = function(msg, fieldNumber, value) {
  if (!msg.wrappers_) {
    msg.wrappers_ = {};
  }
  var data = value ? value.toArray() : value;
  msg.wrappers_[fieldNumber] = value;
  jspb.Message.setField(msg, fieldNumber, data);
};


/**
 * Sets a proto field in a oneof union and syncs it to the backing array.
 * @param {!jspb.Message} msg A jspb proto.
 * @param {number} fieldNumber The field number.
 * @param {!Array<number>} oneof The fields belonging to the union.
 * @param {jspb.Message|undefined} value A new value for this proto field.
 * @protected
 */
jspb.Message.setOneofWrapperField = function(msg, fieldNumber, oneof, value) {
  if (!msg.wrappers_) {
    msg.wrappers_ = {};
  }
  var data = value ? value.toArray() : value;
  msg.wrappers_[fieldNumber] = value;
  jspb.Message.setOneofField(msg, fieldNumber, oneof, data);
};


/**
 * Sets a repeated proto field and syncs it to the backing array.
 * @param {!jspb.Message} msg A jspb proto.
 * @param {number} fieldNumber The field number.
 * @param {Array<!jspb.Message>|undefined} value An array of protos.
 * @protected
 */
jspb.Message.setRepeatedWrapperField = function(msg, fieldNumber, value) {
  if (!msg.wrappers_) {
    msg.wrappers_ = {};
  }
  value = value || [];
  for (var data = [], i = 0; i < value.length; i++) {
    data[i] = value[i].toArray();
  }
  msg.wrappers_[fieldNumber] = value;
  jspb.Message.setField(msg, fieldNumber, data);
};


/**
 * Converts a JsPb repeated message field into a map. The map will contain
 * protos unless an optional toObject function is given, in which case it will
 * contain objects suitable for Soy rendering.
 * @param {!Array<T>} field The repeated message field to be
 *     converted.
 * @param {function() : string?} mapKeyGetterFn The function to get the key of
 *     the map.
 * @param {?function(boolean=): Object|
 *     function((boolean|undefined),T): Object} opt_toObjectFn The
 *     toObject function for this field. We need to pass this for effective
 *     dead code removal.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object.<string, Object>} A map of proto or Soy objects.
 * @template T
 */
jspb.Message.toMap = function(
    field, mapKeyGetterFn, opt_toObjectFn, opt_includeInstance) {
  var result = {};
  for (var i = 0; i < field.length; i++) {
    result[mapKeyGetterFn.call(field[i])] = opt_toObjectFn ?
        opt_toObjectFn.call(field[i], opt_includeInstance,
            /** @type {!jspb.Message} */ (field[i])) : field[i];
  }
  return result;
};


/**
 * Returns the internal array of this proto.
 * <p>Note: If you use this array to construct a second proto, the content
 * would then be partially shared between the two protos.
 * @return {!Array} The proto represented as an array.
 */
jspb.Message.prototype.toArray = function() {
  return this.array;
};




/**
 * Creates a string representation of the internal data array of this proto.
 * <p>NOTE: This string is *not* suitable for use in server requests.
 * @return {string} A string representation of this proto.
 * @override
 */
jspb.Message.prototype.toString = function() {
  return this.array.toString();
};


/**
 * Gets the value of the extension field from the extended object.
 * @param {jspb.ExtensionFieldInfo.<T>} fieldInfo Specifies the field to get.
 * @return {T} The value of the field.
 * @template T
 */
jspb.Message.prototype.getExtension = function(fieldInfo) {
  if (!this.extensionObject_) {
    return undefined;
  }
  if (!this.wrappers_) {
    this.wrappers_ = {};
  }
  var fieldNumber = fieldInfo.fieldIndex;
  if (fieldInfo.isRepeated) {
    if (fieldInfo.ctor) {
      if (!this.wrappers_[fieldNumber]) {
        this.wrappers_[fieldNumber] =
            goog.array.map(this.extensionObject_[fieldNumber] || [],
                function(arr) {
                  return new fieldInfo.ctor(arr);
                });
      }
      return this.wrappers_[fieldNumber];
    } else {
      return this.extensionObject_[fieldNumber];
    }
  } else {
    if (fieldInfo.ctor) {
      if (!this.wrappers_[fieldNumber] && this.extensionObject_[fieldNumber]) {
        this.wrappers_[fieldNumber] = new fieldInfo.ctor(
            /** @type {Array|undefined} */ (
                this.extensionObject_[fieldNumber]));
      }
      return this.wrappers_[fieldNumber];
    } else {
      return this.extensionObject_[fieldNumber];
    }
  }
};


/**
 * Sets the value of the extension field in the extended object.
 * @param {jspb.ExtensionFieldInfo} fieldInfo Specifies the field to set.
 * @param {jspb.Message|string|number|boolean|Array} value The value to set.
 */
jspb.Message.prototype.setExtension = function(fieldInfo, value) {
  if (!this.wrappers_) {
    this.wrappers_ = {};
  }
  jspb.Message.maybeInitEmptyExtensionObject_(this);
  var fieldNumber = fieldInfo.fieldIndex;
  if (fieldInfo.isRepeated) {
    value = value || [];
    if (fieldInfo.ctor) {
      this.wrappers_[fieldNumber] = value;
      this.extensionObject_[fieldNumber] = goog.array.map(
          /** @type {Array<jspb.Message>} */ (value), function(msg) {
        return msg.toArray();
      });
    } else {
      this.extensionObject_[fieldNumber] = value;
    }
  } else {
    if (fieldInfo.ctor) {
      this.wrappers_[fieldNumber] = value;
      this.extensionObject_[fieldNumber] = value ? value.toArray() : value;
    } else {
      this.extensionObject_[fieldNumber] = value;
    }
  }
};


/**
 * Creates a difference object between two messages.
 *
 * The result will contain the top-level fields of m2 that differ from those of
 * m1 at any level of nesting. No data is cloned, the result object will
 * share its top-level elements with m2 (but not with m1).
 *
 * Note that repeated fields should not have null/undefined elements, but if
 * they do, this operation will treat repeated fields of different length as
 * the same if the only difference between them is due to trailing
 * null/undefined values.
 *
 * @param {!jspb.Message} m1 The first message object.
 * @param {!jspb.Message} m2 The second message object.
 * @return {!jspb.Message} The difference returned as a proto message.
 *     Note that the returned message may be missing required fields. This is
 *     currently tolerated in Js, but would cause an error if you tried to
 *     send such a proto to the server. You can access the raw difference
 *     array with result.toArray().
 * @throws {Error} If the messages are responses with different types.
 */
jspb.Message.difference = function(m1, m2) {
  if (!(m1 instanceof m2.constructor)) {
    throw new Error('Messages have different types.');
  }
  var arr1 = m1.toArray();
  var arr2 = m2.toArray();
  var res = [];
  var start = 0;
  var length = arr1.length > arr2.length ? arr1.length : arr2.length;
  if (m1.getJsPbMessageId()) {
    res[0] = m1.getJsPbMessageId();
    start = 1;
  }
  for (var i = start; i < length; i++) {
    if (!jspb.Message.compareFields(arr1[i], arr2[i])) {
      res[i] = arr2[i];
    }
  }
  return new m1.constructor(res);
};


/**
 * Tests whether two messages are equal.
 * @param {jspb.Message|undefined} m1 The first message object.
 * @param {jspb.Message|undefined} m2 The second message object.
 * @return {boolean} true if both messages are null/undefined, or if both are
 *     of the same type and have the same field values.
 */
jspb.Message.equals = function(m1, m2) {
  return m1 == m2 || (!!(m1 && m2) && (m1 instanceof m2.constructor) &&
      jspb.Message.compareFields(m1.toArray(), m2.toArray()));
};


/**
 * Compares two message fields recursively.
 * @param {*} field1 The first field.
 * @param {*} field2 The second field.
 * @return {boolean} true if the fields are null/undefined, or otherwise equal.
 */
jspb.Message.compareFields = function(field1, field2) {
  if (goog.isObject(field1) && goog.isObject(field2)) {
    var keys = {}, name, extensionObject1, extensionObject2;
    for (name in field1) {
      field1.hasOwnProperty(name) && (keys[name] = 0);
    }
    for (name in field2) {
      field2.hasOwnProperty(name) && (keys[name] = 0);
    }
    for (name in keys) {
      var val1 = field1[name], val2 = field2[name];
      if (goog.isObject(val1) && !goog.isArray(val1)) {
        if (extensionObject1 !== undefined) {
          throw new Error('invalid jspb state');
        }
        extensionObject1 = goog.object.isEmpty(val1) ? undefined : val1;
        val1 = undefined;
      }
      if (goog.isObject(val2) && !goog.isArray(val2)) {
        if (extensionObject2 !== undefined) {
          throw new Error('invalid jspb state');
        }
        extensionObject2 = goog.object.isEmpty(val2) ? undefined : val2;
        val2 = undefined;
      }
      if (!jspb.Message.compareFields(val1, val2)) {
        return false;
      }
    }
    if (extensionObject1 || extensionObject2) {
      return jspb.Message.compareFields(extensionObject1, extensionObject2);
    }
    return true;
  }
  // Primitive fields, null and undefined compare as equal.
  // This also forces booleans and 0/1 to compare as equal to ensure
  // compatibility with the jspb serializer.
  return field1 == field2;
};


/**
 * Static clone function. NOTE: A type-safe method called "cloneMessage" exists
 * on each generated JsPb class. Do not call this function directly.
 * @param {!jspb.Message} msg A message to clone.
 * @return {!jspb.Message} A deep clone of the given message.
 */
jspb.Message.clone = function(msg) {
  // Although we could include the wrappers, we leave them out here.
  return jspb.Message.cloneMessage(msg);
};


/**
 * @param {!jspb.Message} msg A message to clone.
 * @return {!jspb.Message} A deep clone of the given message.
 * @protected
 */
jspb.Message.cloneMessage = function(msg) {
  // Although we could include the wrappers, we leave them out here.
  return new msg.constructor(jspb.Message.clone_(msg.toArray()));
};


/**
 * Takes 2 messages of the same type and copies the contents of the first
 * message into the second. After this the 2 messages will equals in terms of
 * value semantics but share no state. All data in the destination message will
 * be overridden.
 *
 * @param {MESSAGE} fromMessage Message that will be copied into toMessage.
 * @param {MESSAGE} toMessage Message which will receive a copy of fromMessage
 *     as its contents.
 * @template MESSAGE
 */
jspb.Message.copyInto = function(fromMessage, toMessage) {
  goog.asserts.assertInstanceof(fromMessage, jspb.Message);
  goog.asserts.assertInstanceof(toMessage, jspb.Message);
  goog.asserts.assert(fromMessage.constructor == toMessage.constructor,
      'Copy source and target message should have the same type.');
  var copyOfFrom = jspb.Message.clone(fromMessage);

  var to = toMessage.toArray();
  var from = copyOfFrom.toArray();

  // Empty destination in case it has more values at the end of the array.
  to.length = 0;
  // and then copy everything from the new to the existing message.
  for (var i = 0; i < from.length; i++) {
    to[i] = from[i];
  }

  // This is either null or empty for a fresh copy.
  toMessage.wrappers_ = copyOfFrom.wrappers_;
  // Just a reference into the shared array.
  toMessage.extensionObject_ = copyOfFrom.extensionObject_;
};


/**
 * Helper for cloning an internal JsPb object.
 * @param {!Object} obj A JsPb object, eg, a field, to be cloned.
 * @return {!Object} A clone of the input object.
 * @private
 */
jspb.Message.clone_ = function(obj) {
  var o;
  if (goog.isArray(obj)) {
    // Allocate array of correct size.
    var clonedArray = new Array(obj.length);
    // Use array iteration where possible because it is faster than for-in.
    for (var i = 0; i < obj.length; i++) {
      if ((o = obj[i]) != null) {
        clonedArray[i] = typeof o == 'object' ? jspb.Message.clone_(o) : o;
      }
    }
    return clonedArray;
  }
  var clone = {};
  for (var key in obj) {
    if ((o = obj[key]) != null) {
      clone[key] = typeof o == 'object' ? jspb.Message.clone_(o) : o;
    }
  }
  return clone;
};


/**
 * Registers a JsPb message type id with its constructor.
 * @param {string} id The id for this type of message.
 * @param {Function} constructor The message constructor.
 */
jspb.Message.registerMessageType = function(id, constructor) {
  jspb.Message.registry_[id] = constructor;
  // This is needed so we can later access messageId directly on the contructor,
  // otherwise it is not available due to 'property collapsing' by the compiler.
  constructor.messageId = id;
};


/**
 * The registry of message ids to message constructors.
 * @private
 */
jspb.Message.registry_ = {};


/**
 * The extensions registered on MessageSet. This is a map of extension
 * field number to field info object. This should be considered as a
 * private API.
 *
 * This is similar to [jspb class name].extensions object for
 * non-MessageSet. We special case MessageSet so that we do not need
 * to goog.require MessageSet from classes that extends MessageSet.
 *
 * @type {!Object.<number, jspb.ExtensionFieldInfo>}
 */
jspb.Message.messageSetExtensions = {};

/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.Engine');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.Engine = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.Engine, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.Engine.displayName = 'proto.ship.Engine';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.Engine.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.Engine.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.Engine} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.Engine.toObject = function(includeInstance, msg) {
  var f, obj = {
    radius: jspb.Message.getField(msg, 1),
    length: jspb.Message.getField(msg, 2),
    group: jspb.Message.getField(msg, 3)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.Engine}
 */
proto.ship.Engine.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.Engine;
  return proto.ship.Engine.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.Engine} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.Engine}
 */
proto.ship.Engine.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setRadius(value);
      break;
    case 2:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setLength(value);
      break;
    case 3:
      var value = /** @type {number} */ (reader.readInt32());
      msg.setGroup(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.Engine} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Engine.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.Engine.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Engine.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getRadius();
  if (f != null) {
    writer.writeDouble(
      1,
      f
    );
  }
  f = this.getLength();
  if (f != null) {
    writer.writeDouble(
      2,
      f
    );
  }
  f = this.getGroup();
  if (f != null) {
    writer.writeInt32(
      3,
      f
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.Engine} The clone.
 */
proto.ship.Engine.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.Engine} */ (jspb.Message.cloneMessage(this));
};


/**
 * required double radius = 1;
 * @return {number}
 */
proto.ship.Engine.prototype.getRadius = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 1));
};


/** @param {number|undefined} value  */
proto.ship.Engine.prototype.setRadius = function(value) {
  jspb.Message.setField(this, 1, value);
};


proto.ship.Engine.prototype.clearRadius = function() {
  jspb.Message.setField(this, 1, undefined);
};


/**
 * required double length = 2;
 * @return {number}
 */
proto.ship.Engine.prototype.getLength = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 2));
};


/** @param {number|undefined} value  */
proto.ship.Engine.prototype.setLength = function(value) {
  jspb.Message.setField(this, 2, value);
};


proto.ship.Engine.prototype.clearLength = function() {
  jspb.Message.setField(this, 2, undefined);
};


/**
 * required int32 group = 3;
 * @return {number}
 */
proto.ship.Engine.prototype.getGroup = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 3));
};


/** @param {number|undefined} value  */
proto.ship.Engine.prototype.setGroup = function(value) {
  jspb.Message.setField(this, 3, value);
};


proto.ship.Engine.prototype.clearGroup = function() {
  jspb.Message.setField(this, 3, undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.Vessel');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.Vessel = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.Vessel, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.Vessel.displayName = 'proto.ship.Vessel';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.Vessel.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.Vessel.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.Vessel} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.Vessel.toObject = function(includeInstance, msg) {
  var f, obj = {
    width: jspb.Message.getField(msg, 1),
    length: jspb.Message.getField(msg, 2)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.Vessel}
 */
proto.ship.Vessel.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.Vessel;
  return proto.ship.Vessel.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.Vessel} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.Vessel}
 */
proto.ship.Vessel.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setWidth(value);
      break;
    case 2:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setLength(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.Vessel} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Vessel.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.Vessel.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Vessel.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getWidth();
  if (f != null) {
    writer.writeDouble(
      1,
      f
    );
  }
  f = this.getLength();
  if (f != null) {
    writer.writeDouble(
      2,
      f
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.Vessel} The clone.
 */
proto.ship.Vessel.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.Vessel} */ (jspb.Message.cloneMessage(this));
};


/**
 * required double width = 1;
 * @return {number}
 */
proto.ship.Vessel.prototype.getWidth = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 1));
};


/** @param {number|undefined} value  */
proto.ship.Vessel.prototype.setWidth = function(value) {
  jspb.Message.setField(this, 1, value);
};


proto.ship.Vessel.prototype.clearWidth = function() {
  jspb.Message.setField(this, 1, undefined);
};


/**
 * required double length = 2;
 * @return {number}
 */
proto.ship.Vessel.prototype.getLength = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 2));
};


/** @param {number|undefined} value  */
proto.ship.Vessel.prototype.setLength = function(value) {
  jspb.Message.setField(this, 2, value);
};


proto.ship.Vessel.prototype.clearLength = function() {
  jspb.Message.setField(this, 2, undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.FuelTank');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.FuelTank = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.FuelTank, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.FuelTank.displayName = 'proto.ship.FuelTank';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.FuelTank.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.FuelTank.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.FuelTank} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.FuelTank.toObject = function(includeInstance, msg) {
  var f, obj = {
    radius: jspb.Message.getField(msg, 1),
    length: jspb.Message.getField(msg, 2)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.FuelTank}
 */
proto.ship.FuelTank.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.FuelTank;
  return proto.ship.FuelTank.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.FuelTank} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.FuelTank}
 */
proto.ship.FuelTank.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setRadius(value);
      break;
    case 2:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setLength(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.FuelTank} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.FuelTank.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.FuelTank.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.FuelTank.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getRadius();
  if (f != null) {
    writer.writeDouble(
      1,
      f
    );
  }
  f = this.getLength();
  if (f != null) {
    writer.writeDouble(
      2,
      f
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.FuelTank} The clone.
 */
proto.ship.FuelTank.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.FuelTank} */ (jspb.Message.cloneMessage(this));
};


/**
 * required double radius = 1;
 * @return {number}
 */
proto.ship.FuelTank.prototype.getRadius = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 1));
};


/** @param {number|undefined} value  */
proto.ship.FuelTank.prototype.setRadius = function(value) {
  jspb.Message.setField(this, 1, value);
};


proto.ship.FuelTank.prototype.clearRadius = function() {
  jspb.Message.setField(this, 1, undefined);
};


/**
 * required double length = 2;
 * @return {number}
 */
proto.ship.FuelTank.prototype.getLength = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 2));
};


/** @param {number|undefined} value  */
proto.ship.FuelTank.prototype.setLength = function(value) {
  jspb.Message.setField(this, 2, value);
};


proto.ship.FuelTank.prototype.clearLength = function() {
  jspb.Message.setField(this, 2, undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.Part');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('proto.ship.Engine');
goog.require('proto.ship.FuelTank');
goog.require('proto.ship.Vessel');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.Part = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, proto.ship.Part.oneofGroups_);
};
goog.inherits(proto.ship.Part, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.Part.displayName = 'proto.ship.Part';
}
/**
 * Oneof group definitions for this message. Each group defines the field
 * numbers belonging to that group. When of these fields' value is set, all
 * other fields in the group are cleared. During deserialization, if multiple
 * fields are encountered for a group, only the last value seen will be kept.
 * @private {!Array<!Array<number>>}
 * @const
 */
proto.ship.Part.oneofGroups_ = [[1,2,3]];

/**
 * @enum {number}
 */
proto.ship.Part.PartCase = {
  PART_NOT_SET: 0,
  VESSEL: 1,
  FUELTANK: 2,
  ENGINE: 3
};

/**
 * @return {proto.ship.Part.PartCase}
 */
proto.ship.Part.prototype.getPartCase = function() {
  return /** @type {proto.ship.Part.PartCase} */(jspb.Message.computeOneofCase(this, proto.ship.Part.oneofGroups_[0]));
};



if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.Part.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.Part.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.Part} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.Part.toObject = function(includeInstance, msg) {
  var f, obj = {
    vessel: (f = msg.getVessel()) && proto.ship.Vessel.toObject(includeInstance, f),
    fueltank: (f = msg.getFueltank()) && proto.ship.FuelTank.toObject(includeInstance, f),
    engine: (f = msg.getEngine()) && proto.ship.Engine.toObject(includeInstance, f)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.Part}
 */
proto.ship.Part.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.Part;
  return proto.ship.Part.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.Part} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.Part}
 */
proto.ship.Part.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = new proto.ship.Vessel;
      reader.readMessage(value,proto.ship.Vessel.deserializeBinaryFromReader);
      msg.setVessel(value);
      break;
    case 2:
      var value = new proto.ship.FuelTank;
      reader.readMessage(value,proto.ship.FuelTank.deserializeBinaryFromReader);
      msg.setFueltank(value);
      break;
    case 3:
      var value = new proto.ship.Engine;
      reader.readMessage(value,proto.ship.Engine.deserializeBinaryFromReader);
      msg.setEngine(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.Part} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Part.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.Part.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Part.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getVessel();
  if (f != null) {
    writer.writeMessage(
      1,
      f,
      proto.ship.Vessel.serializeBinaryToWriter
    );
  }
  f = this.getFueltank();
  if (f != null) {
    writer.writeMessage(
      2,
      f,
      proto.ship.FuelTank.serializeBinaryToWriter
    );
  }
  f = this.getEngine();
  if (f != null) {
    writer.writeMessage(
      3,
      f,
      proto.ship.Engine.serializeBinaryToWriter
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.Part} The clone.
 */
proto.ship.Part.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.Part} */ (jspb.Message.cloneMessage(this));
};


/**
 * optional Vessel vessel = 1;
 * @return {proto.ship.Vessel}
 */
proto.ship.Part.prototype.getVessel = function() {
  return /** @type{proto.ship.Vessel} */ (
    jspb.Message.getWrapperField(this, proto.ship.Vessel, 1));
};


/** @param {proto.ship.Vessel|undefined} value  */
proto.ship.Part.prototype.setVessel = function(value) {
  jspb.Message.setOneofWrapperField(this, 1, proto.ship.Part.oneofGroups_[0], value);
};


proto.ship.Part.prototype.clearVessel = function() {
  this.setVessel(undefined);
};


/**
 * optional FuelTank fuelTank = 2;
 * @return {proto.ship.FuelTank}
 */
proto.ship.Part.prototype.getFueltank = function() {
  return /** @type{proto.ship.FuelTank} */ (
    jspb.Message.getWrapperField(this, proto.ship.FuelTank, 2));
};


/** @param {proto.ship.FuelTank|undefined} value  */
proto.ship.Part.prototype.setFueltank = function(value) {
  jspb.Message.setOneofWrapperField(this, 2, proto.ship.Part.oneofGroups_[0], value);
};


proto.ship.Part.prototype.clearFueltank = function() {
  this.setFueltank(undefined);
};


/**
 * optional Engine engine = 3;
 * @return {proto.ship.Engine}
 */
proto.ship.Part.prototype.getEngine = function() {
  return /** @type{proto.ship.Engine} */ (
    jspb.Message.getWrapperField(this, proto.ship.Engine, 3));
};


/** @param {proto.ship.Engine|undefined} value  */
proto.ship.Part.prototype.setEngine = function(value) {
  jspb.Message.setOneofWrapperField(this, 3, proto.ship.Part.oneofGroups_[0], value);
};


proto.ship.Part.prototype.clearEngine = function() {
  this.setEngine(undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.Beam');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.Beam = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.Beam, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.Beam.displayName = 'proto.ship.Beam';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.Beam.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.Beam.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.Beam} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.Beam.toObject = function(includeInstance, msg) {
  var f, obj = {
    length: jspb.Message.getField(msg, 1)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.Beam}
 */
proto.ship.Beam.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.Beam;
  return proto.ship.Beam.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.Beam} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.Beam}
 */
proto.ship.Beam.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setLength(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.Beam} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Beam.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.Beam.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Beam.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getLength();
  if (f != null) {
    writer.writeDouble(
      1,
      f
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.Beam} The clone.
 */
proto.ship.Beam.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.Beam} */ (jspb.Message.cloneMessage(this));
};


/**
 * required double length = 1;
 * @return {number}
 */
proto.ship.Beam.prototype.getLength = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 1));
};


/** @param {number|undefined} value  */
proto.ship.Beam.prototype.setLength = function(value) {
  jspb.Message.setField(this, 1, value);
};


proto.ship.Beam.prototype.clearLength = function() {
  jspb.Message.setField(this, 1, undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.StructureNode');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('proto.ship.Beam');
goog.require('proto.ship.Part');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.StructureNode = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, proto.ship.StructureNode.oneofGroups_);
};
goog.inherits(proto.ship.StructureNode, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.StructureNode.displayName = 'proto.ship.StructureNode';
}
/**
 * Oneof group definitions for this message. Each group defines the field
 * numbers belonging to that group. When of these fields' value is set, all
 * other fields in the group are cleared. During deserialization, if multiple
 * fields are encountered for a group, only the last value seen will be kept.
 * @private {!Array<!Array<number>>}
 * @const
 */
proto.ship.StructureNode.oneofGroups_ = [[1,2]];

/**
 * @enum {number}
 */
proto.ship.StructureNode.NodeCase = {
  NODE_NOT_SET: 0,
  BEAM: 1,
  PART: 2
};

/**
 * @return {proto.ship.StructureNode.NodeCase}
 */
proto.ship.StructureNode.prototype.getNodeCase = function() {
  return /** @type {proto.ship.StructureNode.NodeCase} */(jspb.Message.computeOneofCase(this, proto.ship.StructureNode.oneofGroups_[0]));
};



if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.StructureNode.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.StructureNode.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.StructureNode} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.StructureNode.toObject = function(includeInstance, msg) {
  var f, obj = {
    beam: (f = msg.getBeam()) && proto.ship.Beam.toObject(includeInstance, f),
    part: (f = msg.getPart()) && proto.ship.Part.toObject(includeInstance, f)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.StructureNode}
 */
proto.ship.StructureNode.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.StructureNode;
  return proto.ship.StructureNode.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.StructureNode} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.StructureNode}
 */
proto.ship.StructureNode.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = new proto.ship.Beam;
      reader.readMessage(value,proto.ship.Beam.deserializeBinaryFromReader);
      msg.setBeam(value);
      break;
    case 2:
      var value = new proto.ship.Part;
      reader.readMessage(value,proto.ship.Part.deserializeBinaryFromReader);
      msg.setPart(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.StructureNode} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.StructureNode.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.StructureNode.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.StructureNode.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getBeam();
  if (f != null) {
    writer.writeMessage(
      1,
      f,
      proto.ship.Beam.serializeBinaryToWriter
    );
  }
  f = this.getPart();
  if (f != null) {
    writer.writeMessage(
      2,
      f,
      proto.ship.Part.serializeBinaryToWriter
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.StructureNode} The clone.
 */
proto.ship.StructureNode.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.StructureNode} */ (jspb.Message.cloneMessage(this));
};


/**
 * optional Beam beam = 1;
 * @return {proto.ship.Beam}
 */
proto.ship.StructureNode.prototype.getBeam = function() {
  return /** @type{proto.ship.Beam} */ (
    jspb.Message.getWrapperField(this, proto.ship.Beam, 1));
};


/** @param {proto.ship.Beam|undefined} value  */
proto.ship.StructureNode.prototype.setBeam = function(value) {
  jspb.Message.setOneofWrapperField(this, 1, proto.ship.StructureNode.oneofGroups_[0], value);
};


proto.ship.StructureNode.prototype.clearBeam = function() {
  this.setBeam(undefined);
};


/**
 * optional Part part = 2;
 * @return {proto.ship.Part}
 */
proto.ship.StructureNode.prototype.getPart = function() {
  return /** @type{proto.ship.Part} */ (
    jspb.Message.getWrapperField(this, proto.ship.Part, 2));
};


/** @param {proto.ship.Part|undefined} value  */
proto.ship.StructureNode.prototype.setPart = function(value) {
  jspb.Message.setOneofWrapperField(this, 2, proto.ship.StructureNode.oneofGroups_[0], value);
};


proto.ship.StructureNode.prototype.clearPart = function() {
  this.setPart(undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.Attach');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.Attach = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.Attach, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.Attach.displayName = 'proto.ship.Attach';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.Attach.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.Attach.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.Attach} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.Attach.toObject = function(includeInstance, msg) {
  var f, obj = {
    location: jspb.Message.getField(msg, 1),
    rotation: jspb.Message.getField(msg, 2)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.Attach}
 */
proto.ship.Attach.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.Attach;
  return proto.ship.Attach.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.Attach} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.Attach}
 */
proto.ship.Attach.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setLocation(value);
      break;
    case 2:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setRotation(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.Attach} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Attach.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.Attach.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Attach.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getLocation();
  if (f != null) {
    writer.writeDouble(
      1,
      f
    );
  }
  f = this.getRotation();
  if (f != null) {
    writer.writeDouble(
      2,
      f
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.Attach} The clone.
 */
proto.ship.Attach.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.Attach} */ (jspb.Message.cloneMessage(this));
};


/**
 * required double location = 1;
 * @return {number}
 */
proto.ship.Attach.prototype.getLocation = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 1));
};


/** @param {number|undefined} value  */
proto.ship.Attach.prototype.setLocation = function(value) {
  jspb.Message.setField(this, 1, value);
};


proto.ship.Attach.prototype.clearLocation = function() {
  jspb.Message.setField(this, 1, undefined);
};


/**
 * required double rotation = 2;
 * @return {number}
 */
proto.ship.Attach.prototype.getRotation = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 2));
};


/** @param {number|undefined} value  */
proto.ship.Attach.prototype.setRotation = function(value) {
  jspb.Message.setField(this, 2, value);
};


proto.ship.Attach.prototype.clearRotation = function() {
  jspb.Message.setField(this, 2, undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.Root');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.Root = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.Root, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.Root.displayName = 'proto.ship.Root';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.Root.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.Root.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.Root} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.Root.toObject = function(includeInstance, msg) {
  var f, obj = {

  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.Root}
 */
proto.ship.Root.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.Root;
  return proto.ship.Root.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.Root} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.Root}
 */
proto.ship.Root.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.Root} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Root.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.Root.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Root.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.Root} The clone.
 */
proto.ship.Root.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.Root} */ (jspb.Message.cloneMessage(this));
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.StructureLink');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('proto.ship.Attach');
goog.require('proto.ship.Root');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.StructureLink = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, proto.ship.StructureLink.oneofGroups_);
};
goog.inherits(proto.ship.StructureLink, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.StructureLink.displayName = 'proto.ship.StructureLink';
}
/**
 * Oneof group definitions for this message. Each group defines the field
 * numbers belonging to that group. When of these fields' value is set, all
 * other fields in the group are cleared. During deserialization, if multiple
 * fields are encountered for a group, only the last value seen will be kept.
 * @private {!Array<!Array<number>>}
 * @const
 */
proto.ship.StructureLink.oneofGroups_ = [[1,2]];

/**
 * @enum {number}
 */
proto.ship.StructureLink.LinkCase = {
  LINK_NOT_SET: 0,
  ROOT: 1,
  ATTACH: 2
};

/**
 * @return {proto.ship.StructureLink.LinkCase}
 */
proto.ship.StructureLink.prototype.getLinkCase = function() {
  return /** @type {proto.ship.StructureLink.LinkCase} */(jspb.Message.computeOneofCase(this, proto.ship.StructureLink.oneofGroups_[0]));
};



if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.StructureLink.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.StructureLink.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.StructureLink} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.StructureLink.toObject = function(includeInstance, msg) {
  var f, obj = {
    root: (f = msg.getRoot()) && proto.ship.Root.toObject(includeInstance, f),
    attach: (f = msg.getAttach()) && proto.ship.Attach.toObject(includeInstance, f)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.StructureLink}
 */
proto.ship.StructureLink.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.StructureLink;
  return proto.ship.StructureLink.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.StructureLink} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.StructureLink}
 */
proto.ship.StructureLink.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = new proto.ship.Root;
      reader.readMessage(value,proto.ship.Root.deserializeBinaryFromReader);
      msg.setRoot(value);
      break;
    case 2:
      var value = new proto.ship.Attach;
      reader.readMessage(value,proto.ship.Attach.deserializeBinaryFromReader);
      msg.setAttach(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.StructureLink} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.StructureLink.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.StructureLink.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.StructureLink.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getRoot();
  if (f != null) {
    writer.writeMessage(
      1,
      f,
      proto.ship.Root.serializeBinaryToWriter
    );
  }
  f = this.getAttach();
  if (f != null) {
    writer.writeMessage(
      2,
      f,
      proto.ship.Attach.serializeBinaryToWriter
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.StructureLink} The clone.
 */
proto.ship.StructureLink.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.StructureLink} */ (jspb.Message.cloneMessage(this));
};


/**
 * optional Root root = 1;
 * @return {proto.ship.Root}
 */
proto.ship.StructureLink.prototype.getRoot = function() {
  return /** @type{proto.ship.Root} */ (
    jspb.Message.getWrapperField(this, proto.ship.Root, 1));
};


/** @param {proto.ship.Root|undefined} value  */
proto.ship.StructureLink.prototype.setRoot = function(value) {
  jspb.Message.setOneofWrapperField(this, 1, proto.ship.StructureLink.oneofGroups_[0], value);
};


proto.ship.StructureLink.prototype.clearRoot = function() {
  this.setRoot(undefined);
};


/**
 * optional Attach attach = 2;
 * @return {proto.ship.Attach}
 */
proto.ship.StructureLink.prototype.getAttach = function() {
  return /** @type{proto.ship.Attach} */ (
    jspb.Message.getWrapperField(this, proto.ship.Attach, 2));
};


/** @param {proto.ship.Attach|undefined} value  */
proto.ship.StructureLink.prototype.setAttach = function(value) {
  jspb.Message.setOneofWrapperField(this, 2, proto.ship.StructureLink.oneofGroups_[0], value);
};


proto.ship.StructureLink.prototype.clearAttach = function() {
  this.setAttach(undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.StructureTree');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('proto.ship.StructureLink');
goog.require('proto.ship.StructureNode');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.StructureTree = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.StructureTree, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.StructureTree.displayName = 'proto.ship.StructureTree';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.StructureTree.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.StructureTree.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.StructureTree} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.StructureTree.toObject = function(includeInstance, msg) {
  var f, obj = {
    node: (f = msg.getNode()) && proto.ship.StructureNode.toObject(includeInstance, f),
    link: (f = msg.getLink()) && proto.ship.StructureLink.toObject(includeInstance, f)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.StructureTree}
 */
proto.ship.StructureTree.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.StructureTree;
  return proto.ship.StructureTree.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.StructureTree} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.StructureTree}
 */
proto.ship.StructureTree.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = new proto.ship.StructureNode;
      reader.readMessage(value,proto.ship.StructureNode.deserializeBinaryFromReader);
      msg.setNode(value);
      break;
    case 2:
      var value = new proto.ship.StructureLink;
      reader.readMessage(value,proto.ship.StructureLink.deserializeBinaryFromReader);
      msg.setLink(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.StructureTree} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.StructureTree.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.StructureTree.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.StructureTree.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getNode();
  if (f != null) {
    writer.writeMessage(
      1,
      f,
      proto.ship.StructureNode.serializeBinaryToWriter
    );
  }
  f = this.getLink();
  if (f != null) {
    writer.writeMessage(
      2,
      f,
      proto.ship.StructureLink.serializeBinaryToWriter
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.StructureTree} The clone.
 */
proto.ship.StructureTree.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.StructureTree} */ (jspb.Message.cloneMessage(this));
};


/**
 * required StructureNode node = 1;
 * @return {!proto.ship.StructureNode}
 */
proto.ship.StructureTree.prototype.getNode = function() {
  return /** @type{!proto.ship.StructureNode} */ (
    jspb.Message.getWrapperField(this, proto.ship.StructureNode, 1, 1));
};


/** @param {proto.ship.StructureNode|undefined} value  */
proto.ship.StructureTree.prototype.setNode = function(value) {
  jspb.Message.setWrapperField(this, 1, value);
};


proto.ship.StructureTree.prototype.clearNode = function() {
  this.setNode(undefined);
};


/**
 * required StructureLink link = 2;
 * @return {!proto.ship.StructureLink}
 */
proto.ship.StructureTree.prototype.getLink = function() {
  return /** @type{!proto.ship.StructureLink} */ (
    jspb.Message.getWrapperField(this, proto.ship.StructureLink, 2, 1));
};


/** @param {proto.ship.StructureLink|undefined} value  */
proto.ship.StructureTree.prototype.setLink = function(value) {
  jspb.Message.setWrapperField(this, 2, value);
};


proto.ship.StructureTree.prototype.clearLink = function() {
  this.setLink(undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.EndMarker');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.EndMarker = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.EndMarker, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.EndMarker.displayName = 'proto.ship.EndMarker';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.EndMarker.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.EndMarker.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.EndMarker} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.EndMarker.toObject = function(includeInstance, msg) {
  var f, obj = {

  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.EndMarker}
 */
proto.ship.EndMarker.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.EndMarker;
  return proto.ship.EndMarker.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.EndMarker} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.EndMarker}
 */
proto.ship.EndMarker.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.EndMarker} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.EndMarker.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.EndMarker.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.EndMarker.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.EndMarker} The clone.
 */
proto.ship.EndMarker.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.EndMarker} */ (jspb.Message.cloneMessage(this));
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.StructureData');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('proto.ship.EndMarker');
goog.require('proto.ship.StructureTree');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.StructureData = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, proto.ship.StructureData.oneofGroups_);
};
goog.inherits(proto.ship.StructureData, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.StructureData.displayName = 'proto.ship.StructureData';
}
/**
 * Oneof group definitions for this message. Each group defines the field
 * numbers belonging to that group. When of these fields' value is set, all
 * other fields in the group are cleared. During deserialization, if multiple
 * fields are encountered for a group, only the last value seen will be kept.
 * @private {!Array<!Array<number>>}
 * @const
 */
proto.ship.StructureData.oneofGroups_ = [[1,2]];

/**
 * @enum {number}
 */
proto.ship.StructureData.StructureCase = {
  STRUCTURE_NOT_SET: 0,
  MARKER: 1,
  TREE: 2
};

/**
 * @return {proto.ship.StructureData.StructureCase}
 */
proto.ship.StructureData.prototype.getStructureCase = function() {
  return /** @type {proto.ship.StructureData.StructureCase} */(jspb.Message.computeOneofCase(this, proto.ship.StructureData.oneofGroups_[0]));
};



if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.StructureData.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.StructureData.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.StructureData} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.StructureData.toObject = function(includeInstance, msg) {
  var f, obj = {
    marker: (f = msg.getMarker()) && proto.ship.EndMarker.toObject(includeInstance, f),
    tree: (f = msg.getTree()) && proto.ship.StructureTree.toObject(includeInstance, f)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.StructureData}
 */
proto.ship.StructureData.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.StructureData;
  return proto.ship.StructureData.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.StructureData} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.StructureData}
 */
proto.ship.StructureData.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = new proto.ship.EndMarker;
      reader.readMessage(value,proto.ship.EndMarker.deserializeBinaryFromReader);
      msg.setMarker(value);
      break;
    case 2:
      var value = new proto.ship.StructureTree;
      reader.readMessage(value,proto.ship.StructureTree.deserializeBinaryFromReader);
      msg.setTree(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.StructureData} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.StructureData.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.StructureData.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.StructureData.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getMarker();
  if (f != null) {
    writer.writeMessage(
      1,
      f,
      proto.ship.EndMarker.serializeBinaryToWriter
    );
  }
  f = this.getTree();
  if (f != null) {
    writer.writeMessage(
      2,
      f,
      proto.ship.StructureTree.serializeBinaryToWriter
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.StructureData} The clone.
 */
proto.ship.StructureData.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.StructureData} */ (jspb.Message.cloneMessage(this));
};


/**
 * optional EndMarker marker = 1;
 * @return {proto.ship.EndMarker}
 */
proto.ship.StructureData.prototype.getMarker = function() {
  return /** @type{proto.ship.EndMarker} */ (
    jspb.Message.getWrapperField(this, proto.ship.EndMarker, 1));
};


/** @param {proto.ship.EndMarker|undefined} value  */
proto.ship.StructureData.prototype.setMarker = function(value) {
  jspb.Message.setOneofWrapperField(this, 1, proto.ship.StructureData.oneofGroups_[0], value);
};


proto.ship.StructureData.prototype.clearMarker = function() {
  this.setMarker(undefined);
};


/**
 * optional StructureTree tree = 2;
 * @return {proto.ship.StructureTree}
 */
proto.ship.StructureData.prototype.getTree = function() {
  return /** @type{proto.ship.StructureTree} */ (
    jspb.Message.getWrapperField(this, proto.ship.StructureTree, 2));
};


/** @param {proto.ship.StructureTree|undefined} value  */
proto.ship.StructureData.prototype.setTree = function(value) {
  jspb.Message.setOneofWrapperField(this, 2, proto.ship.StructureData.oneofGroups_[0], value);
};


proto.ship.StructureData.prototype.clearTree = function() {
  this.setTree(undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.Structure');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('proto.ship.StructureData');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.Structure = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, proto.ship.Structure.repeatedFields_, null);
};
goog.inherits(proto.ship.Structure, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.Structure.displayName = 'proto.ship.Structure';
}
/**
 * List of repeated fields within this message type.
 * @private {!Array<number>}
 * @const
 */
proto.ship.Structure.repeatedFields_ = [1];



if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.Structure.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.Structure.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.Structure} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.Structure.toObject = function(includeInstance, msg) {
  var f, obj = {
    attachmentsList: jspb.Message.toObjectList(msg.getAttachmentsList(),
    proto.ship.StructureData.toObject, includeInstance)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.Structure}
 */
proto.ship.Structure.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.Structure;
  return proto.ship.Structure.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.Structure} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.Structure}
 */
proto.ship.Structure.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = new proto.ship.StructureData;
      reader.readMessage(value,proto.ship.StructureData.deserializeBinaryFromReader);
      msg.getAttachmentsList().push(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.Structure} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Structure.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.Structure.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Structure.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getAttachmentsList();
  if (f.length > 0) {
    writer.writeRepeatedMessage(
      1,
      f,
      proto.ship.StructureData.serializeBinaryToWriter
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.Structure} The clone.
 */
proto.ship.Structure.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.Structure} */ (jspb.Message.cloneMessage(this));
};


/**
 * repeated StructureData attachments = 1;
 * If you change this array by adding, removing or replacing elements, or if you
 * replace the array itself, then you must call the setter to update it.
 * @return {!Array.<!proto.ship.StructureData>}
 */
proto.ship.Structure.prototype.getAttachmentsList = function() {
  return /** @type{!Array.<!proto.ship.StructureData>} */ (
    jspb.Message.getRepeatedWrapperField(this, proto.ship.StructureData, 1));
};


/** @param {Array.<!proto.ship.StructureData>|undefined} value  */
proto.ship.Structure.prototype.setAttachmentsList = function(value) {
  jspb.Message.setRepeatedWrapperField(this, 1, value);
};


proto.ship.Structure.prototype.clearAttachmentsList = function() {
  this.setAttachmentsList([]);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.actions.Active');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.actions.Active = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, proto.actions.Active.repeatedFields_, null);
};
goog.inherits(proto.actions.Active, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.actions.Active.displayName = 'proto.actions.Active';
}
/**
 * List of repeated fields within this message type.
 * @private {!Array<number>}
 * @const
 */
proto.actions.Active.repeatedFields_ = [1];



if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.actions.Active.prototype.toObject = function(opt_includeInstance) {
  return proto.actions.Active.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.actions.Active} msg The msg instance to transform.
 * @return {!Object}
 */
proto.actions.Active.toObject = function(includeInstance, msg) {
  var f, obj = {
    groupsList: jspb.Message.getField(msg, 1)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.actions.Active}
 */
proto.actions.Active.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.actions.Active;
  return proto.actions.Active.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.actions.Active} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.actions.Active}
 */
proto.actions.Active.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readInt32());
      msg.getGroupsList().push(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.actions.Active} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.actions.Active.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.actions.Active.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.actions.Active.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getGroupsList();
  if (f.length > 0) {
    writer.writeRepeatedInt32(
      1,
      f
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.actions.Active} The clone.
 */
proto.actions.Active.prototype.cloneMessage = function() {
  return /** @type {!proto.actions.Active} */ (jspb.Message.cloneMessage(this));
};


/**
 * repeated int32 groups = 1;
 * If you change this array by adding, removing or replacing elements, or if you
 * replace the array itself, then you must call the setter to update it.
 * @return {!Array.<number>}
 */
proto.actions.Active.prototype.getGroupsList = function() {
  return /** @type {!Array.<number>} */ (jspb.Message.getField(this, 1));
};


/** @param {Array.<number>|undefined} value  */
proto.actions.Active.prototype.setGroupsList = function(value) {
  jspb.Message.setField(this, 1, value || []);
};


proto.actions.Active.prototype.clearGroupsList = function() {
  jspb.Message.setField(this, 1, []);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.PhysicsState');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.PhysicsState = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.PhysicsState, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.PhysicsState.displayName = 'proto.ship.PhysicsState';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.PhysicsState.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.PhysicsState.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.PhysicsState} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.PhysicsState.toObject = function(includeInstance, msg) {
  var f, obj = {
    x: jspb.Message.getField(msg, 1),
    y: jspb.Message.getField(msg, 2),
    theta: jspb.Message.getField(msg, 3),
    vx: jspb.Message.getField(msg, 4),
    vy: jspb.Message.getField(msg, 5),
    omega: jspb.Message.getField(msg, 6)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.PhysicsState}
 */
proto.ship.PhysicsState.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.PhysicsState;
  return proto.ship.PhysicsState.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.PhysicsState} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.PhysicsState}
 */
proto.ship.PhysicsState.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setX(value);
      break;
    case 2:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setY(value);
      break;
    case 3:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setTheta(value);
      break;
    case 4:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setVx(value);
      break;
    case 5:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setVy(value);
      break;
    case 6:
      var value = /** @type {number} */ (reader.readDouble());
      msg.setOmega(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.PhysicsState} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.PhysicsState.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.PhysicsState.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.PhysicsState.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getX();
  if (f != null) {
    writer.writeDouble(
      1,
      f
    );
  }
  f = this.getY();
  if (f != null) {
    writer.writeDouble(
      2,
      f
    );
  }
  f = this.getTheta();
  if (f != null) {
    writer.writeDouble(
      3,
      f
    );
  }
  f = this.getVx();
  if (f != null) {
    writer.writeDouble(
      4,
      f
    );
  }
  f = this.getVy();
  if (f != null) {
    writer.writeDouble(
      5,
      f
    );
  }
  f = this.getOmega();
  if (f != null) {
    writer.writeDouble(
      6,
      f
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.PhysicsState} The clone.
 */
proto.ship.PhysicsState.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.PhysicsState} */ (jspb.Message.cloneMessage(this));
};


/**
 * required double x = 1;
 * @return {number}
 */
proto.ship.PhysicsState.prototype.getX = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 1));
};


/** @param {number|undefined} value  */
proto.ship.PhysicsState.prototype.setX = function(value) {
  jspb.Message.setField(this, 1, value);
};


proto.ship.PhysicsState.prototype.clearX = function() {
  jspb.Message.setField(this, 1, undefined);
};


/**
 * required double y = 2;
 * @return {number}
 */
proto.ship.PhysicsState.prototype.getY = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 2));
};


/** @param {number|undefined} value  */
proto.ship.PhysicsState.prototype.setY = function(value) {
  jspb.Message.setField(this, 2, value);
};


proto.ship.PhysicsState.prototype.clearY = function() {
  jspb.Message.setField(this, 2, undefined);
};


/**
 * required double theta = 3;
 * @return {number}
 */
proto.ship.PhysicsState.prototype.getTheta = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 3));
};


/** @param {number|undefined} value  */
proto.ship.PhysicsState.prototype.setTheta = function(value) {
  jspb.Message.setField(this, 3, value);
};


proto.ship.PhysicsState.prototype.clearTheta = function() {
  jspb.Message.setField(this, 3, undefined);
};


/**
 * required double vx = 4;
 * @return {number}
 */
proto.ship.PhysicsState.prototype.getVx = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 4));
};


/** @param {number|undefined} value  */
proto.ship.PhysicsState.prototype.setVx = function(value) {
  jspb.Message.setField(this, 4, value);
};


proto.ship.PhysicsState.prototype.clearVx = function() {
  jspb.Message.setField(this, 4, undefined);
};


/**
 * required double vy = 5;
 * @return {number}
 */
proto.ship.PhysicsState.prototype.getVy = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 5));
};


/** @param {number|undefined} value  */
proto.ship.PhysicsState.prototype.setVy = function(value) {
  jspb.Message.setField(this, 5, value);
};


proto.ship.PhysicsState.prototype.clearVy = function() {
  jspb.Message.setField(this, 5, undefined);
};


/**
 * required double omega = 6;
 * @return {number}
 */
proto.ship.PhysicsState.prototype.getOmega = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 6));
};


/** @param {number|undefined} value  */
proto.ship.PhysicsState.prototype.setOmega = function(value) {
  jspb.Message.setField(this, 6, value);
};


proto.ship.PhysicsState.prototype.clearOmega = function() {
  jspb.Message.setField(this, 6, undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.ship.Ship');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('proto.actions.Active');
goog.require('proto.ship.PhysicsState');
goog.require('proto.ship.Structure');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.ship.Ship = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, null);
};
goog.inherits(proto.ship.Ship, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.ship.Ship.displayName = 'proto.ship.Ship';
}


if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.ship.Ship.prototype.toObject = function(opt_includeInstance) {
  return proto.ship.Ship.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.ship.Ship} msg The msg instance to transform.
 * @return {!Object}
 */
proto.ship.Ship.toObject = function(includeInstance, msg) {
  var f, obj = {
    entityid: jspb.Message.getField(msg, 1),
    structure: (f = msg.getStructure()) && proto.ship.Structure.toObject(includeInstance, f),
    physicsstate: (f = msg.getPhysicsstate()) && proto.ship.PhysicsState.toObject(includeInstance, f),
    active: (f = msg.getActive()) && proto.actions.Active.toObject(includeInstance, f)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.ship.Ship}
 */
proto.ship.Ship.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.ship.Ship;
  return proto.ship.Ship.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.ship.Ship} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.ship.Ship}
 */
proto.ship.Ship.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readUint64());
      msg.setEntityid(value);
      break;
    case 2:
      var value = new proto.ship.Structure;
      reader.readMessage(value,proto.ship.Structure.deserializeBinaryFromReader);
      msg.setStructure(value);
      break;
    case 3:
      var value = new proto.ship.PhysicsState;
      reader.readMessage(value,proto.ship.PhysicsState.deserializeBinaryFromReader);
      msg.setPhysicsstate(value);
      break;
    case 4:
      var value = new proto.actions.Active;
      reader.readMessage(value,proto.actions.Active.deserializeBinaryFromReader);
      msg.setActive(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.ship.Ship} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Ship.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.ship.Ship.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.ship.Ship.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getEntityid();
  if (f != null) {
    writer.writeUint64(
      1,
      f
    );
  }
  f = this.getStructure();
  if (f != null) {
    writer.writeMessage(
      2,
      f,
      proto.ship.Structure.serializeBinaryToWriter
    );
  }
  f = this.getPhysicsstate();
  if (f != null) {
    writer.writeMessage(
      3,
      f,
      proto.ship.PhysicsState.serializeBinaryToWriter
    );
  }
  f = this.getActive();
  if (f != null) {
    writer.writeMessage(
      4,
      f,
      proto.actions.Active.serializeBinaryToWriter
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.ship.Ship} The clone.
 */
proto.ship.Ship.prototype.cloneMessage = function() {
  return /** @type {!proto.ship.Ship} */ (jspb.Message.cloneMessage(this));
};


/**
 * required uint64 entityId = 1;
 * @return {number}
 */
proto.ship.Ship.prototype.getEntityid = function() {
  return /** @type {number} */ (jspb.Message.getField(this, 1));
};


/** @param {number|undefined} value  */
proto.ship.Ship.prototype.setEntityid = function(value) {
  jspb.Message.setField(this, 1, value);
};


proto.ship.Ship.prototype.clearEntityid = function() {
  jspb.Message.setField(this, 1, undefined);
};


/**
 * required Structure structure = 2;
 * @return {!proto.ship.Structure}
 */
proto.ship.Ship.prototype.getStructure = function() {
  return /** @type{!proto.ship.Structure} */ (
    jspb.Message.getWrapperField(this, proto.ship.Structure, 2, 1));
};


/** @param {proto.ship.Structure|undefined} value  */
proto.ship.Ship.prototype.setStructure = function(value) {
  jspb.Message.setWrapperField(this, 2, value);
};


proto.ship.Ship.prototype.clearStructure = function() {
  this.setStructure(undefined);
};


/**
 * required PhysicsState physicsState = 3;
 * @return {!proto.ship.PhysicsState}
 */
proto.ship.Ship.prototype.getPhysicsstate = function() {
  return /** @type{!proto.ship.PhysicsState} */ (
    jspb.Message.getWrapperField(this, proto.ship.PhysicsState, 3, 1));
};


/** @param {proto.ship.PhysicsState|undefined} value  */
proto.ship.Ship.prototype.setPhysicsstate = function(value) {
  jspb.Message.setWrapperField(this, 3, value);
};


proto.ship.Ship.prototype.clearPhysicsstate = function() {
  this.setPhysicsstate(undefined);
};


/**
 * required actions.Active active = 4;
 * @return {!proto.actions.Active}
 */
proto.ship.Ship.prototype.getActive = function() {
  return /** @type{!proto.actions.Active} */ (
    jspb.Message.getWrapperField(this, proto.actions.Active, 4, 1));
};


/** @param {proto.actions.Active|undefined} value  */
proto.ship.Ship.prototype.setActive = function(value) {
  jspb.Message.setWrapperField(this, 4, value);
};


proto.ship.Ship.prototype.clearActive = function() {
  this.setActive(undefined);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.world.Snapshot');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('proto.ship.Ship');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.world.Snapshot = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, proto.world.Snapshot.repeatedFields_, null);
};
goog.inherits(proto.world.Snapshot, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.world.Snapshot.displayName = 'proto.world.Snapshot';
}
/**
 * List of repeated fields within this message type.
 * @private {!Array<number>}
 * @const
 */
proto.world.Snapshot.repeatedFields_ = [1];



if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.world.Snapshot.prototype.toObject = function(opt_includeInstance) {
  return proto.world.Snapshot.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.world.Snapshot} msg The msg instance to transform.
 * @return {!Object}
 */
proto.world.Snapshot.toObject = function(includeInstance, msg) {
  var f, obj = {
    shipsList: jspb.Message.toObjectList(msg.getShipsList(),
    proto.ship.Ship.toObject, includeInstance)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.world.Snapshot}
 */
proto.world.Snapshot.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.world.Snapshot;
  return proto.world.Snapshot.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.world.Snapshot} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.world.Snapshot}
 */
proto.world.Snapshot.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = new proto.ship.Ship;
      reader.readMessage(value,proto.ship.Ship.deserializeBinaryFromReader);
      msg.getShipsList().push(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.world.Snapshot} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.world.Snapshot.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.world.Snapshot.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.world.Snapshot.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getShipsList();
  if (f.length > 0) {
    writer.writeRepeatedMessage(
      1,
      f,
      proto.ship.Ship.serializeBinaryToWriter
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.world.Snapshot} The clone.
 */
proto.world.Snapshot.prototype.cloneMessage = function() {
  return /** @type {!proto.world.Snapshot} */ (jspb.Message.cloneMessage(this));
};


/**
 * repeated ship.Ship ships = 1;
 * If you change this array by adding, removing or replacing elements, or if you
 * replace the array itself, then you must call the setter to update it.
 * @return {!Array.<!proto.ship.Ship>}
 */
proto.world.Snapshot.prototype.getShipsList = function() {
  return /** @type{!Array.<!proto.ship.Ship>} */ (
    jspb.Message.getRepeatedWrapperField(this, proto.ship.Ship, 1));
};


/** @param {Array.<!proto.ship.Ship>|undefined} value  */
proto.world.Snapshot.prototype.setShipsList = function(value) {
  jspb.Message.setRepeatedWrapperField(this, 1, value);
};


proto.world.Snapshot.prototype.clearShipsList = function() {
  this.setShipsList([]);
};



/**
 * @fileoverview
 * @enhanceable
 * @public
 */
// GENERATED CODE -- DO NOT EDIT!

goog.provide('proto.world.GameUpdate');

goog.require('jspb.Message');
goog.require('jspb.BinaryReader');
goog.require('jspb.BinaryWriter');
goog.require('proto.world.Snapshot');


/**
 * Generated by JsPbCodeGenerator.
 * @param {Array=} opt_data Optional initial data array, typically from a
 * server response, or constructed directly in Javascript. The array is used
 * in place and becomes part of the constructed object. It is not cloned.
 * If no data is provided, the constructed object will be empty, but still
 * valid.
 * @extends {jspb.Message}
 * @constructor
 */
proto.world.GameUpdate = function(opt_data) {
  jspb.Message.initialize(this, opt_data, 0, -1, null, proto.world.GameUpdate.oneofGroups_);
};
goog.inherits(proto.world.GameUpdate, jspb.Message);
if (goog.DEBUG && !COMPILED) {
  proto.world.GameUpdate.displayName = 'proto.world.GameUpdate';
}
/**
 * Oneof group definitions for this message. Each group defines the field
 * numbers belonging to that group. When of these fields' value is set, all
 * other fields in the group are cleared. During deserialization, if multiple
 * fields are encountered for a group, only the last value seen will be kept.
 * @private {!Array<!Array<number>>}
 * @const
 */
proto.world.GameUpdate.oneofGroups_ = [[1,2]];

/**
 * @enum {number}
 */
proto.world.GameUpdate.UpdateCase = {
  UPDATE_NOT_SET: 0,
  FOCUSENTITYID: 1,
  SNAPSHOT: 2
};

/**
 * @return {proto.world.GameUpdate.UpdateCase}
 */
proto.world.GameUpdate.prototype.getUpdateCase = function() {
  return /** @type {proto.world.GameUpdate.UpdateCase} */(jspb.Message.computeOneofCase(this, proto.world.GameUpdate.oneofGroups_[0]));
};



if (jspb.Message.GENERATE_TO_OBJECT) {
/**
 * Creates an object representation of this proto suitable for use in Soy templates.
 * Field names that are reserved in JavaScript and will be renamed to pb_name.
 * To access a reserved field use, foo.pb_<name>, eg, foo.pb_default.
 * For the list of reserved names please see:
 *     com.google.apps.jspb.JsClassTemplate.JS_RESERVED_WORDS.
 * @param {boolean=} opt_includeInstance Whether to include the JSPB instance
 *     for transitional soy proto support: http://goto/soy-param-migration
 * @return {!Object}
 */
proto.world.GameUpdate.prototype.toObject = function(opt_includeInstance) {
  return proto.world.GameUpdate.toObject(opt_includeInstance, this);
};


/**
 * Static version of the {@see toObject} method.
 * @param {boolean|undefined} includeInstance Whether to include the JSPB
 *     instance for transitional soy proto support:
 *     http://goto/soy-param-migration
 * @param {!proto.world.GameUpdate} msg The msg instance to transform.
 * @return {!Object}
 */
proto.world.GameUpdate.toObject = function(includeInstance, msg) {
  var f, obj = {
    focusentityid: jspb.Message.getField(msg, 1),
    snapshot: (f = msg.getSnapshot()) && proto.world.Snapshot.toObject(includeInstance, f)
  };

  if (includeInstance) {
    obj.$jspbMessageInstance = msg
  }
  return obj;
};
}


/**
 * Deserializes binary data (in protobuf wire format).
 * @param {jspb.ByteSource} bytes The bytes to deserialize.
 * @return {!proto.world.GameUpdate}
 */
proto.world.GameUpdate.deserializeBinary = function(bytes) {
  var reader = new jspb.BinaryReader(bytes);
  var msg = new proto.world.GameUpdate;
  return proto.world.GameUpdate.deserializeBinaryFromReader(msg, reader);
};


/**
 * Deserializes binary data (in protobuf wire format) from the
 * given reader into the given message object.
 * @param {!proto.world.GameUpdate} msg The message object to deserialize into.
 * @param {!jspb.BinaryReader} reader The BinaryReader to use.
 * @return {!proto.world.GameUpdate}
 */
proto.world.GameUpdate.deserializeBinaryFromReader = function(msg, reader) {
  while (reader.nextField()) {
    if (reader.isEndGroup()) {
      break;
    }
    var field = reader.getFieldNumber();
    switch (field) {
    case 1:
      var value = /** @type {number} */ (reader.readUint64());
      msg.setFocusentityid(value);
      break;
    case 2:
      var value = new proto.world.Snapshot;
      reader.readMessage(value,proto.world.Snapshot.deserializeBinaryFromReader);
      msg.setSnapshot(value);
      break;
    default:
      reader.skipField();
      break;
    }
  }
  return msg;
};


/**
 * Class method variant: serializes the given message to binary data
 * (in protobuf wire format), writing to the given BinaryWriter.
 * @param {!proto.world.GameUpdate} message
 * @param {!jspb.BinaryWriter} writer
 */
proto.world.GameUpdate.serializeBinaryToWriter = function(message, writer) {
  message.serializeBinaryToWriter(writer);
};


/**
 * Serializes the message to binary data (in protobuf wire format).
 * @return {!Uint8Array}
 */
proto.world.GameUpdate.prototype.serializeBinary = function() {
  var writer = new jspb.BinaryWriter();
  this.serializeBinaryToWriter(writer);
  return writer.getResultBuffer();
};


/**
 * Serializes the message to binary data (in protobuf wire format),
 * writing to the given BinaryWriter.
 * @param {!jspb.BinaryWriter} writer
 */
proto.world.GameUpdate.prototype.serializeBinaryToWriter = function (writer) {
  var f = undefined;
  f = this.getFocusentityid();
  if (f != null) {
    writer.writeUint64(
      1,
      f
    );
  }
  f = this.getSnapshot();
  if (f != null) {
    writer.writeMessage(
      2,
      f,
      proto.world.Snapshot.serializeBinaryToWriter
    );
  }
};


/**
 * Creates a deep clone of this proto. No data is shared with the original.
 * @return {!proto.world.GameUpdate} The clone.
 */
proto.world.GameUpdate.prototype.cloneMessage = function() {
  return /** @type {!proto.world.GameUpdate} */ (jspb.Message.cloneMessage(this));
};


/**
 * optional uint64 focusEntityId = 1;
 * @return {number?}
 */
proto.world.GameUpdate.prototype.getFocusentityid = function() {
  return /** @type {number?} */ (jspb.Message.getField(this, 1));
};


/** @param {number?|undefined} value  */
proto.world.GameUpdate.prototype.setFocusentityid = function(value) {
  jspb.Message.setOneofField(this, 1, proto.world.GameUpdate.oneofGroups_[0], value);
};


proto.world.GameUpdate.prototype.clearFocusentityid = function() {
  jspb.Message.setOneofField(this, 1, proto.world.GameUpdate.oneofGroups_[0], undefined);
};


/**
 * optional Snapshot snapshot = 2;
 * @return {proto.world.Snapshot}
 */
proto.world.GameUpdate.prototype.getSnapshot = function() {
  return /** @type{proto.world.Snapshot} */ (
    jspb.Message.getWrapperField(this, proto.world.Snapshot, 2));
};


/** @param {proto.world.Snapshot|undefined} value  */
proto.world.GameUpdate.prototype.setSnapshot = function(value) {
  jspb.Message.setOneofWrapperField(this, 2, proto.world.GameUpdate.oneofGroups_[0], value);
};


proto.world.GameUpdate.prototype.clearSnapshot = function() {
  this.setSnapshot(undefined);
};



