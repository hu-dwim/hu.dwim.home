// usage:
//   ~/common-lisp/hu.dwim.web-server/etc/build-dojo.sh --dojo ~/workspace/dojotoolkit/ --dojo-release-dir ~/common-lisp/hu.dwim.home/www/ --profile ~/common-lisp/hu.dwim.home/etc/dojo-build-profile.js
//   ${DWIM_WORKSPACE}/hu.dwim.web-server/etc/build-dojo.sh --dojo ${DWIM_WORKSPACE}/dojotoolkit/ --dojo-release-dir ${DWIM_WORKSPACE}/hu.dwim.home/www/ --profile ${DWIM_WORKSPACE}/hu.dwim.home/etc/dojo-build-profile.js
//
// based on https://github.com/csnover/dojo-boilerplate/blob/master/profiles/app.profile.js

var profile = {
    // `basePath` is relative to the directory containing this profile file
    // it's provided by a command line switch
    // basePath: '../../../workspace/dojotoolkit/',

    // Builds a new release.
    action: 'release',

    // Strips all comments and whitespace from CSS files and inlines @imports where possible.
    cssOptimize: 'comments',

    // Excludes tests, demos, and original template files from being included in the built version.
    mini: true,

    // Uses Closure Compiler as the JavaScript minifier. This can also be set to "shrinksafe" to use ShrinkSafe,
    // though ShrinkSafe is deprecated and not recommended.
    // This option defaults to "" (no compression) if not provided.
    optimize: 'closure',

    // We're building layers, so we need to set the minifier to use for those, too.
    // This defaults to "shrinksafe" if not provided.
    layerOptimize: 'closure',

    // A list of packages that will be built. The same packages defined in the loader should be defined here in the
    // build profile.
    packages: [
	// Using a string as a package is shorthand for `{ name: 'app', location: 'app' }`
	'dijit',
	'dojo',
	'dojox',
        // { name: 'ekor', location: '../www/js/ekor/'},
    ],

    // Strips all calls to console functions within the code. You can also set this to "warn" to strip everything
    // but console.error, and any other truthy value to strip everything but console.warn and console.error.
    // This defaults to "normal" (strip all but warn and error) if not provided.
    stripConsole: 'all',

    // The default selector engine is not included by default in a dojo.js build in order to make mobile builds
    // smaller. We add it back here to avoid that extra HTTP request. There is also an "acme" selector available; if
    // you use that, you will need to set the `selectorEngine` property in index.html, too.
    selectorEngine: 'lite',

    // Any module in an application can be converted into a "layer" module, which consists of the original module +
    // additional dependencies built into the same file. Using layers allows applications to reduce the number of HTTP
    // requests by combining all JavaScript into a single file.
    layers: {
	// This is the main loader module. It is a little special because it is treated like an AMD module even though
	// it is actually just plain JavaScript. There is some extra magic in the build system specifically for this
	// module ID.
	'dojo/dojo': {
	    // By default, the build system will try to include `dojo/main` in the built `dojo/dojo` layer, which adds
	    // a bunch of stuff we do not want or need. We want the initial script load to be as small and quick to
	    // load as possible, so we configure it as a custom, bootable base.
	    boot: true,
	    customBase: true,
            include:
            ["dojo/main",
             "dojo/parser",
             "dojo/fx",
             "dojo/string",
             "dojo/cache",
             "dojox/xml/parser",

             "dijit/MenuBar",
             "dijit/MenuBarItem",
             "dijit/PopupMenuBarItem",
             "dijit/Menu",
             "dijit/MenuItem",
             "dijit/form/NumberTextBox",
             "dijit/form/DateTextBox",
             "dijit/Dialog",
             "dijit/TooltipDialog",
             "dijit/InlineEditBox",
             "dijit/form/Form",
             "dijit/form/ComboBoxMixin",
             "dijit/form/ToggleButton",
             "dijit/form/FilteringSelect",
             "dijit/form/SimpleTextarea",
             "dijit/form/ValidationTextBox",
             "dijit/Editor",
             "dijit/_editor/plugins/AlwaysShowToolbar",
             "dijit/_editor/plugins/EnterKeyHandling",
             "dijit/_editor/plugins/FontChoice",
             "dijit/_editor/plugins/FullScreen",
             "dijit/_editor/plugins/LinkDialog",
             "dijit/_editor/plugins/TextColor",

             "dojox/form/Uploader",
             "dojox/form/uploader/plugins/HTML5",
             "dojox/form/uploader/FileList",

             "dojox/editor/plugins/CollapsibleToolbar",
             "dojox/editor/plugins/StatusBar",
             "dojox/editor/plugins/TablePlugins",

             // "ekor/_editor/plugins/InsertVariable",
            ],
	},
    },

    // Providing hints to the build system allows code to be conditionally removed on a more granular level than simple
    // module dependencies can allow. This is especially useful for creating tiny mobile builds. Keep in mind that dead
    // code removal only happens in minifiers that support it! Currently, only Closure Compiler to the Dojo build system
    // with dead code removal. A documented list of has-flags in use within the toolkit can be found at
    // <http://dojotoolkit.org/reference-guide/dojo/has.html>.
    staticHasFeatures: {
	// The trace & log APIs are used for debugging the loader, so we do not need them in the build.
	'dojo-trace-api': false,
	'dojo-log-api': false,

	// This causes normally private loader data to be exposed for debugging. In a release build, we do not need
	// that either.
	'dojo-publish-privates': false,

	// If this application is pure AMD, then get rid of the legacy loader.
	'dojo-sync-loader': true,

	// `dojo-xhr-factory` relies on `dojo-sync-loader`.
	'dojo-xhr-factory': true,

	// We are not loading tests in production, so we can get rid of some test sniffing code.
	'dojo-test-sniff': false
    }
};
