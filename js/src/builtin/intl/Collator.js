/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/********** Intl.Collator **********/


/**
 * Mapping from Unicode extension keys for collation to options properties,
 * their types and permissible values.
 *
 * Spec: ECMAScript Internationalization API Specification, 10.1.1.
 */
var collatorKeyMappings = {
    kn: {property: "numeric", type: "boolean"},
    kf: {property: "caseFirst", type: "string", values: ["upper", "lower", "false"]}
};


/**
 * Compute an internal properties object from |lazyCollatorData|.
 */
function resolveCollatorInternals(lazyCollatorData)
{
    assert(IsObject(lazyCollatorData), "lazy data not an object?");

    var internalProps = std_Object_create(null);

    // Step 7.
    internalProps.usage = lazyCollatorData.usage;

    // Step 8.
    var Collator = collatorInternalProperties;

    // Step 9.
    var collatorIsSorting = lazyCollatorData.usage === "sort";
    var localeData = collatorIsSorting
                     ? Collator.sortLocaleData
                     : Collator.searchLocaleData;

    // Compute effective locale.
    // Step 14.
    var relevantExtensionKeys = Collator.relevantExtensionKeys;

    // Step 15.
    var r = ResolveLocale(callFunction(Collator.availableLocales, Collator),
                          lazyCollatorData.requestedLocales,
                          lazyCollatorData.opt,
                          relevantExtensionKeys,
                          localeData);

    // Step 16.
    internalProps.locale = r.locale;

    // Steps 17-19.
    var key, property, value, mapping;
    var i = 0, len = relevantExtensionKeys.length;
    while (i < len) {
        // Step 19.a.
        key = relevantExtensionKeys[i];
        if (key === "co") {
            // Step 19.b.
            property = "collation";
            value = r.co === null ? "default" : r.co;
        } else {
            // Step 19.c.
            mapping = collatorKeyMappings[key];
            property = mapping.property;
            value = r[key];
            if (mapping.type === "boolean")
                value = value === "true";
        }

        // Step 19.d.
        internalProps[property] = value;

        // Step 19.e.
        i++;
    }

    // Compute remaining collation options.
    // Steps 21-22.
    var s = lazyCollatorData.rawSensitivity;
    if (s === undefined) {
        if (collatorIsSorting) {
            // Step 21.a.
            s = "variant";
        } else {
            // Step 21.b.
            var dataLocale = r.dataLocale;
            var dataLocaleData = localeData(dataLocale);
            s = dataLocaleData.sensitivity;
        }
    }
    internalProps.sensitivity = s;

    // Step 24.
    internalProps.ignorePunctuation = lazyCollatorData.ignorePunctuation;

    // Step 25.
    internalProps.boundFormat = undefined;

    // The caller is responsible for associating |internalProps| with the right
    // object using |setInternalProperties|.
    return internalProps;
}


/**
 * Returns an object containing the Collator internal properties of |obj|.
 */
function getCollatorInternals(obj) {
    assert(IsObject(obj), "getCollatorInternals called with non-object");
    assert(IsCollator(obj), "getCollatorInternals called with non-Collator");

    var internals = getIntlObjectInternals(obj);
    assert(internals.type === "Collator", "bad type escaped getIntlObjectInternals");

    // If internal properties have already been computed, use them.
    var internalProps = maybeInternalProperties(internals);
    if (internalProps)
        return internalProps;

    // Otherwise it's time to fully create them.
    internalProps = resolveCollatorInternals(internals.lazyData);
    setInternalProperties(internals, internalProps);
    return internalProps;
}


/**
 * Initializes an object as a Collator.
 *
 * This method is complicated a moderate bit by its implementing initialization
 * as a *lazy* concept.  Everything that must happen now, does -- but we defer
 * all the work we can until the object is actually used as a Collator.  This
 * later work occurs in |resolveCollatorInternals|; steps not noted here occur
 * there.
 *
 * Spec: ECMAScript Internationalization API Specification, 10.1.1.
 */
function InitializeCollator(collator, locales, options) {
    assert(IsObject(collator), "InitializeCollator called with non-object");
    assert(IsCollator(collator), "InitializeCollator called with non-Collator");

    // Steps 1-2 (These steps are no longer required and should be removed
    // from the spec; https://github.com/tc39/ecma402/issues/115).;

    // Lazy Collator data has the following structure:
    //
    //   {
    //     requestedLocales: List of locales,
    //     usage: "sort" / "search",
    //     opt: // opt object computed in InitializeCollator
    //       {
    //         localeMatcher: "lookup" / "best fit",
    //         kn: true / false / undefined,
    //         kf: "upper" / "lower" / "false" / undefined
    //       }
    //     rawSensitivity: "base" / "accent" / "case" / "variant" / undefined,
    //     ignorePunctuation: true / false
    //   }
    //
    // Note that lazy data is only installed as a final step of initialization,
    // so every Collator lazy data object has *all* these properties, never a
    // subset of them.
    var lazyCollatorData = std_Object_create(null);

    // Step 3.
    var requestedLocales = CanonicalizeLocaleList(locales);
    lazyCollatorData.requestedLocales = requestedLocales;

    // Steps 4-5.
    //
    // If we ever need more speed here at startup, we should try to detect the
    // case where |options === undefined| and Object.prototype hasn't been
    // mucked with.  (|options| is fully consumed in this method, so it's not a
    // concern that Object.prototype might be touched between now and when
    // |resolveCollatorInternals| is called.)  For now, just keep it simple.
    if (options === undefined)
        options = {};
    else
        options = ToObject(options);

    // Compute options that impact interpretation of locale.
    // Step 6.
    var u = GetOption(options, "usage", "string", ["sort", "search"], "sort");
    lazyCollatorData.usage = u;

    // Step 10.
    var opt = new Record();
    lazyCollatorData.opt = opt;

    // Steps 11-12.
    var matcher = GetOption(options, "localeMatcher", "string", ["lookup", "best fit"], "best fit");
    opt.localeMatcher = matcher;

    // Step 13, unrolled.
    var numericValue = GetOption(options, "numeric", "boolean", undefined, undefined);
    if (numericValue !== undefined)
        numericValue = numericValue ? 'true' : 'false';
    opt.kn = numericValue;

    var caseFirstValue = GetOption(options, "caseFirst", "string", ["upper", "lower", "false"], undefined);
    opt.kf = caseFirstValue;

    // Compute remaining collation options.
    // Step 20.
    var s = GetOption(options, "sensitivity", "string",
                      ["base", "accent", "case", "variant"], undefined);
    lazyCollatorData.rawSensitivity = s;

    // Step 23.
    var ip = GetOption(options, "ignorePunctuation", "boolean", undefined, false);
    lazyCollatorData.ignorePunctuation = ip;

    // Step 26.
    //
    // We've done everything that must be done now: mark the lazy data as fully
    // computed and install it.
    initializeIntlObject(collator, "Collator", lazyCollatorData);
}


/**
 * Returns the subset of the given locale list for which this locale list has a
 * matching (possibly fallback) locale. Locales appear in the same order in the
 * returned list as in the input list.
 *
 * Spec: ECMAScript Internationalization API Specification, 10.2.2.
 */
function Intl_Collator_supportedLocalesOf(locales /*, options*/) {
    var options = arguments.length > 1 ? arguments[1] : undefined;

    var availableLocales = callFunction(collatorInternalProperties.availableLocales,
                                        collatorInternalProperties);
    var requestedLocales = CanonicalizeLocaleList(locales);
    return SupportedLocales(availableLocales, requestedLocales, options);
}


/**
 * Collator internal properties.
 *
 * Spec: ECMAScript Internationalization API Specification, 9.1 and 10.2.3.
 */
var collatorInternalProperties = {
    sortLocaleData: collatorSortLocaleData,
    searchLocaleData: collatorSearchLocaleData,
    _availableLocales: null,
    availableLocales: function()
    {
        var locales = this._availableLocales;
        if (locales)
            return locales;

        locales = intl_Collator_availableLocales();
        addSpecialMissingLanguageTags(locales);
        return (this._availableLocales = locales);
    },
    relevantExtensionKeys: ["co", "kn", "kf"]
};


/**
 * Returns the default caseFirst values for the given locale and usage. The
 * first element in the returned array denotes the default value per ES2017
 * Intl, 9.1 Internal slots of Service Constructors.
 */
function collatorCaseFirst(locale, usage) {
    assert(typeof locale === "string", "locale should be string");
    assert(usage === "sort" || usage === "search", "invalid usage option");

    if (usage === "sort") {
        // If |locale| is the default locale (e.g. da-DK), but only supported
        // through a fallback (da), we need to get the actual locale before we
        // can call intl_isUpperCaseFirst. Also see BestAvailableLocaleHelper.
        var availableLocales = callFunction(collatorInternalProperties.availableLocales,
                                            collatorInternalProperties);
        var actualLocale = BestAvailableLocaleIgnoringDefault(availableLocales, locale);

        if (intl_isUpperCaseFirst(actualLocale))
            return ["upper", "false", "lower"];
    }

    // Default caseFirst values for all other languages.
    return ["false", "lower", "upper"];
}


function collatorSortLocaleData(locale) {
    return {
        co: intl_availableCollations(locale),
        kn: ["false", "true"],
        kf: collatorCaseFirst(locale, "sort"),
    };
}


function collatorSearchLocaleData(locale) {
    return {
        co: [null],
        kn: ["false", "true"],
        kf: collatorCaseFirst(locale, "search"),
        // In theory the default sensitivity is locale dependent;
        // in reality the CLDR/ICU default strength is always tertiary.
        sensitivity: "variant"
    };
}


/**
 * Function to be bound and returned by Intl.Collator.prototype.format.
 *
 * Spec: ECMAScript Internationalization API Specification, 12.3.2.
 */
function collatorCompareToBind(x, y) {
    // Steps 1.a.i-ii implemented by ECMAScript declaration binding instantiation,
    // ES5.1 10.5, step 4.d.ii.

    // Step 1.a.iii-v.
    var X = ToString(x);
    var Y = ToString(y);
    return intl_CompareStrings(this, X, Y);
}


/**
 * Returns a function bound to this Collator that compares x (converted to a
 * String value) and y (converted to a String value),
 * and returns a number less than 0 if x < y, 0 if x = y, or a number greater
 * than 0 if x > y according to the sort order for the locale and collation
 * options of this Collator object.
 *
 * Spec: ECMAScript Internationalization API Specification, 10.3.2.
 */
function Intl_Collator_compare_get() {
    // Check "this Collator object" per introduction of section 10.3.
    if (!IsObject(this) || !IsCollator(this))
        ThrowTypeError(JSMSG_INTL_OBJECT_NOT_INITED, "Collator", "compare", "Collator");

    var internals = getCollatorInternals(this);

    // Step 1.
    if (internals.boundCompare === undefined) {
        // Step 1.a.
        var F = collatorCompareToBind;

        // Steps 1.b-d.
        var bc = callFunction(FunctionBind, F, this);
        internals.boundCompare = bc;
    }

    // Step 2.
    return internals.boundCompare;
}


/**
 * Returns the resolved options for a Collator object.
 *
 * Spec: ECMAScript Internationalization API Specification, 10.3.3 and 10.4.
 */
function Intl_Collator_resolvedOptions() {
    // Check "this Collator object" per introduction of section 10.3.
    if (!IsObject(this) || !IsCollator(this))
        ThrowTypeError(JSMSG_INTL_OBJECT_NOT_INITED, "Collator", "resolvedOptions", "Collator");

    var internals = getCollatorInternals(this);

    var result = {
        locale: internals.locale,
        usage: internals.usage,
        sensitivity: internals.sensitivity,
        ignorePunctuation: internals.ignorePunctuation
    };

    var relevantExtensionKeys = collatorInternalProperties.relevantExtensionKeys;
    for (var i = 0; i < relevantExtensionKeys.length; i++) {
        var key = relevantExtensionKeys[i];
        var property = (key === "co") ? "collation" : collatorKeyMappings[key].property;
        _DefineDataProperty(result, property, internals[property]);
    }
    return result;
}

