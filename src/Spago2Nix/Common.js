"use strict";

exports.implStringifyPretty = function (spaces, json) {
    return JSON.stringify(json, null, spaces)
}