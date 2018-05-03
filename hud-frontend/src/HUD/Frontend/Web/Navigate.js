
exports.navigate_ = function (path) {
    return function () {
        window.location.href = '/' + path;
    };
};

exports.redirect_ = function (url) {
    return function () {
        window.location.href = url;
    };
};

exports.origin_ = function () {
    return window.location.origin
};
