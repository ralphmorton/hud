
exports.navigate_ = function (path) {
    return function () {
        window.location.href = '/' + path;
    };
};
