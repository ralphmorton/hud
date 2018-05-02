
exports.getPathInfo_ = function () {
    function buildM (c, s) {
        var sp = s.replace(c, '');
        var qpx = sp.split('&');
        var qmap = {};
        for (var i = 0; i < qpx.length; i++) {
            var parts = qpx[i].split('=');
            var key = parts[0] || '';
            var val = parts[1] || '';
            qmap[key] = val;
        }
        return qmap;
    };

    function extractPI (path, qpx, hpx) {
        var parts = path.split('/');
        var res = [];
        for (var i = 0; i < parts.length; i++) {
            var part = parts[i];
            if (part) {
                res.push(decodeURIComponent(part));
            }
        }
        return {
            px: res,
            qpx: buildM('?', qpx),
            hpx: buildM('#', hpx)
        };
    };

    var path = document.location.pathname;
    var search = document.location.search || '';
    var hash = document.location.hash || '';

    return extractPI(path, search, hash);
};
