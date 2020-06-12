
/*
function htmlspecialchars(str) {
    var map = {
        "&": "&amp;",
        "<": "&lt;",
        ">": "&gt;",
        "\"": "&quot;",
        "'": "&#39;" // ' -> &apos; for XML only
    };
    return str.replace(/[&<>"']/g, function(m) { return map[m]; });
}
*/

exports.unEscapeHTML = function(str) {
    var map = {
        "&amp;": "&",
        "&lt;": "<",
        "&gt;": ">",
        "&quot;": "\"",
        "&#39;": "'"
    };
    return str.replace(/(&amp;|&lt;|&gt;|&quot;|&#39;)/g, function(m) { return map[m]; });
}
