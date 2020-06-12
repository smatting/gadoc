/*
ported from Hoogle


    -1 if not a substring
    0 if exact match (+ case of first letter matches)
    1 if exact match
    2 if startswith
    4 if is substr
tie breaking by index of haystack

for multiple needles: minimum

*/
exports.textSearch = function(haystack) { return function(needles) { return function(exact) {
    var res = [];

    var uppers = [];
    var strings = [];
    for(i = 0; i < needles.length; i++) {
        uppers.push(needles[i][0] == " ");
        strings.push(uppers[i] ? needles[i].slice(1) : needles[i]);
    }

    for (var index = 0; ; index++)
    {
        var string = haystack[index];
        if (typeof string === 'undefined') {
            break;
        }
        const upper = string[0] == " ";
        if (upper) {
            string = string.slice(1);
        }

        var score = 4;
        for (var i = 0; i < needles.length; i++)
        {
            const pos = string.indexOf(strings[i]);
            if (pos < 0)
            {
                score = -1;
                break;
            }
            else if (pos == 0)
            {
                const complete = strings[i].length == string.length;
                const cased = uppers[i] == upper;
                score = Math.min(score, (!complete ? 2 : 0) + (!cased ? 1 : 0));
            }
        }
        if (score >= 0 && (!exact || score == 0)) {
            res.push((score << 24) | index);
        }
    }
    res = res.sort(function(a, b) {return a-b;})
    var results = []
    for (var i = 0; i < res.length; i++) {
        results.push(haystack[res[i] & 0xffffff])
    }
    return results;
}}}
