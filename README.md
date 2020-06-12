mkdir -p ./.hoogle
hoogle generate --database=.hoogle/db.hoo -l
hoogle server --port=8008 --local --database=.hoogle/db.hoo

https://stackoverflow.com/questions/11832591/javascript-library-for-search-engine-style-searching

https://github.com/spacchetti/purescript-docs-search
https://github.com/spacchetti/purescript-docs-search/blob/410a14eb1f757602b282ae5cdaad121d9f5398f7/src/Docs/Search/TypeIndex.js









mkdir -p ./html
rm -f ./html/*


cd frontend/
spago build
parcel build  --no-content-hash --public-url "./" --no-source-maps index.html



mv dist2/page*js dist2/page.js
mv dist2/style*css dist2/style.css
