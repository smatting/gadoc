var Main = require('./output/Main');


function main () {
    /*
    Here we could add variables such as


    Parcel will replace `process.env.BASE_URL`
    with the string contents of the BASE_URL environment
    variable at bundle/build time.
    A .env file can also be used to override shell variables
    for more information, see https://en.parceljs.org/env.html

    These variables can be supplied to the Main.main function.
    However, you will need to change the type to accept variables, by default it is an Effect.
    You will probably want to make it a function from String -> Effect ()
  */
  window.hdassets = {};

  Main.main(process.env.WHNGAPI_BASEURL);
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    // Use this if below doesnt work
    // location.reload();

    console.log('hot module accepted');
    Array.from(document.querySelectorAll('body *')).map(x => x.remove());
    console.log('deleted old entries');
    main()
  });
}

console.log('Starting app');

main();
