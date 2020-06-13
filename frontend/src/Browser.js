exports.setDocumentTitle =
  function (title)
  {
    return function ()
    {
      window.document.title = title;
    };
  };
