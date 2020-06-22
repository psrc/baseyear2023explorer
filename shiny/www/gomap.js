// When locator icon in datatable is clicked, go to that spot on the map
// Adapted from https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: long,
    nonce: Math.random()
  });
});

