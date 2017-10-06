// Thanks to Dean Attali for the shinyjs package.

// Thanks to Calli Gross for explaining how to use shinyjs with cookies.
// https://calligross.de/post/using-cookie-based-authentication-with-shiny/
// https://gist.github.com/calligross/e779281b500eb93ee9e42e4d72448189

// sets of functions to set, get, and remove named cookies
//    first set, "id", is for session id (ses$id in R code)
   shinyjs.setid = function(params){
      var defaultParams = {
         id : null,
         days : 1
      };
      params = shinyjs.getParams(params, defaultParams);
      if(params.days>0) {
         Cookies.set("id", escape(params.id), { expires: params.days });
      } else {
         Cookies.set("id", escape(params.id));
      }
      Shiny.onInputChange("js.id", params.id);
   }
   shinyjs.getid = function() {
      var id = Cookies.get("id");
      if (typeof id == "undefined") {
         var id = ""; }
      Shiny.onInputChange("js.id", id);
   }

   shinyjs.removeid = function() {
      Cookies.remove("id");
      Shiny.onInputChange("js.id", "");
   }

// Moreover, now we can do stuff like:

   shinyjs.redirect = function(url) {
      window.location = url;
   }
