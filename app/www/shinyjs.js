

//   shinyjs.getcookie = function(params) {
//      var cookie = Cookies.get("id");
//      if (typeof cookie == "undefined") {
//         var cookie = ""; }
//      Shiny.onInputChange("jscookie", cookie);
//   }

//   shinyjs.setcookie = function(params) {
//      Cookies.set("id", escape(params), { expires: 0.5 });
//      Cookies.set("id", escape(params));
//      Shiny.onInputChange("jscookie", params);
//   }

//   shinyjs.rmcookie = function(params) {
//      Cookies.remove("id");
//      Shiny.onInputChange("jscookie", "");
//   }

   shinyjs.redirect = function(url) {
      window.location = url;
   }

   shinyjs.alert = function(txt) {
      alert(txt);
   }

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
