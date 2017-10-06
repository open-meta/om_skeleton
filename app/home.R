### om_skeleton home.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

output$pageStub <- renderUI({rv$limn; isolate({   # <-- This way of starting a renderUI() function makes it work more like
   if(page_debug_on) {                                                   # an observeEvent() function. First the buzzer or
      cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))      # trigger variables are named and then then rest of
   }                                                                     # the render is wrapped in isolate() so nothing
   if(session$userData$user$sp) { # User sp > 0 will be TRUE               inside will cause the code to re-render accidentally
      return(tagList(                                                    # or unnecessarily. Also note that Shiny expects
         HTML('<h4>You are logged in. This is your data:</h4>'),         # a renderUI() to return text with embedded HTML.
         dataTableOutput("user")
      ))
   } else {
      return(tagList(                                                    # This is an "explicit return". See profile.R for
         HTML('<h4>You are logged out. This is an empty user table.</h4>'),   # detailed information on why life is better
         dataTableOutput("user")                                              # when you use return()s in render functions.
      ))
   }
})})                                                                     # <-- And you need to end it like this.

output$user <- renderDataTable(session$userData$user)                    # Note that this output is nested inside
                                                                         #    output$pageStub above


