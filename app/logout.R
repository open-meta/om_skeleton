### om_skeleton logout.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

output$pageStub <- renderUI({rv$limn; isolate({
   if(page_debug_on) {
      cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
   }
   rv$logout <- rv$logout+1                      # run observer below
   return(HTML("<h4>You are logged out.</h4>"))
})})


observeEvent(rv$logout, {
   if(rv$logout>0) {                            # Don't run if zero (initial run).
      if(session$userData$user$sp) {            # Don't run if already logged out; can happen via URL.
         js$removeid()                          # Delete session id from browser...
         session$userData$user$sessionid <- ""  #   and session$userData$users
         userSave(session$userData$user)        #   and users table.
         session$userData$user <- buildU()      # Now clear session$userData$users.
      }
   }
})
