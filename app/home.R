### om_skeleton home.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

rv$trigger = 0
output$pageStub <- renderUI({
   x = rv$limn
   if(page_debug_on) {
      cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
   }
   if(session$userData$user$sp) {
      tagList(
         HTML(paste0('<h4>You are logged in. This is your data:</h4>')),
         dataTableOutput("user")
      )
   } else {
      tagList(
         HTML(paste0("<h4>You are logged out.</h4>
                     <p>This is a skeleton for a multi-page, multi-user web site with user
                     authentication, built with R and Shiny. The commented code is at
                     <a href='https://github.com/open-meta/om_skeleton/tree/master/app'>GitHub</a>
                     and <a href='http://www.open-meta.org/technology/multi-page-url-based-shiny-web-site-skeleton-with-authentication/'>
                     you can read more about the project here</a>.</p>
                     <p>To sign into the administrative account you must use the password you specified in the
                     credentials.R file. After you are logged in, you will see an additional menu item
                     for the administrator.</p>
                     <p>In order to register for additional accounts, you have to provide email credentials
                     in the credentials.R file. Registration requires verification of the user's email adddress.
                     In this example code, user data is saved in a table in the file system, which isn't the
                     best way to do it for a multi-user system, but it's easy and works universally.</p>"))
      )
   }
})

output$user = renderDataTable(session$userData$user)


