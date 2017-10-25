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
                     authentication, built with R and Shiny. You can see the commented code at
                     <a href='https://github.com/open-meta/om_skeleton/tree/master/app'>GitHub</a>
                     and <a href='http://www.open-meta.org/technology/multi-page-url-based-shiny-web-site-skeleton-with-authentication/'>
                     you can read more about the project here</a>.</p>
                     <p>In this example, you can sign into the administrative account with the username <i>admin</i>
                     and the password <i>default</i>. After you are logged in, you will see an additional menu item
                     for the administrator.</p>
                     <p>You can also register for an account, which requires verification of the email adddress you
                     give. The user table is erased every time the app restarts, so your account won't last long,
                     but you can see how the email verification works if you are interested.</p>"))

      )
   }
})

output$user = renderDataTable(session$userData$user)


