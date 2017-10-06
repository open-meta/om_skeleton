### om_skeleton admin.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

output$pageStub <- renderUI({rv$limn; isolate({
   if(page_debug_on) {
      cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
   }
   if(session$userData$user$sp<webpage$sp) {       # Although users without superpowers can't get here via the
      return(taglist(                              #    menus, they CAN get here by typing in the URL.
         HTML(paste0("You haven't been granted access to this page."))
      ))
   } else {
      return(tagList(
         HTML(paste0('<h6>Welcome administrator!</h6><h4>Users table:</h4>')),
         dataTableOutput("users"),
         HTML(paste0('<h4>Pages table:</h4>')),
         tableOutput("pages")
      ))
   }
})})


output$users <- renderDataTable(site_users[,c("username", "emailverified", "sp", "sessionid", "reg_date", "ev_date", "lastlogin_date")])

output$pages <- renderTable(site_pages, digits=0)
