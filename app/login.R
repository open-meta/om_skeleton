### om_skeleton login.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

output$pageStub <- renderUI({
   x = rv$limn
   if(page_debug_on) {
      cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
   }
   if(session$userData$user$sp) {         # User superpower of 1 or more (TRUE) means a user is logged in
      tagList(                            # Menus should not lead here if logged in; but still possible via URL
         HTML(paste0("<p>You are logged in, ", session$userData$user$username, "</p>")),
         actionButton("logout_btn", "Logout", class="btn-primary btn-sm", style="margin:.8em;")
      )
   } else {                               # Otherwise, show login page
      tagList(
         fluidRow(
            column(4, offset=4,
               fluidRow(
                  column(12,
                     ttextInput("username", "User Name", value="", style="width: 100%;", autofocus=TRUE),
                     passwordInput("password", "Password:", value = "")
                  ),
                  column(6,
                     HTML('<h5><a href="?profile">Register</a></h5>',
                          '<h5><a href="?lostpassword">Lost Password</a></h5>'),
                           checkboxInput("stayloggedin", "Stay logged in.")
                  ),
                  column(2, offset=2,
                     actionButton("login_btn", "Login", class="btn-primary btn-sm", style="margin:.8em;")
                  )
               )
            )
         )
      )
   }
})

observeEvent(input$login_btn, {
   errormsg = ""
   u = userGet("username", input$username)
   if(u$username != "") {                                  # valid username?
      if(checkpw(input$password, u$hashed_pw)) {           # right password?
         newID <- generate_id()                            # create a new session id
         if(input$stayloggedin) {                          # save ID to user's browser
            js$setCookie("sessionID", newID, days=14)        # expire after 14 days
         } else {
            js$setCookie("sessionID", newID, days=0)         # expire after 0 days
         }
         session$userData$user <- u
         session$userData$user$sessionid <- newID          # update sessionid
         session$userData$user$lastlogin_date <- now()     # update last login date
         userSave(session$userData$user)                   # save new user data
         if(!u$emailverified) {
            js$redirect("?profile")                        # if email is invalid, go to profile to fix it
         } else {
         js$redirect("?home")                              # on successful login, go to home page
         }
      } else {
         errormsg = "Incorrect Password."
      }
   } else {
      errormsg = "Unknown User Name."
   }
   if(nchar(errormsg)>0) {
      session$userData$modal_title <- "Whoops!"
      session$userData$modal_text <- paste0("<p>", errormsg, "</p>")
      rv$modal_warning <- rv$modal_warning + 1
   }
})

observeEvent(input$logout_btn, {
   js$removeCookie("sessionID")                            # delete session id from browser...
   session$userData$user$sessionid <- ""                   #   and session$userData$users
   userSave(session$userData$user)                         #   and om_users
   session$userData$user <- buildU()                       # now clear session$userData$users
   rv$limn = rv$limn + 1
})
