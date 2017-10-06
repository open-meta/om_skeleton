### om_skeleton lostpassword.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

session$userData$userexists <- FALSE                 # flags for where we are in the process
session$userData$codematch <- FALSE

output$pageStub <- renderUI({rv$limn; isolate({
   if(page_debug_on) {
      cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
   }
   if(session$userData$user$sp && !session$userData$userexists) { # user is already logged in; can happen via URL
      js$redirect("?login")
   } else {                                          # step 1, get the username
      if(!session$userData$codematch && !session$userData$userexists) {
         return(tagList(
            fluidRow(
               column(4, offset=4,
                  ttextInput("username", "User Name", value="", style="width: 100%;", autofocus=TRUE),
                  actionButton("submitName_btn", "Submit", class="btn-primary btn-sm",
                     style="display: block; margin-left: auto; margin-right: auto")
               )
            )
         ))
      }                                              # step 2, and send and receive code
      if(!session$userData$codematch && session$userData$userexists) {
         session$userData$tempcode <- generate_code()
         sendmail(paste0("<", sendmail_from, ">"), paste0("<", session$userData$user$email, ">"),
               paste0("Code to verify your ", site_name," account."),
               paste0("Here's the code you must enter to reset your ", site_name," password: ", session$userData$tempcode))
         return(tagList(
            fluidRow(
               column(6, offset=3,
                  HTML(paste0(
                     "<h3>Almost there...</h3><p>We've just sent an email to <b>", session$userData$user$email,
                     "</b> with a 6-digit temporarty PIN. To update your password, ",
                     "enter the PIN here and click the OK button.</p>")),
                  ttextInput("tempcode", "PIN from email", value="", style="width: 100%;"),
                  actionButton("submitCode_btn", "OK", class="btn-primary btn-sm")
               )
            )
         ))
      }                                              # step 3, get the new password
      if(session$userData$codematch) {
         return(tagList(
            fluidRow(
               column(4, offset=4,
                  HTML("<h4>Enter your new password:</h4>"),
                  passwordInput("password1", "New Password:", value <- ""),
                  passwordInput("password2", "Repeat New Password:", value <- ""),
                  actionButton("submitPW_btn", "Submit", class="btn-primary btn-sm",
                     style="display: block; margin-left: auto; margin-right: auto")
               )
            )
         ))
      }
   }
})})




observeEvent(input$submitName_btn, {                 # Handle name submission
   alertText <- ""
   u <- userGet("username", input$username)
   if(u$username == "") {
      alertText <- paste0(alertText, "<p>Sorry, we don't have a user with that name.")
   } else {
      if(!u$emailverified) {
         alertText <- paste0(alertText, "<p>Sorry, we don't have a verified email address for that name, so we cannot
         send you a code to reset your password. You can abandon this user name and create a new account.</p>")
      }
   }
   if(nchar(alertText)>0) {
      session$userData$modal_title <- "Whoops!"
      session$userData$modal_text <- alertText
      rv$modal_warning <- rv$modal_warning + 1
   } else {
      session$userData$userexists <- TRUE
      session$userData$user <- u                     # We have a user with a verified email; go get the code
      rv$limn <- rv$limn + 1
   }
})

observeEvent(input$submitCode_btn, {                 # Did user get our email?
   wrongcode <- 0                                    # counter to limit code guessing
   if(input$tempcode != session$userData$tempcode) { # handle wrong code
      wrongcode <- wrongcode + 1
      if(wrongcode<4) {                              # You only get 3 tries to enter the code
         session$userData$modal_title <- "Whoops!"
         session$userData$modal_text <- "<p>Wrong code. Recheck that email and try again.</p>"
         rv$modal_warning <- rv$modal_warning + 1
      } else {
         session$userData$modal_title <- "Whoops!"
         session$userData$modal_text <- "<p>Sorry, that's too many bad tries. Let's start over.</p>"
         rv$modal_warning <- rv$modal_warning + 1
         js$redirect("?home")
      }
   } else {
      session$userData$codematch <- TRUE             # go to step 3, getting the new password
      rv$limn <- rv$limn + 1
   }
})

observeEvent(input$submitPW_btn, {                   # make sure the password is ok
   alertText <- ""
   if(input$password1 == "") {
      alertText <- paste0(alertText, "<p>Password can't be blank.</p>")
   }
   if(nchar(input$password1) > 30) {
      alertText <- paste0(alertText, "<p>Password is limited to 30 characters.</p>")
   }
   if(input$password1 != input$password2) {
      alertText <- paste0(alertText, "<p>Passwords don't match.</p>")
   }
   if(nchar(alertText)>0) {
      session$userData$modal_title <- "Whoops!"
      session$userData$modal_text <- alertText
      rv$modal_warning <- rv$modal_warning + 1
   } else {
      newID <- generate_id()                                       # create a new session id
      js$setid(id=newID, days=0)                                   # set cookie
      session$userData$user$hashed_pw <- hashpw(input$password1)   # set password; already have username and email
      session$userData$user$sessionid <- newID                     # set sessionid in user
      session$userData$user$lastlogin_date <- now()                # set login date
      userSave(session$userData$user)                              #    and save the record
      session$userData$modal_title <- "All set!"
      session$userData$modal_text <- "<p>Your password has been changed.</p>"
      rv$modal_warning <- rv$modal_warning + 1
      js$redirect("?home")                           # Successful password change leads to home
   }
})
