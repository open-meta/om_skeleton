### om_skeleton profile.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

session$userData$fixingemail <- FALSE     # flag to separate fixing email from new regisitation

output$pageStub <- renderUI({rv$limn; isolate({
   if(page_debug_on) {
      cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
   }
   if(session$userData$user$sp==0) {      # not logged in; return registration inputs
      pageText <- tagList(
         fluidRow(
            column(4, offset=4,
               ttextInput("username", "User Name:", value="", style="width: 100%;", autofocus=TRUE),
               passwordInput("password1", "Password:", value=""),
               passwordInput("password2", "Repeat Password:", value=""),
               ttextInput("email", "Email address (for account verification):", value="", style="width: 100%;"),
               actionButton("register_btn", "Register", class="btn-primary btn-sm",
                            style="display: block; margin-top: 2em; margin-left: auto; margin-right: auto;")
            )
         )
      )
   } else {                               # logged in but email unverified, send email and return verification inputs
      if(session$userData$user$sp && !session$userData$user$emailverified) {
         session$userData$tempcode <- generate_code()
         if(session$userData$fixingemail) {           # if email is already verified, user is changing email address
            send.email(session$userData$user$username, session$userData$user$email,
               paste0("Code to verify your ", site_name," account."),
               paste0("Here's the code you must enter to change your ", site_name," email address: ", session$userData$tempcode))
         } else {                                     #    otherwise user is verifing email address for a new account
            send.email(session$userData$user$username, session$userData$user$email,
               paste0("Code to verify your new ", site_name," account."),
               paste0("Here's the code you must enter to complete your ", site_name," registration: ", session$userData$tempcode))
         }
         pageText <- tagList(
            fluidRow(
               column(6, offset=3,
                  HTML(paste0(
                     "<h3>Almost there...</h3><p>We've just sent an email to <b>", session$userData$user$email,
                     "</b> with a 6-digit temporarty PIN. To complete your registration, ",
                     "enter the PIN here and click the OK button.</p>")),
                  ttextInput("tempcode", "PIN from email", value="", style="width: 100%;"),
                  actionButton("ok_btn", "OK", class="btn-primary btn-sm"),
                  HTML('<div class="panel panel-primary" style="margin:2em 0 0 0">',
                     '<div class="panel-heading"><b><i>Need to revise your email address?</i></b></div>',
                     '<div class="panel-body">'),
                  ttextInput("email", "", value=session$userData$user$email, style="width: 100%;"),
                  actionButton("fixemail_btn", "Whoops, fix my email address", class="btn-danger btn-sm"),
                  HTML("</div></div>")
               )
            )
         )
      } else {                            # logged in and email verified; show profile update inputs
         if(session$userData$user$sp) {
            session$userData$fixingemail <- TRUE
            pageText <- tagList(
               fluidRow(
                  column(4, offset=4,
                     HTML("<h4 style='margin-bottom: 1em;'>Your username is: ", session$userData$user$username,"</h4>"),
                     ttextInput("email", "Update email address:", value=session$userData$user$email, style="width: 100%;"),
                     actionButton("fixemail_btn", "Update email address", class="btn-primary btn-sm",
                                  style="display: block; margin: 0 auto 3em auto"),
                     passwordInput("password", "Confirm Old Password:", value=""),
                     passwordInput("password1", "New Password:", value=""),
                     passwordInput("password2", "Repeat New Password:", value=""),
                     actionButton("chgPassword_btn", "Change Password", class="btn-primary btn-sm",
                                  style="display: block; margin-left: auto; margin-right: auto")
                  )
               )
            )
         }
      }
   }
   return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})                    #    Shiny expects renderUI to return some text, which may have embedded
                        #    HTML. Although Shiny examples rarely use it, if you include an
                        #    explicit return, your code looks more R-like and it helps to keep
                        #    straight what part of your renderUI is actual code and what part
                        #    is building and returning the HTML. Nested tagLists() are ok.
                        #    Other pages here embed the returns in the code rather than using a
                        #    variable that is returned at the end of the code. Either way is ok.

# This observer is for initial registration
observeEvent(input$register_btn, {
   username <- str_replace_all(input$username, "[^[:alnum:]]", "")       # make username alpahnumeric...
   username <- str_replace_all(username, " ", "")                        #    ...with no spaces
   updateTextInput(session, "username", value=username)
   alertText <- ""

   if(username=="") {
      alertText <- paste0(alertText, "<p>Please provide a user name.</p>")
   }
   if(nchar(username)!=nchar(input$username)) {
      alertText <- paste0(alertText, "<p>User name can't have punctuation or spaces.</p>")
   }
   if(nchar(username) > 30) {
      alertText <- paste0(alertText, "<p>User name is limited to 30 characters.</p>")
   }
   if(userGet("username", username)$username !="") {                     # check againt all existing usernames
      alertText <- paste0(alertText, "<p>", username, " is taken.</p>")
   }
   if(input$password1 == "") {
      alertText <- paste0(alertText, "<p>Password can't be blank.</p>")
   }
   if(nchar(input$password1) > 30) {
      alertText <- paste0(alertText, "<p>Password is limited to 30 characters.</p>")
   }
   if(input$password1 != input$password2) {
      alertText <- paste0(alertText, "<p>Passwords don't match.</p>")
   }
   if(input$email=="") {
      alertText <- paste0(alertText, "<p>Please provide an email address.</p>")
   } else {
      if(!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", input$email, ignore.case=TRUE)) {  # from r-bloggers.com
         alertText <- paste0(alertText, "<p>Please provide a valid email address.</p>")
      }
   }
   if(nchar(input$email) > 254) {
      alertText <- paste0(alertText, "<p>An email address is limited to 254 characters.</p>")
   }
   if(alertText=="") {                                             # if no one is logged in, the app.R cookie-checker
      session$userData$user$username <- username                   #    created a blank session$userData$user for us
      session$userData$user$hashed_pw <- hashpw(input$password1)
      session$userData$user$reg_date <- now()
      session$userData$user$sp <- 1                                # new registrants have a superpower of 1
      session$userData$user$email <- input$email
      userSave(session$userData$user)
      rv$limn <- rv$limn + 1                                       # re-render to show email verification text
   } else {
      session$userData$modal_title <- "Whoops!"
      session$userData$modal_text <- alertText
      rv$modal_warning <- rv$modal_warning + 1
#     rv$limn <- rv$limn + 1   # A re-render initializes fields to their original values; no render leaves user-entered text
   }
})

# This observer allows registered users to change their email address and also
#    takes care of the fix email address button on the "enter emailed code" page
observeEvent(input$fixemail_btn, {
   alertText <- ""
   if(input$email=="") {                                           # error checking on email address user entered
      alertText <- paste0(alertText, "<p>Please provide an email address.</p>")
   } else {
      if(!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", input$email, ignore.case=TRUE)) {  # from r-bloggers.com
         alertText <- paste0(alertText, "<p>Please provide a valid email address.</p>")
      }
   }
   if(alertText=="") {                                             # No error
      if(input$email!=session$userData$user$email) {                  # This is the same address we have now!
            session$userData$user$email <- input$email                   # Get new email address
            session$userData$user$emailverified <- FALSE                 # This will take us to verification on render
            userSave(session$userData$user)                              # Save what we have so far
         }
      rv$limn <- rv$limn + 1                                       # This will send an email with a new code.
   } else {
      session$userData$modal_title <- "Whoops!"
      session$userData$modal_text <- alertText
      rv$modal_warning <- rv$modal_warning + 1
   }
})

# This observer is for the OK button after the user enters the code from the email
observeEvent(input$ok_btn, {
   wrongcode <- 0                                           # counter to limit code guessing
   if(input$tempcode == session$userData$tempcode) {        # If the codes match, save user and login
      newID <- generate_id()                                # create a new session id
      js$setCookie("sessionID", newID, days=0)              # set cookie
      session$userData$user$sessionid <- newID              # set sessionid in user
      session$userData$user$lastlogin_date <- now()         # set login date
      session$userData$user$emailverified <- TRUE           # set emailverified
      session$userData$user$ev_date <- now()                # set emailverified date
      userSave(session$userData$user)                       #    and save the record
      session$userData$modal_title <- "Welcome to the site!"
      session$userData$modal_text <- "<p>Your email address has been verified.</p>"
      rv$modal_warning <- rv$modal_warning + 1
      js$redirect("?home")                                  # Successful registration leads to home
   } else {
      wrongcode <- wrongcode + 1
      if(wrongcode<4) {                                     # You only get 3 tries to enter the code
         session$userData$modal_title <- "Whoops!"
         session$userData$modal_text <- "<p>Wrong code. Recheck that email and try again.</p>"
         rv$modal_warning <- rv$modal_warning + 1
      } else {
         session$userData$modal_title <- "Whoops!"
         session$userData$modal_text <- "<p>Sorry, that's too many bad tries. Let's start over.</p>"
         rv$modal_warning <- rv$modal_warning + 1
         js$redirect("?home")
      }
   }
})

# This observer allows registered users to change their password
observeEvent(input$chgPassword_btn, {
   session$userData$modal_title <- "Whoops!"
   alertText <- ""
   if(!checkpw(input$password, session$userData$user$hashed_pw)) {       # right password?
      alertText <- paste0(alertText, "<p>That's not your current password.</p>")
   }
   if(input$password1 == "") {
      alertText <- paste0(alertText, "<p>Your new password can't be blank.</p>")
   }
   if(nchar(input$password1) > 30) {
      alertText <- paste0(alertText, "<p>Passwords are limited to 30 characters.</p>")
   }
   if(input$password1 != input$password2) {
      alertText <- paste0(alertText, "<p>Your new passwords don't match.</p>")
   }
   if(nchar(alertText)==0) {                                             # Success!
      session$userData$user$hashed_pw <- hashpw(input$password1)
      userSave(session$userData$user)
      session$userData$modal_title <- "Success."
      alertText <- "<p>Your password has been changed.</p>"
   }
   session$userData$modal_text <- alertText
   rv$modal_warning <- rv$modal_warning + 1
   rv$limn <- rv$limn + 1
})
