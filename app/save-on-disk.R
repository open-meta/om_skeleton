### om_skeleton save-on-disk.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

### Users table and data persistence
###    There are a variety of ways to make data persistent. Saving it to disk is the least reliable, because
###       on a server, it gets deleted every time the app stops. But it's easy to implement and works universally,
###       so it's a decent choice for this skeleton. It's also fine for an app that only runs locally on a
###       single computer. A server-based, multi-user site, however,  MUST use something better. For details
###       on better methods, see: https://shiny.rstudio.com/articles/persistent-data-storage.html

# A function that builds a user row for the database table. Having a single function for this
#    makes it somewhat easier to modify the table during program development.
buildU <- function() {                                                  # tibbles are dplyr's enhanced dataframes
   return(tibble(username="", hashed_pw="", email="", emailverified=F, sp=0, sessionid="",
                 reg_date=as.POSIXct(NA), ev_date=as.POSIXct(NA), lastlogin_date=as.POSIXct(NA)))
}
# reg_date = registration date
# ev_date = email verified date
# sessionid = the value of the cookie saved in the user's browser on login (and removed on logout)
# sp = user superpower level; logged out = 0; logged in > 0; A page can require a higher level to
#    view it (see admin.R). Also see the menu1 eventReactive() for using superpowers in menu building.

save_users <- function() { saveRDS(site_users, file="users-table.RDS") }
load_users <- function() { readRDS(file="users-table.RDS") }

# An initialization function that creates the first user, who must be a superuser.
init_users <- function() { users <- buildU()
                          users$username = "Admin"
                          users$hashed_pw = hashpw(admin_password)
                          users$email = admin_email_address
                          users$emailverified = TRUE
                          users$sp = 1000
                          users$sessionid = ""
                          users$reg_date = now()
                          users$ev_date = now()
                          return(users)
}

# Set up connection and figure out whether initialization is needed. If so, do it.
if(file.exists("users-table.R")) { site_users <- load_users() } else { site_users <- init_users(); save_users() }

# function for saving / updating a row of the user table
userSave <- function(u) {                                       # u is an entire row for the users table; see buildU()
   if(u$username =="") {
      cat("\nWARNING: In saveUser(), $username was blank.\n\n")
   } else {
      if(any(site_users$username == u$username)) {              # do we already have this user?
         site_users[site_users$username == u$username,] <<- u   #    update all columns
      } else {
         site_users <<- rbind(site_users, u)                    # if not, add the user's row to the table
      }
      save_users()                                              # save
   }
}

# function for getting a user's row from the user table by username or sessionid
userGet <- function(column, value) {            # This function returns the whole row for a user by username or sessionid
   colvec <- pull(site_users, column)           #   If not found, returns a blank row. (pull() is a dplyr function for
   if(column=="username") {                     #   getting a vector out of a tibble; otherwise it would be a tibble.)
      tfvec <- tolower(colvec)==tolower(value)  # For usernames be case insensitive
   } else {
      tfvec <- colvec==value                    # create a logical vector
   }
   if(sum(tfvec)==0) {                          # sum = 0 means there are no TRUEs
      return(buildU())                          #    return a blank user
   }
   if(sum(tfvec)==1) {                          # test for 1 and only 1 hit
      return(site_users[tfvec,])                #    return all columns for this user
   }
   cat(paste0("\nWARNING: Database has duplicates in ", column, ".\n\n"))  # This should never happen, he said.
}

# Need to determine whether a user exists in the user table by username or sessionid?
#    Do a userGet; if returned user's superpower is zero (FALSE), nothing was found.

