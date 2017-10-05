### om_skeleton dataPersistence-SaveOnDisk.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

### Data persistence functions for users table
###    There are a variety of ways to make data persistent. This is the least reliable, but it's easy to implement
###       and works universally. For details, see: https://shiny.rstudio.com/articles/persistent-data-storage.html

### You MUST plan on coming up with a better way to do data persistence in your final project. This page shows the
###    kind of functions you would need for the Users table, if, for example, you wanted to change this to MySQL.

# A function that builds a user row for the database table. Having a single function for this
#    makes it somewhat easier to modify the table during program development.
buildU <- function() {                                                  # tibbles are dplyr's enhanced dataframes
   return(tibble(username="", hashed_pw="", email="", emailverified=F, sp=0, sessionid="",
                 reg_date=as.POSIXct(NA), ev_date=as.POSIXct(NA), lastlogin_date=as.POSIXct(NA)))
}

save_users = function() { saveRDS(site_users, file="users-table.R") }
load_users = function() { readRDS(file="users-table.R") }

# An initialization function that creates the first user, who must be a superuser.
init_users = function() { users <- buildU()
                          users$username="Admin"
                          users$hashed_pw=hashpw(admin_password)
                          users$email=admin_email_address
                          users$emailverified = TRUE
                          users$sp = 1000
                          users$sessionid=""
                          users$reg_date=now()
                          users$ev_date=now()
                          return(users)
}

# Set up connection and figure out whether initialization is needed. If so, do it.
if(file.exists("users-table.R")) { site_users <- load_users() } else { site_users <- init_users(); save_users() }

# function for saving / updating a row of the user table
userSave = function(u) {                                        # u is an entire row for the users table; see buildU()
   if(u$username =="") {
      print("Whoops - In saveUser(), $username is blank.")
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
userGet = function(column, value) {             # This function returns the whole row for a user by username or sessionid
   colvec = pull(site_users, column)            #   If user or session isn't found, it returns a new, blank row
   if(column=="username") {
      tfvec = tolower(colvec)==tolower(value)   # for usernames only, be case insensitive
   } else {
      tfvec = colvec==value                     # create a logical vector
   }
   if(sum(tfvec)==0) {                          # sum = 0 means there are no TRUEs
      return(buildU())                          #    return a blank user
   }
   if(sum(tfvec)==1) {                          # test for 1 and only 1 hit
      return(site_users[tfvec,])                #    return all columns for this user
   }
   stop(paste0("Database has duplicates in ", column, "."))  # This should never happen, he said.
}

# How to determine whether a user exists in the user table by username or sessionid
#    Do a userGet; if returned user has a blank username, that user didn't exist


