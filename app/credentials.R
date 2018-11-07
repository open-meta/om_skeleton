### Credentials and Initializations
#
#   Make sure you don't put this file on GitHub or
#      allow it to be visible on your web site.

### Put your data here
admin_email_address <- ""       # Email address and password for the
admin_password <- ""            #    initial administrator superuser account
site_name <- ""                 # Displayed at upper left on all pages in this design; also used in emails

### To send email with the Amazon Simple Email Service api:
#   To obtain the needed credentials, log into the AWS console. In "AWS services" choose
#   Identity and Access Management (IAM). In the vertical menu at the left, pick "Users".
#   In the screen that appears, pick "Add User". Give this user a name like "SESapi" and
#   for Access Type pick "Programmatic access". Click "Next: Permissions" and on that page
#   pick the box for "Attach Existing Policies Directly". In "Filter policies" enter "SES".
#   This will show you two permission policies for SES, one for Full Access and one for
#   read-only access. Since you have to POST (ie, write) to send email, check the box on
#   the Full Access line and click Next: Review. On the review page, click Create User.
#   You now get your one and only chance to download the key and secret you need as a .csv
#   file. If you miss that chance, or if your key and secret become compromised, you can
#   click on the SESapi user in the Users menu, then click on the Security Credentials tab.
#   From there you can create new Access Key and Secret pairs and inactivate old pairs.

#   If you don't know the region of your AWS-SES server, go back to "AWS services" and
#   search for "Simple Email Service." At the upper right on the screen that appears, between
#   your name and the word "Support", is a dropdown that lets you select the region you want
#   to use. Note that you have to mentally convert the region names in the console to one of
#   the acceptable strings shown in the SESregion comment below.

# Amazon SES credentials
SESkey <- ""
SESsecret <- ""
SESregion <- ""            # The region of your SES server (us-east-1, us-west-2, or eu-west-1)
SESfromName <- ""
SESfromAdr <- ""           # Default "From" name and address. Address must be AWS verified.

### debugging
page_debug_on <- TRUE      # if TRUE, prints some debugging info to the console or server log


