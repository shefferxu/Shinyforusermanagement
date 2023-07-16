
# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)
library(shinythemes)
# you can use keyring package to set database key
library(keyring)
# passphrase <- key_set("R-shinymanager-key", "55mayday")

# 
# 
# new random user
# dailydf <- data.frame(
#   user =random_strings_df$strings, # mandatory
#   password =random_strings_df$strings, # mandatory
#   start = c("2019-04-15"), # optinal (all others)
#   expire = c("2029-05-15"),
#   admin = c(FALSE),
#   # comment = "Simple and secure authentification mechanism
#   # for single ‘Shiny’ applications.",
#   stringsAsFactors = FALSE
# )

# define some credentials
credentials <- data.frame(
  user = c("shiny", "shinymanager"), # mandatory
  password = c("shiny", "shinymanager"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c("2029-05-15",NA),
  admin = c(FALSE, TRUE),
  # comment = "Simple and secure authentification mechanism 
  # for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

credentials <- rbind(dailydf,credentials)

# Create credentials DB (only once)
create_db(
  credentials_data = credentials,
  sqlite_path = "credentials.sqlite",
  # passphrase =passphrase
)

set_labels(
  language = "en",
  "Please authenticate" = "登入页面",
  "Username:" = "用户名:",
  "Password:" = "密码:"
)
