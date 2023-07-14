
# Global ------------------------------------------------------------------

library(shiny)
library(shinymanager)
library(shinythemes)
# you can use keyring package to set database key
# library(keyring)
# key_set("xxh")
# 创建包含大小写英文字母的字符向量
letters <- c(letters, LETTERS)

# 创建一个空的数据框
random_strings_df <- data.frame(strings = character(), stringsAsFactors = FALSE)

# 生成100个随机字符串并放入数据框
for (i in 1:2) {
  random_letters <- sample(letters, 6)
  random_string <- paste(random_letters, collapse = "")
  random_strings_df <- rbind(random_strings_df, data.frame(strings = random_string, stringsAsFactors = FALSE))
}
random_strings_df$strings <- paste0("day",random_strings_df$strings)
# 
# 
# 
dailydf <- data.frame(
  user =random_strings_df$strings, # mandatory
  password =random_strings_df$strings, # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c("2029-05-15"),
  admin = c(FALSE),
  # comment = "Simple and secure authentification mechanism
  # for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

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
# create_db(
#   credentials_data = credentials,
#   sqlite_path = "credentials.sqlite",
#   # passphrase = key_get("xxh")
# )

set_labels(
  language = "en",
  "Please authenticate" = "登入页面",
  "Username:" = "用户名:",
  "Password:" = "密码:"
)
