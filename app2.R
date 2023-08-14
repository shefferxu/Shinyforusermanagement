library(shiny)
library(RSQLite)
library(tidyverse)

# 连接到 SQLite 数据库
con <- dbConnect(RSQLite::SQLite(), "credentials.sqlite")
# 执行查询
query <- "SELECT * FROM credentials"
userdatabase <- dbGetQuery(con, query)

load("./data/key.RData")
# 创建帐号表格（如果数据库中没有已存在的表格）
dbExecute(con, "CREATE TABLE IF NOT EXISTS accounts (username TEXT, password TEXT)")

ui <- fluidPage(
  titlePanel("Add Account"),
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      passwordInput("confirm_password", "Confirm Password"),
      textInput("verification_code", "Verification Code"),
      actionButton("add_btn", "Add Account")
    ),
    mainPanel(
      # 显示图像
      tags$img(src = "img/test.png", height = "300px", width = "300px")
    )
  )
)









server <- function(input, output, session) {
  # 添加帐号到 SQLite 数据库
  observeEvent(input$add_btn, {
    user = input$username
    password = input$password
    confirm_password <- input$confirm_password
    verification_code <- input$verification_code
    keylist <-  keydatabase$key
    # 验证输入的特定字符串
    if (verification_code %in% keylist) {
      # 验证密码匹配
      if (password == confirm_password) {
        # 验证密码格式
        password_valid <- nchar(password) >= 6 && any(grepl("[A-Za-z]", password)) && any(grepl("[0-9]", password))

        # 验证用户名格式
        username_valid <- nchar(password) >= 6 && any(grepl("[A-Za-z]", user)) && any(grepl("[0-9]", user))

        if (password_valid && username_valid) {
          # 验证用户名是否可用
          # 执行查询
          query <- "SELECT * FROM credentials"
          credentialsresult <- dbGetQuery(con, query)
          # username_exists <- dbGetQuery(con, paste("SELECT COUNT(*) FROM users WHERE username =", shQuote(credentials, type = "char")))
          username_available <- !user %in% credentialsresult$user
            # (as.integer(username_exists) == 0)

          if (username_available) {
            # verification_code <-  "psbnDijfaFIxGBKyNJhc" # T
            keydatabase_selected <- keydatabase %>% filter(key==verification_code)
            # 从 keydatabase 向量中删除 verification_code 值
            keydatabase_filtered <- keydatabase %>% filter(!key==verification_code)

        verification_code <-  "psbnDijfaFIxGBKyNJhc"# T
        keydatabase_selected <- keydatabase %>% filter(key==verification_code)
        # 从 keydatabase 向量中删除 verification_code 值
        keydatabase_filtered <- keydatabase %>% filter(!key==verification_code)

         # 保存 valid_verification_codes 到 RData 文件
        # save(keydatabase_filtered, file = "./data/key.RData") # FT



        new_rows <- data.frame(
          # user = "Asd", # T
          # password = "Asd",  # T
          user = user,
          password =password, # scrypt::hashPassword("azerty") 密码加密
          start = as.character(as.Date(Sys.time())),
          expire = as.character(as.Date(Sys.time() + as.difftime(keydatabase_selected[,"day"], units = "days"))),
          admin = as.character("FALSE"),
          is_hashed_password = as.character("0"),
          stringsAsFactors = FALSE
        )
        # 执行 SQL 插入语句将帐号添加到数据库中
        # Insert the new rows into the database table
        dbWriteTable(con, "credentials", new_rows, append = TRUE)
        output$output <- renderText({
          "Account added successfully."
        })
        # 清空输入框
        updateTextInput(session, "username", value = "")
        updateTextInput(session, "password", value = "")
        updateTextInput(session, "confirm_password", value = "")
        updateTextInput(session, "verification_code", value = "")
          }  else {
            output$output <- renderText({
              "Username already exists. Please choose a different username."
            })
          }
        } else {
          output$output <- renderText({
            "Invalid password or username format. Please ensure they are at least 6 characters long and contain both letters and numbers."
          })
        }
      } else {
        output$output <- renderText({
          "Passwords do not match."
        })
      }
    } else {
      output$output <- renderText({
        "Invalid verification code."
      })
    }
  })
}
# port setting
# options(shiny.port = 1237,shiny.host="0.0.0.0")
options(shiny.port = 1237,shiny.host="0.0.0.0")
shinyApp(ui = ui, server = server)
