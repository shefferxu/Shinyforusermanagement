library(shiny)
library(RSQLite)

# 连接到 SQLite 数据库
con <- dbConnect(RSQLite::SQLite(), "credentials.sqlite")

# # 创建帐号表格（如果数据库中没有已存在的表格）
# dbExecute(con, "CREATE TABLE IF NOT EXISTS accounts (username TEXT, password TEXT)")

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
      verbatimTextOutput("output")
    )
  )
)

server <- function(input, output, session) {
  # 添加帐号到 SQLite 数据库
  observeEvent(input$add_btn, {
    user = input$username
    password = input$password
    new_rows <- data.frame(
      # user = "Asd",
      # password = "Asd",
      user = user,
      password =password,
      start = as.character(as.Date(Sys.time())),
      expire = as.character(as.Date(Sys.time() + as.difftime(1, units = "days"))),
      admin = as.character("FALSE"),
      is_hashed_password = as.character("0"),
      stringsAsFactors = FALSE
    )

    confirm_password <- input$confirm_password
    verification_code <- input$verification_code
    
    # 验证输入的特定字符串
    if (verification_code == "a") {
      # 验证密码匹配
      if (password == confirm_password) {
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

shinyApp(ui, server)
