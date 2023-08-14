library(shiny)
library(shinymanager)
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
  
  # classic app
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(iris)),
    selectInput('ycol', 'Y Variable', names(iris),
                selected=names(iris)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1'),
    verbatimTextOutput("res_auth")
  )
  
)

ui <- secure_app(ui,
                 theme=shinytheme("slate"),
                 
                 enable_admin = TRUE,
                 # tags$img(
                 #   src = "/test.png", width = 100
                 # )
                 # background = tags$div(
                 #   style = "background-image: url('D:\\Deltawork\\Software\\ShinyPay\\Shinyforusermanagement\\logo'); background-repeat: no-repeat;
                 #   background-position: center center; background-size: cover;")
                 # tags_top = 
                 #   tags$div(
                 # tags$h4("Demo", style = "align:center"),
                 # tags$img(
                 #   src = "https://www.r-project.org/logo/Rlogo.png", width = 100
                 # )
                 # add information on bottom ?
                 tags_bottom = tags$div(
                   style = "text-align: center;",
                   tags$p(
                     "如有问题，欢迎微信扫码入群或联系",
                     tags$a(
                       href = "mailto:xiaohuixu71@outlook.com?Subject=Shiny%20aManager",
                       target="_top", "开发人员"
                     ),
                     tags$img(
                       src = "https://www.r-project.org/logo/Rlogo.png", width = 100
                     )
                   )
                 ),
                 background  = " url('https://i.postimg.cc/MKpkJYhT/Snipaste-2022-08-26-14-21-31.png')
                 no-repeat center center / cover;;;"
)



server <- function(input, output, session) {
  
  # Call secure_server() with DB info
  # its' an example, don't put password in clear like that
  auth_out <- secure_server(
    check_credentials = check_credentials(
      db = "credentials.sqlite",
      # passphrase = "supersecret"
    ), timeout = 0, 
    inputs_list = list(group = list(fun = "selectInput", 
                                    args = list(choices = c("all", "restricted"), 
                                                multiple = TRUE, 
                                                selected = c("all", "restricted")
                                    )
    )
    )
  )
  
  observe({
    print(input$shinymanager_where)
    print(input$shinymanager_language)
  })
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(auth_out)
  })
  
  # classic app
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}
options(shiny.port = 1238,shiny.host="0.0.0.0")
shinyApp(ui = ui, server = server)