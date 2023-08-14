# UI 部分 ---------------------------------------------------------

ui <- fluidPage(
  titlePanel("使用 fitdistrplus 进行分布拟合"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("templateBtn_fit", "下载模板文件"),
      br(),
      br(),
      fileInput("file", "上传CSV文件:", accept = ".csv"),
      selectInput("dist_type", "选择分布类型:", 
                  choices = c("norm" = "norm", "lnorm" = "lnorm", "exp" = "exp", 
                              "gamma" = "gamma", "logis" = "logis", "beta" = "beta", "weibull" = "weibull"),
                  selected = "norm"),
      actionButton("fit_button", "拟合分布")
    ),
    mainPanel(
      plotOutput("hist_plot"),
      verbatimTextOutput("fit_result")
    )
  )
)

# 服务器部分 ---------------------------------------------------------

server <- function(input, output) {
  
  # 下载模板文件
  output$templateBtn_fit <- downloadHandler(
    filename = function() {
      "templatedata.csv"
    },
    content = function(file) {
      template_data <- data.frame("value" = NA)
      write.csv(template_data, file, row.names = FALSE)
    }
  )
  
  # 其他代码保持不变
  # ...
  # ...
}

# ...其余代码保持不变...
shinyApp(ui, server)
