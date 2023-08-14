library(shiny)
library(DT)
library(ggplot2)

ui <- fluidPage(
  titlePanel("文件上传和自定义代码示例"),
  
  downloadButton("templateBtn_cal", "下载模板"),
  
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_cal", "选择CSV文件",
                accept = c(".csv", ".tsv", ".txt")),
      
      br(),
      
      # Add a text area for user-defined code
      textAreaInput("userCode_cal", "输入自定义代码：", value = "EDI=consumption_gd_1 * concentration_mgkg_1 / weight_kg"),
      
      br(),
      
      actionButton("runCodeBtn_cal", "运行自定义代码")
    ),
    
    mainPanel(
      DTOutput("contents_cal"), # Use DTOutput instead of tableOutput
      
      br(),
      
      # Output for summary statistics
      h4("汇总统计："),
      verbatimTextOutput("summaryOutput_cal"),
      
      br(),
      
      # Output for distribution plot
      h4("分布图："),
      plotOutput("distributionPlot_cal")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    inFile <- input$file_cal
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = TRUE)
  })
  
  output$contents_cal <- renderDT({ # Use renderDT instead of renderTable
    data()
  })
  
  # Function to create and download the template file
  output$templateBtn_cal <- downloadHandler(
    filename = function() {
      "template.csv" # Change the filename and extension as needed
    },
    content = function(file) {
      template_data <- data.frame("consumption" = NA, "concentration" = NA, "weight" = NA) # Replace NA with appropriate default values if needed
      write.csv(template_data, file, row.names = FALSE)
    }
  )
  
  # Function to run the user-defined code and calculate EDI
  observeEvent(input$runCodeBtn_cal, {
    code <- input$userCode_cal
    tryCatch({
      eval(parse(text = code), envir = data())
    }, error = function(e) {
      # Handle errors if the code is invalid
      showModal(modalDialog(
        title = "错误",
        paste("发生错误：", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Function to calculate summary statistics
  output$summaryOutput_cal <- renderPrint({
    summary(data())
  })
  
  # Function to create distribution plot
  output$distributionPlot_cal <- renderPlot({
    ggplot(data(), aes(x = EDI)) +
      geom_histogram(fill = "steelblue", color = "white") +
      labs(y = "Frequency") +
      theme_minimal()
  })
  
  # Create a reactive variable for EDI
  EDI <- reactive({
    req(data()) # Make sure data is available
    data()$EDI <- with(data(), eval(parse(text = "consumption * concentration/ weight")))
    data()
  })
  
  # Update the table when EDI is calculated
  observeEvent(EDI(), {
    output$contents_cal <- renderDT({
      EDI()
    })
  })
}

shinyApp(ui, server)
