library(shiny)
library(DT)
library(ggplot2)

ui <- fluidPage(
  titlePanel("文件上传和分布图"),
  
  downloadButton("templateBtn_cal", "下载模板"),
  
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_cal", "选择CSV文件",
                accept = c(".csv", ".tsv", ".txt")),
      
      br(),
      
      # Single button for generating plot and summary table
      actionButton("runAnalysisBtn_cal", "开始分析"),
      
      br(),
      br(),
      
      # Text input for editing x-axis label
      textInput("xLabelInput_cal", "编辑横坐标标签:", value = "EDI") # Default label is "EDI"
    ),
    
    mainPanel(
      DTOutput("contents_cal"),
      
      br(),
      
      # Output for distribution plot
      plotOutput("distributionPlot_cal"),
      
      br(),
      
      # Download button for the generated plot
      downloadButton("downloadPlotBtn_cal", "下载分布图"),
      
      br(),
      
      # Output for summary statistics table
      h4("分析结果："),
      tableOutput("summaryTable_cal")  # Use tableOutput instead of DTOutput
    )
  )
)

server <- function(input, output) {
  data_cal <- reactiveVal(NULL)
  
  observeEvent(input$file_cal, {
    inFile_cal <- input$file_cal
    if (is.null(inFile_cal))
      return(NULL)
    data_cal(read.csv(inFile_cal$datapath, header = TRUE))
  })
  
  # Function to create and download the template file
  output$templateBtn_cal <- downloadHandler(
    filename = function() {
      "template.csv" # Change the filename and extension as needed
    },
    content = function(file) {
      template_data <- data.frame("value" = NA) # Replace NA with an appropriate default value if needed
      write.csv(template_data, file, row.names = FALSE)
    }
  )
  
  # Function to generate the distribution plot
  output$distributionPlot_cal <- renderPlot({
    req(data_cal())
    if (input$runAnalysisBtn_cal) {
      x_label <- input$xLabelInput_cal
      ggplot(data_cal(), aes(x = value)) +
        geom_histogram(fill = "steelblue", color = "white") +
        labs(y = "Frequency", x = x_label) +
        theme_minimal() +
        theme(text = element_text(family = "SimSun"))
    }
  })
  
  # Function to generate summary statistics table
  output$summaryTable_cal <- renderTable({
    req(data_cal())
    if (input$runAnalysisBtn_cal) {
      column_names <- colnames(data_cal())  # Get the column names from the data
      summary_table <- data.frame(
        Mean = mean(data_cal()[, column_names]),
        P5 = quantile(data_cal()[, column_names], 0.05),
        P25 = quantile(data_cal()[, column_names], 0.25),
        P50 = median(data_cal()[, column_names]),
        P75 = quantile(data_cal()[, column_names], 0.75),
        P95 = quantile(data_cal()[, column_names], 0.95)
      )
      summary_table
    }
  })
  
  # Function to download the generated plot
  output$downloadPlotBtn_cal <- downloadHandler(
    filename = function() {
      "distribution_plot.png" # Change the filename and extension as needed
    },
    content = function(file) {
      ggsave(file, plot = {
        req(data_cal())
        x_label <- input$xLabelInput_cal
        ggplot(data_cal(), aes(x = value)) +
          geom_histogram(fill = "steelblue", color = "white") +
          labs(y = "Frequency", x = x_label) +
          theme_minimal() +
          theme(text = element_text(family = "SimSun"))
      }, device = "png")
    }
  )
}

shinyApp(ui, server)
