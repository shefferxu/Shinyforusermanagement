library(shiny)
library(ggplot2)
library(shinyalert)

# UI
ui <- fluidPage(
  titlePanel("Random Number Generator"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("download_template_button", "Download Template File"),
      br(),
      br(),
      fileInput("file_random", "Upload a CSV file:", accept = ".csv"),
      br(),
      numericInput("num_samples_input_random", "Number of Random Samples:", value = 10000),
      br(),
      actionButton("generate_button_random", "Generate Random Numbers"),
      br(),
      downloadButton("download_generated_numbers_button_random", "Download Generated Numbers")
    ),
    mainPanel(
      plotOutput("sampled_numbers_plot_random")
    )
  )
)

# Server
server <- function(input, output) {
  
  data_random <- reactiveVal(NULL)
  data_sampled_random <- reactiveVal(NULL)
  
  # Read data from the uploaded file
  observeEvent(input$file_random, {
    req(input$file_random)
    data_random(read.csv(input$file_random$datapath, header = TRUE)[["values"]])
  })
  
  # Generate random numbers from the data based on the user's input
  observeEvent(input$generate_button_random, {
    if (!is.null(data_random())) {
      num_samples <- input$num_samples_input_random
      data_sampled_random(sample(data_random(), num_samples, replace = TRUE))
    } else {
      # If there is no data, show a popup alert
      shinyalert::shinyalert(
        title = "No Data Uploaded",
        text = "Please upload a CSV file first.",
        type = "warning"
      )
    }
  })
  
  # Plot the histogram of the sampled numbers using ggplot2
  output$sampled_numbers_plot_random <- renderPlot({
    data_sampled_random <- data_sampled_random()
    if (!is.null(data_sampled_random)) {
      ggplot(data.frame(Value = data_sampled_random), aes(x = Value)) +
        geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue", alpha = 0.7) +
        labs(title = "Randomly Sampled Numbers",
             x = "Value",
             y = "Frequency") +
        theme_minimal()
    }
  })
  
  # Download the uploaded template file
  observeEvent(input$file_random, {
    file_name <- basename(input$file_random$name)
    output$download_template_button_random <- downloadHandler(
      filename = function() {
        paste("template_", file_name, sep = "")
      },
      content = function(file) {
        file.copy(input$file_random$datapath, file)
      }
    )
  })
  
  # Download the generated random numbers
  observeEvent(input$generate_button_random, {
    output$download_generated_numbers_button_random <- downloadHandler(
      filename = function() {
        paste("generated_numbers_", input$num_samples_input_random, ".csv", sep = "")
      },
      content = function(file) {
        if (!is.null(data_sampled_random())) {
          write.csv(data.frame(Value = data_sampled_random()), file, row.names = FALSE)
        }
      }
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)
