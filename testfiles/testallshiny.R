library(shiny)
library(shinydashboard)
library(fitdistrplus)
library(shinydashboardPlus)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(shinythemes)
library(slickR)
library(DT)
library(shinyalert)
library(shinydisconnect)
google.red <- '#ea4235'
google.yellow <- '#fabd03'
google.green <- '#34a853'
google.blue <- '#4286f5'

# Define the UI

# home UI--------------------------------------------------------------------

home_ui <- fluidPage(
  titlePanel("Distribution Fitting with fitdistrplus"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a CSV file:", accept = ".csv"),
      selectInput("dist_type", "Select Distribution:", 
                  choices = c("norm" = "norm", "lnorm" = "lnorm", "exp" = "exp", 
                              "gamma" = "gamma", "logis" = "logis", "beta" = "beta", "weibull" = "weibull"),
                  selected = "norm"),
      actionButton("fit_button", "Fit Distribution")
    ),
    mainPanel(
      plotOutput("hist_plot"),
      verbatimTextOutput("fit_result")
    )
  )
)

# fitgenerator UI ---------------------------------------------------------

fitgenerator_ui <- fluidPage(
  title = "Distribution Fitting with ggplot2",
  
  tabPanel("Fitted Distributions",
           fluidPage(
             titlePanel("Generate Fitted Distributions with Varying Parameters"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dist_type_fit", "Select Distribution for Fitting:", 
                             choices = c("norm" = "norm", "lnorm" = "lnorm", "exp" = "exp", 
                                         "gamma" = "gamma", "logis" = "logis", "beta" = "beta", "weibull" = "weibull"),
                             selected = "norm"),
                 
                 conditionalPanel(
                   condition = "input.dist_type_fit == 'norm'",
                   numericInput("mean", "Mean:", value = 0),
                   numericInput("sd", "Standard Deviation:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type_fit == 'lnorm'",
                   numericInput("meanlog", "Log-Mean:", value = 0),
                   numericInput("sdlog", "Log-Standard Deviation:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type_fit == 'exp'",
                   numericInput("rate", "Rate:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type_fit == 'gamma'",
                   numericInput("shape", "Shape:", value = 1),
                   numericInput("rate_gamma", "Rate:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type_fit == 'logis'",
                   numericInput("location", "Location:", value = 0),
                   numericInput("scale", "Scale:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type_fit == 'beta'",
                   numericInput("shape1", "Shape1:", value = 1),
                   numericInput("shape2", "Shape2:", value = 1)
                 ),
                 conditionalPanel(
                   condition = "input.dist_type_fit == 'weibull'",
                   numericInput("shape_weibull", "Shape:", value = 1),
                   numericInput("scale_weibull", "Scale:", value = 1)
                 ),
                 numericInput("num_iterations", "Number of Iterations:", value = 10000),
                 actionButton("generate_button", "Generate Fitted Distribution"),
                 br(),
                 br(),
                 downloadButton("download_data_button", "Download Data")
               ),
               mainPanel(
                 plotOutput("fitted_dist_plot")
               )
             )
           ))
)

# randomvalue UI -------------------------------------------------------------

randomvalue_ui <- fluidPage(
  title = "Distribution Fitting with ggplot2",
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
      br(),
      downloadButton("download_generated_numbers_button_random", "Download Generated Numbers")
    ),
    mainPanel(
      plotOutput("sampled_numbers_plot_random")
    )
  )
)


# plotanalysis ------------------------------------------------------------

plot_ui <- fluidPage(
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


# Define the server
server <- function(input, output) {
  
  # fit part ----------------------------------------------------------------
  
  
  # Load the data from the uploaded file
  data <- reactive({
    req(input$file)
    raw_data <- read.csv(input$file$datapath, header = TRUE)
    
    # Function to try converting each column to numeric and handle non-numeric values
    to_numeric <- function(col) {
      as.numeric(tryCatch(col, error = function(e) NA))
    }
    
    # Apply the to_numeric function to each column of the data frame
    num_data <- as.data.frame(lapply(raw_data, to_numeric))
    return(num_data)
  })
  
  # Plot the histogram of the uploaded data with the fitted density curve
  output$hist_plot <- renderPlot({
    req(data())
    fit <- NULL
    
    # Fit the selected distribution to the data
    if (input$fit_button > 0) {
      fit <- fitdist(data()$values, input$dist_type)
    }
    
    # Plot the histogram with the fitted density curve using plotdist
    plotdist(data()$values, input$dist_type, para = as.list(fit$estimate), demp = TRUE)
    
    # Add the fitted density curve for Weibull distribution
    if (input$fit_button > 0 && input$dist_type == "weibull") {
      curve(dweibull(x, shape = fit$estimate[1], scale = fit$estimate[2]), 
            col = "red", lwd = 2, add = TRUE)
    }
  })
  
  # Fit the selected distribution to the data
  fit_dist <- eventReactive(input$fit_button, {
    req(data())
    fitdist(data()$values, input$dist_type)
  })
  
  # Display the fitting results including AIC and BIC
  output$fit_result <- renderPrint({
    req(input$fit_button)
    dist_type <- switch(input$dist_type,
                        "norm" = "Normal",
                        "lnorm" = "Log-Normal",
                        "exp" = "Exponential",
                        "gamma" = "Gamma",
                        "logis" = "Logistic",
                        "beta" = "Beta",
                        "weibull" = "Weibull")
    fit <- fit_dist()
    criteria <- c(AIC = fit$aic, BIC = fit$bic)
    
    output_string <- sprintf("Fitted %s Distribution Parameters:\n", dist_type)
    output_string <- paste(output_string, "------------------------------------\n")
    for (param_name in names(fit$estimate)) {
      output_string <- sprintf("%s%s: %f\n", output_string, param_name, fit$estimate[[param_name]])
    }
    output_string <- paste(output_string, "\nEvaluation Criteria:\n", "------------------------------------\n")
    for (criterion_name in names(criteria)) {
      output_string <- sprintf("%s%s: %f\n", output_string, criterion_name, criteria[[criterion_name]])
    }
    
    cat(output_string)  # Print to console for cleaner output
  })
  
  
  # fit generator part -------------------------------------------------------------
  # Generate fitted distributions for user-defined parameters
  observeEvent(input$generate_button, {
    # ...
    
    # Use a different name for the local variable inside the observeEvent
    data_generated_local <- switch(dist_type, ...)
    
    # Convert the generated vector to a data frame with column name "data"
    data_generated_df <- data.frame(data = data_generated_local)
    
    # Output the plot
    output$fitted_dist_plot <- renderPlot({
      ggplot(data_generated_df, aes(x = data)) +
        geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "black", fill = "lightblue", alpha = 0.7) +
        geom_density(color = "red") +
        labs(title = "Fitted Distribution Plot",
             x = "Values",
             y = "Density") +
        theme_minimal()
    })
    
    # Download button to save the generated data as a CSV file
    output$download_data_button <- downloadHandler(
      filename = function() {
        paste("generated_data_", dist_type, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data_generated_df, file, row.names = FALSE)
      }
    )
  })
  
  # Add a reactive expression to hold the sampled random data
  data_sampled_random <- reactiveVal(NULL)
  
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
  
  
  # Add a reactive expression to hold the data from the uploaded file
  data_cal <- reactiveVal(NULL)
  
  # Function to update data_cal when a file is uploaded
  observeEvent(input$file_cal, {
    inFile_cal <- input$file_cal
    if (is.null(inFile_cal))
      return(NULL)
    data_cal(read.csv(inFile_cal$datapath, header = TRUE))
  })
  
  # Function to generate the distribution plot
  output$distributionPlot_cal <- renderPlot({
    req(data_cal())
    if (input$runAnalysisBtn_cal) {
      x_label <- input$xLabelInput_cal
      ggplot(data_cal(), aes(x = data_cal()[, 1])) +
        geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "black", fill = "lightblue", alpha = 0.7) +
        labs(title = "Distribution Plot",
             x = x_label,
             y = "Density") +
        theme_minimal()
    }
  })
  
  # Function to generate the summary statistics table
  output$summaryTable_cal <- renderTable({
    req(data_cal())
    if (input$runAnalysisBtn_cal) {
      summary_stats <- summary(data_cal()[, 1])
      data.frame("Statistics" = names(summary_stats), "Values" = unname(summary_stats))
    }
  })
  
  # Download the generated plot
  observeEvent(input$downloadPlotBtn_cal, {
    output$downloadPlotBtn_cal <- downloadHandler(
      filename = function() {
        paste("distribution_plot_", input$xLabelInput_cal, ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = output$distributionPlot_cal(), height = 6, width = 8, units = "in", dpi = 300)
      }
    )
  })
  
  # Function to generate the summary table
  output$contents_cal <- renderDT({
    req(data_cal())
    if (input$runAnalysisBtn_cal) {
      datatable(data_cal(), options = list(pageLength = 10))
    }
  })
  
  # Function to download the template
  output$templateBtn_cal <- downloadHandler(
    filename = function() {
      "data_template.csv"
    },
    content = function(file) {
      write.csv(data.frame(EDI = NA), file, row.names = FALSE)
    }
  )
}


# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Distribution Fitting"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Fitted Distributions", tabName = "fitgenerator", icon = icon("chart-line")),
      menuItem("Random Number Generator", tabName = "randomvalue", icon = icon("random")),
      menuItem("Plot Analysis", tabName = "plotanalysis", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", home_ui),
      tabItem(tabName = "fitgenerator", fitgenerator_ui),
      tabItem(tabName = "randomvalue", randomvalue_ui),
      tabItem(tabName = "plotanalysis", plot_ui)
    )
  )
)

# Run the Shiny app
shinyApp(ui, server)