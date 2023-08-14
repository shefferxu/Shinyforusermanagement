library(shiny)
library(shinydashboard)
library(fitdistrplus)
library(shinydashboardPlus)
library(shinyjs)
#library(shinyWidgets)
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

# fitgenerator UI ---------------------------------------------------------

fitgenerator_ui <- fluidPage(
  title = "使用 ggplot2 进行分布拟合",
  
  tabPanel("拟合分布",
           fluidPage(
             titlePanel("根据不同参数生成拟合分布"),
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
  title = "使用 ggplot2 进行分布拟合",
  titlePanel("随机数生成器"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("download_template_button_random", "下载模板文件"),
      br(),
      br(),
      fileInput("file_random", "上传CSV文件:", accept = ".csv"),
      br(),
      numericInput("num_samples_input_random", "随机样本数量:", value = 10000),
      br(),
      actionButton("generate_button_random", "生成随机数"),
      br(),
      br(),
      downloadButton("download_generated_numbers_button_random", "下载生成的随机数")
    ),
    mainPanel(
      plotOutput("sampled_numbers_plot_random")
    )
  )
)


# plotanalysis ------------------------------------------------------------

plot_ui <- fluidPage(
  titlePanel("文件上传和分布图"),

  sidebarLayout(
    sidebarPanel(
      downloadButton("templateBtn_cal", "下载模板文件"),
      br(),
      br(),
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
      fit <- fitdist(data()$value, input$dist_type)
    }
    
    # Plot the histogram with the fitted density curve using plotdist
    plotdist(data()$value, input$dist_type, para = as.list(fit$estimate), demp = TRUE)
    
    # Add the fitted density curve for Weibull distribution
    if (input$fit_button > 0 && input$dist_type == "weibull") {
      curve(dweibull(x, shape = fit$estimate[1], scale = fit$estimate[2]), 
            col = "red", lwd = 2, add = TRUE)
    }
  })
  
  # Fit the selected distribution to the data
  fit_dist <- eventReactive(input$fit_button, {
    req(data())
    fitdist(data()$value, input$dist_type)
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
  
  # Function to download the template file
  output$templateBtn_fit <- downloadHandler(
    filename = function() {
      "template.csv" # Change the filename as needed
    },
    content = function(file) {
      # Create a template data frame with one column named "value"
      template_data <- data.frame("value" = NA) # You can modify the data here if needed
      # Write the template data to the CSV file
      write.csv(template_data, file, row.names = FALSE)
    }
  )
  
  # fit generator part -------------------------------------------------------------
  # Generate fitted distributions for user-defined parameters
  observeEvent(input$generate_button, {
    
    # Generate data based on the selected distribution and user-defined parameters
    n_iterations <- input$num_iterations
    dist_type <- input$dist_type_fit
    

    
    data_generated <- switch(dist_type,
                             "norm" = {
                               rnorm(n_iterations, mean = input$mean, sd = input$sd)
                             },
                             "lnorm" = {
                               rlnorm(n_iterations, meanlog = input$meanlog, sdlog = input$sdlog)
                             },
                             "exp" = {
                               rexp(n_iterations, rate = input$rate)
                             },
                             "gamma" = {
                               rgamma(n_iterations, shape = input$shape, rate = input$rate_gamma)
                             },
                             "logis" = {
                               rlogis(n_iterations, location = input$location, scale = input$scale)
                             },
                             "beta" = {
                               rbeta(n_iterations, shape1 = input$shape1, shape2 = input$shape2)
                             },
                             "weibull" = {
                               rweibull(n_iterations, shape = input$shape_weibull, scale = input$scale_weibull)
                             }
    )
    
    # Convert the generated vector to a data frame with column name "data"
    data_generated <- data.frame(data = data_generated)
    
    # Output the plot
    output$fitted_dist_plot <- renderPlot({
      ggplot(data_generated, aes(x = data)) +
        geom_histogram(aes(y = ..density..),binwidth = 0.1, color = "black", fill = "lightblue", alpha = 0.7) +
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
        write.csv(data_generated, file, row.names = FALSE)
      }
    )
  })
  
  # randomvalue part --------------------------------------------------------
  
  data_random <- reactiveVal(NULL)
  data_sampled_random <- reactiveVal(NULL)
  
  # Read data from the uploaded file
  observeEvent(input$file_random, {
    req(input$file_random)
    data_random(read.csv(input$file_random$datapath, header = TRUE)[["value"]])
  })
  
  # Generate random numbers from the data based on the user's input
  observeEvent(input$generate_button_random, {
    if (!is.null(data_random())) {
      num_samples <- input$num_samples_input_random
      data_sampled_random(sample(data_random(), num_samples, replace = TRUE))
    } else {
      # 如果没有数据，显示弹出警报
      shinyalert::shinyalert(
        title = "未上传数据",
        text = "请先上传CSV文件，并模板的列名相同。",
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
  

    output$download_template_button_random <- downloadHandler(
      filename = function() {
        "template.csv" # Change the filename as needed
      },
      content = function(file) {
        # Create a template data frame with one column named "value"
        template_data <- data.frame("value" = NA) # You can modify the data here if needed
        # Write the template data to the CSV file
        write.csv(template_data, file, row.names = FALSE)
      }
    )
  
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
  

# plot part ---------------------------------------------------------------

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

# UI Distribution ---------------------------------------------------------
ui <- fluidPage(
  
  #div(img(src = "img/CancerMIRNome_logo_white_ucr_new_database.jpg", style='margin-left: -20; margin-right: auto; width:1250px;height:130px')),
  div(img(src = "img/logo.png", style='margin-left: -20; margin-right: auto; width:100%;;height:auto')), # margin-left: -100
  
  # includeCSS("www/css/style.css"),
  #includeCSS("www/css/footer.css"),
  useShinyjs(),
  useShinyalert(),
  
  disconnectMessage(
    text = "您的会话已超时，请重新加载页面。",
    refresh = "立即刷新",
    width = 'full', top = 'center', css = 'font-family:Georgia',
    size = 30,
    colour = google.blue,
    background = "rgba(64, 64, 64, 0.9)",
    overlayColour = "#999",
    overlayOpacity = 0.7,
    refreshColour = google.green
  ),
  
  #extendShinyjs(text = jscode, functions = "refresh"),
  # tags$head(tags$meta(name = "viewport", content = "width=1280")),
  #
  # tags$style(HTML(".box.box-solid.box-primary>.box-header {
  #                   }
  #                   .box.box-primary {
  #                   background:#f5f5f5
  #                   }"
  # )),
  
  tags$style(HTML(".box.box-solid.box-danger>.box-header {
                    background:#d2d6dc;
                    color:black;
                    border-color:#d2d6dc
                    }
                    .box.box-solid.box-danger {
                    border-bottom-color:#d2d6dc;
                    border-left-color:#d2d6dc;
                    border-right-color:#d2d6dc;
                    border-top-color:#d2d6dc;
                    }"
  )),
  
  
  navbarPage(
    title = NULL,
    id = 'navbar',
    windowTitle = "概率评估和数据拟合",
    
    #tags$script(HTML("$('body').addClass('fixed');")), # fix header & sidebar
    
    theme = shinytheme("sandstone"),
    
    tabPanel('数据拟合',  home_ui, icon=icon('database')), #,'fa-2x'
    tabPanel('拟合抽样',fitgenerator_ui, icon = icon('database')),
    tabPanel('随机抽样',  randomvalue_ui, icon = icon('database')),
    tabPanel('概率结果绘图',  plot_ui, icon = icon('database')),
    # tabPanel('Risk assessment',tab_risk, icon = icon('search')),
    # tabPanel("Download", tab_download, icon = icon('download')),
    # tabPanel("Tutorial", tab_tutorial, icon = icon('file-alt')),
    
    tags$style(type = 'text/css', href = 'bootstrap.css')
    # tags$style(type = 'text/css', '.navbar-nav {padding-left: 400px; font-size: 24px;}',
    #            '.navbar-default {margin-left: 2px;margin-right: 18px;margin-top: -2px;}'
  ),
  dashboardFooter(right = HTML('<footer><script type="text/javascript" src="//rf.revolvermaps.com/0/0/3.js?i=5wbxdg3p0kl&amp;b=0&amp;s=20&amp;m=2&amp;cl=ffffff&amp;co=010020&amp;cd=aa0000&amp;v0=60&amp;v1=60&amp;r=1" async="async"></script></footer>'),
                  #"<footer><script type='text/javascript' id='clustrmaps' src='//cdn.clustrmaps.com/map_v2.js?cl=ffffff&w=150&t=tt&d=8Q_eTawC68onB0QcBDRdo3M5mhU-pCoRPOCaNoKbUCY'></script></footer>"),
                  #https://www.revolvermaps.com/
                  left = HTML("<footer><h6>Contact: <a href='https://github.com/shefferxu' target='_blank'>XiaohuiXu</a><br>Email: xiaohuixu71@outlook.com</h6></strong></footer>"))
  #left_text = HTML("<footer><h6>\t\tCopyright &#169 2020 <a href='http://jialab.ucr.acsitefactory.com/' target='_blank'>Jia Lab</a>. <br><a href='https://plantbiology.ucr.edu/' target='_blank'>Department of Botany & Plant Sciences</a>, <br><a href='https://plantbiology.ucr.edu/' target='_blank'>University of California, Riverside</a></h6></footer>"))
)

# Run the Shiny app
# port setting
options(shiny.port = 1239,shiny.host="0.0.0.0")
shinyApp(ui, server)
