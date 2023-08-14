# Load required libraries
library(shiny)
library(fitdistrplus)
library(ggplot2)

# Define the UI
ui <- navbarPage(
  title = "Distribution Fitting with fitdistrplus",
  
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
                 numericInput("num_iterations", "Number of Iterations:", value = 100),
                 actionButton("generate_button", "Generate Fitted Distribution"),
                 downloadButton("download_data_button", "Download Data")
               ),
               mainPanel(
                 plotOutput("fitted_dist_plot")
               )
             )
           ))
)

# Define the server
server <- function(input, output) {
  
  # Generate fitted distributions for user-defined parameters
  observeEvent(input$generate_button, {
    req(data())
    
    # Fit the selected distribution to the data with user-defined parameters
    params <- NULL
    dist_type <- input$dist_type_fit
    switch(dist_type,
           "norm" = {
             params <- list(mean = input$mean, sd = input$sd)
           },
           "lnorm" = {
             params <- list(meanlog = input$meanlog, sdlog = input$sdlog)
           },
           "exp" = {
             params <- list(rate = input$rate)
           },
           "gamma" = {
             params <- list(shape = input$shape, rate = input$rate_gamma)
           },
           "logis" = {
             params <- list(location = input$location, scale = input$scale)
           },
           "beta" = {
             params <- list(shape1 = input$shape1, shape2 = input$shape2)
           },
           "weibull" = {
             params <- list(shape = input$shape_weibull, scale = input$scale_weibull)
           }
    )

    
    # Output the plot
    output$fitted_dist_plot <- renderPlot({
      fitted_dist_plot
    })
    
    # Generate data based on the fitted distribution
    data_generated <- eventReactive(input$generate_button, {
      set.seed(123) # For reproducibility
      n_iterations <- input$num_iterations
      switch(dist_type,
             "norm" = {
               rnorm(n_iterations, mean = fit$estimate[["mean"]], sd = fit$estimate[["sd"]])
             },
             "lnorm" = {
               rlnorm(n_iterations, meanlog = fit$estimate[["meanlog"]], sdlog = fit$estimate[["sdlog"]])
             },
             "exp" = {
               rexp(n_iterations, rate = fit$estimate[["rate"]])
             },
             "gamma" = {
               rgamma(n_iterations, shape = fit$estimate[["shape"]], rate = fit$estimate[["rate"]])
             },
             "logis" = {
               rlogis(n_iterations, location = fit$estimate[["location"]], scale = fit$estimate[["scale"]])
             },
             "beta" = {
               rbeta(n_iterations, shape1 = fit$estimate[["shape1"]], shape2 = fit$estimate[["shape2"]])
             },
             "weibull" = {
               rweibull(n_iterations, shape = fit$estimate[["shape"]], scale = fit$estimate[["scale"]])
             }
      )
    })
    
    # Download button to save the generated data as a CSV file
    output$download_data_button <- downloadHandler(
      filename = function() {
        paste("generated_data_", dist_type, ".csv", sep = "")
      },
      content = function(file) {
        data <- data_generated()
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)
