# Load required libraries
library(shiny)
library(ggplot2)

# Define the UI
ui <- navbarPage(
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
}

# Run the Shiny app
shinyApp(ui, server)
