

## UI ----------------------------------------------------------------------

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

# Call UI inside secure_app()
# secure_app(
#   ui = ui,
#   enable_admin = TRUE,
#   language = "en"
#   # choose_language = c("fr")
# )

# change auth ui background ?
ui <- secure_app(ui,
                 theme=shinytheme("slate"),

                 enable_admin = TRUE,
                 background  = " url('https://i.postimg.cc/MKpkJYhT/Snipaste-2022-08-26-14-21-31.png')no-repeat center center / cover;;
;"
)
# url(')
