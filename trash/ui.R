

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


