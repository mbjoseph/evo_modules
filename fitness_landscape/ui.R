shinyUI(pageWithSidebar(
  headerPanel("Fitness landscape simulation"),
  
  sidebarPanel(
    selectInput("random", "Randomize landscape?", 
                list("No" = 0,
                     "Yes" = 1)), 
    sliderInput("strength", "Strength of selection", 
                min=0.00001, max=10, value=1, step=0.0001),
    sliderInput("ngen", "Number of timesteps",
                min=10, max=2000, value=1000, step=1), 
    sliderInput("mutation", "Mutation rate",
                min=.01, max=4, value=.3, step=0.001)
    ), 
  mainPanel(
    tabsetPanel(
      tabPanel("Landscape contour & trajectory", plotOutput("p1"))
    )
    )
  )
        )