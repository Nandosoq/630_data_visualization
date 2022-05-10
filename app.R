library(shiny)
library(datasets)

mpg$drv <- as.factor(mpg$drv)

df_4w <- mpg[mpg$drv=='4',]
df_forward <- mpg[mpg$drv=='f',]
df_rear <- mpg[mpg$drv=='r',]

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("overall_boxes", label = h3("Choose drv types: "), 
                         choices = list("4 wheel" = "4w", "Forward" = "fw", "rear" = "rw"),
                         selected = c("4w", "fw", "rw"))
    ),

      plotOutput("mpgPlot")
    )
  )

server <- function(input, output) {
  
  merge_results <- reactive({
    
    bind_rows(
      if ("4w" %in% input$overall_boxes) { df_4w } else { df_4w[F,] }, 
      if ("fw" %in% input$overall_boxes) { df_forward } else { df_forward[F,] } ,
      if ("rw" %in% input$overall_boxes) { df_rear } else { df_rear[F,]}
    )
  })
  

  output$mpgPlot <- renderPlot({
    ggplot(merge_results(), aes(displ, hwy, colour = drv)) +
      geom_point(size = 2) + 
      geom_smooth(size = 1.4, se = FALSE)
  })
  
}

shinyApp(ui = ui, server = server)