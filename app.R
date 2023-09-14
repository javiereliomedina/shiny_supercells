library(shiny)
library(supercells)
library(terra)
library(sf)
library(shinybusy)

distance <- c(philentropy::getDistMethods(),
              #"jsd",
              "dtw")
averaging <- c("mean", "median", "max", "min")
transformations <- c("NULL", "to_LAB")

ui <- fluidPage(
  
  add_busy_spinner(spin = "fading-circle", color = "#98c1d9"),
  theme = shinythemes::shinytheme("cerulean"),
  
  h1("Shiny implementation of supercells package"), 
  p("This Shiny app is an implementation of the R-package",
    a("supercells.", href = "https://jakubnowosad.com/supercells/index.html"),
    "Please read the package manual for more information, I am only a user and
      have no developed it (I've just found it very interesting!!)"),
  
  fluidRow(
    column(2,
           # Read raster
           fileInput("img", "Select spatial raster", accept = ".tif")
    )
  ),
  
  fluidRow(
    
    # Parameter for supercells
    column(2, numericInput("k", "Number of supercells", 
                           value = 50,
                           min = 50, max = 5000, step = 50)
           ),
    column(2, numericInput("compactness", "Compactness",
                           value = 1,
                           min = 1, max = 20, step = 1)
           ),
    column(2, selectInput("dist_fun", "Distance function", distance)),
    column(2, selectInput("avg_fun", "Averaging function", averaging)),
    column(2, selectInput("clean", "Clean",
                          choices = c(TRUE, FALSE),
                          selected = TRUE))
    ),
  
  fluidRow(
    
    column(2, numericInput("iter", "Iterations", 
                           value = 10,
                           min = 5, max = 50, step = 5)
           ),
    column(2, selectInput("transform", "Transform", transformations))
    
  ),
  
  fluidRow(
    column(6,
           # Plot input raster
           p(strong("Original raster:")),
           plotOutput("plot_in")
    ),
    
    column(6,
           # Plot output rastrer
           p(strong("Segmented raster:")),
           plotOutput("plot_out"))
  ),
  
  fluidRow(
    downloadButton("download", "Save output (.gpkg)")
  ),
  
  a(HTML(
    "<script data-name=\"BMC-Widget\"
          data-cfasync=\"false\" 
          src=\"https://cdnjs.buymeacoffee.com/1.0.0/widget.prod.min.js\" 
          data-id=\"javierelio\" 
          data-description=\"Support me on Buy me a coffee!\"
          data-message=\"\" 
          data-color=\"#ff813f\" 
          data-position=\"Right\" 
          data-x_margin=\"18\" 
          data-y_margin=\"18\">
          </script>"
  )
  )
)

server <- function(input, output, session) {
  
  # Read original raster
  imagen_in <- reactive({
    req(input$img$datapath)
    terra::rast(input$img$datapath)
  })
  
  # Supercells
  image_out <- reactive({
    supercells(imagen_in(),
               k = input$k,
               compactness = input$compactness,
               dist_fun = input$dist_fun,
               avg_fun = input$avg_fun,
               clean = as.logical(input$clean),
               iter = input$iter,
               transform = input$transform
               )
    })
  
  # Plots
  output$plot_in <- renderPlot({
    if (is.null(imagen_in()))
      return(NULL)
    plot(imagen_in())
  })
  
  output$plot_out <- renderPlot({
    if (is.null(imagen_in()))
      return(NULL)
    plot(imagen_in())
    plot(st_geometry(image_out()), add = TRUE, lwd = 0.2)
  })
  
  # Export output
  output$download <- downloadHandler(
    filename = function(){
      paste0(gsub(".tif", "", input$img), "_supercell.gpkg")
    },
    content = function(file){
      write_sf(image_out(), file)
    }
  )
}

shinyApp(ui, server)

