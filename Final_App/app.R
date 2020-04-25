

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(scales)
library(vembedr)
library(htmltools)

# Read in csv's with corps 

devils <- read.csv("bluedevils.csv")
coats <- read.csv("bluecoats.csv")

corps_names <- c("Blue Devils", "Bluecoats")


ui <- navbarPage(
    
    # Title the navbar
    
    title = "Modernization of Drum Corps International",
    
    # Apply a premade Shiny theme to the app
    
    theme = shinytheme("united"),
    
    # Add first element to the navbar and code the page to which it links. This
    # page, "Conference Plot", is coded to contain a dropdown selector and radio
    # buttons beside a main plot. The plot reacts when hovered over.
    
    tabPanel(
        title = "Corps Plot",
        titlePanel("Plotted Correlations"),
        br(),
        sidebarPanel(
            selectInput("corps", "Corps", corps_names)
        ),
        mainPanel(plotOutput("plot", hover = hoverOpts(id = "plot_hover")), uiOutput("hover_info"))
    )
    
)




server <- function(input, output) {
    
    # Creates dropdown menu to select which corps you want to view.
    
    data_input <- reactive({switch(input$corps,
                                   "Blue Devils" = devils, 
                                   "Bluecoats" = coats
                                  
    )
    })
    
# Plots out the 
    
    output$plot <- renderPlot(    
        ggplot(data = data_input(), aes(x = year, y = diff)) +
            geom_point() +
            geom_smooth(se = FALSE) +
            theme_classic() +
            scale_x_continuous(n.breaks = 10) +
            labs(title = paste("Modernity of Song Selections", input$corps),
                 x = "Year",
                 y = "Difference Between 
      Average Song Release Date and Current Year")
    )
    
    output$hover_info <- renderUI({
        
        # Create new variables "hover" and "point" that tell R whether the cursor
        # is hovering over the data and how far the cursor is from the data points.
        
        hover <- input$plot_hover
        point <- nearPoints(data_input(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        
        # This tells R to only display a window if the cursor is near a datapoint
        
        if(nrow(point) == 0){return(NULL)} 
        
        # Calculate the percent location of the cursor from the left and top sides
        # of the window
        
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
        # Calculate the pixel distance of the cursor from the left and bottom
        # sides of the window
        
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        # Specify the styling and position of the panel that will appear when a
        # user hovers over a datapoint
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px - 200, "px; top:", top_px + 2, "px;")
        
        # Code what information to show on the the panel that will appear when a
        # user hovers over a datapoint
        
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Corps: </b>", point$corps, "<br/>",
                          "<b> Show: </b>", point$show, "<br/>",
                          "<b> Place: </b>", point$place, "<br/>", 
                          "<b> Score: </b>", point$score)))
        )
    })    
}   
   





# Run the application 
shinyApp(ui = ui, server = server)
