#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Rough Sketch of DCI Finals, 2010-2019"),
    
    mainPanel(
        imageOutput("dist_plot")
    ))


server <- function(input, output, session) {
    
    output$dist_plot <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        basic_graph <- normalizePath(file.path('./',
                                             paste('basic_graph', input$n, '.png', sep='')))
        
        # Return a list containing the filename and alt text
        #return(dist_plot)
        list(src = basic_graph,
             width="800", 
             height="600",
             alt = paste("1", input$n))
    }, deleteFile = FALSE)
}
# Run the application 
shinyApp(ui = ui, server = server)