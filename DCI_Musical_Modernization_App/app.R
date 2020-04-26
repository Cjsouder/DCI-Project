

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(scales)
library(vembedr)
library(htmltools)
library(png)

# Read in csv's with corps 

devils <- read.csv("bluedevils.csv")
coats <- read.csv("bluecoats.csv")

corps_names <- c("Blue Devils", "Bluecoats")


ui <- navbarPage(
    
    # Title the navbar
    
    title = "Modernization of Drum Corps International",
    
    # Apply a premade Shiny theme to the app
    
    theme = shinytheme("cerulean"),
    
    # Add first element to the navbar and code the page to which it links. This
    # page, "Conference Plot", is coded to contain a dropdown selector and radio
    # buttons beside a main plot. The plot reacts when hovered over.
    
    tabPanel(title = "About",
             fluidRow(column(
               12, wellPanel(htmlOutput("about"))
             ))),
    
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
    
# Plots a scatter plot with a linear regression  
    
    output$plot <- renderPlot(    
        ggplot(data = data_input(), aes(x = year, y = diff)) +
            geom_point() +
            geom_smooth(method = lm,
                        formula = y ~ x,
                        se = FALSE) +
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
                        "left:", left_px - 200, "px; top:", top_px - 200, "px;")
        
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
   
    
     output$about <- renderUI({
      HTML(
        paste(
          h2("About This Project"),
          br(),
          div(img(src = "dcilogo.jpg", height = 250, width = 250, align = "center")),
          br(),
          div(
            "Info about why I started project"
          ),
          br(),
          div(
            "Information about methods and analysis"
          ),
          br(),
          h4("Modeling Decisions"),
          div(
            "There are two variables of interest in this model: percent change in football wins and percent change in college applications. Looking at percent change is more insightful than looking at raw changes in wins and applications. For example, it is a greater change to go from winning two games to four in a twelve-game season than it is to go from winning seven games to nine, and fans probably prefer large improvements to small ones. It is possible that the number of games won is more important than the change in the percentage won - perhaps when a college suddenly wins enough games to enter the AP list of top 25 programs or become eligible to play in a bowl game it causes a spike in applications - but that is an assumption for a different model."
          ),
          br(),
          div(
            "Results are shown in a scatterplot since this type of graph allows the values of both variables to be displayed for many different data points. To plot each data point the change in applications for each year is matched with the change in football wins that occurred in the previous year. For example, a datapoint representing the change in applications between 2010 and 2011 will be matched with football data representing the change in wins from 2009 to 2010. The reasoning for modeling the data in this manner is that applicants apply before seeing the end of the current season and therefore rely on data from the previous season. Because both application and football seasons begin at roughly the end of August it is possible that applicants are influenced by more recent data when they make application decisions, but it is assumed that this is not the case. "
          ),
          br(),
          div(
            "A density plot of R-squared values is shown to provide context for how unusual each college's observed R-squared value may be. The plot uses 1000 bootstrapped samples taken from eleven years of observed data for each college. A necessary assumption is that eleven data points are not too few to bootstrap."
          ),
          br(), 
          h4("External Links"),
          div(
            "Data about Corp's shows, including reperoires, scores, and placements, can be found at",
            a("The Drum Corps Xperience.", href = "http://www.dcxmuseum.org/index.cfm?view=corpslist&corpsid=19&corpsyear=2010")
          ),
          br(),
          div(
            "The Github Repo for this project can be found",
            a("here.", href = "https://github.com/Cjsouder/DCI-Project")
          ),
          br(),
          h4("About the Author"),
          p(
            "Chase Souder is currently a first-year at Harvard College, planning on concentrating in government."
          ),
          p("Any questions or comments can be directed to chasesouder@college.harvard.edu")
        )
      )
    })
}   
   





# Run the application 
shinyApp(ui = ui, server = server)
