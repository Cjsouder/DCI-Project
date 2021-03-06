

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(scales)
library(vembedr)
library(htmltools)
library(png)

# Read in csv's with corps data

devils <- read.csv("bluedevils.csv")
coats <- read.csv("bluecoats.csv")
phantom <- read.csv("phantom.csv")
knights <- read.csv("knights.csv")
crown <- read.csv("crown.csv")
vanguard <- read.csv("vanguard.csv")
all <- read.csv("all_corps.csv")

# Create a list of corps names, for use in a dropdown menu.

corps_names <- c("All","Blue Devils", "Bluecoats", "Phantom Regiment", "Blue Knights", "Carolina Crown", "Santa Clara Vanguard")

# Set UI display

ui <- navbarPage(
    
    # Title the navbar
    
    title = "Modernization of Drum Corps International",
    
    # Apply a premade Shiny theme to the app, using cerulean to roughly match
    # DCI logo color.
    
    theme = shinytheme("cerulean"),
    
   
    # Create first tab of the shiny app, graphing the modernization of musical
    # selections. This tab allows a user to input which corps they want to view,
    # and then outputs an interactive plot which will show details about each
    # data point when it is hovered over.
  
    tabPanel(
        title = "Musical Modernization",
        titlePanel("Are musical selections becoming more modern?"),
        br(),
        sidebarPanel(
            selectInput("corps", "Corps", corps_names)
        ),
        mainPanel(plotOutput("plot", hover = hoverOpts(id = "plot_hover")), uiOutput("hover_info"))
    ),
    
  # Creating another graphing tab, to analyze the relationship between 
  # modernization and performance. Very similar UI to the previous tab, but now
  # has radio buttons to change the y - variable being graphed (this doesn't)
  # work yet but it's sure going to eventually.
    
    tabPanel(
      title = "Incentives for Modernization?",
      titlePanel("Are modern musical selections linked to higher placements?"),
      br(),
      sidebarPanel(
        selectInput("corp", "Corps", corps_names)),
      mainPanel(plotOutput("stat_plot")
      )
    ),
  
  # Another tab to detail conclusions drawn from the data, this displays a simple
  # text output.
    
  tabPanel(title = "Conclusions",
           fluidRow(column(
             12, wellPanel(htmlOutput("conclusion"))
           ))),
  
  
  # Creates the about tab, which outputs only text and one image.
    
    tabPanel(title = "About",
             fluidRow(column(
               12, wellPanel(htmlOutput("about"))
             )))
    
  
)




server <- function(input, output) {
    
# Creates dropdown menu to select which corps you want to view.
    
    data_input <- reactive({
        switch(input$corps,
               "Blue Devils" = devils, 
               "Bluecoats" = coats,
               "Phantom Regiment" = phantom,
               "Blue Knights" = knights,
               "Carolina Crown" = crown,
               "Santa Clara Vanguard" = vanguard,
               "All" = all)
      })

    corps_input <- reactive({
      switch(input$corp,
             "Blue Devils" = devils, 
             "Bluecoats" = coats,
             "Phantom Regiment" = phantom,
             "Blue Knights" = knights,
             "Carolina Crown" = crown,
             "Santa Clara Vanguard" = vanguard,
             "All" = all)
    })
  
  # Assigns colors to points for each corps when "all" is selected  

    all_colors <- c(
      "Blue Devils" = "blue",
      "Bluecoats" = "royalblue",
      "Phantom Regiment" = "gray63",
      "Blue Knights" = "deepskyblue",
      "Carolina Crown" = "goldenrod1",
      "Santa Clara Vanguard" = "firebrick3"
    )
    
  # Not sure if this is totally necessary, but explicitly assigns colors to each 
  # corps for when they are selected individually.
   
    
  bd_color <- ("Blue Devils" = "blue")  
  bloo_color <- ("Bluecoats" = "royalblue")
  phantom_color <- ("Phantom Regiment" = "gray63")
  knights_color <- ("Blue Knights" = "deepskyblue")
  crown_color <- ("Carolina Crown" = "goldenrod1")
  vanguard_color <- ("Santa Clara Vanguard" = "firebrick3")
  
    # Plots a scatter plot with a linear regression. Geom_smooth used with lm to
    # demonstrate trend over time clearly to viewers. Scaled so each year is
    # shown on x-axis, and includes changing title to reflect corps data being
    # presented.
    
    output$plot <- renderPlot(    
        ggplot(data = data_input(), aes(x = year, y = diff)) +
            geom_point(aes_string(color = ifelse(input$corps == "All", "corps", "corps"))) +
            geom_smooth(method = lm,
                        formula = y ~ x,
                        se = FALSE) +
            theme_classic() +
            scale_x_continuous(n.breaks = 10) +
            labs(title = paste("Modernity of Song Selections:", input$corps),
                 x = "Year",
                 y = "Difference Between 
      Average Song Release Date and Current Year",
                 color = "Corps") +
    
    # Sets color for points to reflect real (or near) corps color.
          
          scale_color_manual(values = if (input$corps == "All") {
            all_colors
          } else if (input$corps == "Blue Devils") {
            bd_color
          } else if (input$corps == "Bluecoats") {
            bloo_color
          } else if (input$corps == "Phantom Regiment") {
            phantom_color
          } else if (input$corps == "Blue Knights") {
            knights_color
          } else if (input$corps == "Carolina Crown"){
            crown_color
          } else {
            vanguard_color
          })
    )
    
    # Creating another plot output, for the Impact on Placement and Score tab.
    # Can't get the right colors in currently due to error not finding "corp"
    # object in case if code.
    
    output$stat_plot <- renderPlot(    
      ggplot(data = corps_input(), aes(x = place, y = diff)) +
        geom_point(aes(color = corps)) +
        geom_smooth(method = lm,
                    formula = y ~ x,
                    se = FALSE) +
        theme_classic() +
        scale_x_continuous(n.breaks = 12, limits = c(1, 12)) +
        labs(title = paste("Relationship Between Modernization and Placement:", input$corp),
             x = "Place",
             y = "Difference Between 
      Average Song Release Date and Current Year")
    )
  
    
    # Creates the ability to interact with the graph via hover. This method 'works'
    # but has flaws - as detailed later.
    
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
        # user hovers over a datapoint. This currently has issues due to position
        # of display being proportional to point location, which will need to be
        # addressed in an update.
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px - 200, "px; top:", top_px - 200, "px;")
        
        # Code what information to show on the the panel that will appear when a
        # user hovers over a datapoint. Shows name of corps, show title, result,
        # and score on finals night.
        
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Corps: </b>", point$corps, "<br/>",
                          "<b> Show: </b>", point$show, "<br/>",
                          "<b> Place: </b>", point$place, "<br/>", 
                          "<b> Score: </b>", point$score)))
        )
    })
    
  # Sets the title and text for the Conclusions tab. Fairly straightforward. 
      
    output$conclusion <- renderUI({
      HTML(
        paste(
          h2("Conclusions"),
          br(),
          div(
            "At this stage, it is too early to make firm conclusions. However, early results are hinting at three major points."
          ),
          br(),
          div(
            "Firstly, there does appear to be a general trend towards modernizing musical selections across the drum corps activity. The outlier among the current data set is the Bluecoats, as they are the only group with an positive correlation between modernization and year. This overall trend is expected, but the Bluecoats trend is certainly contrary to expectations - something I will discuss again later."
          ),
          br(),
          div(
            "Secondly, there is seemingly a positive correlation between modernization and placement across the activity - as music becomes more modern, placements seem to increase. This relation again meets up with my initial hypothesis, but upon viewing each corps' data, this may not be as significant as anticipated. There is wide variance from corp to corp, which averages out to be a positive correlation, but only slightly. Perhaps this means that some corps have better adjusted to the modernizing environment, but based upon the Bluecoats data, this seems a flawed conclusion - again, more on that in a minute."
          ),
          br(),
          div(
            "Thirdly, and most clearly, this was not an accurate way to quantify modernization of drum corps. While it does provide interesting data analysis, anyone who has watched drum corps over the past decade will scoff at any analysis which indicates that the Bluecoats are becoming far LESS modern. Arguably since 2014, and certainly since 2016, the Bluecoats have pioneered new methods and styles, truly revolutionizing the activity with their 2016 program, 'Downside Up'. Additionally, a fair amount of skew occurs in the Blue Devils data as a result of the original compositions they use. These are technically released the very same year as the show, but they may be in the style of much older, more traditional drum corps music."
          ),
          br(),
          div(
          "It is my hope that as I continue to incorporate more data into this shiny app, more conclusive results will appear. However, in future iterations of this project, I will also look to incorporate new methods of quantifying modernization of the drum corps activity.")
          )
        )
    })
    
   
    # Almost identical formatting to the conclusions tab, but slightly more complex
    # in that it contains an image and more headers. 
     
     output$about <- renderUI({
      HTML(
        paste(
          h2("About This Project"),
          br(),
          div(img(src = "dcilogo.jpg", height = 250, width = 250)),
          br(),
          div(
            "Drum Corps International is a marching music organization which has operated since 1972, putting young musicians in the position to perform high quality shows for audiences all across the country. Like all long standing organizations, however, many fans take note of the changes over time, both taking positive and negative views of these changes. The debate over the merits of 'modernization' led me to question just how much the acitivty is changing - and in what ways is it changing?"
          ),
          br(),
          div(
            "I first set out to analyze whether musical selections were changing - were shows utilizing more relatively modern pieces, or were they remaining static? To analyze this, I calculated the difference between the release date for songs in a show's repertoire and the year of the show, analyzing whether the gap was growing or changing over time - both for the activity as a whole and each individual corps. I further ran regressions to investigate the effects of 'musical modernization' on score and placement, hoping to uncover whether or not there was an incentive to do so."
          ),
          br(),
          h4("Project Decisions and Limitations"),
          div(
            "Firstly, it is important to address the most important limitation - the years of analysis. Due to the methods used in constructing data sets which could be used, the years under consideration were limited to the 2010's. A second limitation were the corps under consideration, which consist of only those corps which made it to World Class Finals at least one year in the decade - with the exclusion of the Glassmen, who ceased operations in 2014."
          ),
          br(),
          div(
            "The results of the investigation have been displayed in the form of scatter plots with best-fit lines, as seeing each individual show is critical, but the most important element is the change over time (or the change in relation to another variable)."
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
          h4("About Me"),
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
