library(shiny)
library(treemap)
library(d3treeR)
library(ggplot2)
library(dplyr)
library(reshape2)
library(fmsb)


# Read the dataset from the CSV file
df <- read.csv("final_dataset.csv")

# color palette

ui <- fluidPage(navbarPage(
  "Mental Health In Tech Company",
  
  # First page - Intro
  tabPanel(
    "Overview",
    fluidPage(
      h1("Examining Mental Health Disorder in Tech Company"),
      # div(img(src = "google cover.png", height = 507, width = 826), style="text-align: center;"),
      tags$img(src = "https://mir-s3-cdn-cf.behance.net/project_modules/max_1200/add3ae87838251.5dc411d8b658f.jpg"),
      br(),
      h4("What is this analysis about?"),
      p(
        "This analysis focuses on mental health problems within the technology industry
                         and their implications, particularly during the COVID-19 pandemic.
                         Our dataset comprises surveys conducted before and during the pandemic,
                         shedding light on the increasing prevalence of mental health issues.
                         Participants' feedback emphasizes ongoing struggles and the need for open discussions.
                         Additionally, we compare global mental health data to provide context."
      ),
      br(),
      p(
        "The technology industry exhibits a higher rate of mental health problems compared to the general population.
                           We investigate the unique factors contributing to this disparity and assess the availability of mental health support.
                           Conducted by Open Sourcing Mental Illness, LTD, the surveys offer insights into the prevalence of mental health problems and their underlying causes."
      ),
      br(),
      br(),
      h4("What did we find through this analysis?"),
      p(
        "Our findings reveal a notable rise in mental health problems within the technology industry, especially amid the pandemic. 
        Instances of depression increased, alongside an upsurge in searches for mental health information. 
        This trend reflects a heightened awareness and demand for resources."
      ),
      
      br(),
      br(),
      h4("Why this analysis may be relevant to you?"),
      p(
        "Our analysis aims to provide valuable insights for technology companies, policymakers, and industry professionals. 
        Understanding these challenges is crucial for fostering a supportive work environment and implementing effective support systems. 
        By addressing mental health concerns, we can cultivate well-being and enhance productivity in the technology sector."
      )
    )
    
  ),
  
  tabPanel(
    "Radar Chart  ",
    
    sidebarLayout(
      sidebarPanel(
        h4("Description"),
        helpText("The interactive radar chart showcases the patterns of mental disorders in tech industries compares to US over 2016 to 2017. 
                 The chart enables users to explore and analyze the changing patterns of mental disorders, aiding in understanding the impact of industry-specific factors on mental health."),
        h4("Instruction"),
        helpText("Select a specific year and code to generate a radar chart that visualizes the prevalence of five mental health disorders"),
        selectInput("year", "Select year:", choices = unique(df$Year)),
        selectInput("code", "Select code:", choices = unique(df$Code)),
        actionButton("showChart", "Show Radar Chart")
      ),
      
      mainPanel(
        plotOutput("radarChart")
      )
    ),
    p("From 2016 to 2017, the average probabilities of having each mental disorders increased especially in tech company. Depression, bipolar disorder and anxiety are most prevalence in tech workers"),
  ),
  
  # Second Page - Interactive Treemap 
  tabPanel(
    "Interactive Treemap",
    sidebarLayout(
      sidebarPanel(
        h4("Description"),
        helpText( "The treemap presents the three major mental health disorders prevailed from 2016 to 2017. 
                They are anxiety disorder, bipolar disorder, and depression. "
        ),
        helpText("You can examine the rates of having these mental health disorders 
               separately for people who worked in tech companies and normal people."),
        hr(),
        h4("Instruction"),
        helpText(
          "Click the block and examine the mental health disorder rate in detail."
        ),
        helpText("When you click a block, it will expand and provide detailed rates of having certain mental health disorder
               for US citizens and people who worked for tech companies in 2016 or 2017."),
      ),
      mainPanel(
        d3treeOutput("treemapPlot")
      )
    )
    
  ),
  
  tabPanel(
    "Summary",
    fluidPage(
      h4("Here is our conclusion"),
      p("Based on surveys conducted by Open Sourcing Mental Illness, LTD, 
        we observed an increasing prevalence of mental health issues within the technology sector. 
        Feedback from participants emphasized ongoing struggles and the need for open discussions. 
        This analysis underscores the significance of addressing mental health concerns within the technology industry 
          to create healthier work environments and enhance the overall productivity and satisfaction of employees."),
      br(),
      hr(),
      br(),
      h4("Here is our reference"),
      p(
        "Reference:
             Devastator, T. (2022, December 14). ", a("Global trends in mental health disorder.", href = "https://www.kaggle.com/datasets/thedevastator/uncover-global-trends-in-mental-health-disorder"),
        
        "Hao, Y. (2020, June 15). ", a("Covid-19 and Mental Health Search terms.", href = "https://www.kaggle.com/datasets/luckybro/mental-health-search-term"),
        
        "Inthavong, A. (2020, September 27).", a("Mental health in the Tech Industry.", href ="https://www.kaggle.com/datasets/anth7310/mental-health-in-the-tech-industry"),
        
        "Open Sourcing Mental Illness, L. (2019, November 17). ", a("Osmi Mental Health in Tech Survey 2016.", href = "https://www.kaggle.com/datasets/osmi/mental-health-in-tech-2016")
      ), 
    )
  ),
),
)

server <- function(input, output) {
  output$treemapPlot <- renderD3tree({
    # dataset
    group <-
      c(rep("Anxiety", 4), rep("Bipolar", 4), rep("Depression", 4))
    subgroup <-
      paste(
        "subgroup" ,
        c(
          "Tech 2016",
          "Tech 2017",
          "US 2016",
          "US 2017",
          "Tech 2016",
          "Tech 2017",
          "US 2016",
          "US 2017",
          "Tech 2016",
          "Tech 2017",
          "US 2016",
          "US 2017"
        ),
        sep = "-"
      )
    value <-
      c(
        23.442809,
        38.722555,
        6.642099,
        6.635055,
        28.652322,
        41.716567,
        0.651569,
        0.651236,
        28.652322,
        41.716567,
        4.813114,
        4.83561
      )
    data <- data.frame(group, subgroup, value)
    
    # basic treemap
    p <- treemap(
      data,
      index = c("group", "subgroup"),
      vSize = "value",
      type = "index",
      palette = "Set2",
      bg.labels = c("white"),
      align.labels = list(c("center", "center"),
                          c("right", "bottom"))
    )
    
    # make it interactive ("rootname" becomes the title of the plot)
    d3tree2(p, rootname = "General")
  })
  
  # Generate the radar chart
  output$radarChart <- renderPlot({
    req(input$showChart)  # Wait for the "Show Radar Chart" button to be clicked
    
    # Filter the data based on selected year and code
    filtered_data <- dplyr::filter(df, Year == input$year, Code == input$code)
    
    # Select the relevant columns for the radar chart
    selected_data <- filtered_data[, c("Schizophrenia....", "Bipolar.disorder....", "Eating.disorders....", "Anxiety.disorders....", "Depression....")]
    
    # Calculate the mean values for each disorder
    means <- dplyr::summarise_all(selected_data, mean)
    
    # Reshape the data into long format
    melted_data <- reshape2::melt(means)
    
    # Create the radar chart using ggplot
    ggplot(melted_data, aes(x = variable, y = value, group = 1)) +
      geom_polygon(fill = "blue", alpha = 0.5) +
      geom_line() +
      geom_text(aes(label = value), vjust = -0.5) + 
      coord_polar() +
      labs(title = paste("Disorder distribution for", input$year, "at", input$code), x = NULL, y = NULL) +
      theme_minimal()
    
  })
}

shinyApp(ui, server)




