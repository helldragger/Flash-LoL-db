#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinydashboard)
# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "Datazilla", disable = F),
  dashboardSidebar(
    sidebarMenu(
      #menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("1) Upload dataset", tabName = "dataset_load", icon = icon("th")),
      menuItem("2) Clustering settings", tabName = "clustering", icon = icon("th")),
      menuItem("3) Data filtering", tabName = "filtering", icon = icon("th")),
      menuItem("4) Mining settings", tabName = "mining", icon = icon("th")),
      menuItem("5) Sampling settings", tabName = "sampling", icon = icon("th")),
      menuItem("6) Patterns analysis", tabName = "analysis", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #tabItem(tabName = "overview",
      #        h1("Datazilla status"),
              
              # h2 
      #        tabBox(title = "Models Training", side="right",
      #               tabPanel("RNN"),
      #               tabPanel("SVM"),
      #               tabPanel("Recommender"),
      #               tabPanel("...")
      #        ),
      #        h4("About"),
      #        p("Blabla bla bla")
      #        ),
      
      # A] Data loading and cleaning. (necessary)
      # A.1 : load data file in memory,
      # A.2 : display a subset of it  
      # A.3 : determine data attributes types for type coercion
      ##### A.1 & A.2
      tabItem(tabName = "dataset_load",
              fluidRow(
                    box( width=3,
                      title="Upload",
                      fileInput('dataset', 'Choose CSV File',
                                accept=c(c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv"))),
                      tags$hr(),
                      checkboxInput('dataset_header', 'Header', TRUE),
                      radioButtons('dataset_sep', 'Separator',
                                   c(Comma=',',
                                     Semicolon=';',
                                     Tab='\t',
                                     Whitespace=' '),
                                   ','),
                      radioButtons("dataset_quote", "Quote",
                                   choices = c(None = "",
                                               "Double Quote" = '"',
                                               "Single Quote" = "'"),
                                   selected = '"'),
                      
                      # Horizontal line ----
                      tags$hr(),
                      
                      # Input: Select number of rows to display ----
                      radioButtons("dataset_disp", "Display",
                                   choices = c(Head = "head",
                                               All = "all"),
                                   selected = "head")
                    ),
                    box(width=9,
                        title="Preview",
                        DTOutput("dataset_preview")
                    )
                  )
                ),
      
      
      # Data clustering. (helps for analysis and pattern filtering)
      # 1 : Choose a clustering method.
      # 2 : Determine the clustering method parameters.
      # 3 : start the clustering and display some visualization of it.
      # 4 : displays a subset of each cluster data to play with the data. 
      tabItem(tabName = "clustering",
              h1("Choose a method"),
              uiOutput("clusteringMethodsNames"),
              h2("Determine the parameters"),
              numericInput("nbClusters", "Amount of clusters ( 0 for automatic detection )", min = 0, max=10, value=0, step=1),
              uiOutput("catVarSetting"),
              uiOutput("clusteringMethodParameters"),
              fluidRow(
              tabBox(title = "Preview",
                     tabPanel("Clusters",
                              plotOutput("clusteringMethodPreviewIndividuals")
                     ),
                     tabPanel("Variables contribution",
                              plotOutput("clusteringMethodPreviewVariables")
                     )
                ),
                box(title="Cluster info",
                    uiOutput("clusterInfoSelector"),
                    DTOutput("clusterInfo"))
              )
              ),
      
      # Data filtering. (necessary, too much data to analyze)
      # 1 : Choose a method of subsetting { top n sur une valeur numerique, model, random }.
      # 2 : Inclusive filtering, specify interesting attributes you want to see.
      # 3 : Exclusive filtering, specify constraints the patterns must respect.
      
      tabItem(tabName = "filtering",
              fluidRow(
                box(width=4,
                    title = "Preview",
                    plotOutput("filteringClustersPreview")),
                
                box(width=8,
                    h2("What are you interested in?"),
                    textInput("userFilteringText", label="", placeholder = "var1==1 | var2>10 | (var1==var3)"),#,
                    uiOutput("user_filter_error")# recommender filtering (OR filter)
                    #h3("This might interest you..."),
                    #uiOutput("inclusiveRecommendations")
                    # recommender prediction
                    
                )
              ),
              fluidRow(
                box(width=12,
                    title = "Filtered data",
                    DTOutput("filteredDT"))
              )
      ),
      
      # Data mining. (necessary, needs something to analyze)
      # 1 : set up the mining parameters (min support, min confidence)
      # 2 : launch the mining
      tabItem(tabName = "mining",
              fluidRow(
                uiOutput("miningSettings"),
                valueBoxOutput("patternsAmountBox", width=3),
                valueBoxOutput("patternsExploredBox", width=3)
                ),
              fluidRow(
                box(width=6,
                    title="Clusters distribution",
                    plotOutput("patternsClustersDistribution")),
                box(width=6,
                    title="Patterns overview",
                    plotOutput("patternsOverview"))
              )
              ),
      
      
      tabItem(tabName = "sampling",
              fluidRow(
                box(width=6,
                    title="Sampling settings"#,
                    #TODO exploration/exploitation ratio, the explored vector is troublesome when reordering the patterns.
                    #sliderInput("explorationRatio", "Data exploration/Exploitation ratio", min=0, max=100, value=50, step=10, post = "%"),
                ),
                box(width=6,
                    title="Sampler model settings",
                    radioButtons("sampling_model","Sampling model: ",selected ="random",  choices = c("random")),
                    #radioButtons("sampling_model","Sampling model: ",selected ="random",  choices = c("random", "topn")),
                    #textInput("orderExpr", label="Top columns, in decreasing order (col1, col2, col3 = col1 > col2 > col3): ", placeholder = "lift, support, count, jaccard, chiSquared, gini, counterexample"),
                    renderUI("modelSettings") ##TODO deal with model specific settings.
                    )
              )
      ),
      
      
      # Data analysis. (necessary) projection of selected interest measures against every clusters. (jaccard, etc)
      # 1 : Select 1 to 10 patterns amongst the 10 filtered patterns.
      # A ) patterns against clusters
      # details : X axis = measure value, Y axis = pattern name, facetting X axis = cluster, facetting Y axis = measure variable, color = pattern name.
      # B ) specific pattern against clusters
      # details : X axis = measure value, Y axis = cluster, facetting Y axis = measure variable, facetting X axis = pattern name, color= pattern name 
      tabItem(tabName = "analysis",
              fluidRow(
                valueBoxOutput("samplingIterationBox", width=1),
                valueBoxOutput("samplingExploitationExplorationRatio", width=1),
                valueBoxOutput("modelCurrentType", width=2),
                valueBoxOutput("modelCurrentMode", width=2),
                valueBoxOutput("modelPrecisionRating", width=1),
                actionButton("sample_patterns", "New pattern sample")
              ),
              fluidRow(
                title="Selected data",
                DTOutput("subsetData")
              ),
              fluidRow(
                box(title="Controls", width=12,                
                    fluidRow(
                      tags$label("Mark selected patterns as :"),
                      actionButton("dislikeButton", "Disliked"),
                      actionButton("notInterestingButton", "Not interesting"),
                      actionButton("interestingButton", "Interesting"),
                      actionButton("likeButton", "Liked"))
                    ,
                    DTOutput("subsetDT"))
              )
          )
      
    )
    
  )
)
