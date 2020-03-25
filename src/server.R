#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(dplyr)
library(magrittr)
library(lazyeval)
library(janitor)
library(plotly)
library(formattable)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  generate_DT <- function(data){
    return(
      datatable(
        data,
        selection = "none",  
        filter = "top", 
        options = list(
            order = list(c(0, "asc")),
            dom = "Bfrtip",
            scrollX = TRUE
          )
        )
      )
  }
  
  # 1.1) dataset loading
  #loads the dataset
  df <- reactive({
    if (is.null(input$dataset))
      return(NULL);
    df <- read.csv(input$dataset$datapath,
                   header = input$dataset_header,
                   sep = input$dataset_sep,
                   quote = input$dataset_quote) %>% mutate_all(as.factor)
    return(df)
  });
  
  
  #displays the basic dataset preview
  output$dataset_preview <- renderDataTable({
    data <- df()
    if (is.null(data)){
      return(NULL)
    }
    if(input$dataset_disp == "head") {
      return(generate_DT(head(data)))
    }
    else {
      return(generate_DT(data))
    }
  });
  
  ##### Projection
  
  
  
  ##### CLUSTERIZATION
  
  data.famd<-reactive({
    return(FAMD(df(), graph=F))
  })
  
  projectedData <- reactive({
    return(df() %>% mutate(viz_x=as.numeric(data.famd()$ind$coord[,1]), viz_y=as.numeric(data.famd()$ind$coord[,2])))
  })
  
  data.hcpc<-reactive({
    if(!is.null(input$catVar) && input$catVar == "automatic"){
      return(HCPC(data.famd(), graph = F, nb.clust = input$nbClusters))
    }
    return(NULL)
  })
  
  clusterizedData<- reactive({
    if (!is.null(input$catVar)){
      data <- projectedData()
      
      if (input$catVar == "automatic"){
        return( data %>% 
            mutate(cluster_dat=data.hcpc()$data.clust$clust))
      }else{
        data.hcpc<-NULL
        # specific var used.
        return(data %>% 
                 mutate(cluster_dat=as.factor( data[[input$catVar]] )) )
      }
    }
  })
  
  output$clusterInfo <- renderDT( {
      
      return(generate_DT(as.data.frame(data.hcpc()$desc.var$category[input$clusterInfoSelected])))
  })
  
  output$clusterInfoSelector <- renderUI({
    if(is.null(data.hcpc())){
      tags$h3("Only available in automatic mode.")
    }else{
      selectInput("clusterInfoSelected", "Cluster: ", selected=1, choices=c(1:length(data.hcpc()$desc.var$category)))
    }
  })
  
  
  
  output$clusteringMethodPreviewExplanation<- renderUI({
    if(is.null(data.hcpc())){
        tags$h3("Only available in automatic mode.")
    }else{
      selectInput("clusterInfoSelected", "Cluster: ", selected=1, choices=c(1:length(data.hcpc()$desc.var$category)))
    }
  })
  
  
  output$catVarSetting<- renderUI({
    choices <- c("automatic", colnames(df()))
    selectInput("catVar", "Specific known groups used : ", selected =choices[1], choices = choices )
    
  })
  
  
  output$clusteringMethodPreview <- renderPlot({
    if (is.null(clusterizedData()))
      return();
    
    return(ggplot(clusterizedData(), 
                  aes(x=viz_x, 
                      y=viz_y, 
                      color=cluster_dat,
                      group=cluster_dat))+
             geom_point()+
             geom_encircle()+
             theme_minimal())
  })
  
  
  output$clusteringMethodPreviewVariables <- renderPlot({
    return(fviz_famd_var(data.famd(), choice="var", repel=T))
  })
  
  output$clusteringMethodPreviewIndividuals <- renderPlot({
    data <- as_tibble(clusterizedData())
    # split the fdata for each cluster
    split.data<- split(data, as.factor(data$cluster_dat))
    # calculate the convex hull of each cluster 
    applied.data <- lapply(split.data, function(df){ df[chull(df$viz_x, df$viz_y),] })
    # combine the separated data into a single one,
    combined.data <- do.call(rbind,applied.data)
    plot <- ggplot(data=data, aes(x=viz_x, y=viz_y, color=cluster_dat, shape=cluster_dat))+
      geom_polygon(data=combined.data,
                   aes(x=viz_x, y=viz_y, fill=cluster_dat),
                   alpha=0.5)+
      geom_point()+
      theme_minimal()
    return(plot)
  })
  
  ##### FILTERING
  user_filter_error_message <- NULL;
  filter_errored <- F;
  output$user_filter_error<-renderUI({
    if(filter_errored){
      renderText("This filter is invalid:")
      renderText(user_filter_error_message)
    }else{
      
    }
  })
  
  user_filter <- reactive({
    tryCatch({
        print("INCLUSIVE FILTER: ")
        print(input$userFilteringText)
        return(input$userFilteringText)
      },
      error=function(e){
        user_filter_error_message<-e
        return("")
      }
    )
  })
  
  df.unfiltered <- reactive({
    print("calculating unfiltered df")
    return(clusterizedData() %>% mutate_all(as.factor))  
  })
  
  df.filtered <- reactive({
    print("calculating filtered df")
    tryCatch({
      filt<- user_filter()
      result <- df.unfiltered()
      if (filt != "")
      {
        result %<>% filter_(filt)
      }
      print("Filtered df calculated")
      
      filter_errored <- F
      showNotification("Updating filter...", duration = 1,  type="message")
      return(result)
    },
    error=function(e){
      print("ERROR: ")
      print(e)
      showNotification(paste0(e), type="error")
      filter_errored <- T
      print("Filtered df reverted to no filter")
      return(df.unfiltered())
    })
  })
  
  output$filteringClustersPreview <- renderPlot({
    print("updating filteringClusters preview...")
    pair_filtering_viz(df.unfiltered(), df.filtered())+
      coord_flip()
  })
  
  output$filteredDT <- renderDT({
    return(generate_DT(df.filtered()));
  })
  
  
  ##### MINING
  
  exploited.patterns <- reactiveVal()
  
  output$miningSettings<- renderUI({
    box(width=6,
        title="Mining settings",
        sliderInput("support", "Support", min=0.05, max=1, value=c(0.2, 1), step=0.05),
        sliderInput("minConfidence", "Minimum confidence", min=0, max=1, value=0.5, step=0.05),
        sliderInput("len", "Pattern length ", min=1, max=length(colnames(df.filtered())), value=c(1,min(10, length(colnames(df.filtered())))), step=1),
        selectInput("target", "Mining target", choices=c("maximally frequent itemsets", "rules", "hyperedgesets"), selected="rules"),
        numericInput("maxTime", "Maximum Mining time (0 to disable)", value= 5, min = 0, step = 1),
        actionButton("mining_start", "Launch mining >")
        )
    
  })
  
  
  patterns <- eventReactive(input$mining_start, {
    showNotification("Mining ongoing. Please wait...", type="message")
    transactions <- as(df.filtered(), "transactions")
    clusterItems <- grep("^cluster_dat=", itemLabels(transactions), value = TRUE)
    rules <- unique(apriori(transactions, 
                            parameter=list(support=input$support[1],
                                           smax=input$support[2],
                                           confidence=input$minConfidence,
                                           minlen=as.numeric(input$len[1]),
                                           maxlen=as.numeric(input$len[2]),
                                           maxtime=as.numeric(input$maxTime)), 
                            appearance = list(rhs=clusterItems)))
    interest_rules<- interestMeasure(rules, c("jaccard", "chiSquared", "gini", "counterexample"), transactions=transactions)
    result_rules<- cbind(DATAFRAME(rules),interest_rules)
    ## filtering
    
    switch (input$target,
      "maximal rules" = {
        result_rules%<>%filter(is.maximal(rules))
      }#,
      #"closed frequent itemsets" = {
        #print("item set from rules:")
        #itemSets<-generatingItemsets(rules)
        #print(itemSets)
        #print("size of 1 itemset: ")
        #print(nitems(itemSets[1]))
        #print("view of 1 itemset: ")
        #summary(itemSets[1])
        #result_rules%<>%filter(is.closed(new("itemsets", items=rules@lhs, quality=rules@quality )))
      #}
    )
    exploited.patterns(rep(F, nrow(result_rules)))
    return(result_rules)
  })
  
  output$patternsAmountBox <- renderValueBox({
    valueBox(nrow(patterns()), " patterns extracted")
  })
  
  output$patternsExploredBox <- renderValueBox({
    valueBox(paste0(nrow(patterns()%>%filter(exploited.patterns()))/nrow(patterns())*100, "%"), " patterns exploited")
  })
  
  output$patternsClustersDistribution<- renderPlot({
    return( ggplot(patterns(), aes(x=RHS, fill=RHS, color=RHS))+
      geom_bar(position = "identity")+
      theme_minimal()+
        coord_flip())
  })
  
  
  
  output$patternsOverview <- renderPlot({
    #return(ggpairs(patterns()%>% select(-LHS),mapping = aes(binwidth=10,color=RHS, fill=RHS), lower = list(continuous="density", combo="facethist", discrete="facetbar")))
  })
  
  ##### SAMPLING PARAMETERS
  
  samplerModel<- reactive({
    return(sampler_models[[input$modelType]])
  })
  
  modelPrecision <- "--"
  
  modelMode<- "--"
  
  output$samplingIterationBox<-renderValueBox({
    valueBox(input$sample_patterns+1, " iteration")
  })
  output$samplingExploitationExplorationRatio<-renderValueBox({
    valueBox(paste0(input$explorationRatio, "%"), " exploration ratio")
  })
  
  output$modelCurrentType<-renderValueBox({
    valueBox(input$modelType, " model")
  })
  output$modelCurrentMode<-renderValueBox({
    valueBox(modelMode, " mode")
  })
  
  output$modelPrecisionRating<-renderValueBox({
    valueBox(paste0(modelPrecision,"%"), " accuracy")
  })
  
  
  
  ##### PATTERN ANALYSIS
  pattern.ranking <- reactiveVal(rep("--", 10))
  
  pattern.subset <- reactive({
    input$sample_patterns
    # SAMPLING TRAINING HERE
    switch (input$sampling_model,
            random={
              
              n_exploited <- sum(exploited.patterns())
              if(n_exploited == 0) # first iteration only 
                {
                subset<- sample_n(patterns(), 10)
              } else {
                exploited_probability <- input$explorationRatio / 100
                weights <- lapply(exploited.patterns(), function(exploited){if(exploited) return(exploited_probability) else return(1 - exploited_probability)})
                subset<- sample_n(patterns(), 10, weight = weights)                
              }
              exploited.patterns(or(exploited.patterns(), which(patterns() %in% subset)))
              pattern.ranking(rep("--", 10))
              return(subset)
            },
            topn={
              # TODO
              # sort by the column orders
              print("TOPN -> determining column orders")
              # the dynamc arrangement is not ready
              #order.expr <- input$orderExpr
              # this works though
              order.expr <- quos("lift, support, count, jaccard, chiSquared, gini, counterexample")
              print("TOPN -> fetching exploration patterns")
              print("TOPN -> exploited patterns:")
              print(exploited.patterns())
              exploration.patterns <- patterns()%>% 
                filter(!exploited.patterns()) 
              
              print("TOPN -> fetching exploitation patterns")
              exploitation.patterns <- patterns()%>% 
                filter(exploited.patterns()) 

              print("TOPN -> filtering 10 top values.")
              
              if (sum(exploited.patterns()) == 0){ # first iteration
                subset <- exploration.patterns%>%
                  arrange(!!!order.expr) %>%
                  filter(row_number() <= 10)
                print(nrow(subset))
              }else{
                exploRatio <- input$explorationRatio
                explorAmount<- exploRatio/100*10
                exploiAmount<- 10 - explorAmount
                subset <- rbind(
                  exploration.patterns%>%
                    arrange(!!!order.expr) %>%
                    filter(row_number() <= explorAmount),
                  exploitation.patterns%>%
                    arrange(!!!order.expr) %>%
                    filter(row_number() <= exploiAmount)
                )
              }
              print("TOPN -> subset calculated!")
              print(subset)
              print("TOPN -> new exploited patterns mask: ")
              
              # TODO UPDATE EXPLOITED PATTERNS
              #exploited.patterns(or(exploited.patterns(), which(patterns() %in% (patterns() %>% all_equal(subset, ignore_row_order = T)))))
              print(exploited.patterns())
              pattern.ranking(rep("--", 10))
              return(subset)
            })
    
    return(samplerModel()$sample_patterns(patterns()))
  })
  
  output$subsetDT<-renderDT({
    input$dislikeButton
    input$likeButton
    input$notInterestingButton
    input$interestingButton
    print("SUBSETDT -> Getting values")
    subset <-pattern.subset()
    print("DT fetched :")
    print(subset)
    feedback <- pattern.ranking()
    print("Feedback fetched :")
    print(feedback)
    
    print("SUBSETDT -> merging values")
    df<- cbind(list(feedback=feedback), subset)
    print(df)
    print("SUBSETDT -> DF ASSEMBLED")
    df %<>% mutate_if(is.numeric, function(x){digits(x,2)})
    print("SUBSETDT -> Numerical values coerced.")
    dt<-as.datatable(
      formattable(df,
        list(
          jaccard= color_bar("lightgray"),
          count= color_bar("lightgray"),
          lift= color_bar("lightgray"),
          confidence= color_bar("lightgray"),
          chiSquared= color_bar("lightgray"),
          gini= color_bar("lightgray"),
          counterexample= color_bar("lightgray"),
          support= color_bar("lightgray")
          )
        ), 
      selection="multiple",  
      filter = "top",
      options = list(
        dom = "t",
        scrollX = TRUE
      )
    ) 
    print("SUBSETDT -> DT created")
    return(dt)
  })
  
  
  patterns_selected <- reactive({
    print("SELECTED -> GETTING CURRENT SUBSET")
    res <- pattern.subset() %>% mutate(selected=F)
    print("SELECTED -> CHECKING HIDDEN ROWS")
    if(!is.null(input$subsetDT_rows_all)){
      res<- res[input$subsetDT_rows_all,] 
    }
    print("SELECTED -> CHECKING ALREADY SELECTED ROWS")
    if(!is.null(input$subsetDT_rows_selected)){
      res[input$subsetDT_rows_selected,]$selected <- T
    }
    print("SELECTED -> READY")
    return(res)
  })

  
  observeEvent(input$dislikeButton,{
    pattern.ranking(
      mapply(
        function(selected, current_val){
          if (selected)
            return("Disliked")
          else
            return(current_val)
        },
        patterns_selected()$selected,
        pattern.ranking()
      )
    )
    })

  observeEvent(input$likeButton,{
    pattern.ranking(
      mapply(
        function(selected, current_val){
          if (selected)
            return("Liked")
          else
            return(current_val)
        },
        patterns_selected()$selected,
        pattern.ranking()
      )
    )
    })

  observeEvent(input$interestingButton,{
    pattern.ranking(
      mapply(
        function(selected, current_val){
          if (selected)
            return("Interested")
          else
            return(current_val)
        },
        patterns_selected()$selected,
        pattern.ranking()
      )
    )
    })

  observeEvent(input$notInterestingButton,{
    pattern.ranking(
      mapply(
        function(selected, current_val){
          if (selected)
            return("Not interested")
          else
            return(current_val)
        },
        patterns_selected()$selected,
        pattern.ranking()
      )
    )
  })
  
  
  ##### plots
  
  
  
  
  ##### ANALYSTICS
  
  
  ##
  
  
  
  
})
