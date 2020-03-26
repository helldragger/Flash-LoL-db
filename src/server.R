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
require(mongolite)
require(r2d3)
require(extrafont)
windowsFonts(`quadrata`=windowsFont("Friz Quadrata Std"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  generate_DT <- function(data){
    as.datatable(
      formattable(data,
                  list(
                    atk = color_bar("#f8766d"),
                    def = color_bar("#00ba38"),
                    mag = color_bar("#619cff"),
                    difficulty = color_bar("#6F6F6F")
                  )), 
      filter = "top", 
      selection=list(
        mode="single",
        selected=c(1),
        target="row"), 
      options = list(
        order = list(c(0, "asc")),
        dom = "Bfrtip",
        scrollX = TRUE
      ),
      rownames=F) %>%
      return()
  }
  ## TODO add tags filtering and name partial search
  
  df <- reactive({
    mongoCon <- mongo(db="Champions",
                      collection = "fulldata",
                      url=paste0("mongodb+srv://",USER,":",PASS,"@",URL),
                      verbose=T);
    
    df <- mongoCon$find('{}') %>% 
      select(image, name, title, info, lore, skins, tags ) %>% 
      mutate(full = image$full, atk=info$attack, def=info$defense, mag=info$magic, difficulty=info$difficulty) %>% 
      select(-image, -info)
    return(df)
  });
  
  
  
  selected <- reactive({
    if(input$HEROES == "TOUT"){
      TOUT()[input$TOUT_rows_selected,]
      
    }else if(input$HEROES == "ASSASSINS"){
      ASSASSINS()[input$ASSASSINS_rows_selected,]
      
    }else if(input$HEROES == "COMBATTANTS"){
      COMBATTANTS()[input$COMBATTANTS_rows_selected,]
      
    }else if(input$HEROES == "MAGES"){
      MAGES()[input$MAGES_rows_selected,]
      
    }else if(input$HEROES == "TIREURS"){
      TIREURS()[input$TIREURS_rows_selected,]
      
    }else if(input$HEROES == "SUPPORTS"){
      SUPPORTS()[input$SUPPORTS_rows_selected,]
      
    }else if(input$HEROES == "TANKS"){
      TANKS()[input$TANKS_rows_selected,]
    }
  })
  
  selectedFullData<- reactive({
    if(nrow(selected()) != 0){
      df() %>% filter(name == str_replace(selected()$name, ".png", ""))
    }
  })
  
  TOUT = reactive({ 
    df() %>% 
      select(name, atk, def, mag, difficulty)
  }) 
  
  output$TOUT <- DT::renderDataTable({
    TOUT() %>%
      generate_DT()
  })
  
  ASSASSINS = reactive({ 
    df() %>% 
      filter(as.logical(lapply(tags, function(x){"Assassin" %in% x}))) %>%
      select(name, atk, def, mag, difficulty)
  }) 
  
  output$ASSASSINS <- DT::renderDataTable({
    ASSASSINS() %>%
      generate_DT()
  })
  
  COMBATTANTS = reactive({ 
    df() %>% 
      filter(as.logical(lapply(tags, function(x){"Fighter" %in% x}))) %>%
      select(name, atk, def, mag, difficulty)
  }) 
  
  output$COMBATTANTS <- DT::renderDataTable({
    COMBATTANTS() %>%
      generate_DT()
  })
  
  MAGES = reactive({ 
    df() %>% 
      filter(as.logical(lapply(tags, function(x){"Mage" %in% x}))) %>%
      select(name, atk, def, mag, difficulty)
  }) 
  output$MAGES <- DT::renderDataTable({
    MAGES() %>%
      generate_DT()
  })
  
  TIREURS = reactive({ 
    df() %>% 
      filter(as.logical(lapply(tags, function(x){"Marksman" %in% x}))) %>%
      select(name, atk, def, mag, difficulty) 
  }) 
  output$TIREURS <- DT::renderDataTable({
    TIREURS() %>%
      generate_DT()
  })
  
  SUPPORTS = reactive({ 
    df() %>% 
      filter(as.logical(lapply(tags, function(x){"Support" %in% x}))) %>%
      select(name, atk, def, mag, difficulty)
  }) 
  
  output$SUPPORTS <- DT::renderDataTable({
    SUPPORTS() %>%
      generate_DT()
  })
  
  TANKS = reactive({
    df() %>% 
      filter(as.logical(lapply(tags, function(x){"Tank" %in% x}))) %>%
      select(name, atk, def, mag, difficulty)
  }) 
  output$TANKS <- DT::renderDataTable({
    TANKS() %>%
      generate_DT()
  })
  
  output$splash <- renderImage({
    if(nrow(selected()) != 0){
      champname = str_replace(selected()$name, ".png", "")
      
      version = selectedFullData() %>% .$skins %>% as.data.frame() %>% .$num %>% sample(1)
      filename = paste(paste(champname, version, sep="_"),"jpg", sep=".")
      
      SRC = normalizePath(file.path('./assets/champion_splash', filename))
      ALT = selectedFullData() %>% .$skins %>% as.data.frame() %>% filter(num == version) %>% .$name 
    }else{
      SRC = normalizePath(file.path("./assets/banner.png"))
      ALT = "League of Legends Database"
    }
    print("[EVENT] Changing splash")
    print(paste("src: ",SRC))
    print(paste("alt:",ALT))
    result = list(src=SRC, alt=ALT)
  }, deleteFile = FALSE)
  
  output$icon <- renderImage({
    if(nrow(selected()) != 0){
      SRC = normalizePath(file.path('./assets/champion_icon', paste0(selected()$name, ".png")))
      ALT = selected()$name
    }else{
      SRC = ""
      ALT = ""
    }
    print("[EVENT] Changing icon")
    print(paste("src: ",SRC))
    print(paste("alt:",ALT))
    result = list(src=SRC, alt=ALT)
  },
  deleteFile = FALSE)
  
  output$loading <- renderImage({
    if(nrow(selected()) != 0){
      champname = str_replace(selected()$name, ".png", "")
      
      version = selectedFullData() %>% .$skins %>% as.data.frame() %>% .$num %>% sample(1)
      filename = paste(paste(champname, version, sep="_"),"jpg", sep=".")
      
      SRC = normalizePath(file.path('./assets/champion_loading', filename))
      ALT = selectedFullData() %>% .$skins %>% as.data.frame() %>% filter(num == version) %>% .$name 
    }else{
      SRC = ""
      ALT = ""
    }
    print("[EVENT] Changing loading image")
    print(paste("src: ",SRC))
    print(paste("alt:",ALT))
    result = list(src=SRC, alt=ALT)
  },
  deleteFile = FALSE)
  
  output$name <- renderUI({
    data = selectedFullData()
    if (is.null(data)){return()}
    string = data %>% select(name)
    if(nrow(string) != 0){
      paste(string)
    }else{
      paste("Sélectionnez un héros en cliquant sur sa ligne pour afficher son profil")
    }
  })
  
  output$title <- renderUI({
    data = selectedFullData()
    if (is.null(data)){return()}
    string = data %>% select(title)
    if(nrow(string) != 0){
      paste(string)
    }
  })
  
  output$desc <- renderUI({
    data = selectedFullData()
    if (is.null(data)){return()}
    string = data %>% select(lore)
    if(nrow(string) != 0){
      paste(string)
    }
  })
  
  output$info <- renderPlot({
    
    df = selected() %>% 
      select(atk, def, mag)
    if(nrow(df) == 0){
      return()
    }
    
    polygon = data.frame(x=c(0, -0.86/10*df$def, 0.86/10*df$mag),
                         y=c(1/10*df$atk, -0.5/10*df$def, -0.5/10*df$mag),
                         type=c("ATTAQUE", "DÉFENSE", "MAGIE"),
                         value=c(df$atk, df$def, df$mag))
    
    scales = data.frame(x=c(1:10,1:10,1:10), y=c(1:10,1:10,1:10), type = c(rep.int("ATTAQUE", 10), rep.int("DÉFENSE", 10), rep.int("MAGIE", 10))) %>% 
      mutate(x=ifelse(type == "ATTAQUE",0,ifelse(type=="DÉFENSE", -.86/10*x, .86/10*x)), 
             y=ifelse(type == "ATTAQUE",1/10*y,ifelse(type=="DÉFENSE", -.5/10*y, -.5/10*y)))
    
    
    polygons = data.frame(x=rep.int(0,40),y=rep.int(0,40), value=rep.int(0,40))
    for (val in 0:5) {
      val = val * 2
      polygons[(3*val)+1:(3*val)+4,]$value = val  
      # atk
      polygons[(3*val)+1,]$x = 0  
      polygons[(3*val)+1,]$y = 1/10*val  
      # def
      polygons[(3*val)+2,]$x = -.86/10*val
      polygons[(3*val)+2,]$y = -.5/10*val  
      # mag
      polygons[(3*val)+3,]$x = .86/10*val  
      polygons[(3*val)+3,]$y = -.5/10*val  
      # atk again
      polygons[(3*val)+4,]$x = 0  
      polygons[(3*val)+4,]$y = 1/10*val  
      
    }
    
    
    annotFamily = "quadrata"
    
    p = polygon %>%
      ggplot(aes(x=x, y=y))+
      geom_polygon(data=polygons, aes(group=value),fill='transparent', color="gray70", size=0.3)+
      annotate(geom="text", label="ATTAQUE", x=0, y=1, angle=-57,hjust=0, vjust=0, fontface="bold", color="gray65", size=7, family = annotFamily)+
      annotate(geom="text", label="DEFENSE", x=-.86, y=-.5, angle=58, hjust=0, vjust=0, fontface="bold", color="gray65", size=7, family = annotFamily)+
      annotate(geom="text", label="MAGIE", x=.86, y=-.5, vjust=0,hjust=1, fontface="bold", color="gray65", size=7, family = annotFamily)+
      geom_polygon(fill='gray75', color="gray45", size=1.3)+
      geom_segment(aes(x=0,y=0,xend=0,yend=1, color="ATTAQUE"))+
      geom_segment(aes(x=0,y=0,xend=-.86,yend=-.5, color="DÉFENSE"))+
      geom_segment(aes(x=0,y=0,xend=.86,yend=-.5, color="MAGIE"))+
      geom_point(data=scales, aes(x=x, y=y, color=type))+
      geom_point(aes(x=x, y=y, color=type))+
      geom_label(aes(color=type, label=value))+
      theme_void()+
      guides(color=F)
    
    p
    
  }, bg="transparent")
})
