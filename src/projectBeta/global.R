#install.packages("rio")
#library(rio)
#install_formats()

library(tidyverse)

library(FactoMineR)
library(factoextra)
library(shinydashboard)
library(shiny)
library(arules)
library(reshape)
library(tidyr)
library(DT)
library(GGally)
library(dplyr)
library(ggalt)
library(ggfortify)
library(tibble)
options(shiny.reactlog=TRUE) 
USER <- "loladmin"
PASS <- "jeanDB"
URL <- "loldb-9slby.mongodb.net"
clustering_methods<- list(
  # a clustering method contains:
  # a clusterize method taking as input >
  # - at least a dataframe of data, 
  # - anything else needed.
  # this method must result as >
  # - a dataframe containing the input dataframe + a categorical cluster_dat variable
  
  # a visualization list containing cluster general visualizations functions is also needed.
  # these must be able to only need clusterized data to run and output a visualization.
  
  # a method having parameters needs to specify as much args setup functions as there are parameters to setup.
  # a parameter setup function needs to accept the id and the dataframe of loaded data only.
  # a parameter also nees a process function to parse the necessary data from the shiny output objects.
  
  # binarizing a numerical value
  # pareto = list(),
  # using a categorical variable of known groups
  # TODO fix argument management
  known_groups=list(
    clusterize=function(
      df,
      cat_var,
      nb_clusters=-1,
      ...
    ){
      print("Clusters calculated.")
      return(df %>% 
               mutate(cluster_dat=as.factor( df[[cat_var]] )))
    },
    visualization=list(
      preview=function(
        df.famd,
        cat_var,
        ...){
        return(fviz_famd_ind(df.famd, geom = "point", repel=T, habillage = cat_var, addEllipses = T))
      }
    )
  ),
  # generating clusters via MCA + HCPC from the data itself
  # TODO GERER LES ARGUMENTS
  #MCA_HCPC=list(
  #  clusterize=function(df, ...){
  #    print("calculating MCA") 
  #    print(as_tibble(df))
  #    df.mca <- MCA(df, graph=F)
  #    print("calculating HCPC")
  #    df.hcpc<- HCPC(df.mca, graph = F, nb.clust = -1)
      
  #    print("Clusters calculated.")
  #    return(df %>% 
  #             mutate(cluster_dat=df.hcpc$data.clust$clust))
  #  },
  #  visualization=list(
  #    count=function(df){
  #      return(ggplot(df,
  #                    aes(x=cluster_dat, color=cluster_dat, fill=cluster_dat))+
  #              geom_bar()+
  #              theme_minimal())
  #   },
  #   mca_var=function(df.mca){
  #     return(fviz_mca_var(df.mca, choice = "mca.cor", 
  #                         repel = TRUE, # Avoid text overlapping (slow)
  #                         ggtheme = theme_minimal()))
  #   },
  #   mca_biplot=function(df, df.mca, cluster_var="cluster_dat", label="none"){
  #     return(fviz_mca_biplot(df.mca,
  #                            label=label, 
  #                           col.var ="blue",
  #                            habillage=as.factor(df[, cluster_var]), 
  #                            addEllipses=TRUE, 
  #                            ellipse.level=0.95))
  #   },
  #   mca_ind=function(df, df.mca, cluster_var="cluster_dat", label="none"){
  #     return(fviz_mca_ind(df.mca,
  #                         label=label, 
  #                         col.var ="blue",
  #                         habillage=as.factor(df[, cluster_var]), 
  #                         addEllipses=TRUE, 
  #                         ellipse.level=0.95))
  #   },
  #   hcpc_clusters=function(df.hcpc){
  #     return(fviz_cluster(df.hcpc, 
  #                         repel=T,
  #                         geom="point",
  #                         ggtheme = theme_minimal()))
  #   }
  # ),
  # util=list(
  #    df_to_mca=function(df){
  #         return(MCA(df, graph = F))
  #    },
  #    mca_to_hcpc=function(df.mca){
  #      return(HCPC(df.mca, graph=F))
  #    }
  #  )
  #),
    # generating clusters via MCA + HCPC from the data itself
    HCPC=list(
      clusterize=function(df, df.famd, nb_clusters=-1,...){
        if(nb_clusters == 0){
          nb_clusters<--1
        }
        df.hcpc<- HCPC(df.famd, graph = F, nb.clust = nb_clusters)
        print("Clusters calculated.")
        return(df %>% 
                 mutate(cluster_dat=df.hcpc$data.clust$clust))
      },
      visualization=list(
        
        preview = function(df.famd,
                           nb_clusters=0,
                           ...){
          if(nb_clusters == 0){
            nb_clusters<--1
          }
          df.hcpc <- HCPC(df.famd, graph=F, nb.clust = nb_clusters)
          return(fviz_cluster(df.hcpc,
                              repel=T,
                              geom="point",
                              ggtheme = theme_minimal()))
        }
      )
  )
  # etc....
)

pair_filtering_viz <- function(df, df.filtered, unfiltered.name="before", filtered.name="after")
  {
  unfiltered_stats <- df %>% 
    group_by(cluster_dat, .drop = F) %>% 
    summarize(kept=sum(n()))
  
  filtered_stats <- df.filtered %>% 
    group_by(cluster_dat, .drop = F) %>% 
    summarize(kept=sum(n()))
  
  filter_stats <- unfiltered_stats %>% select(-kept)
  filter_stats[[unfiltered.name]] <- unfiltered_stats$kept
  filter_stats[[filtered.name]] <- filtered_stats$kept
  filter_stats %<>% 
    gather(unfiltered.name,
           filtered.name, 
           key="filter", 
           value="kept")
  
  return(ggplot(filter_stats,
                aes(x=cluster_dat, color=filter, fill=filter, y=kept))+
           geom_bar(stat="identity", position="dodge")+
           theme_minimal())
}

full_filtering_viz <- function(df.unfiltered, df.exclusive, df.inclusive, df.combined)
{
  unfiltered_stats <- df.unfiltered %>% 
    group_by(cluster_dat, .drop = F) %>% 
    summarize(kept=sum(n()))
  
  exclusive_stats <- df.exclusive %>% 
    group_by(cluster_dat, .drop = F) %>% 
    summarize(kept=sum(n()))
  
  inclusive_stats <- df.inclusive %>% 
    group_by(cluster_dat, .drop = F) %>% 
    summarize(kept=sum(n()))
  
  combined_stats <- df.combined %>% 
    group_by(cluster_dat, .drop = F) %>% 
    summarize(kept=sum(n()))
  
  filter_stats <- unfiltered_stats %>% select(cluster_dat)
  
  filter_stats$unfiltered <- unfiltered_stats$kept
  filter_stats$exclusive <- exclusive_stats$kept
  filter_stats$inclusive <- inclusive_stats$kept
  filter_stats$combined <- combined_stats$kept
  filter_stats %<>% 
    gather(`combined`,
           `exclusive`,
           `inclusive`,
           `unfiltered`, 
           key="filter", 
           value="kept")
  
  return(ggplot(filter_stats,
                aes(x=cluster_dat, color=filter, fill=filter, y=kept))+
           geom_bar(stat="identity", position="dodge")+
           theme_minimal())
}




#data <- read.csv("~/Stage/Datazilla/zoo.csv") %>% mutate_all(as.factor)
#data.famd <- FAMD(data)
#fviz(data.famd,element="ind", repel=F, label="ind", mean.point =F, habillage="class", addEllipses = T, ellipse.type = "convex")
#fviz_famd_var(data.famd, repel = T)
#fviz_famd_var(data.famd, choice = "var", repel = T)
#var.data <- data.famd$var$coord[,1:2]
#data.hcpc<-HCPC(data.famd, nb.clust = -1)
#print(data.hcpc$desc.var$category)

#data <- readRDS("~/svn/zoo/Stage/zoodb.Rds")


# detect clusters using mca projection on a 2D plane and then hierarchical clustering on it.
# projection
# clusterization

# select clusters from the hair categorical variable (example)
#cat_var <- "class"
#result_cat_var <- data %>% 
#  mutate(cluster_dat=as.factor( data[[cat_var]] )) 

#nb_clusters <- -1
#result_hcpc <- data %>% 
#  mutate(cluster_dat=HCPC(data.famd, graph = F, nb.clust = nb_clusters)$data.clust$clust)

#result_projected <- result_cat_var %>% mutate(viz_x=as.numeric(data.famd$ind$coord[,1]), viz_y=as.numeric(data.famd$ind$coord[,2]))

#ggplot(result_projected, 
#       aes(x=viz_x, 
#           y=viz_y, 
#           color=cluster_dat,
#           group=cluster_dat))+
#  geom_point()+
#  geom_encircle()


#fviz_famd_ind(data.famd, habillage=cat_var, geom="point", )
#fviz_c
#data.mca<- clustering_methods$MCA_HCPC$util$df_to_mca(data)
#data.hcpc <- clustering_methods$MCA_HCPC$util$mca_to_hcpc(data.mca)

# visualize clusters
#clustering_methods$MCA_HCPC$visualization$hcpc_clusters(data.hcpc)

# visualize mca data, clusters from the hcpc
#clustering_methods$MCA_HCPC$visualization$mca_ind(result_mca_hcpc, data.mca)

# visualize mca data, clusters from the selected variable
#clustering_methods$MCA_HCPC$visualization$mca_ind(result_cat_var, data.mca)

# visualize mca data variable influence
#clustering_methods$MCA_HCPC$visualization$mca_var(data.mca)

#clustering_methods$known_groups$visualization$count(result_cat_var)
#clustering_methods$known_groups$visualization$count(result_mca_hcpc)

#filtering inclusively a frame
#inclusive_filter <- quote(Class==1|Class==2)
#inclusive_result_cat_var <- result_cat_var %>% filter_(inclusive_filter)
#pair_filtering_viz(result_cat_var, inclusive_result_cat_var, filtered.name = "inclusive")


#filtering exclusively a frame
#exclusive_filter <- quote(venomous==0&toothed==0)
#exclusive_result_cat_var <- result_cat_var %>% filter_(exclusive_filter)
#pair_filtering_viz(result_cat_var, exclusive_result_cat_var, filtered.name = "exclusive")

# combining inclusive and exclusive filters
#filtered_result_cat_var <- result_cat_var %>% filter_(inclusive_filter) %>% filter_(exclusive_filter)
#pair_filtering_viz(result_cat_var, filtered_result_cat_var, filtered.name = "combined")

#full comparison
#full_filtering_viz(result_cat_var, exclusive_result_cat_var, inclusive_result_cat_var, filtered_result_cat_var)
