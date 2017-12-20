# ----------------------------
# Description: Shiny dashboard for automated anomaly detection
# Author: Biju
# Date: 2017-12-19
# Version: 00.01
# Date: 
# Version: 
# Changed: 
# ----------------------------

library(shiny)
library(tidyverse)
library(scales)
library(plyr)


DF_Data <- readRDS(paste0(dataPath, "DF_Data_Process_Recent.data"))
DF_Equip <- read_csv(paste0(dataPath, "DF_EquipmData.csv"))
DF_EvCode <- read_csv(paste0(dataPath, "DF_EvCodeDataProject.csv"))
  
DF_TEMP <- DF_Data %>% 
  inner_join(DF_Equip, by = "IDEquipment") %>%
  inner_join(DF_EvCode, by = "EventCode") %>%
  select(StartDate, Name, AnalogVal, EventText)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # ====================   
  StartDate <- reactive({as.POSIXct(input$DateStart)})
  EndDate <- reactive({as.POSIXct(input$DateEnd)})
  StatErr <- reactive({input$cboxSE})
  Classes <- reactive(({input$numClasses}))
  # ====================
  
  DF_SUM <- reactive({
    
    DF_TEMP %>% filter(EventText == input$selInput) %>%
      group_by(Name) %>%
      filter(StartDate > StartDate(), StartDate < EndDate())

  })
  
  DF_SUM_ALL <- reactive({
    
    DF_KM <- DF_TEMP %>% filter(EventText == input$Step)
    
    KM1 <- DF_KM %>%
      select(Name, AnalogVal) %>%
      mutate(Name = revalue(Name, c("Machine #1" = "1", "Machine #2" = "2", "Machine #3" = "3", "Machine #4" = "4"))) %>%
      mutate(Name = as.numeric(Name)) 
    
    if(!input$scaled){
      KM <- KM1 %>% kmeans(centers = Classes(), nstart = 20)
    }else{
      KM <- KM1 %>% scale() %>% as.data.frame() %>% kmeans(centers = Classes(), nstart = 20)
    }
    
    vector <- as.data.frame.vector(KM$cluster)
    names(vector) <- "Clust"
    
    DF_SUMM_ALL <- DF_KM %>% select(StartDate, AnalogVal, Name) %>%
      bind_cols(vector) %>%
      mutate(Clust = as.factor(Clust))
  })
  
  
  mainPlot <- function(){
    if(input$points)  {
      DF_SUM() %>% ggplot(aes(x=StartDate, y=AnalogVal, col=EventText)) +
        geom_smooth(se=StatErr()) +
        geom_point(alpha=0.4) +
        facet_wrap(~Name) + ylab("Process parameter, Arbitrary unit") +
        ggtitle(paste("Overview of Steps ", "from: ",
                      StartDate(), " to: ", EndDate(), sep = "")) 
    }else{
      DF_SUM() %>% 
        ggplot(aes(x = StartDate, y = AnalogVal, col = as.factor(EventText))) + 
        geom_smooth(alpha = 0.5, se = StatErr()) +
        facet_wrap(~Name) + ylab("Process parameter, Arbitrary unit") +
        ggtitle(paste("Overview of Steps ", "from: ",
                      StartDate(), " to: ", EndDate(), sep = "")) 
    }
  }
  
  boxplot <- function() {
    DF_SUM() %>% ggplot(aes(x=StartDate, y=AnalogVal, col=EventText)) +geom_boxplot() +
      facet_grid(~Name) +
      ylab("Process parameter, Arbitrary unit") +
      theme(legend.direction = "horizontal", legend.position = "bottom")+
      ggtitle(label = paste("Box Plot from all data. From: ", StartDate(), " To: ", EndDate(), sep = ""), 
              subtitle = "Box plots can help to indicate average values and outliers")
  }
  
  deviationPlot <- function(){
    
    DF_SUM_ALL() %>% 
      filter(StartDate > StartDate(), StartDate < EndDate()) %>% 
      ggplot(aes(x = StartDate, y = AnalogVal, col = Clust)) + geom_point() + facet_wrap(~Name)+
      ylab("Process parameter, Arbitrary unit") +
      theme(legend.direction = "horizontal", legend.position = "bottom")+
      ggtitle(label = paste("Anomaly Detection of the Arbitrary Parameter. From: ", StartDate(), " To: ", EndDate(), sep = ""), 
              subtitle = "Different colors may highlight potential anomaly") 
    
  }
  
  output$plot <- renderPlot({
    ggsave(paste0(appPath, "plot.png"), plot = mainPlot(), device = "png")
    mainPlot()}, height = "auto", width = 650
  )
  
  output$plot2 <- renderPlot({ 
    ggsave(paste0(appPath, "plot.png"), plot = boxplot(), device = "png")
    boxplot() }, height = "auto", width = 650)
  
  output$plot3 <- renderPlot({ 
    ggsave(paste0(appPath, "plot.png"), plot = deviationPlot(), device = "png")
    deviationPlot() }, height = "auto", width = 650)
  
  output$downloadPlot <- downloadHandler(
    filename = function(){"plot.png"},
    content = function(file){
      file.copy("plot.png", file, overwrite = TRUE)
    }
  )
})
