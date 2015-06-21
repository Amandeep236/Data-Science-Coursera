

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(markdown)


#setwd("C:/Users/Cindy/Desktop/ShinyApp")
data <- read.csv("data.csv")

shinyServer(
     function(input, output) {

       output$Plot <- renderPlot({
         
         Q1 <- data %>%
           select(evtype, fatalities, injuries) %>%
           group_by(evtype) %>%
           summarise(fatalities = sum(fatalities),
                     injuries = sum(injuries)) %>%
           mutate(population = fatalities + injuries) %>%
           arrange(desc(population)) %>%
           mutate(total_Rank = rank(desc(population)),
                  f_Rank = rank(desc(fatalities)),
                  i_Rank = rank(desc(injuries)))
         
         if(input$health == "Fatality") {
           gather <- Q1 %>%
             select(evtype,fatalities,f_Rank) %>%
             arrange(f_Rank) %>%
             filter(f_Rank < 11) %>%
             rename(Number = fatalities)
           
          P <- ggplot(gather, aes(x = reorder(evtype, Number), y = Number)) +
             geom_bar(stat="identity",fill = "#006699") +
             theme_bw() +
             theme(axis.text.x = element_text(size = 10)) +
             xlab("Event Type") +
             ylab("Total Population of Fatalities") +
             coord_flip() +
             ggtitle("Top 10 Harmful Weather Events for Fatalities") + 
             theme(plot.title = element_text(lineheight = .8, face = "bold", size = 15))
          
           
         } else if(input$health == "Both") {

           gather <- Q1 %>% filter(total_Rank < 11) %>%
             select(evtype,fatalities,injuries) %>%
             gather(Type,Total_Population,-evtype)
           
           P <- ggplot(gather, aes(x = reorder(evtype, Total_Population), ymax = 110000, y = Total_Population, fill = Type)) +
             geom_bar(stat="identity") +
             theme_bw() +
             theme(axis.text.x = element_text(size = 10)) +
             xlab("Event Type") +
             ylab("Total Population of Fatalities and Injuries") +
             theme(legend.position = "top") +
             coord_flip() +
             scale_fill_brewer(palette="Set1") +
             ggtitle("Top 10 Harmful Weather Events for Population Health") + 
             theme(plot.title = element_text(lineheight = .8, face = "bold", size = 15))
         }else {
           gather <- Q1 %>%
             select(evtype,injuries,i_Rank) %>%
             arrange(i_Rank) %>%
             filter(i_Rank < 11) %>%
             rename(Number = injuries)
           
           P <- ggplot(gather, aes(x = reorder(evtype, Number), y = Number)) +
             geom_bar(stat="identity", fill = "#CC0000") +
             theme_bw() +
             theme(axis.text.x = element_text(size = 10)) +
             xlab("Event Type") +
             ylab("Total Population of Injuries") +
             coord_flip() +
             #scale_fill_brewer(palette="Set1") +
             ggtitle("Top 10 Harmful Weather Events for Injuries") + 
             theme(plot.title = element_text(lineheight = .8, face = "bold", size = 15))
         }
         print(P)
       })     
       
       
       
       output$Plot2 <- renderPlot({
         
         input$action
         
         isolate(
           if(input$eco == "Property") {
           
             Q2_prop <- data %>%
             select(evtype,propdmg,propdmgexp) %>%
             mutate(New_PROPDMGEXP = ifelse(propdmgexp == "B", propdmg*1000000000,
                                     ifelse(propdmgexp == "K", propdmg*1000,
                                     ifelse(propdmgexp == "m" | propdmg =="M", propdmg*1000000,
                                      ifelse(propdmgexp == "h" | propdmg =="H", propdmg*100,"NotAValue"))))) %>%
             filter(!New_PROPDMGEXP == "NotAValue") %>%
             mutate(New_PROPDMGEXP = as.numeric(New_PROPDMGEXP)) %>%
             group_by(evtype) %>%
             summarise(PROP_DMG_EXP = (sum(New_PROPDMGEXP))/1000000) %>%
             arrange(desc(PROP_DMG_EXP)) %>%
             mutate(pro_Rank = rank(desc(PROP_DMG_EXP))) %>%
             filter(pro_Rank < 11)
           
           ggplot(Q2_prop, aes(x = reorder(evtype, PROP_DMG_EXP), y = PROP_DMG_EXP)) +
             geom_bar(stat="identity",fill = "#FF6633") +
             theme_bw() +
             theme(axis.text.x = element_text(size = 10)) +
             xlab("Event Type") +
             ylab("Total Damage Expense of Property(Million Dollars)") +
             coord_flip() +
             ggtitle("Top 10 Harmful Weather Events for Property") + 
             theme(plot.title = element_text(lineheight = .8, face = "bold", size = 15))
         } else if(input$eco == "Crop") {
           
           Q2_crop <- data %>%
             select(evtype,cropdmg,cropdmgexp) %>%
             mutate(New_CROPDMGEXP = ifelse(cropdmgexp == "B", cropdmg*1000000000,
                                            ifelse(cropdmgexp == "m" | cropdmgexp =="M", cropdmg*1000000,
                                                   ifelse(cropdmgexp == "k" | cropdmgexp =="K", cropdmg*1000,"NotAValue")))) %>%
             filter(!New_CROPDMGEXP == "NotAValue") %>%
             mutate(New_CROPDMGEXP = as.numeric(New_CROPDMGEXP)) %>%
             group_by(evtype) %>%
             summarise(CROP_DMG_EXP = (sum(New_CROPDMGEXP))/1000000) %>%
             arrange(desc(CROP_DMG_EXP)) %>%
             mutate(crop_Rank = rank(desc(CROP_DMG_EXP))) %>%
             filter(crop_Rank < 11)
           
          ggplot(Q2_crop, aes(x = reorder(evtype, CROP_DMG_EXP), y = CROP_DMG_EXP)) +
             geom_bar(stat="identity", fill = "#66CC99") +
             theme_bw() +
             theme(axis.text.x = element_text(size = 10)) +
             xlab("Event Type") +
             ylab("Total Damage Expense of Crop") +
             coord_flip() +
             ggtitle("Top 10 Harmful Weather Events for Crop (Million Dollars)") + 
             theme(plot.title = element_text(lineheight = .8, face = "bold", size = 15))
         }else {
           Q2_prop <- data %>%
             select(evtype,propdmg,propdmgexp) %>%
             mutate(New_PROPDMGEXP = ifelse(propdmgexp == "B", propdmg*1000000000,
                                            ifelse(propdmgexp == "K", propdmg*1000,
                                                   ifelse(propdmgexp == "m" | propdmg =="M", propdmg*1000000,
                                                          ifelse(propdmgexp == "h" | propdmg =="H", propdmg*100,"NotAValue"))))) %>%
             filter(!New_PROPDMGEXP == "NotAValue") %>%
             mutate(New_PROPDMGEXP = as.numeric(New_PROPDMGEXP)) %>%
             group_by(evtype) %>%
             summarise(PROP_DMG_EXP = (sum(New_PROPDMGEXP))/1000000) %>%
             arrange(desc(PROP_DMG_EXP)) %>%
             mutate(pro_Rank = rank(desc(PROP_DMG_EXP))) %>%
             filter(pro_Rank < 11)
           
           Q2_crop <- data %>%
             select(evtype,cropdmg,cropdmgexp) %>%
             mutate(New_CROPDMGEXP = ifelse(cropdmgexp == "B", cropdmg*1000000000,
                                            ifelse(cropdmgexp == "m" | cropdmgexp =="M", cropdmg*1000000,
                                                   ifelse(cropdmgexp == "k" | cropdmgexp =="K", cropdmg*1000,"NotAValue")))) %>%
             filter(!New_CROPDMGEXP == "NotAValue") %>%
             mutate(New_CROPDMGEXP = as.numeric(New_CROPDMGEXP)) %>%
             group_by(evtype) %>%
             summarise(CROP_DMG_EXP = (sum(New_CROPDMGEXP))/1000000) %>%
             arrange(desc(CROP_DMG_EXP)) %>%
             mutate(crop_Rank = rank(desc(CROP_DMG_EXP))) %>%
             filter(crop_Rank < 11)
           
           economics <- Q2_prop %>%
             left_join(Q2_crop, by = "evtype") %>%
             mutate(CROP_DMG_EXP = ifelse(is.na(CROP_DMG_EXP), 0,CROP_DMG_EXP),
                    Total_Eco = CROP_DMG_EXP + PROP_DMG_EXP) %>%
             arrange(desc(Total_Eco)) %>%
             mutate(Rank = rank(desc(Total_Eco)),
                    Property_Rank = rank(desc(PROP_DMG_EXP)),
                    Crop_Rank = rank(desc(CROP_DMG_EXP))) %>%
             select(evtype, PROP_DMG_EXP, CROP_DMG_EXP)
          
           gather_Q2 <- economics %>% gather(Type,Total_Exp,-evtype)
           
           ggplot(gather_Q2, aes(x = reorder(evtype, Total_Exp), ymax = 110000, y = Total_Exp, fill = Type)) +
             geom_bar(stat="identity") +
             theme_bw() +
             theme(axis.text.x = element_text(size = 10)) +
             xlab("Event Type") +
             ylab("Total Damage Expense (Million Dollars)") +
             theme(legend.position = "top") +
             coord_flip() +
             scale_fill_brewer(palette="Set2") +
             ggtitle("Total Damage Expense For Top 10 Weather Events") + 
             theme(plot.title = element_text(lineheight = .8, face = "bold", size = 15)) +
             scale_y_continuous(breaks = pretty_breaks(n = 10))
         }
         )
       
       })     

       
  Re <- reactive({
     data %>%
      select(evtype, fatalities, injuries) %>%
      group_by(evtype) %>%
      summarise(fatalities = sum(fatalities),
                injuries = sum(injuries)) %>%
      mutate(population = fatalities + injuries) %>%
      arrange(desc(population)) %>%
      mutate(total_Rank = rank(desc(population)),
             f_Rank = rank(desc(fatalities)),
             i_Rank = rank(desc(injuries)))
              })     
       
       
  output$Data1 <- renderTable({
    
    
    if(input$Topeventforpopulationhealth == "Top 10") {
      
      Re() %>% filter(total_Rank < 11) %>% select(evtype, population, fatalities, injuries)  
      
      
    } else if(input$Topeventforpopulationhealth == "Top 20") {
      
      Re() %>% filter(total_Rank < 21) %>% select(evtype, population, fatalities, injuries) 
      
    }else {
      
      Re() %>% filter(total_Rank < 51) %>% select(evtype, population, fatalities, injuries)
    }
    
  })
  
  Co <- reactive({
    prop <- data %>%
      select(evtype,propdmg,propdmgexp) %>%
      mutate(New_PROPDMGEXP = ifelse(propdmgexp == "B", propdmg*1000000000,
                              ifelse(propdmgexp == "K", propdmg*1000,
                              ifelse(propdmgexp == "m" | propdmg =="M", propdmg*1000000,
                              ifelse(propdmgexp == "h" | propdmg =="H", propdmg*100,"NotAValue"))))) %>%
      filter(!New_PROPDMGEXP == "NotAValue") %>%
      mutate(New_PROPDMGEXP = as.numeric(New_PROPDMGEXP)) %>%
      group_by(evtype) %>%
      summarise(PROP_DMG_EXP = (sum(New_PROPDMGEXP))/1000000) %>%
      arrange(desc(PROP_DMG_EXP))
    
    crop <- data %>%
      select(evtype,cropdmg,cropdmgexp) %>%
      mutate(New_CROPDMGEXP = ifelse(cropdmgexp == "B", cropdmg*1000000000,
                              ifelse(cropdmgexp == "m" | cropdmgexp =="M", cropdmg*1000000,
                              ifelse(cropdmgexp == "k" | cropdmgexp =="K", cropdmg*1000,"NotAValue")))) %>%
      filter(!New_CROPDMGEXP == "NotAValue") %>%
      mutate(New_CROPDMGEXP = as.numeric(New_CROPDMGEXP)) %>%
      group_by(evtype) %>%
      summarise(CROP_DMG_EXP = (sum(New_CROPDMGEXP))/1000000) %>%
      arrange(desc(CROP_DMG_EXP)) %>%
      mutate(crop_Rank = rank(desc(CROP_DMG_EXP))) 
    
      prop %>%
      left_join(crop, by = "evtype") %>%
      mutate(CROP_DMG_EXP = ifelse(is.na(CROP_DMG_EXP), 0,CROP_DMG_EXP),
             Total_Eco = CROP_DMG_EXP + PROP_DMG_EXP) %>%
      arrange(desc(Total_Eco)) %>%
      mutate(Rank = rank(desc(Total_Eco))) %>%
      select(evtype, Total_Eco, PROP_DMG_EXP, CROP_DMG_EXP, Rank)
  }) 
  
  
  output$Data2 <- renderTable({
    
    if(input$TopeventforEcoloss == "Top 10") {
      
      Co() %>% filter(Rank < 11)  %>% select(-Rank)
    } else if(input$TopeventforEcoloss == "Top 20") {
      
      Co() %>% filter(Rank < 21) %>% select(-Rank)
      
    }else {
      
      Co() %>% filter(Rank < 51) %>% select(-Rank)
    }
    
  })
    
  })
       
       
    
     
     