
#-------------------------------
#Description: This is a shiny web app for Real time water visaulization of 
# Hubbard Brook Watershed sites 3 and 9 - snow, well, and other specific watershed data 
#
#
# Authors: Sam Lausten, Michelle Uchitel, Alison Walters
#
#-------------------------------

#load libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(lubridate)
library(DT) #MU: Helpful for displaying data tables.
library(tidyverse) #MU: I added tidyverse because it has ggplot2 and other good functions 
library(grid)
library(shinythemes)
library(ggplot2)
#reading in WS3 well data
#setwd("/Volumes/GoogleDrive/My Drive/CLASSES/EI Capstone/EI_Capstone_S21")
ws3_upper_wells <- read_csv("water_data_files/Water_table_WS3upper_WS_3Up_wells.dat",
                            skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", X3 ="Batt_Volt", 
                                                    X4= "ptemp_Max" , X5= "WS3_N1_psi",X6="WS3_N1_rawdepth",
                                                    X7="WS3_N1_depth_corr",X8="WS3_N1_corr_depth",
                                                    X9="WS3_N1_welltemp",X10="WS3_N2_psi",
                                                    X11="WS3_N2_rawdepth",X12="WS3_N2_depth_corr",
                                                    X13="WS3_N2_corr_depth",X14="WS3_N2_welltemp",
                                                    X15="WS3_42_4_d2_psi",X16="WS3_42_4_d2_rawdepth",
                                                    X17="WS3_42_4_d2_depth_corr",X18="WS3_42_4_d2_corr_depth",
                                                    X19="WS3_42_4_d2_welltemp"))%>%
    select(TIMESTAMP, WS3_N1_corr_depth, WS3_N2_corr_depth, WS3_42_4_d2_corr_depth)  

ws3_upper_wells <-  ws3_upper_wells %>%
    group_by(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP), hour = hour(TIMESTAMP)) %>% 
    summarise(WS3_N1_corr_depth = mean(WS3_N1_corr_depth),
              WS3_N2_corr_depth = mean(WS3_N2_corr_depth), 
              WS3_42_4_d2_corr_depth = mean(WS3_42_4_d2_corr_depth)) %>%
    ungroup() %>%
    mutate(TIMESTAMP = mdy_h(paste(month, day, year, hour)))%>%
    select(-c(month, day, year, hour))



ws9_upper_wells <- read_csv("water_data_files/Water_table_WS9_WS_9_wells.dat",
                            skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD",
                                                    X3 ="Batt_Volt", X4= "ptemp_Max" , 
                                                    X5= "HB156_psi", X6= "HB156_rawdepth", 
                                                    X7= "HB156_depth_corr", X8= "HB156_corr_depth",
                                                    X9=  "HB156_welltemp" , X10= "HB179s_psi",
                                                    X11= "HB179s_rawdepth", X12=  "HB179s_depth_corr" , 
                                                    X13= "HB179s_corr_depth" , X14= "HB179s_welltemp" ,
                                                    X15= "HB176d_psi" , X16= "HB176d_rawdepth", 
                                                    X17= "HB176d_depth_corr" , X18= "HB176d_corr_depth", 
                                                    X19 = "HB176d_welltemp"))%>%
    select(TIMESTAMP, HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth) 

ws9_upper_wells <-  ws9_upper_wells %>%
    group_by(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP), hour = hour(TIMESTAMP)) %>% 
    summarise(HB156_corr_depth = mean(HB156_corr_depth),
              HB179s_corr_depth = mean(HB179s_corr_depth), 
              HB176d_corr_depth = mean(HB176d_corr_depth)) %>%
    ungroup() %>%
    mutate(TIMESTAMP = mdy_h(paste(month, day, year, hour)))%>%
    select(-c(month, day, year, hour))

#cleaning snow data further
cleanDepth <- function(depth, cutoff1=-15, cutoff2=150, cutoff3=5){
  
  #replace extreme values:
  depth[which(depth < cutoff1 | depth > cutoff2)] <- NA
  
  #remove unreasonable changes in depth:
  depthDiffs <- c(NA, diff(depth))
  depth[which(abs(depthDiffs) > cutoff3)] <- NA
  
  #Replace anything below zero with zero:
  depth[which(depth <0)] <- 0
  
  return(depth)
}

#reading in WS3 Snow 15 mins 

ws3_upper_snowdat15mins <- read_csv("water_data_files/Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
                                    skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", 
                                                            X3 ="Batt_Volt", X4= "ptemp" , 
                                                            X5= "H2O_Content_1", X6= "H2O_Content_2", 
                                                            X7= "Avg_Period_1", X8= "Avg_Period_2", 
                                                            X9=  "RTD(1)" , X10= "RTD(2)", X11= "RTD(3)", 
                                                            X12=  "RTD(4)" , X13= "RTD(5)" , X14= "RTD(6)" , 
                                                            X15= "RTD(7)" , X16= "RTD(8)", X17= "RTD(9)" , 
                                                            X18= "Air_TempC_Avg", X19 = "Depthraw", 
                                                            X20= "Depthscaled"))%>%
    select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) %>% 
    mutate(H2O_Content_1 = replace(H2O_Content_1, which(H2O_Content_1 < 0), NA)) %>%   #AW - change the two H20 contents to NA for the negatives 
    mutate(H2O_Content_2 = replace(H2O_Content_2, which(H2O_Content_2 < 0), NA)) 

#AW - adds a column with the average ignoring the NA 
ws3_upper_snowdat15mins <- ws3_upper_snowdat15mins %>% 
    mutate(VWC_average = rowMeans(ws3_upper_snowdat15mins[,c('H2O_Content_1','H2O_Content_2')],
                                  na.rm = TRUE ))


#reading in WS3 Snow hourly data 

ws3_upper_snowdat_hr <- read_csv("water_data_files/Water_table_WS3upper_WS_3Up_snowdat_hr.dat",
                                 skip = 4, col_names = c(X1 = "TIMESTAMP" , 
                                                         X2 = "RECORD", X3 ="H2O_Content_1_Avg", 
                                                         X4= "H2O_Content_2_Avg", X5= "Avg_Period_1_Avg", 
                                                         X6= "Avg_Period_2_Avg", X7="RTD_Avg(1)", 
                                                         X8= "RTD_Avg(2)", X9=  "RTD_Avg(3)" , 
                                                         X10= "RTD_Avg(4)", X11= "RTD_Avg(5)", 
                                                         X12=  "RTD_Avg(6)" , X13= "RTD_Avg(7)" , 
                                                         X14= "RTD_Avg(8)" , X15= "RTD_Avg(9)" , 
                                                         X16= "Air_TempC_Avg", X17= "Depthraw_Avg" , 
                                                         X18= "Depthscaled_Avg"))%>%
    select(TIMESTAMP, H2O_Content_1_Avg, H2O_Content_2_Avg, Depthscaled_Avg) %>% 
    mutate(H2O_Content_1_Avg = replace(H2O_Content_1_Avg, which(H2O_Content_1_Avg < 0), NA)) %>%   #AW - change the two H20 contents to NA for the negatives 
    mutate(H2O_Content_2_Avg = replace(H2O_Content_2_Avg, which(H2O_Content_2_Avg < 0), NA)) 

#AW - adds a column with the average ignoring the NA 
ws3_upper_snowdat_hr <- ws3_upper_snowdat_hr %>% 
  mutate(VWC_average = rowMeans(ws3_upper_snowdat_hr[,c('H2O_Content_1_Avg','H2O_Content_2_Avg')],
                                na.rm = TRUE ))

#clean:
ws3_upper_snowdat_hr$Depthcleaned <- cleanDepth(depth = ws3_upper_snowdat_hr$Depthscaled_Avg, cutoff1=-15, cutoff2=150, cutoff3=5)


#note that it probably makes sense to start the ws3 snow depth time series after the big gap when things 
#start looking like they are working correctly...


#####################################################################
# handy trick for cleaning multiple columns of data in a df:

#conditionally replace extremely low and extremely high air/snow temps with NAfor all colnames containing "RTD":
ws3_upper_snowdat_hr[,grep("RTD", colnames(ws3_upper_snowdat_hr))] <- lapply(ws3_upper_snowdat_hr[,grep("RTD", colnames(ws3_upper_snowdat_hr))], function(x) replace(x, x > 50, NA))
ws3_upper_snowdat_hr[,grep("RTD", colnames(ws3_upper_snowdat_hr))] <- lapply(ws3_upper_snowdat_hr[,grep("RTD", colnames(ws3_upper_snowdat_hr))], function(x) replace(x, x < -50, NA))

#reading in WS9 Snow 15 mins 
# AW - currently the VWC is either 0 or NA for all entries 

ws9_upper_snowdat15mins <- read_csv("water_data_files/Water_table_WS9_WS_9_snowdat_15min.dat",
                                    skip = 4, col_names = c(X1 = "TIMESTAMP" ,
                                                            X2 = "RECORD", X3 ="Batt_Volt", 
                                                            X4= "ptemp" , X5= "H2O_Content_1", 
                                                            X6= "H2O_Content_2", X7= "Avg_Period_1", 
                                                            X8= "Avg_Period_2a", X9=  "RTD(1)" , 
                                                            X10= "RTD(2)", X11= "RTD(3)", X12=  "RTD(4)" , 
                                                            X13= "RTD(5)" , X14= "RTD(6)" , X15= "RTD(7)" , 
                                                            X16= "RTD(8)", X17= "RTD(9)" , 
                                                            X18= "Air_TempC_Avg", X19 = "Depthraw", 
                                                            X20= "Depthscaled"))%>%
    select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) %>% 
    mutate(H2O_Content_1 = replace(H2O_Content_1, which(H2O_Content_1 < 0), NA)) %>%   #AW - change the two H20 contents to NA for the negatives 
    mutate(H2O_Content_2 = replace(H2O_Content_2, which(H2O_Content_2 < 0), NA)) 

#AW - adds a column with the average ignoring the NA 
ws9_upper_snowdat15mins <- ws9_upper_snowdat15mins %>% 
    mutate(VWC_average = rowMeans(ws9_upper_snowdat15mins[,c('H2O_Content_1','H2O_Content_2')],
                                  na.rm = TRUE ))

#reading in WS9 Snow hourly data 

ws9_upper_snowdat_hr <- read_csv("water_data_files/Water_table_WS9_WS_9_snowdat_hr.dat",
                                 skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", 
                                                         X3 ="H2O_Content_1_Avg", X4= "H2O_Content_2_Avg", 
                                                         X5= "Avg_Period_1_Avg", X6= "Avg_Period_2_Avg", 
                                                         X7="RTD_Avg(1)", X8= "RTD_Avg(2)", 
                                                         X9=  "RTD_Avg(3)" , X10= "RTD_Avg(4)", 
                                                         X11= "RTD_Avg(5)", X12=  "RTD_Avg(6)" , 
                                                         X13= "RTD_Avg(7)" , X14= "RTD_Avg(8)" , 
                                                         X15= "RTD_Avg(9)" , X16= "Air_TempC_Avg", 
                                                         X17= "Depthraw_Avg" , X18= "Depthscaled_Avg")) %>% 
  select(TIMESTAMP, H2O_Content_1_Avg, H2O_Content_2_Avg, Depthscaled_Avg) %>% 
  mutate(H2O_Content_1_Avg = replace(H2O_Content_1_Avg, which(H2O_Content_1_Avg < 0), NA)) %>%   #AW - change the two H20 contents to NA for the negatives 
  mutate(H2O_Content_2_Avg = replace(H2O_Content_2_Avg, which(H2O_Content_2_Avg < 0), NA)) 
    

#AW - adds a column with the average
ws9_upper_snowdat_hr <- ws9_upper_snowdat_hr %>% 
  mutate(VWC_average = rowMeans(ws9_upper_snowdat_hr[,c('H2O_Content_1_Avg','H2O_Content_2_Avg')],
                                na.rm = TRUE ))

#clean:
ws9_upper_snowdat_hr$Depthcleaned <- cleanDepth(depth = ws9_upper_snowdat_hr$Depthscaled_Avg, cutoff1=-15, cutoff2=150, cutoff3=5)


#note that it probably makes sense to start the ws3 snow depth time series after the big gap when things 
#start looking like they are working correctly...


#####################################################################
# handy trick for cleaning multiple columns of data in a df:

#conditionally replace extremely low and extremely high air/snow temps with NAfor all colnames containing "RTD":
ws9_upper_snowdat_hr[,grep("RTD", colnames(ws9_upper_snowdat_hr))] <- lapply(ws9_upper_snowdat_hr[,grep("RTD", colnames(ws9_upper_snowdat_hr))], function(x) replace(x, x > 50, NA))
ws9_upper_snowdat_hr[,grep("RTD", colnames(ws9_upper_snowdat_hr))] <- lapply(ws9_upper_snowdat_hr[,grep("RTD", colnames(ws9_upper_snowdat_hr))], function(x) replace(x, x < -50, NA))


#min_snow<- min(ws9_upper_snowdat_hr$H2O_Content_2_Avg)

#AW - Read in precip & discharge data 

WS3_weir <- read_csv("water_data_files/weir3_Ws_3b.dat", 
                     skip = 4,
                     col_names = c(X1 = "TIMESTAMP", X2 = "Record", X3 = "Batt",
                                   X4 = "Ptemp", X5 = "OptMed", X6 = "OptMax",
                                   X7 = "OptMin", X8 = "Flow_Eq", X9 = "Q",
                                   X10 = "Discharge", X11 = "StreamTemp")) %>% 
  select(TIMESTAMP, Discharge) %>% 
  group_by(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP), hour = hour(TIMESTAMP)) %>% 
  summarise(Discharge = mean(Discharge)) %>%
  ungroup() %>%
  mutate(TIMESTAMP = mdy_h(paste(month, day, year, hour)))%>%
  select(-c(month, day, year, hour), TIMESTAMP, Discharge)


WS9_weir <- read_csv("water_data_files/weir9_Ws_9b.dat", 
                     skip = 4,
                     col_names = c(X1 = "TIMESTAMP", X2 = "Record", X3 = "Batt",
                                   X4 = "Ptemp", X5 = "OptMed", X6 = "OptMax",
                                   X7 = "OptMin", X8 = "Flow_Eq", X9 = "Q",
                                   X10 = "Discharge", X11 = "StreamTemp")) %>% 
  select(TIMESTAMP, Discharge) %>% 
  group_by(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP), hour = hour(TIMESTAMP)) %>% 
  summarise(Discharge = mean(Discharge)) %>%
  ungroup() %>%
  mutate(TIMESTAMP = mdy_h(paste(month, day, year, hour)))%>%
  select(-c(month, day, year, hour), TIMESTAMP, Discharge)

WS9_Precip <- read_csv("water_data_files/rrg19_Rg_19-2019-08-09.dat", 
                       skip = 4, 
                       col_names = c(X1 = "TIMESTAMP", X2 = "Record", X3 = "GageMinV",
                                     X4 = "ActTemp", X5 = "ActDepth", X6 = "ReportPCP",
                                     X7 = "ODPCounts", X8 = "blockedSec", X9 = "Scan10",
                                     X10 = "ActDepthRA")) %>% 
  select(TIMESTAMP, ReportPCP) %>% 
  group_by(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP), hour = hour(TIMESTAMP)) %>% 
  summarise(ReportPCP = mean(ReportPCP)) %>%
  ungroup() %>%
  mutate(TIMESTAMP = mdy_h(paste(month, day, year, hour)))%>%
  select(-c(month, day, year, hour), TIMESTAMP, ReportPCP) %>% 
  mutate(ReportPCP = ReportPCP * 10)

WS3_Precip <- read_csv("water_data_files/wxsta1_Wx_1_rain.dat", 
                       skip = 4, 
                       col_names = c(X1 = "TIMESTAMP", X2 = "Record", X3 = "GageMinV",
                                     X4 = "ActTemp", X5 = "ActDepth", X6 = "ReportPCP",
                                     X7 = "ODPCounts", X8 = "blockedSec", X9 = "Scan10",
                                     X10 = "ActDepthRA")) %>% 
  select(TIMESTAMP, ReportPCP) %>% 
  group_by(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP), hour = hour(TIMESTAMP)) %>% 
  summarise(ReportPCP = mean(ReportPCP)) %>%
  ungroup() %>%
  mutate(TIMESTAMP = mdy_h(paste(month, day, year, hour)))%>%
  select(-c(month, day, year, hour), TIMESTAMP, ReportPCP) %>% 
  mutate(ReportPCP = ReportPCP * 10)


#AW - full join and pivot longer for the WS 3 & 9 precipitation data 
WS_precip <- full_join(WS3_Precip, WS9_Precip, by = "TIMESTAMP") %>% 
  `colnames<-`(c("W3_Precip", "TIMESTAMP", "W9_Precip")) %>% 
  pivot_longer(!TIMESTAMP, names_to = "Watershed", values_to = "Precip")

#AW - full join and pivot longer for the WS 3 & 9 discharge data 
WS_discharge <- full_join(WS3_weir, WS9_weir, by = "TIMESTAMP") %>% 
  `colnames<-`(c("WS3_Discharge", "TIMESTAMP", "W9_Discharge")) %>% 
  pivot_longer(!TIMESTAMP, names_to = "Watershed", values_to = "Discharge")

#AW - not sure if this is beneficial; full join for both watersheds of the precip
# and the discharge data so it is all in one table 
#The times do not line up at all so mostly it just simplifies it by being all in one table 
WS_precip_dis <- full_join(WS_precip, WS_discharge, by = c("TIMESTAMP", "Watershed"))


# Define UI for application
ui <- fluidPage(navbarPage("Hubbard Brook - Water Storage Data App",
                           theme = shinytheme('cosmo'),
                           
                           
                           
                           #define tabs to be used in the app
                           tabPanel('About',
                                    fluidRow(
                                        column(1, tags$h3("Watershed 3", align = "left")), #MU: Watershed 3 Label
                                        column(10, tags$h3("Watershed 9", align = "right"))), #MU: Watershed 9 Label
                                    fluidRow(
                                        column(1, tags$img(src = "WS3map.png", align = "left", width = 340 , height = 230)), #MU: Watershed 3 Map
                                        column(10, tags$img(src = "WS9map.png", align = "right", width = 340 , height = 230))), #MU: Watershed 9 Map
                                    fluidRow(
                                        tags$h4("This app visualizes data from Watershed 3 and 9 of the Hubbard
                                    Brook Experimental Forest through graphs, a map showing where the data was collected,
                   and a table. The data can also be filtered using the various filters found in each tab.")),
                                    fluidRow(
                                        tags$p("Map Credit: Hubbard Brook Experimental Forest"),
                                        tags$p("This application will attempt to:
                    - Visualize Realtime and Past Watershed Data.
                    - Create a user-friendly dashboard that allows for data exploration.
                    - Assist Hubbard Brook Scientists in testing hypothetical data and results.")
                                    )),
                           tabPanel('Watershed 3',
                                    sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     dateInput("startdate", label = "Start Date", val= "2020-12-16"), #MU: Should we make the default start value the first data present in the data we read in?
                                                     dateInput("enddate", label= "End Date", value=Sys.Date(), max=Sys.Date()),
                                                     # selectInput(inputId = "toview", label = "Select dataset to view:", 
                                                     #             choices = unique(ws3_standard$name), 
                                                     #             selected = unique(ws3_standard$name)[1]),
                                                     numericInput("porosSoil","Soil Porosity:",
                                                                  0.1, step = 0.1, min = 0, max = 1),
                                                     numericInput("porosPM","Parent Material Porosity:",
                                                                  0.1, step = 0.1, min = 0, max = 1),
                                                     verbatimTextOutput("valueSoil"),
                                                     verbatimTextOutput("valuePM"),
                                                     fluid = TRUE),
                                        mainPanel(
                                            plotOutput("plot1"),
                                            plotOutput("discharge1"),
                                            plotOutput("precip1")
                                        )
                                    ) 
                           ),
                           
                           tabPanel('Watershed 9',
                                    sidebarLayout(
                                    sidebarPanel(width = 3,
                                                 dateInput("startdate1", label = "Start Date", val= "2020-12-16"), #MU: Should we make the default start value the first data present in the data we read in?
                                                 dateInput("enddate1", label= "End Date", value=Sys.Date(), max=Sys.Date()),
                                                 # selectInput(inputId = "toview", label = "Select dataset to view:",
                                                 #             choices = unique(ws3_standard$name),
                                                 #             selected = unique(ws3_standard$name)[1]),
                                                 numericInput("porosSoil1","Soil Porosity:",
                                                              0.1, step = 0.1, min = 0, max = 1),
                                                 numericInput("porosPM1","Parent Material Porosity:",
                                                              0.1, step = 0.1, min = 0, max = 1),
                                                 verbatimTextOutput("valueSoil1"),
                                                 verbatimTextOutput("valuePM1"),
                                                 fluid = TRUE),
                                      mainPanel(
                                        plotOutput("plot2"),
                                        plotOutput("discharge2"),
                                        plotOutput("precip2")
                                      )
                                  )
                            ),
                          
                           tabPanel('Table View' ,
                                    # selectInput("chooseTable", "Choose a Dataset:",
                                    #             choices = list(WS_3 = ws3_standard(),
                                    #                            WS_9 = ws9_standard())),
                                    DTOutput("table")),
                           
                           
                           tabPanel('Map', leafletOutput("map",width = '100%'))
                           
))                          


# Define server
server <- function(input, output) {
    #----------------
    # read in cleaned watershed data
    # ---------------
    
    #MU: standardized well ws3 data to mm H2O
    standardized_Well_WS3 <-  reactive({
        ws3_upper_wells %>% 
            mutate(standardized_well_1 = ((WS3_N1_corr_depth * 10) * input$porosSoil)) %>% 
            mutate(standardized_well_2 = ((WS3_N2_corr_depth * 10) * input$porosSoil)) %>% 
            mutate(standardized_deep_well = ((WS3_42_4_d2_corr_depth * 10) * input$porosPM)) %>%
            select(TIMESTAMP, standardized_well_1, standardized_well_2, standardized_deep_well)
    })
    #Mu: standardized well ws9 data to mm H2O
    standardized_Well_WS9 <-  reactive({
        ws9_upper_wells %>% 
            mutate(standardized_well_1 = ((HB156_corr_depth * 10) * input$porosSoil1)) %>% 
            mutate(standardized_well_2 = ((HB179s_corr_depth * 10) * input$porosSoil1)) %>% 
            mutate(standardized_deep_well = ((HB176d_corr_depth * 10) * input$porosPM1)) %>%
            select(TIMESTAMP, standardized_well_1, standardized_well_2, standardized_deep_well)
    })
    
    #MU: standardized snow ws3 data to mm H2O
    standardized_SnowHr_WS3 <-  reactive({
        ws3_upper_snowdat_hr %>% 
            mutate(standardized_snow = (VWC_average * (Depthscaled_Avg * 10)))
    })
    
    #MU: standardized snow ws9 data to mm H2O
    standardized_SnowHr_WS9 <- reactive({
        ws9_upper_snowdat_hr %>%
            mutate(standardized_snow = (VWC_average * (Depthscaled_Avg * 10))) 
        
    })
    #creat a var min value, divide by response range (max-min)
    #pseudo 
    ----
      #var for min value
      #var for max val
      #mutate data (val - min) / (max - min)
      #slider suggesting max value, only reactive with snow .... range min = current max, max= roughly 100% 
    #MU: standardized precip data ws3 to mm H2O
    
    
    #MU: joined ws3 data
    ws3_standard <- reactive ({
        full_join(standardized_Well_WS3(), standardized_SnowHr_WS3(), by = "TIMESTAMP") %>% 
            select(TIMESTAMP, standardized_snow, standardized_well_2, standardized_deep_well) %>% 
            pivot_longer(!TIMESTAMP, names_to = "Water", values_to = "mm") %>%
            filter(TIMESTAMP > ymd("2020-12-16"))
    })
    
    ws9_standard <- reactive ({
        full_join(standardized_Well_WS9(), standardized_SnowHr_WS9(), by = "TIMESTAMP") %>% 
            select(TIMESTAMP, standardized_snow, standardized_well_2, standardized_deep_well) %>%
            pivot_longer(!TIMESTAMP, names_to = "Water", values_to = "mm") %>% 
            filter(TIMESTAMP > ymd("2020-12-16"))
    })
    output$table <- DT::renderDataTable({DT::datatable(ws3_standard(),#input$chooseTable, #MU: When we do the calculations we can put them in one dataset and output that.
                                                       class = "display", #MU: this is the style of the table
                                                       caption = 'Table 1: This table shows x.', #MU: adds a caption to the table
                                                       filter = "top")
    })#MU: This places the filter at the top of the table
    #MU: This is a placeholder table for when we finish cleaning the data and can input summarized values
    output$plot1 <- renderPlot({
        ws3_standard() %>%
        #select(TIMESTAMP, standardized_well_2, standardized_deep_well, Depthcleaned) %>% 
            filter(mm > 0 & TIMESTAMP >= input$startdate & TIMESTAMP <= input$enddate) %>% 
            ggplot(aes(x = TIMESTAMP, y = mm, fill=Water))+
            geom_area(alpha=0.8) +
        labs(x = "Time", y = "H20 (mm)") 
        #scale_x_datetime(labels=date_format("%Y-%m-%d"), breaks = date_breaks("week"))
    })
    output$plot2 <- renderPlot({
      ws9_standard() %>%
        filter(mm > 0 & TIMESTAMP >= input$startdate1 & TIMESTAMP <= input$enddate1) %>%
        ggplot(aes(x = TIMESTAMP, y = mm, fill=Water))+
        geom_area(alpha=0.8) +
        labs(x = "Time", y = "H20 (mm)", labels=c("Deep Well", "Snow", "Shalllow Well"))+ 
        scale_fill_brewer()+
        theme_dark()
      
        
      #scale_x_datetime(labels=date_format("%Y-%m-%d"), breaks = date_breaks("week"))
    })
    
    output$discharge1 <- renderPlot({
      WS_discharge %>% 
        filter(Watershed == "WS3_Discharge" & TIMESTAMP >= input$startdate & TIMESTAMP <= input$enddate) %>% 
        ggplot(aes(x = TIMESTAMP, y = Discharge)) +
        geom_line()
      #MU: This is where discharge plot for WS3 goes.
    })
    
    output$discharge2 <- renderPlot({
      WS_discharge %>% 
        filter(Watershed == "W9_Discharge" & TIMESTAMP >= input$startdate1 & TIMESTAMP <= input$enddate1) %>% 
        ggplot(aes(x = TIMESTAMP, y = Discharge)) +
        geom_line()
      #MU: This is where discharge plot for WS9 goes.
    })

    output$precip1 <- renderPlot({
      #MU: This is where precip plot for WS3 goes.
      WS_precip %>% 
        filter(Watershed == "W3_Precip" & TIMESTAMP >= input$startdate & TIMESTAMP <= input$enddate) %>% 
        ggplot(aes(x = TIMESTAMP, y = Precip)) +
        geom_bar(stat = "identity")
    })
    
    output$precip2 <- renderPlot({
      #MU: This is where precip plot for WS9 goes.
      WS_precip %>% 
        filter(Watershed == "W9_Precip" & TIMESTAMP >= input$startdate & TIMESTAMP <= input$enddate) %>% 
        ggplot(aes(x = TIMESTAMP, y = Precip)) +
        geom_bar(stat = "identity")
    })
    
    # output$porosPlot <- renderPlot({
    #     x <- seq(from = 0, to = 100, by = 0.1)
    #     y <- x*input$poros2 + input$change
    #     plot(x,y)
    # })
    
    
    # Plot map of station locations using leaflet
    #---------------------------------------------
    
    m <-leaflet() %>% 
        addProviderTiles("OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>% 
        addMarkers(lng= -71.7185, lat = 43.9403, popup = "Hubbard Brook Experimental Forest")
    
    output$map <- renderLeaflet(
        m
    )
    
    #---------------------------------------------
} # END Server function
#---------------------------------------------
#---------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)
