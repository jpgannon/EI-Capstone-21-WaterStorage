
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
library(rgdal)
library(googledrive)
library(ggplot2)
#reading in WS3 well data
#setwd("/Volumes/GoogleDrive/My Drive/CLASSES/EI Capstone/EI_Capstone_S21")
ws3_upper_wells <- read_csv("Water_table_WS3upper_WS_3Up_wells.dat",
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



ws9_upper_wells <- read_csv("Water_table_WS9_WS_9_wells.dat",
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
    select(TIMESTAMP, HB156_corr_depth, HB179s_corr_depth, HB176d_corr_depth) %>% 
    filter(TIMESTAMP > "2021-01-20")

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

ws3_upper_snowdat15mins <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
                                    skip = 4, col_names = c(X1 = "TIMESTAMP" , X2 = "RECORD", 
                                                            X3 ="Batt_Volt", X4= "ptemp" , 
                                                            X5= "H2O_Content_1", X6= "H2O_Content_2", 
                                                            X7= "Avg_Period_1", X8= "Avg_Period_2", 
                                                            X9=  "RTD(1)" , X10= "RTD(2)", X11= "RTD(3)", 
                                                            X12=  "RTD(4)" , X13= "RTD(5)" , X14= "RTD(6)" , 
                                                            X15= "RTD(7)" , X16= "RTD(8)", X17= "RTD(9)" , 
                                                            X18= "Air_TempC_Avg", X19 = "Depthraw", 
                                                            X20= "Depthscaled"))%>%
    select(TIMESTAMP, H2O_Content_1, H2O_Content_2, Depthscaled) 

#AW - adds a column with the average ignoring the NA 
ws3_upper_snowdat15mins <- ws3_upper_snowdat15mins %>% 
    mutate(VWC_average = rowMeans(ws3_upper_snowdat15mins[,c('H2O_Content_1','H2O_Content_2')],
                                  na.rm = TRUE ))


#reading in WS3 Snow hourly data 

ws3_upper_snowdat_hr <- read_csv("Water_table_WS3upper_WS_3Up_snowdat_hr.dat",
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
    select(TIMESTAMP, H2O_Content_1_Avg, H2O_Content_2_Avg, Depthscaled_Avg) 

#AW - adds a column with the average ignoring the NA 
ws3_upper_snowdat_hr <- ws3_upper_snowdat_hr %>% 
  mutate(VWC_average = rowMeans(ws3_upper_snowdat_hr[,c('H2O_Content_1_Avg','H2O_Content_2_Avg')],
                                na.rm = TRUE ))

#Find the min VWC value for WS 3 -AW 
min_WS3snow<- min(ws3_upper_snowdat_hr$VWC_average, na.rm = TRUE)


#clean:
ws3_upper_snowdat_hr$Depthcleaned <- cleanDepth(depth = ws3_upper_snowdat_hr$Depthscaled_Avg, cutoff1=-15, cutoff2=150, cutoff3=5)


#note that it probably makes sense to start the ws3 snow depth time series after the big gap when things 
#start looking like they are working correctly...
#####################################################################

#conditionally replace extremely low and extremely high air/snow temps with NAfor all colnames containing "RTD":
ws3_upper_snowdat_hr[,grep("RTD", colnames(ws3_upper_snowdat_hr))] <- lapply(ws3_upper_snowdat_hr[,grep("RTD", colnames(ws3_upper_snowdat_hr))], function(x) replace(x, x > 50, NA))
ws3_upper_snowdat_hr[,grep("RTD", colnames(ws3_upper_snowdat_hr))] <- lapply(ws3_upper_snowdat_hr[,grep("RTD", colnames(ws3_upper_snowdat_hr))], function(x) replace(x, x < -50, NA))

#reading in WS9 Snow 15 mins 
# AW - currently the VWC is either 0 or NA for all entries 

ws9_upper_snowdat15mins <- read_csv("Water_table_WS9_WS_9_snowdat_15min.dat",
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
    filter(TIMESTAMP > "2021-01-20")


#AW - adds a column with the average ignoring the NA 
ws9_upper_snowdat15mins <- ws9_upper_snowdat15mins %>% 
    mutate(VWC_average = rowMeans(ws9_upper_snowdat15mins[,c('H2O_Content_1','H2O_Content_2')],
                                  na.rm = TRUE ))

#reading in WS9 Snow hourly data 

ws9_upper_snowdat_hr <- read_csv("Water_table_WS9_WS_9_snowdat_hr.dat",
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
  filter(TIMESTAMP > "2021-01-20")


#AW - adds a column with the average
ws9_upper_snowdat_hr <- ws9_upper_snowdat_hr %>% 
  mutate(VWC_average = rowMeans(ws9_upper_snowdat_hr[,c('H2O_Content_1_Avg','H2O_Content_2_Avg')],
                                na.rm = TRUE ))

#Find the min for VWC value for WS 9 -AW 
min_WS9snow<- min(ws9_upper_snowdat_hr$VWC_average, na.rm = TRUE)

#clean:
ws9_upper_snowdat_hr$Depthcleaned <- cleanDepth(depth = ws9_upper_snowdat_hr$Depthscaled_Avg, cutoff1=-15, cutoff2=150, cutoff3=5)


#note that it probably makes sense to start the ws3 snow depth time series after the big gap when things 
#start looking like they are working correctly...


#####################################################################
# handy trick for cleaning multiple columns of data in a df:

#conditionally replace extremely low and extremely high air/snow temps with NAfor all colnames containing "RTD":
ws9_upper_snowdat_hr[,grep("RTD", colnames(ws9_upper_snowdat_hr))] <- lapply(ws9_upper_snowdat_hr[,grep("RTD", colnames(ws9_upper_snowdat_hr))], function(x) replace(x, x > 50, NA))
ws9_upper_snowdat_hr[,grep("RTD", colnames(ws9_upper_snowdat_hr))] <- lapply(ws9_upper_snowdat_hr[,grep("RTD", colnames(ws9_upper_snowdat_hr))], function(x) replace(x, x < -50, NA))


#AW - Read in precip & discharge data 

WS3_weir <- read_csv("weir3_Ws_3b.dat", 
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


WS9_weir <- read_csv("weir9_Ws_9b.dat", 
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
  select(-c(month, day, year, hour), TIMESTAMP, Discharge) %>% 
  filter(TIMESTAMP > "2021-01-21")

WS9_Precip <- read_csv("rrg19_Rg_19-2019-08-09.dat", 
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
  mutate(ReportPCP = ReportPCP * 10) %>% 
  na_if(0)


WS3_Precip <- read_csv("wxsta1_Wx_1_rain.dat", 
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
  mutate(ReportPCP = ReportPCP * 10) %>% 
  filter(TIMESTAMP > "2021-01-21") %>% 
  na_if(0)


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

#reading in shapefile for map visualization 
ws_shp <- readOGR(dsn = "water_data_files/hbef_wsheds", layer = "hbef_wsheds")

#changing data projection to standard lat/lon
ws_latlon <- spTransform(ws_shp, CRS("+proj=longlat +datum=WGS84"))


#selecting ws areas of study
ws_latlon <- ws_latlon[ws_latlon$WS %in% c("WS3", "WS9"),]

#adding popup information
popup_dat <- paste0("<strong>Watershed: </strong>", 
                    ws_latlon$WS)
# Define UI for application
ui <- fluidPage(navbarPage("Hubbard Brook - Watershed Storage Data App",
                           theme = shinytheme('cosmo'),
                           
                           
                           
                           #define tabs to be used in the app
                           tabPanel('About',
                                    actionButton("dataDL", "Click here to download most recent data"),
                                    fluidRow(
                                      tags$h4("This app visualizes watershed storage data from Watershed 3 and 9 of the Hubbard
                                    Brook Experimental Forest through reactive visualizations, a map showing where the data was collected,
                   and interactive tables. The data can be manipulated using various reactive and interactive elements found in each tab."),
                                      tags$h4(strong('Information about the functionality and parts of this app are below:')),  
                                      strong('Watershed 3'), 
                                      tags$li('Three plots showing total mm H20, discharge, and precipitation. One table displaying the data. All the plots and tables are reactive with the ability to change the date, soil porosity, and volumetric water content (VWC).'),
                                      strong('Watershed 9'),
                                      tags$li('Three plots showing total mm H20, discharge, and precipitation. One table displaying the data. All the plots and tables are reactive with the ability to change the date, soil porosity, and volumetric water content (VWC). (Same format as the Watershed 3 tab)'),
                                      strong('Comparative Watershed Data'),
                                      tags$li('The first plot allows the user to compare any of the wells and snow measurements from either watershed.'),
                                      tags$li('The second plot shows the discharge for both watersheds.'),
                                      tags$li('The third plot shows the precipitation for both watersheds.'),
                                      
                                      strong('Reactive Inputs/User Inputs'),
                                      tags$li('Porosity - Utilized to change the percent of space in between soil particles. This affects how much water is in the soil due to water getting in the pores.'),
                                      tags$li('Maximum Volumetric Water Content (VWC) - Utilized to change the maximum saturation of water in snow. VWC is necessary in order to normalize the amount of water in snow.'),
                                      
                                      strong('Units: All values are in mm of water.')),
                                    fluidRow(
                                        
                                        em("In summary this application will attempt to:"),
                                                tags$li("Visualize Realtime and Past Watershed Storage Data Using Joined and Normalized Datasets."),
                                                tags$li("Create a user-friendly dashboard that allows for data exploration."),
                                                tags$li("Assist Hubbard Brook Scientists in testing hypothetical data and results.")), 
                                    fluidRow(leafletOutput("map",width = '100%'))
                                    ),
                           tabPanel('Watershed 3',
                                    sidebarLayout(
                                        sidebarPanel(width = 3,
                                                     dateInput("startdate", label = "Start Date", val= "2021-01-21"), #MU: Should we make the default start value the first data present in the data we read in?
                                                     dateInput("enddate", label= "End Date", value=Sys.Date(), max=Sys.Date()),
                                                     # selectInput(inputId = "toview", label = "Select dataset to view:", 
                                                     #             choices = unique(ws3_standard$name), 
                                                     #             selected = unique(ws3_standard$name)[1]),
                                                     numericInput("porosSoil","Soil Porosity:",
                                                                  0.5, step = 0.1, min = 0, max = 1),
                                                     numericInput("porosPM","Parent Material Porosity:",
                                                                  0.4, step = 0.1, min = 0, max = 1),
                                                     numericInput("maxVWC","Maximum VWC:",
                                                                  0.01, step = 0.1, min = 0.001, max = 1),
                                                     dateInput("VertDate", label = "Show vertical line at:", val= "2021-01-21"),
                                                     verbatimTextOutput("valueSoil"),
                                                     verbatimTextOutput("valuePM"),
                                                     verbatimTextOutput("maxVWC"),
                                                     verbatimTextOutput("VertDate"),
                                                     fluid = TRUE),
                                        mainPanel(
                                          fluidRow(
                                            plotOutput("plot1",
                                             dblclick = "plot_dblclick",
                                             brush = brushOpts(
                                               id = "plot_brush",
                                               resetOnNew = TRUE))
                                            ),
                                          fluidRow(
                                            plotOutput("discharge1",
                                                       dblclick = "plot_dblclick",
                                                       brush = brushOpts(
                                                         id = "plot_brush",
                                                         resetOnNew = TRUE))),
                                          fluidRow(
                                            plotOutput("precip1")),
                                          fluidRow(
                                            DTOutput("table1"))
                                        )
                                    )
                           ),
                           
                           tabPanel('Watershed 9',
                                    sidebarLayout(
                                    sidebarPanel(width = 3,
                                                 dateInput("startdate1", label = "Start Date", val= "2021-01-21"), #MU: Should we make the default start value the first data present in the data we read in?
                                                 dateInput("enddate1", label= "End Date", value=Sys.Date(), max=Sys.Date()),
                                                 # selectInput(inputId = "toview", label = "Select dataset to view:",
                                                 #             choices = unique(ws3_standard$name),
                                                 #             selected = unique(ws3_standard$name)[1]),
                                                 numericInput("porosSoil1","Soil Porosity:",
                                                              0.5, step = 0.1, min = 0, max = 1),
                                                 numericInput("porosPM1","Parent Material Porosity:",
                                                              0.4, step = 0.1, min = 0, max = 1),
                                                 numericInput("maxVWC1","Maximum VWC:",
                                                              0.01, step = 0.1, min = 0.001, max = 1),
                                                 dateInput("VertDate1", label = "Show vertical line at:", val= "2021-01-21"),
                                                 verbatimTextOutput("valueSoil1"),
                                                 verbatimTextOutput("valuePM1"),
                                                 verbatimTextOutput("maxVWC1"),
                                                 verbatimTextOutput("VertDate1"),
                                                 fluid = TRUE),
                                      mainPanel(
                                        fluidRow(
                                          plotOutput("plot2",
                                                     dblclick = "plot_dblclick",
                                                     brush = brushOpts(
                                                       id = "plot_brush",
                                                       resetOnNew = TRUE))
                                          ),
                                        fluidRow(
                                          plotOutput("discharge2",
                                                     dblclick = "plot_dblclick",
                                                     brush = brushOpts(
                                                       id = "plot_brush",
                                                       resetOnNew = TRUE))),
                                        fluidRow(
                                          plotOutput("precip2")),
                                        fluidRow(
                                          DTOutput("table2"))
                                      )
                                  )
                            ),
                          
                           tabPanel('Comparative Watershed Data', 
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                                   dateInput("startdate2", label = "Start Date", val= "2021-01-21"), #MU: Should we make the default start value the first data present in the data we read in?
                                                   dateInput("enddate2", label= "End Date", value=Sys.Date(), max=Sys.Date()),
                                                   selectInput("variables", "Select Data to Plot:",
                                                                  c("ws3_snow", "ws3_shallow_well", "ws3_deep_well", "ws9_snow", 
                                                                    "ws9_shallow_well", "ws9_deep_well"), 
                                                               selected = c("ws3_snow", "ws9_snow"),
                                                               multiple = TRUE),
                                                   #filters for WS 3 
                                                   numericInput("porosSoil_WS3","WS3 Soil Porosity:",
                                                                0.5, step = 0.1, min = 0, max = 1),
                                                   numericInput("porosSoil_WS9","WS9 Soil Porosity:",
                                                                0.5, step = 0.1, min = 0, max = 1),
                                                   numericInput("porosPM_WS3","WS3 Parent Material Porosity:",
                                                                0.4, step = 0.1, min = 0, max = 1),
                                                   numericInput("porosPM_WS9","WS9 Parent Material Porosity:",
                                                                0.4, step = 0.1, min = 0, max = 1),
                                                   numericInput("maxVWC_WS3","WS3 Maximum VWC:",
                                                                0.01, step = 0.1, min = 0.001, max = 1),
                                                   numericInput("maxVWC_WS9","WS9 Maximum VWC:",
                                                                0.01, step = 0.1, min = 0.001, max = 1),
                                                   dateInput("VertDate2", label = "Show vertical line at:", val= "2021-01-21"),
                                                   verbatimTextOutput("valueSoil_WS3"),
                                                   verbatimTextOutput("valueSoil_WS9"),
                                                   verbatimTextOutput("valuePM_WS3"),
                                                   verbatimTextOutput("valuePM_WS9"),
                                                   verbatimTextOutput("maxVWC_WS3"),
                                                   verbatimTextOutput("maxVWC_WS9"),
                                                   verbatimTextOutput("VertDate2"),
                                                   fluid = TRUE),
                                      mainPanel(
                                      #line plots for comparing the two watersheds 
                                        fluidRow(
                                          plotOutput("compare",
                                                     dblclick = "plot_dblclick",
                                                     brush = brushOpts(
                                                       id = "plot_brush",
                                                       resetOnNew = TRUE))),
                                        fluidRow(
                                          plotOutput("dis_compare",
                                                     dblclick = "plot_dblclick",
                                                     brush = brushOpts(
                                                       id = "plot_brush",
                                                       resetOnNew = TRUE))),
                                        fluidRow(
                                          plotOutput("precip_compare"))
                                      )
                                    ))
))                          

# Define server
server <- function(input, output) {
    #----------------
    # read in cleaned watershed data
    # ---------------
    
  #MU: Date range to filter brushing
  ranges <- reactiveValues(x = ymd(c(start = "2021-01-21",
                                    end = toString(Sys.Date()))))
  #MU: Date range to filter brushing
  ranges2 <- reactiveValues(x = ymd(c(start = "2021-01-21",
                                     end = toString(Sys.Date()))))
  #MU: Date range to filter brushing
  ranges3 <- reactiveValues(x = ymd(c(start = "2021-01-21",
                                     end = toString(Sys.Date()))))
  
    
    #MU: standardized well ws3 data to mm H2O
    standardized_Well_WS3 <-  reactive({
        ws3_upper_wells %>% 
            mutate(standardized_well_1 = ((WS3_N1_corr_depth * 10) * input$porosSoil)) %>% 
            mutate(standardized_well_2 = ((WS3_N2_corr_depth * 10) * input$porosSoil)) %>% 
            mutate(standardized_deep_well = ((WS3_42_4_d2_corr_depth * 10) * input$porosPM)) %>%
            select(TIMESTAMP, standardized_well_2, standardized_deep_well)
    })
    #Mu: standardized well ws9 data to mm H2O
    standardized_Well_WS9 <-  reactive({
        ws9_upper_wells %>% 
            mutate(standardized_well_1 = ((HB156_corr_depth * 10) * input$porosSoil1)) %>% 
            mutate(standardized_well_2 = ((HB179s_corr_depth * 10) * input$porosSoil1)) %>% 
            mutate(standardized_deep_well = ((HB176d_corr_depth * 10) * input$porosPM1)) %>%
            select(TIMESTAMP, standardized_well_2, standardized_deep_well)
    })
    
    
    #MU: standardized snow ws3 data to mm H2O
    standardized_SnowHr_WS3 <-  reactive({
        ws3_upper_snowdat_hr %>% 
            mutate(VWC_average = ((VWC_average - min_WS3snow) / (input$maxVWC - min_WS3snow ))) %>% 
            mutate(standardized_snow = (VWC_average * (Depthscaled_Avg * 10))) %>% 
            select(TIMESTAMP, standardized_snow)
    })
    
    #MU: standardized snow ws9 data to mm H2O
    standardized_SnowHr_WS9 <- reactive({
        ws9_upper_snowdat_hr %>%
            mutate(VWC_average = ((VWC_average - min_WS9snow) / (input$maxVWC1 - min_WS9snow ))) %>% 
            mutate(standardized_snow = (VWC_average * (Depthscaled_Avg * 10))) %>% 
            select(TIMESTAMP, standardized_snow)
        
    })
    
    #MU: joined ws3 data
    ws3_standard <- reactive ({
        full_join(standardized_Well_WS3(), standardized_SnowHr_WS3(), by = "TIMESTAMP") %>% 
            select(TIMESTAMP, standardized_snow, standardized_well_2, standardized_deep_well) %>% 
            `colnames<-`(c("TIMESTAMP", "ws3_snow", "ws3_shallow_well", "ws3_deep_well")) %>% 
            pivot_longer(!TIMESTAMP, names_to = "Water", values_to = "mm") %>%
            filter(TIMESTAMP >= ranges$x[1] & TIMESTAMP <= ranges$x[2])
    })
    
    ws9_standard <- reactive ({
        full_join(standardized_Well_WS9(), standardized_SnowHr_WS9(), by = "TIMESTAMP") %>% 
            select(TIMESTAMP, standardized_snow, standardized_well_2, standardized_deep_well) %>%
            `colnames<-`(c("TIMESTAMP", "ws9_snow", "ws9_shallow_well", "ws9_deep_well")) %>% 
            pivot_longer(!TIMESTAMP, names_to = "Water", values_to = "mm") %>% 
            filter(TIMESTAMP >= ranges2$x[1] & TIMESTAMP <= ranges2$x[2])
    })
    
    standard_full <- reactive ({
      full_join(ws3_standard(), ws9_standard(), by = c("TIMESTAMP", "Water", "mm")) %>%
        #select(TIMESTAMP, Water.x, mm.x) %>% 
        `colnames<-`(c("TIMESTAMP", "Water", "mm")) %>% 
        filter(TIMESTAMP >= ranges3$x[1] & TIMESTAMP <= ranges3$x[2])
       })
    
    #Combines data to display on WS 3 page
    ws3_full <- reactive({
      full_join(standardized_Well_WS3(), standardized_SnowHr_WS3(), by = "TIMESTAMP") %>%
        full_join(., WS3_Precip, by = "TIMESTAMP") %>% 
        full_join(., WS3_weir, by = "TIMESTAMP") %>% 
        select(TIMESTAMP, standardized_snow, standardized_well_2, standardized_deep_well, ReportPCP, Discharge)
    })
    
    #MU: Combines data to display on WS 9 page
    ws9_full <- reactive({
      full_join(standardized_Well_WS9(), standardized_SnowHr_WS9(), by = "TIMESTAMP") %>%
        full_join(., WS9_Precip, by = "TIMESTAMP") %>% 
        full_join(., WS9_weir, by = "TIMESTAMP") %>% 
        select(TIMESTAMP, standardized_snow, standardized_well_2, standardized_deep_well, ReportPCP, Discharge)
    })
    
    output$table1 <- DT::renderDataTable({DT::datatable(ws3_full(),#input$chooseTable, #MU: When we do the calculations we can put them in one dataset and output that.
                                                       class = "display", #MU: this is the style of the table
                                                       caption = 'Table 1: This table shows precipitation, snow, discharge, and groundwater data for Watershed 3.', #MU: adds a caption to the table
                                                       filter = "top")
    })#MU: This places the filter at the top of the table
    
    output$table2 <- DT::renderDataTable({DT::datatable(ws9_full(),#input$chooseTable, #MU: When we do the calculations we can put them in one dataset and output that.
                                                        class = "display", #MU: this is the style of the table
                                                        caption = 'Table 2: This table shows precipitation, snow, discharge, and groundwater data for Watershed 9.', #MU: adds a caption to the table
                                                        filter = "top")
    })#MU: This places the filter at the top of the table
    
    output$plot1 <- renderPlot({
        ws3_standard() %>%
        #select(TIMESTAMP, standardized_well_2, standardized_deep_well, Depthcleaned) %>% 
            filter(mm > 0 & TIMESTAMP >= ranges$x[1] & TIMESTAMP <= ranges$x[2]) %>% 
            ggplot(mapping = aes(x = TIMESTAMP, y = mm, fill=Water))+
            #geom_vline(xintercept=as.numeric(as.POSIXct(input$vertDate[120])), linetype=4)+
            geom_area()+
            geom_vline(xintercept = (as.POSIXct(input$VertDate)), color = "red", linetype=4)+
            labs(x = "Time", y = "Storage (mm)", labels=c("Deep Well", "Snow", "Shalllow Well"))+
            scale_fill_brewer()+
            theme_dark()+ 
            theme(legend.position="bottom")+
            theme(axis.title.x = element_blank())+
            theme(text = element_text(size=16))
        })
    
    output$plot2 <- renderPlot({
      ws9_standard() %>%
        filter(mm > 0 & TIMESTAMP >= ranges2$x[1] & TIMESTAMP <= ranges2$x[2]) %>%
        ggplot(aes(x = TIMESTAMP, y = mm, fill=Water))+
        geom_area() +
        geom_vline(xintercept = (as.POSIXct(input$VertDate1)), color = "red", linetype=4)+
        labs(x = "Time", y = "Storage (mm)", labels=c("Deep Well", "Snow", "Shalllow Well"))+
        scale_fill_brewer()+
        theme_dark()+
        theme(legend.position="bottom")+
        theme(axis.title.x = element_blank()) +
        theme(text = element_text(size=16))
    })
    
    output$discharge1 <- renderPlot({
      WS_discharge %>% 
        filter(Watershed == "WS3_Discharge" & TIMESTAMP >= ranges$x[1] & TIMESTAMP <= ranges$x[2]) %>% 
        ggplot(aes(x = TIMESTAMP, y = Discharge)) +
        geom_line(size = 1)+
        geom_vline(xintercept = (as.POSIXct(input$VertDate)), color = "red", linetype=4)+
        labs(y="Discharge (mm)")+
        scale_fill_brewer()+
        theme_dark()+
        theme(axis.title.x = element_blank())+
        theme(text = element_text(size=16))
      
      #MU: This is where discharge plot for WS3 goes.
    })
    
    output$discharge2 <- renderPlot({
      WS_discharge %>% 
        filter(Watershed == "W9_Discharge" & TIMESTAMP >= ranges2$x[1] & TIMESTAMP <= ranges2$x[2]) %>% 
        ggplot(aes(x = TIMESTAMP, y = Discharge)) +
        geom_line(size = 1)+
        geom_vline(xintercept = (as.POSIXct(input$VertDate1)), color = "red", linetype=4)+
        labs(y="Discharge (mm)")+
        scale_fill_brewer()+
        theme_dark()+ 
        theme(axis.title.x = element_blank())+
        theme(text = element_text(size=16))
      
      #MU: This is where discharge plot for WS9 goes.
    })

    output$precip1 <- renderPlot({
      #MU: This is where precip plot for WS3 goes.
      WS_precip %>% 
        filter(Watershed == "W3_Precip" & TIMESTAMP >= ranges$x[1] & TIMESTAMP <= ranges$x[2]) %>% 
        ggplot(aes(x = TIMESTAMP, y = Precip)) +
        geom_bar(stat = "identity", fill="skyblue3")+
        geom_vline(xintercept = (as.POSIXct(input$VertDate)), color = "red", linetype=4)+
        labs(y="Precipitation (mm)")+
        ylim(0, 0.6)+
        theme_dark()+
        theme(axis.title.x = element_blank())+
        theme(text = element_text(size=16))
      
    })
    
    output$precip2 <- renderPlot({
      #MU: This is where precip plot for WS9 goes.
      WS_precip %>% 
        filter(Watershed == "W9_Precip" & TIMESTAMP >= ranges2$x[1] & TIMESTAMP <= ranges2$x[2]) %>% 
        ggplot(aes(x = TIMESTAMP, y = Precip)) +
        geom_bar(stat = "identity", fill="skyblue3")+
        geom_vline(xintercept = (as.POSIXct(input$VertDate1)), color = "red", linetype=4)+
        scale_fill_brewer()+
        labs(y="Precipitation (mm)")+
        theme_dark()+
        theme(axis.title.x = element_blank())+
        theme(text = element_text(size=16))
      
    })
    
    #MU: Comparative plot
    output$compare <- renderPlot ({
      compare_full() %>%
        filter(Water %in% input$variables & TIMESTAMP >= ranges3$x[1] & TIMESTAMP <= ranges3$x[2]) %>%
        ggplot(aes(x = TIMESTAMP, y = mm, group = Water)) +
        geom_line(aes(color=Water), size = 1)+
        geom_vline(xintercept = (as.POSIXct(input$VertDate2)), color = "red", linetype=4)+
        scale_fill_brewer()+
        labs(y="Storage (mm)")+
        theme_dark()+
        theme(axis.title.x = element_blank())+
        theme(legend.position="bottom")+
        theme(text = element_text(size=16))
    })
    
    
    output$precip_compare <- renderPlot ({
      WS_precip %>%
      filter(TIMESTAMP >= ranges3$x[1] & TIMESTAMP <= ranges3$x[2]) %>% 
        ggplot(aes(x = TIMESTAMP, y = Precip, group = Watershed)) +
        geom_bar(stat = "identity", aes(color=Watershed))+
        geom_vline(xintercept = (as.POSIXct(input$VertDate2)), color = "red", linetype=4)+
        scale_fill_brewer()+
        labs(y="Precipitation (mm)")+
        theme_dark()+
        theme(axis.title.x = element_blank())+
        theme(legend.position="bottom")+
        theme(text = element_text(size=16))
    }) 
    
    output$dis_compare <- renderPlot({
      WS_discharge %>% 
        filter(TIMESTAMP >= ranges3$x[1] & TIMESTAMP <= ranges3$x[2]) %>% 
        ggplot(aes(x = TIMESTAMP, y = Discharge, group = Watershed)) +
        geom_line(aes(color=Watershed), size = 1)+
        geom_vline(xintercept = (as.POSIXct(input$VertDate2)), color = "red", linetype=4)+
        labs(y="Discharge (mm)")+
        scale_fill_brewer()+
        theme_dark()+ 
        theme(axis.title.x = element_blank())+
        theme(legend.position="bottom")+
        theme(text = element_text(size=16))
    })
    
    # Brushing -----------------------------------
    
    # On double-click event it checks for a brush
    # Yes, display data and zoom
    observeEvent(input$plot_dblclick, {
      brush <- input$plot_brush
      
      
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges2$x <- c(brush$xmin, brush$xmax)
        ranges3$x <- c(brush$xmin, brush$xmax)
      } 
      
      
      else {
        ranges$x <- c(input$startdate, input$enddate)
        ranges2$x <- c(input$startdate1, input$enddate1)
        ranges3$x <- c(input$startdate2, input$enddate2)
      }
    }
    )
    
    # Plot map of station locations using leaflet
    #---------------------------------------------
    
    m <-leaflet() %>% 
        addProviderTiles("OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>% 
        addPolygons(data = ws_latlon, fill = TRUE, stroke = TRUE, color = "#03F", opacity = 0.5, popup = popup_dat) %>% 
        addLegend("bottomright", colors = "#03F", labels = "Wastershed Areas 
                  of Study and Analysis (click to identify)")
    
    output$map <- renderLeaflet(m)
    
    observeEvent(input$dataDL, {
      drive_deauth()
      drive_download(as_id("1yhfJ7zJdumI0cMxQ7i7Zzmebo5pdEV_E"), overwrite = TRUE)
      drive_download(as_id("1wsvu3CXHj81n81eS4wbLE1dj3kpSrOqH"), overwrite = TRUE)
      drive_download(as_id("175Ai1DzFq13ut2J1JD43HExgh7n15HVS"), overwrite = TRUE)
      drive_download(as_id("1eq62MIa6n0tpeLTG-4R64q8iuhOHDDAi"), overwrite = TRUE)
      drive_download(as_id("1g3iBLteoLn_ipqhbY25UahJd--bWlwHf"), overwrite = TRUE)
      drive_download(as_id("1Ckb2L41xRAopHM3y4nnv8JmFEY6JN599"), overwrite = TRUE)
      drive_download(as_id("12CQ5lF-dU9B950eaEOYWp27slikcyNS0"), overwrite = TRUE)
      drive_download(as_id("12CVHTfrD9Qef9GB_CTn0qX-EExo-HgNw"), overwrite = TRUE)
      drive_download(as_id("12C1vsIsA7Fs6pN-EbpF20Z0Lm5YsOsiN"), overwrite = TRUE)
      drive_download(as_id("12DGM7SBNwnXPL9K8I8dYo8huvVjBfifQ"), overwrite = TRUE)
      
    })
#------------------------------------------------------------------------------------------
#--------IDENTICAL CODE TO SOME OF ABOVE CODE USED FOR NEW COMPARATIVE PLOT FILTERS
    compare_Well_WS3 <-  reactive({
      ws3_upper_wells %>% 
        mutate(standardized_well_1 = ((WS3_N1_corr_depth * 10) * input$porosSoil_WS3)) %>% 
        mutate(standardized_well_2 = ((WS3_N2_corr_depth * 10) * input$porosSoil_WS3)) %>% 
        mutate(standardized_deep_well = ((WS3_42_4_d2_corr_depth * 10) * input$porosPM_WS3)) %>%
        select(TIMESTAMP, standardized_well_2, standardized_deep_well)
    })

    compare_Well_WS9 <-  reactive({
      ws9_upper_wells %>% 
        mutate(standardized_well_1 = ((HB156_corr_depth * 10) * input$porosSoil_WS9)) %>% 
        mutate(standardized_well_2 = ((HB179s_corr_depth * 10) * input$porosSoil_WS9)) %>% 
        mutate(standardized_deep_well = ((HB176d_corr_depth * 10) * input$porosPM_WS9)) %>%
        select(TIMESTAMP, standardized_well_2, standardized_deep_well)
    })
    
    compare_SnowHr_WS3 <-  reactive({
      ws3_upper_snowdat_hr %>% 
        mutate(VWC_average = ((VWC_average - min_WS3snow) / (input$maxVWC_WS3 - min_WS3snow ))) %>% 
        mutate(standardized_snow = (VWC_average * (Depthscaled_Avg * 10))) %>% 
        select(TIMESTAMP, standardized_snow)
    })
    
    compare_SnowHr_WS9 <- reactive({
      ws9_upper_snowdat_hr %>%
        mutate(VWC_average = ((VWC_average - min_WS9snow) / (input$maxVWC_WS9 - min_WS9snow ))) %>% 
        mutate(standardized_snow = (VWC_average * (Depthscaled_Avg * 10))) %>% 
        select(TIMESTAMP, standardized_snow)
      
    })

    ws3_compare <- reactive ({
      full_join(compare_Well_WS3(), compare_SnowHr_WS3(), by = "TIMESTAMP") %>% 
        select(TIMESTAMP, standardized_snow, standardized_well_2, standardized_deep_well) %>% 
        `colnames<-`(c("TIMESTAMP", "ws3_snow", "ws3_shallow_well", "ws3_deep_well")) %>% 
        pivot_longer(!TIMESTAMP, names_to = "Water", values_to = "mm") %>%
        filter(TIMESTAMP >= ranges$x[1] & TIMESTAMP <= ranges$x[2])
    }) 
    
    ws9_compare <- reactive ({
      full_join(compare_Well_WS9(), compare_SnowHr_WS9(), by = "TIMESTAMP") %>% 
        select(TIMESTAMP, standardized_snow, standardized_well_2, standardized_deep_well) %>%
        `colnames<-`(c("TIMESTAMP", "ws9_snow", "ws9_shallow_well", "ws9_deep_well")) %>% 
        pivot_longer(!TIMESTAMP, names_to = "Water", values_to = "mm") %>% 
        filter(TIMESTAMP >= ranges$x[1] & TIMESTAMP <= ranges$x[2])
      
    })
    
    compare_full <- reactive ({
      full_join(ws3_compare(), ws9_compare(), by = c("TIMESTAMP", "Water", "mm")) %>%
        #select(TIMESTAMP, Water.x, mm.x) %>% 
        `colnames<-`(c("TIMESTAMP", "Water", "mm")) %>% 
        filter(TIMESTAMP >= ranges$x[1] & TIMESTAMP <= ranges$x[2])
      #`colnames<-`(c("TIMESTAMP", "ws3_snow", "ws3_shallow_well", "ws3_deep_well", "ws9_snow", "ws9_shallow_well", "ws9_deep_well")) #%>% 
      # pivot_longer(!TIMESTAMP, names_to = "Water", values_to = "mm") %>% 
      # filter(TIMESTAMP > ymd("2020-12-16"))
    })
    
#-------END OF SECTION WITH IDENTICAL CODE -----------------------------------------------
#------------------------------------------------------------------------------------------    
    
#------------------------------------------------------------------------------------------   
} # END Server function

# Run the application 
shinyApp(ui = ui, server = server)
