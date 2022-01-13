library(rsconnect)
library(shiny)
library(ggplot2)
library(sp)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(rgdal)
library(dplyr)
library(maptools)
library(tidyverse)
library(shinyWidgets)
library(kableExtra)
library(knitr)

file <- read_csv("CovData.csv") 

file <- as.data.frame(file)

file[is.na(file)] <- 0.01

file$TOTAL_MALE <- as.numeric(file$TOTAL_MALE)
file$TOTAL_FEMALE <- as.numeric(file$TOTAL_FEMALE)
file$TOTAL_MALE_12_17 <- as.numeric(file$TOTAL_MALE_12_17)
file$TOTAL_FEMALE_12_17 <- as.numeric(file$TOTAL_FEMALE_12_17)
file$FULL_VAC_MALE <- as.numeric(file$FULL_VAC_MALE)
file$FULL_VAC_MALE_YD <- as.numeric(file$FULL_VAC_MALE_YD)
file$FULL_VAC_FEMALE <- as.numeric(file$FULL_VAC_FEMALE)
file$FULL_VAC_FEMALE_YD <- as.numeric(file$FULL_VAC_FEMALE_YD)
file$PART_VAC_MALE <- as.numeric(file$PART_VAC_MALE)
file$PART_VAC_MALE_YD <- as.numeric(file$PART_VAC_MALE_YD)
file$PART_VAC_FEMALE <- as.numeric(file$PART_VAC_FEMALE)
file$PART_VAC_FEMALE_YD <- as.numeric(file$PART_VAC_FEMALE_YD)

file18 <- read_csv("CovData18.csv") 

file18 <- as.data.frame(file18)

file18[is.na(file18)] <- 0.01

file18$TOTAL_MALE <- as.numeric(file18$TOTAL_MALE)
file18$TOTAL_FEMALE <- as.numeric(file18$TOTAL_FEMALE)
file18$FULL_VAC_MALE <- as.numeric(file18$FULL_VAC_MALE)
file18$FULL_VAC_MALE_YD <- as.numeric(file18$FULL_VAC_MALE_YD)
file18$FULL_VAC_FEMALE <- as.numeric(file18$FULL_VAC_FEMALE)
file18$FULL_VAC_FEMALE_YD <- as.numeric(file18$FULL_VAC_FEMALE_YD)
file18$PART_VAC_MALE <- as.numeric(file18$PART_VAC_MALE)
file18$PART_VAC_MALE_YD <- as.numeric(file18$PART_VAC_MALE_YD)
file18$PART_VAC_FEMALE <- as.numeric(file18$PART_VAC_FEMALE)
file18$PART_VAC_FEMALE_YD <- as.numeric(file18$PART_VAC_FEMALE_YD)

#MAP FILES

district.df <- aggregate(list(TOTAL_MALE=file$TOTAL_MALE,
                              TOTAL_FEMALE=file$TOTAL_FEMALE,
                              TOTAL_MALE_12_17=file$TOTAL_MALE_12_17,
                              TOTAL_FEMALE_12_17=file$TOTAL_FEMALE_12_17,
                              FULL_VAC_MALE=file$FULL_VAC_MALE,
                              FULL_VAC_FEMALE=file$FULL_VAC_FEMALE,
                              FULL_VAC_MALE_YD=file$FULL_VAC_MALE_YD,
                              FULL_VAC_FEMALE_YD=file$FULL_VAC_FEMALE_YD,
                              PART_VAC_MALE=file$PART_VAC_MALE,
                              PART_VAC_FEMALE=file$PART_VAC_FEMALE,
                              PART_VAC_MALE_YD=file$PART_VAC_MALE_YD,
                              PART_VAC_FEMALE_YD=file$PART_VAC_FEMALE_YD), by = list(file$DISTRICT), sum)

district.df$SUM_TOTAL_BOTH <- rowSums(cbind(district.df$TOTAL_MALE,district.df$TOTAL_FEMALE),na.rm=TRUE)

district.df$SUM_VAC_BOTH <- rowSums(cbind(district.df$FULL_VAC_MALE,district.df$FULL_VAC_FEMALE),na.rm=TRUE)

district.df$TOTAL_COVERAGE <- round(((district.df$SUM_VAC_BOTH/district.df$SUM_TOTAL_BOTH) * 100), digits = 2)


sindhmap <- readOGR(dsn="mapfile", layer="NSSF", stringsAsFactors = TRUE)
sindhmap_shp <- sindhmap[sindhmap$ADM1_EN=="Sindh",]

sindhmap_shp$ADM2_EN <- district.df$Group.1

sindhmap_shp$ADM2_EN[1] <- district.df$Group.1[1]
sindhmap_shp$ADM2_EN[2] <- district.df$Group.1[2]
sindhmap_shp$ADM2_EN[3] <- district.df$Group.1[3]
sindhmap_shp$ADM2_EN[4] <- district.df$Group.1[4]
sindhmap_shp$ADM2_EN[5] <- district.df$Group.1[5]
sindhmap_shp$ADM2_EN[6] <- district.df$Group.1[6]
sindhmap_shp$ADM2_EN[7] <- district.df$Group.1[12]
sindhmap_shp$ADM2_EN[8] <- district.df$Group.1[14]
sindhmap_shp$ADM2_EN[9] <- district.df$Group.1[16]
sindhmap_shp$ADM2_EN[10] <- district.df$Group.1[18]
sindhmap_shp$ADM2_EN[11] <- district.df$Group.1[19]
sindhmap_shp$ADM2_EN[12] <- district.df$Group.1[20]
sindhmap_shp$ADM2_EN[13] <- district.df$Group.1[7]
sindhmap_shp$ADM2_EN[14] <- district.df$Group.1[22]
sindhmap_shp$ADM2_EN[15] <- district.df$Group.1[23]
sindhmap_shp$ADM2_EN[16] <- district.df$Group.1[24]
sindhmap_shp$ADM2_EN[17] <- district.df$Group.1[21]
sindhmap_shp$ADM2_EN[18] <- district.df$Group.1[25]
sindhmap_shp$ADM2_EN[19] <- district.df$Group.1[26]
sindhmap_shp$ADM2_EN[20] <- district.df$Group.1[27]
sindhmap_shp$ADM2_EN[21] <- district.df$Group.1[28]
sindhmap_shp$ADM2_EN[22] <- district.df$Group.1[29]
sindhmap_shp$ADM2_EN[23] <- district.df$Group.1[30]
sindhmap_shp$ADM2_EN[24] <- district.df$Group.1[11]
sindhmap_shp$ADM2_EN[25] <- district.df$Group.1[13]
sindhmap_shp$ADM2_EN[26] <- district.df$Group.1[17]
sindhmap_shp$ADM2_EN[27] <- district.df$Group.1[15]
sindhmap_shp$ADM2_EN[28] <- district.df$Group.1[10]
sindhmap_shp$ADM2_EN[29] <- district.df$Group.1[8]
sindhmap_shp$ADM2_EN[30] <- district.df$Group.1[9]


district18.df <- aggregate(list(TOTAL_MALE=file18$TOTAL_MALE,
                                TOTAL_FEMALE=file18$TOTAL_FEMALE,
                                FULL_VAC_MALE=file18$FULL_VAC_MALE,
                                FULL_VAC_FEMALE=file18$FULL_VAC_FEMALE,
                                FULL_VAC_MALE_YD=file18$FULL_VAC_MALE_YD,
                                FULL_VAC_FEMALE_YD=file18$FULL_VAC_FEMALE_YD,
                                PART_VAC_MALE=file18$PART_VAC_MALE,
                                PART_VAC_FEMALE=file18$PART_VAC_FEMALE,
                                PART_VAC_MALE_YD=file18$PART_VAC_MALE_YD,
                                PART_VAC_FEMALE_YD=file18$PART_VAC_FEMALE_YD), by = list(file18$DISTRICT), sum)

district18.df$SUM_TOTAL_BOTH <- rowSums(cbind(district18.df$TOTAL_MALE,district18.df$TOTAL_FEMALE),na.rm=TRUE)

district18.df$SUM_VAC_BOTH <- rowSums(cbind(district18.df$FULL_VAC_MALE,district18.df$FULL_VAC_FEMALE),na.rm=TRUE)

district18.df$TOTAL_COVERAGE <- round(((district18.df$SUM_VAC_BOTH/district18.df$SUM_TOTAL_BOTH) * 100), digits = 2)

sindhmap18 <- readOGR(dsn="mapfile", layer="NSSF", stringsAsFactors = TRUE)
sindhmap_shp18 <- sindhmap18[sindhmap18$ADM1_EN=="Sindh",]

sindhmap_shp18$ADM2_EN <- district18.df$Group.1

sindhmap_shp18$ADM2_EN[1] <- district18.df$Group.1[1]
sindhmap_shp18$ADM2_EN[2] <- district18.df$Group.1[2]
sindhmap_shp18$ADM2_EN[3] <- district18.df$Group.1[3]
sindhmap_shp18$ADM2_EN[4] <- district18.df$Group.1[4]
sindhmap_shp18$ADM2_EN[5] <- district18.df$Group.1[5]
sindhmap_shp18$ADM2_EN[6] <- district18.df$Group.1[6]
sindhmap_shp18$ADM2_EN[7] <- district18.df$Group.1[12]
sindhmap_shp18$ADM2_EN[8] <- district18.df$Group.1[14]
sindhmap_shp18$ADM2_EN[9] <- district18.df$Group.1[16]
sindhmap_shp18$ADM2_EN[10] <- district18.df$Group.1[18]
sindhmap_shp18$ADM2_EN[11] <- district18.df$Group.1[19]
sindhmap_shp18$ADM2_EN[12] <- district18.df$Group.1[20]
sindhmap_shp18$ADM2_EN[13] <- district18.df$Group.1[7]
sindhmap_shp18$ADM2_EN[14] <- district18.df$Group.1[22]
sindhmap_shp18$ADM2_EN[15] <- district18.df$Group.1[23]
sindhmap_shp18$ADM2_EN[16] <- district18.df$Group.1[24]
sindhmap_shp18$ADM2_EN[17] <- district18.df$Group.1[21]
sindhmap_shp18$ADM2_EN[18] <- district18.df$Group.1[25]
sindhmap_shp18$ADM2_EN[19] <- district18.df$Group.1[26]
sindhmap_shp18$ADM2_EN[20] <- district18.df$Group.1[27]
sindhmap_shp18$ADM2_EN[21] <- district18.df$Group.1[28]
sindhmap_shp18$ADM2_EN[22] <- district18.df$Group.1[29]
sindhmap_shp18$ADM2_EN[23] <- district18.df$Group.1[30]
sindhmap_shp18$ADM2_EN[24] <- district18.df$Group.1[11]
sindhmap_shp18$ADM2_EN[25] <- district18.df$Group.1[13]
sindhmap_shp18$ADM2_EN[26] <- district18.df$Group.1[17]
sindhmap_shp18$ADM2_EN[27] <- district18.df$Group.1[15]
sindhmap_shp18$ADM2_EN[28] <- district18.df$Group.1[10]
sindhmap_shp18$ADM2_EN[29] <- district18.df$Group.1[8]
sindhmap_shp18$ADM2_EN[30] <- district18.df$Group.1[9]

bins <- c(0, 0.25, 0.50, 0.75, Inf)

pal <- colorBin("YlOrRd", domain = district.df$TOTAL_COVERAGE, bins = bins)

mylabels <- paste(sindhmap_shp$ADM2_EN, "<br>" ,"<b>Total Coverage Percentage:<b>", district.df$TOTAL_COVERAGE, sep = " ") %>% lapply(htmltools::HTML) 

bins18 <- c(10, 20, 30, 40, Inf)

pal2 <- colorBin("YlOrRd", domain = district18.df$TOTAL_COVERAGE, bins = bins18)

mylabels2 <- paste(sindhmap_shp18$ADM2_EN, "<br>" ,"<b>Total Coverage Percentage:<b>", district18.df$TOTAL_COVERAGE, sep = " ") %>% lapply(htmltools::HTML)

# EXTREME OBSERVATIONS FILES

PER.tab <- as.data.frame(file %>%
                             group_by(DISTRICT, lat, long) %>%
                             filter(!is.na(TOTAL_MALE)) %>%
                             summarise_at(.vars = vars(TOTAL_MALE,
                                                       TOTAL_FEMALE,
                                                       TOTAL_MALE_12_17,
                                                       TOTAL_FEMALE_12_17,
                                                       FULL_VAC_MALE,
                                                       FULL_VAC_MALE_YD,
                                                       FULL_VAC_FEMALE,
                                                       FULL_VAC_FEMALE_YD,
                                                       PART_VAC_MALE,
                                                       PART_VAC_MALE_YD,
                                                       PART_VAC_FEMALE,
                                                       PART_VAC_FEMALE_YD), .funs = sum) %>%
                             mutate(PER_FVM = (FULL_VAC_MALE/TOTAL_MALE)*100, 
                                    PER_FVF = (FULL_VAC_FEMALE/TOTAL_FEMALE)*100,
                                    PER_PVM = (PART_VAC_MALE/TOTAL_MALE)*100,
                                    PER_PVF = (PART_VAC_MALE/TOTAL_FEMALE)*100))

rownames(PER.tab) <- as.character(PER.tab$DISTRICT)

PER.tab_18 <- as.data.frame(file18 %>%
                                group_by(DISTRICT, lat, long) %>%
                                filter(!is.na(TOTAL_MALE)) %>%
                                summarise_at(.vars = vars(TOTAL_MALE,
                                                          TOTAL_FEMALE,
                                                          FULL_VAC_MALE,
                                                          FULL_VAC_MALE_YD,
                                                          FULL_VAC_FEMALE,
                                                          FULL_VAC_FEMALE_YD,
                                                          PART_VAC_MALE,
                                                          PART_VAC_MALE_YD,
                                                          PART_VAC_FEMALE,
                                                          PART_VAC_FEMALE_YD), .funs = sum) %>%
                                mutate(PER_FVM_18 = (FULL_VAC_MALE/TOTAL_MALE)*100, 
                                       PER_FVF_18 = (FULL_VAC_FEMALE/TOTAL_FEMALE)*100,
                                       PER_PVM_18 = (PART_VAC_MALE/TOTAL_MALE)*100,
                                       PER_PVF_18 = (PART_VAC_MALE/TOTAL_FEMALE)*100))

rownames(PER.tab_18) <- as.character(PER.tab_18$DISTRICT)


#full vaccine male

fvm.max <- rownames(PER.tab)[which.max(PER.tab$PER_FVM)]

fvm.min <- rownames(PER.tab)[which.min(PER.tab$PER_FVM)]


fvm.max.long <- PER.tab$long[PER.tab$DISTRICT==fvm.max]

fvm.max.lat <- PER.tab$lat[PER.tab$DISTRICT==fvm.max]


fvm.min.long <- PER.tab$long[PER.tab$DISTRICT==fvm.min]

fvm.min.lat <- PER.tab$lat[PER.tab$DISTRICT==fvm.min]


#full vaccine female

fvf.max <- rownames(PER.tab)[which.max(PER.tab$PER_FVF)]

fvf.min <- rownames(PER.tab)[which.min(PER.tab$PER_FVF)]


fvf.max.long <- PER.tab$long[PER.tab$DISTRICT==fvf.max]

fvf.max.lat <- PER.tab$lat[PER.tab$DISTRICT==fvf.max]


fvf.min.long <- PER.tab$long[PER.tab$DISTRICT==fvf.min]

fvf.min.lat <- PER.tab$lat[PER.tab$DISTRICT==fvf.min]

#partial vaccine male

pvm.max <- rownames(PER.tab)[which.max(PER.tab$PER_PVM)]

pvm.min <- rownames(PER.tab)[which.min(PER.tab$PER_PVM)]


pvm.max.long <- PER.tab$long[PER.tab$DISTRICT==pvm.max]

pvm.max.lat <- PER.tab$lat[PER.tab$DISTRICT==pvm.max]


pvm.min.long <- PER.tab$long[PER.tab$DISTRICT==pvm.min]

pvm.min.lat <- PER.tab$lat[PER.tab$DISTRICT==pvm.min]


#partial vaccine female

pvf.max <- rownames(PER.tab)[which.max(PER.tab$PER_PVF)]

pvf.min <- rownames(PER.tab)[which.min(PER.tab$PER_PVF)]


pvf.max.long <- PER.tab$long[PER.tab$DISTRICT==pvf.max]

pvf.max.lat <- PER.tab$lat[PER.tab$DISTRICT==pvf.max]


pvf.min.long <- PER.tab$long[PER.tab$DISTRICT==pvf.min]

pvf.min.lat <- PER.tab$lat[PER.tab$DISTRICT==pvf.min]


#full vaccine male

fvm18.max <- rownames(PER.tab_18)[which.max(PER.tab_18$PER_FVM_18)]

fvm18.min <- rownames(PER.tab_18)[which.min(PER.tab_18$PER_FVM_18)]


fvm18.max.long <- PER.tab_18$long[PER.tab_18$DISTRICT==fvm18.max]

fvm18.max.lat <- PER.tab_18$lat[PER.tab_18$DISTRICT==fvm18.max]


fvm18.min.long <- PER.tab_18$long[PER.tab_18$DISTRICT==fvm18.min]

fvm18.min.lat <- PER.tab_18$lat[PER.tab_18$DISTRICT==fvm18.min]


#full vaccine female

fvf18.max <- rownames(PER.tab_18)[which.max(PER.tab_18$PER_FVF_18)]

fvf18.min <- rownames(PER.tab_18)[which.min(PER.tab_18$PER_FVF_18)]


fvf18.max.long <- PER.tab_18$long[PER.tab_18$DISTRICT==fvf18.max]

fvf18.max.lat <- PER.tab_18$lat[PER.tab_18$DISTRICT==fvf18.max]


fvf18.min.long <- PER.tab_18$long[PER.tab_18$DISTRICT==fvf18.min]

fvf18.min.lat <- PER.tab_18$lat[PER.tab_18$DISTRICT==fvf18.min]

#partial vaccine male

pvm18.max <- rownames(PER.tab_18)[which.max(PER.tab_18$PER_PVM_18)]

pvm18.min <- rownames(PER.tab_18)[which.min(PER.tab_18$PER_PVM_18)]


pvm18.max.long <- PER.tab_18$long[PER.tab_18$DISTRICT==pvm18.max]

pvm18.max.lat <- PER.tab_18$lat[PER.tab_18$DISTRICT==pvm18.max]


pvm18.min.long <- PER.tab_18$long[PER.tab_18$DISTRICT==pvm18.min]

pvm18.min.lat <- PER.tab_18$lat[PER.tab_18$DISTRICT==pvm18.min]


#partial vaccine female

pvf18.max <- rownames(PER.tab_18)[which.max(PER.tab_18$PER_PVF_18)]

pvf18.min <- rownames(PER.tab_18)[which.min(PER.tab_18$PER_PVF_18)]


pvf18.max.long <- PER.tab_18$long[PER.tab_18$DISTRICT==pvf18.max]

pvf18.max.lat <- PER.tab_18$lat[PER.tab_18$DISTRICT==pvf18.max]


pvf18.min.long <- PER.tab_18$long[PER.tab_18$DISTRICT==pvf18.min]

pvf18.min.lat <- PER.tab_18$lat[PER.tab_18$DISTRICT==pvf18.min]


PER.table <- data.frame(Observations = c(fvm.max, fvm.min, fvf.max, fvf.min, pvm.max, pvm.min, pvf.max, pvf.min),
                        long = c(fvm.max.long, fvm.min.long, fvf.max.long, fvf.min.long, pvm.max.long, pvm.min.long, pvf.max.long, pvf.min.long),
                        lat = c(fvm.max.lat, fvm.min.lat, fvf.max.lat, fvf.min.lat, pvm.max.lat, pvm.min.lat, pvf.max.lat, pvf.min.lat))

eo.table <- data.frame(Observations = c("Full Vaccine Male", "Full Vaccine Female", "Partial Vaccine Male", "Partial Vaccine Female"),
                       Maximum = c(fvm.max, fvf.max, pvm.max, pvf.max),
                       Minimum = c(fvm.min, fvf.min, pvm.min, pvf.min)
)

PER.table_18 <- data.frame(Observations = c(fvm18.max, fvm18.min, fvf18.max, fvf18.min, pvm18.max, pvm18.min, pvf18.max, pvf18.min),
                           long = c(fvm18.max.long, fvm18.min.long, fvf18.max.long, fvf18.min.long, pvm18.max.long, pvm18.min.long, pvf18.max.long, pvf18.min.long),
                           lat = c(fvm18.max.lat, fvm18.min.lat, fvf18.max.lat, fvf18.min.lat, pvm18.max.lat, pvm18.min.lat, pvf18.max.lat, pvf18.min.lat))

eo.table18 <- data.frame(Observations = c("Full Vaccine Male", "Full Vaccine Female", "Partial Vaccine Male", "Partial Vaccine Female"),
                         Maximum = c(fvm18.max, fvf18.max, pvm18.max, pvf18.max),
                         Minimum = c(fvm18.min, fvf18.min, pvm18.min, pvf18.min)
)


#COMPARISON PLOTS FILES

tehsil.df <- aggregate(list(TOTAL_MALE=file$TOTAL_MALE,
                            TOTAL_FEMALE=file$TOTAL_FEMALE,
                            TOTAL_MALE_12_17=file$TOTAL_MALE_12_17,
                            TOTAL_FEMALE_12_17=file$TOTAL_FEMALE_12_17,
                            FULL_VAC_MALE=file$FULL_VAC_MALE,
                            FULL_VAC_FEMALE=file$FULL_VAC_FEMALE,
                            FULL_VAC_MALE_YD=file$FULL_VAC_MALE_YD,
                            FULL_VAC_FEMALE_YD=file$FULL_VAC_FEMALE_YD,
                            PART_VAC_MALE=file$PART_VAC_MALE,
                            PART_VAC_FEMALE=file$PART_VAC_FEMALE,
                            PART_VAC_MALE_YD=file$PART_VAC_MALE_YD,
                            PART_VAC_FEMALE_YD=file$PART_VAC_FEMALE_YD), by = list(file$TEHSIL), sum)

tehsil18.df <- aggregate(list(TOTAL_MALE=file18$TOTAL_MALE,
                              TOTAL_FEMALE=file18$TOTAL_FEMALE,
                              FULL_VAC_MALE=file18$FULL_VAC_MALE,
                              FULL_VAC_FEMALE=file18$FULL_VAC_FEMALE,
                              FULL_VAC_MALE_YD=file18$FULL_VAC_MALE_YD,
                              FULL_VAC_FEMALE_YD=file18$FULL_VAC_FEMALE_YD,
                              PART_VAC_MALE=file18$PART_VAC_MALE,
                              PART_VAC_FEMALE=file18$PART_VAC_FEMALE,
                              PART_VAC_MALE_YD=file18$PART_VAC_MALE_YD,
                              PART_VAC_FEMALE_YD=file18$PART_VAC_FEMALE_YD), by = list(file18$TEHSIL), sum)

All.Area <- as.data.frame(file[1:49,4:16])

All.Area18 <- as.data.frame(file18[1:49,4:14])

m.Area <- merge(All.Area, All.Area18, by = "AREA")

l <- list(
    font = list(size = 12,
                family = "sans-serif",
                color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 0.5)


ui <- fluidPage( theme = shinytheme("flatly"),
                 
                 navbarPage(
                     title = 'COVID Vaccine Coverage across Sindh',
                     
                     tabPanel(('Interactive Map'),
                              
                             bootstrapPage(
                                  conditionalPanel(
                                      condition = "input.visuBtn == 'Aged 12 to 17'",
                                      leafletOutput("mymap", height = "550px")
                                  ),
                                  conditionalPanel(
                                      condition = "input.visuBtn == 'Above 18 Population'",
                                      leafletOutput("mymap2", height = "550px")
                                  ),
                                  
                                  
                                  
                                  
                                  absolutePanel(style="text-align: center; background-color: white; padding: 8px; opacity: 0.85; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); ",
                                                right = 100, top = 160, width = 275, class = "floater",
                                                
                                                h4(strong("Coverage Profile"), style="text-align: center;"),
                                                htmlOutput("text")
                                  ),
                                  absolutePanel(
                                      left = 40, top = 330, width = 300, class = "floater", style="background-color: white; padding: 8px; opacity: 0.85; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); ",
                                      
                                      h5(strong("Population Coverage"), style="text-align: center;"),
                                      div(style = "font-size:14px;",
                                          radioButtons("visuBtn", label = div(style = "font-size:14px", "Choose a Population Bracket"), choices = c('Aged 12 to 17', 'Above 18 Population'), selected = 'Aged 12 to 17')
                                      )),
                                  
                                  absolutePanel(id = "controls", class = "panel panel-default",
                                                top = 160, left = 40, width = 300, class = "floater", 
                                                style="background-color: white; padding: 8px; opacity: 0.85; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); ",
                                                span(tags$i(h6("The map shows total coverage percentages of two age brackets within each district of Sindh province. The percentages are calculated by taking ratios of Full Vaccines(both male and female) of a district to the Total Population(male and female combined) of that district. Hovering over a district polygon displays the Coverage of that district in the Coverage Profile Box on the right.")), style="color:grey")
                                                
                                  )
                                  
                                  
                                  
                              )
                              
                              
                     ),
                     
                     navbarMenu(('Vaccine Coverage'),
                                    
                                    tabPanel(('Aged 12 to 17'),
                                             dashboardPage(
                                                 dashboardHeader(disable = TRUE),
                                                 dashboardSidebar( 
                                                     
                                                     tags$style(HTML(".main-sidebar{width: 350px;}")),
                                                     span(tags$i(h6("The three dropdown below offers options to select District, Tehsil and Area.")), style="color:#045a8d"),
                                                     span(tags$i(h6("Multiple choices can be made by selecting individual item from the menus. Option to Select-All has also been provided to select all in a given group.")), style="color:#045a8d"),
                                                     
                                                     
                                                     uiOutput("district_selector"),
                                                     uiOutput("tehsil_selector"),
                                                     uiOutput("deh_selector")),
                                                 dashboardBody(tags$head(tags$style(HTML('      .main-sidebar{        width: 350px;      }      .main-header > .navbar {        margin-left: 350px;      }      .main-header .logo {         width: 350px;      }      .content-wrapper, .main-footer, .right-side {          margin-left: 350px;      }    '))),
                                                               
                                                               fluidRow(
                                                                   column(width = 7,
                                                                          box(background = "black",
                                                                              solidHeader = T,  
                                                                              width = NULL,
                                                                              collapsible = F, 
                                                                              textOutput("Title1"), style="text-align: center;"),
                                                                          box(background = "blue",
                                                                              solidHeader = T,  
                                                                              width = NULL,
                                                                              collapsible = F, 
                                                                              textOutput("Text1"), style="text-align: center;"),
                                                                          box(solidHeader = T,  
                                                                              width = NULL,
                                                                              collapsible = F,
                                                                              fluidRow(
                                                                                box(background = "green",
                                                                                    solidHeader = T,  
                                                                                    width = 12,
                                                                                    collapsible = F, 
                                                                                    textOutput("IBText1"), style="text-align: center;")  
                                                                              ),
                                                                              fluidRow(
                                                                                  infoBoxOutput("fvmy", width = 6), 
                                                                                  infoBoxOutput("fvfy", width = 6)),
                                                                              fluidRow(
                                                                                  infoBoxOutput("pvmy", width = 6), 
                                                                                  infoBoxOutput("pvfy", width = 6))
                                                                          )
                                                                   ),
                                                                   column(width = 5,
                                                                          box(solidHeader = T,  
                                                                              width = NULL, 
                                                                              collapsible = F,
                                                                              leafletOutput("mymapB", height = "395px")))),
                                                               
                                                               fluidRow(style="text-align: center;",
                                                                        box(background = "purple",
                                                                            solidHeader = T,  
                                                                            width = 12,
                                                                            collapsible = F, 
                                                                            plotlyOutput("PlotTest1", height = "240px")))
                                                               
                                                 ))),
                                    
                                    tabPanel(('Above 18 Population'),
                                             dashboardPage(
                                                 dashboardHeader(disable = TRUE),
                                                 dashboardSidebar( tags$style(HTML("  .main-sidebar{ width: 350px; } ")),
                                                                   span(tags$i(h6("The three dropdown below offers options to select District, Tehsil and Area.")), style="color:#045a8d"),
                                                                   span(tags$i(h6("Multiple choices can be made by selecting individual item from the menus. Option to Select-All has also been provided to select all in a given group.")), style="color:#045a8d"),
                                                                   
                                                                   
                                                                   uiOutput("district_selector2"),
                                                                   uiOutput("tehsil_selector2"),
                                                                   uiOutput("deh_selector2")),
                                                 dashboardBody(tags$head(tags$style(HTML('      .main-sidebar{        width: 350px;      }      .main-header > .navbar {        margin-left: 350px;      }      .main-header .logo {         width: 350px;      }      .content-wrapper, .main-footer, .right-side {          margin-left: 350px;      }    '))),
                                                               
                                                               fluidRow(
                                                                   column(width = 7,
                                                                          box(background = "black",
                                                                              solidHeader = T,  
                                                                              width = NULL,
                                                                              collapsible = F, 
                                                                              textOutput("Title2"), style="text-align: center;"),
                                                                          box(background = "blue",
                                                                              solidHeader = T,  
                                                                              width = NULL,
                                                                              collapsible = F, 
                                                                              textOutput("Text2"), style="text-align: center;"),
                                                                          box(solidHeader = T,  
                                                                              width = NULL,
                                                                              collapsible = F,
                                                                              fluidRow(
                                                                                box(background = "green",
                                                                                    solidHeader = T,  
                                                                                    width = 12,
                                                                                    collapsible = F, 
                                                                                    textOutput("IBText2"), style="text-align: center;")  
                                                                              ),
                                                                              fluidRow(
                                                                                  infoBoxOutput("fvmy18", width = 6), 
                                                                                  infoBoxOutput("fvfy18", width = 6)),
                                                                              fluidRow(
                                                                                  infoBoxOutput("pvmy18", width = 6), 
                                                                                  infoBoxOutput("pvfy18", width = 6))
                                                                          )
                                                                   ),
                                                                   column(width = 5,
                                                                          box(solidHeader = T,  
                                                                              width = NULL, 
                                                                              collapsible = F,
                                                                              leafletOutput("mymap2B", height = "395px")))),
                                                               
                                                               fluidRow(style="text-align: center;",
                                                                        box(background = "purple",
                                                                            solidHeader = T,  
                                                                            width = 12,
                                                                            collapsible = F, 
                                                                            plotlyOutput("PlotTest2", height = "240px")))
                                                 )))
                         ),
                     tabPanel(('Extreme Observations'),
                              
                                      
                                      column(style="text-align: center;", width = 6,
                                             box(title = "EXTREME OBSERVATIONS - AGED 12 to 17",
                                                 background = "navy",
                                                 solidHeader = T,  
                                                 width = NULL,
                                                 collapsible = F, 
                                                 tableOutput("T1"),
                                                 leafletOutput("mapx", height = "550px"))
                                      ),
                                      
                                      column(style="text-align: center;", width = 6,
                                             box(title = "EXTREME OBSERVATIONS - ABOVE 18 POPULATION",
                                                 background = "olive",
                                                 solidHeader = T,  
                                                 width = NULL, 
                                                 collapsible = F,
                                                 tableOutput("T2"),
                                                 leafletOutput("mapx18", height = "550px"))
                                      )
                                  ),
                     navbarMenu(('Comparisons'),
                                tabPanel(('Target or Coverage Comparison'),
                                         
                                         tabsetPanel(
                                             
                                             tabPanel(("District-level"),
                                                      
                                                      fluidRow(
                                                          
                                                          column(width = 6,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test1", height = "240px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test2", height = "240px"))
                                                          ),
                                                          column(width = 6,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test3", height = "240px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test4", height = "240px"))
                                                          )
                                                      )),
                                             
                                             tabPanel(("Tehsil-level"),
                                                      fluidRow(
                                                          column(width = 12,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test5", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test6", height = "100px")),
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test7", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test8", height = "100px"))
                                                          )
                                                      )),
                                             
                                             tabPanel(("Deh-level"),
                                                      fluidRow(
                                                          column(width = 12,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test9", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test10", height = "100px")),
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test11", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test12", height = "100px"))
                                                          )))
                                         )),
                                
                                
                                tabPanel(('Partial versus Total Vaccination'),
                                         
                                         tabsetPanel(
                                             
                                             tabPanel(("District-level"),
                                                      
                                                      fluidRow(
                                                          
                                                          column(width = 6,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test13", height = "240px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test14", height = "240px"))
                                                          ),
                                                          column(width = 6,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test15", height = "240px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test16", height = "240px"))
                                                          )
                                                      )),
                                             
                                             tabPanel(("Tehsil-level"),
                                                      fluidRow(
                                                          column(width = 12,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test17", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test18", height = "100px")),
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test19", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test20", height = "100px"))
                                                          )
                                                      )),
                                             
                                             tabPanel(("Deh-level"),
                                                      fluidRow(
                                                          column(width = 12,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test21", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test22", height = "100px")),
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test23", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test24", height = "100px"))
                                                          )))
                                         )),
                                tabPanel(('Gender Based Comparison'),
                                         
                                         tabsetPanel(
                                             
                                             tabPanel(("District-level"),
                                                      
                                                      fluidRow(
                                                          
                                                          column(width = 6,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test25", height = "240px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test26", height = "240px"))
                                                          ),
                                                          column(width = 6,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test27", height = "240px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test28", height = "240px"))
                                                          )
                                                      )),
                                             
                                             tabPanel(("Tehsil-level"),
                                                      fluidRow(
                                                          column(width = 12,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test29", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test30", height = "100px")),
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test31", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test32", height = "100px"))
                                                          )
                                                      )),
                                             
                                             tabPanel(("Deh-level"),
                                                      fluidRow(
                                                          column(width = 12,
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test33", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test34", height = "100px")),
                                                                 box(title = "",
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test35", height = "100px")),
                                                                 box(title = "", 
                                                                     solidHeader = T,
                                                                     width = NULL, 
                                                                     collapsible = F,
                                                                     plotlyOutput("my_test36", height = "100px"))
                                                          )))
                                         ))
                                
                     ),
                     tabPanel(("Data Explorer"),
                              tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    .tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
    .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
    .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
    .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
  ")),
                                                
                                                tabsetPanel(
                                                    tabPanel("Aged 12 to 18", DT::dataTableOutput("ftable")),
                                                    tabPanel("Above 18 Population", DT::dataTableOutput("f18table")))),
                     tabPanel("About", fluid = TRUE,
                              fluidRow(
                                  column(6,
                                         #br(),
                                         h4(p("Background")),
                                         h5(p("Vaccine hesitancy hinders the vaccination programs, as is seen in the case for the COVID-19 vaccination program. This results in low uptake of vaccines and thus government efforts to reach their objectives are hampered. Such is the case with Sindh Province of Pakistan, where despite government's much effort, official statistics aren't satisfactory. Vaccination figures in almost all cities of the Province are lower than targets. Having this information, the current project is aimed at visualizing vaccine coverage trends of the Province through infographics.")),
                                         br(),
                                         h4(p("App Functionality")),
                                         h5(p("The aim of this app is to shows an infographic summary of vaccine utilization in the province of Sindh, Pakistan. The app holistically displays vaccine coverage of the province as whole, reporting even the smallest units of the province. Coverage profiling and comparison have been visualized across all districts, tehsils and dehs of Sindh. The partial as well as full vaccine doses of two age brakets have been reported, namely: Aged 12 to 17, and Above 18 Population. The app employes various different tools of visulaization such as: Maps, Plots, Infoboxes, Markers, and Tables.")),
                                         br(),
                                         h4(p("Coverage")),
                                         h5(p("Five tabs have been provided to visualise coverage uniquely")),
                                         h5(p("*   First tab shows a choropleth map. Upon choosing an age bracket, a interactive map of Sindh province is generated where each district of the province is show as a separate polygon of varrying colour scale. Hover over these these polygon to display the coverage profile of that district.")),
                                         h5(p("*   Second tab offer choice to select an age group at first then provides three drop down menus to select a single, multiple or all  of the units within an administrative division. It thereby, generates a bar plot displaying all available vaccine parameters. Four infoboxes draws comparison of today coverage with yesterdays in terms of gendered percentages.  A text box and map have added to show numbers and places of selection."))
                                         ),
                                  column(6,
                                         br(),
                                         h5(p("*   Third tab shows greatest and least values of partial and full vaccine of both genders. The two separate boxes are shown with a table at top and a map below it. The table displays the names of district having these values whereas the map places circle markers of different colours in those districts.")),
                                         h5(p("*   The fourth tab offers three types of comparison. Target based comparison, Partial versus Total comparison, Gender based comparison. Comparisons are carried out between genders of same age group or between same gender of different age groups. Each comparison offers four separate plot boxes at three levels: district, tehsil and deh.")),
                                         h5(p("*   Fifth tab is of data explorer, which displays the data of both age groups catered in a tabular form. The table provides an option to search an area of interest and thereby knowing it's locations and all its current stats.")),
                                         br(),
                                         h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at hassanshah18@gmail.com"),
                                            p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/HasShah"), ".")),
                                         
                                         #hr(),
                                         
                                         br(),
                                         h4(p("About the Author")),
                                         h5(p("Hasan Shah is a Civil Engineer.")),
                                         br(),
                                  )
                              ),
                              br(),
                              hr(),
                              h5("Sources:"),
                              h6(
                                  p("Vaccine Coverage Information from",
                                    a("Health Department of Sindh",
                                      href = "https://www.sindhhealth.gov.pk/"))),
                              h5("Built with",
                                 img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                 "by",
                                 img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                 ".")
                     )
                                  
                                  
                                  
                                  
                              
                     
                     ))


server <- function(input, output) {
    
    output$mymap <- renderLeaflet({
        
        leaflet(sindhmap_shp) %>%
            addTiles() %>%
            setView(lng = 69, lat = 26, zoom = 7) %>%
            addPolygons(
                layerId = ~mylabels,
                fillColor = ~pal(district.df$TOTAL_COVERAGE),
                stroke = TRUE, 
                opacity = 1,
                color = 'White',
                dashArray = "3",
                weight = 2,
                fillOpacity = 1,
                highlight = highlightOptions(
                    weight = 3,
                    fillOpacity = 1,
                    color = "#666",
                    dashArray = "",
                    opacity = 1,
                    bringToFront = TRUE,
                    sendToBack = TRUE)) %>%
            addLegend( pal= pal, 
                       values=~district.df$TOTAL_COVERAGE, 
                       opacity=0.7, 
                       bins = bins,
                       title = "Total Vaccinations", 
                       position = "bottomleft", 
                       labFormat = labelFormat(suffix = "%")
            ) 
        
    })
    
    output$mymap2 <- renderLeaflet({
        
        leaflet(sindhmap_shp18) %>%
            addTiles() %>%
            setView(lng = 69, lat = 26, zoom = 7) %>%
            addPolygons(
                layerId = ~mylabels2,
                fillColor = ~pal2(district18.df$TOTAL_COVERAGE),
                stroke = TRUE, 
                opacity = 1,
                color = 'White',
                dashArray = "3",
                weight = 2,
                fillOpacity = 1,
                highlight = highlightOptions(
                    weight = 3,
                    fillOpacity = 1,
                    color = "#666",
                    dashArray = "",
                    opacity = 1,
                    bringToFront = TRUE,
                    sendToBack = TRUE)) %>%
            addLegend( pal= pal2, 
                       values=~district18.df$TOTAL_COVERAGE, 
                       opacity=0.7,
                       bins = bins18,
                       title = "Total Vaccinations", 
                       position = "bottomleft", 
                       labFormat = labelFormat(suffix = "%")
            ) 
        
    })
    
    observe({
        click <- input$mymap_shape_click
        if(is.null(click))
            return()
        
        leafletProxy("mymap") %>% 
            setView(lng = click$lng, lat = click$lat, zoom = 8)
    })
    
    observe({
        click2 <- input$mymap2_shape_click
        if(is.null(click2))
            return()
        
        leafletProxy("mymap2") %>% 
            setView(lng = click2$lng, lat = click2$lat, zoom = 8)
    })
    
    observe({aa = input$mymap_shape_mouseover
    
    if(is.null(aa$id))
        return()
    else
        output$text <- renderUI({HTML(paste(aa$id))})
    })   
    
    observe({bb = input$mymap2_shape_mouseover
    
    if(is.null(bb$id))
        return()
    else
        output$text <- renderUI({HTML(paste(bb$id))})
    
    })
    
    
    output$district_selector = renderUI({ 
        pickerInput(inputId = "district",
                    label = "DISTRICT",
                    choices = as.character(unique(file$DISTRICT)),
                    selected = "BADIN", width = "100%", options = pickerOptions(`actions-box` = TRUE, size = 5),multiple = T) 
    })
    
    output$tehsil_selector = renderUI({
        
        district_available = file[file$DISTRICT %in% input$district, "TEHSIL"]
        
        
        pickerInput(inputId = "tehsil", 
                    label = "TEHSIL", 
                    choices = as.character(unique(district_available)), 
                    selected = as.character(unique(district_available)), width = "100%", options = pickerOptions(`actions-box` = TRUE, size = 5),multiple = T)
    })
    
    output$deh_selector = renderUI({
        
        tehsil_available = file[file$TEHSIL %in% input$tehsil, "AREA"]
        
        pickerInput(inputId = "deh", 
                    label = "DEH", 
                    choices = as.character(tehsil_available), 
                    selected = as.character(tehsil_available), width = "100%", options = pickerOptions(`actions-box` = TRUE, size = 5),multiple = T)
    })
    
    
    output$district_selector2 = renderUI({ 
        pickerInput(inputId = "district2",
                    label = "DISTRICT",
                    choices = as.character(unique(file18$DISTRICT)),
                    selected = "BADIN", width = "100%", options = pickerOptions(`actions-box` = TRUE, size = 5),multiple = T) 
    })
    
    output$tehsil_selector2 = renderUI({
        
        district_available2 = file18[file18$DISTRICT %in% input$district2, "TEHSIL"]
        
        
        pickerInput(inputId = "tehsil2", 
                    label = "TEHSIL", 
                    choices = as.character(unique(district_available2)), 
                    selected = as.character(unique(district_available2)), width = "100%", options = pickerOptions(`actions-box` = TRUE, size = 5),multiple = T)
    })
    
    output$deh_selector2 = renderUI({
        
        tehsil_available2 = file18[file18$TEHSIL %in% input$tehsil2, "AREA"]
        
        pickerInput(inputId = "deh2", 
                    label = "DEH", 
                    choices = as.character(tehsil_available2), 
                    selected = as.character(tehsil_available2), width = "100%", options = pickerOptions(`actions-box` = TRUE, size = 5),multiple = T)
    })
    
    
    output$Title1 = renderText({
      paste("AGED 12 TO 17")
    })
    
    output$Title2 = renderText({
      paste("ABOVE 18 POPULATION")
    })
    
    
    output$Text1 = renderText({
        paste("The selected", as.character(length(input$district)), "District(s) have,", as.character(length(input$tehsil)), "Tehsil(s) and",  as.character(length(input$deh)), "Areas")
    })
    
    output$Text2 = renderText({
        paste("The selected", as.character(length(input$district2)), "District(s) have,", as.character(length(input$tehsil2)), "Tehsil(s) and",  as.character(length(input$deh2)), "Areas")
    })
    
    
    
    output$IBText1 = renderText({
      paste("OBSERVATIONS SINCE YESTERDAY")
    })
    
    output$IBText2 = renderText({
      paste("OBSERVATIONS SINCE YESTERDAY")
    })
    
    
    output$fvmy <- renderInfoBox({
        infoBox(title = HTML(paste("<b>Full Vacination<b>", "Male",  sep = "<br/>")), 
                value = HTML(paste(h5(strong(as.numeric(unique(file %>% 
                                                                   filter(DISTRICT %in% input$district) %>%
                                                                   filter(TEHSIL %in% input$tehsil) %>% 
                                                                   filter(AREA %in% input$deh) %>%
                                                                   mutate(PercentChange = (sum(FULL_VAC_MALE) - sum(FULL_VAC_MALE_YD))/sum(FULL_VAC_MALE_YD)) %>%
                                                                   select(PROVINCE, DISTRICT, TEHSIL, AREA, PercentChange) %>% 
                                                                   group_by(PercentChange) %>% 
                                                                   summarise(PercentChange = round(sum(PercentChange), digits = -1), .groups = "drop") )), "%")))),
                fill = F) 
        
    })
    
    output$fvfy <- renderInfoBox({
        infoBox(title = HTML(paste("<b>Full Vacination<b>", "Female", sep = "<br/>")), 
                value = HTML(paste(h5(strong(as.numeric(unique(file %>% 
                                                                   filter(DISTRICT %in% input$district) %>%
                                                                   filter(TEHSIL %in% input$tehsil) %>% 
                                                                   filter(AREA %in% input$deh) %>%
                                                                   mutate(PercentChange = (sum(FULL_VAC_FEMALE) - sum(FULL_VAC_FEMALE_YD))/sum(FULL_VAC_FEMALE_YD)) %>%
                                                                   select(PROVINCE, DISTRICT, TEHSIL, AREA, PercentChange) %>% 
                                                                   group_by(PercentChange) %>% 
                                                                   summarise(PercentChange = round(sum(PercentChange), digits = -1), .groups = "drop") )), "%")))),
                fill = F, color = "yellow") 
        
    })
    
    output$pvmy <- renderInfoBox({
        infoBox(title = HTML(paste("<b>Partial Vac<b>", "Male",  sep = "<br/>")), 
                value = HTML(paste(h5(strong(as.numeric(unique(file %>% 
                                                                   filter(DISTRICT %in% input$district) %>%
                                                                   filter(TEHSIL %in% input$tehsil) %>% 
                                                                   filter(AREA %in% input$deh) %>%
                                                                   mutate(PercentChange = (sum(PART_VAC_MALE) - sum(PART_VAC_MALE_YD))/sum(PART_VAC_MALE_YD)) %>%
                                                                   select(PROVINCE, DISTRICT, TEHSIL, AREA, PercentChange) %>% 
                                                                   group_by(PercentChange) %>% 
                                                                   summarise(PercentChange = round(sum(PercentChange), digits = -1), .groups = "drop") )), "%")))),
                fill = F) 
        
    })
    
    output$pvfy <- renderInfoBox({
        infoBox(title = HTML(paste("<b>Partial Vac<b>", "Female",  sep = "<br/>")), 
                value = HTML(paste(h5(strong(as.numeric(unique(file %>% 
                                                                   filter(DISTRICT %in% input$district) %>%
                                                                   filter(TEHSIL %in% input$tehsil) %>% 
                                                                   filter(AREA %in% input$deh) %>%
                                                                   mutate(PercentChange = (sum(PART_VAC_FEMALE) - sum(PART_VAC_FEMALE_YD))/sum(PART_VAC_FEMALE_YD)) %>%
                                                                   select(PROVINCE, DISTRICT, TEHSIL, AREA, PercentChange) %>% 
                                                                   group_by(PercentChange) %>% 
                                                                   summarise(PercentChange = round(sum(PercentChange), digits = -1), .groups = "drop") )), "%")))),
                fill = F, 
                color = "yellow") 
        
    })
    
    
    output$fvmy18 <- renderInfoBox({
        infoBox(title = HTML(paste("<b>Full Vacination<b>", "Male",  sep = "<br/>")), 
                value = HTML(paste(h5(strong(as.numeric(unique(file18 %>% 
                                                                   filter(DISTRICT %in% input$district2) %>%
                                                                   filter(TEHSIL %in% input$tehsil2) %>% 
                                                                   filter(AREA %in% input$deh2) %>%
                                                                   mutate(PercentChange = (sum(FULL_VAC_MALE) - sum(FULL_VAC_MALE_YD))/sum(FULL_VAC_MALE_YD)) %>%
                                                                   select(PROVINCE, DISTRICT, TEHSIL, AREA, PercentChange) %>% 
                                                                   group_by(PercentChange) %>% 
                                                                   summarise(PercentChange = round(sum(PercentChange), digits = -1), .groups = "drop") )), "%")))),
                fill = F) 
        
    })
    
    output$fvfy18 <- renderInfoBox({
        infoBox(title = HTML(paste("<b>Full Vacination<b>", "Female", sep = "<br/>")), 
                value = HTML(paste(h5(strong(as.numeric(unique(file18 %>% 
                                                                   filter(DISTRICT %in% input$district2) %>%
                                                                   filter(TEHSIL %in% input$tehsil2) %>% 
                                                                   filter(AREA %in% input$deh2) %>%
                                                                   mutate(PercentChange = (sum(FULL_VAC_FEMALE) - sum(FULL_VAC_FEMALE_YD))/sum(FULL_VAC_FEMALE_YD)) %>%
                                                                   select(PROVINCE, DISTRICT, TEHSIL, AREA, PercentChange) %>% 
                                                                   group_by(PercentChange) %>% 
                                                                   summarise(PercentChange = round(sum(PercentChange), digits = -1), .groups = "drop") )), "%")))),
                icon=icon("angle-double-right"),
                fill = F, color = "yellow") 
        
    })
    
    output$pvmy18 <- renderInfoBox({
        infoBox(title = HTML(paste("<b>Partial Vac<b>", "Male",  sep = "<br/>")), 
                value = HTML(paste(h5(strong(as.numeric(unique(file18 %>% 
                                                                   filter(DISTRICT %in% input$district2) %>%
                                                                   filter(TEHSIL %in% input$tehsil2) %>% 
                                                                   filter(AREA %in% input$deh2) %>%
                                                                   mutate(PercentChange = (sum(PART_VAC_MALE) - sum(PART_VAC_MALE_YD))/sum(PART_VAC_MALE_YD)) %>%
                                                                   select(PROVINCE, DISTRICT, TEHSIL, AREA, PercentChange) %>% 
                                                                   group_by(PercentChange) %>% 
                                                                   summarise(PercentChange = round(sum(PercentChange), digits = -1), .groups = "drop") )), "%")))),
                fill = F) 
        
    })
    
    output$pvfy18 <- renderInfoBox({
        infoBox(title = HTML(paste("<b>Partial Vac<b>", "Female",  sep = "<br/>")), 
                value = HTML(paste(h5(strong(as.numeric(unique(file18 %>% 
                                                                   filter(DISTRICT %in% input$district2) %>%
                                                                   filter(TEHSIL %in% input$tehsil2) %>% 
                                                                   filter(AREA %in% input$deh2) %>%
                                                                   mutate(PercentChange = (sum(PART_VAC_FEMALE) - sum(PART_VAC_FEMALE_YD))/sum(PART_VAC_FEMALE_YD)) %>%
                                                                   select(PROVINCE, DISTRICT, TEHSIL, AREA, PercentChange) %>% 
                                                                   group_by(PercentChange) %>% 
                                                                   summarise(PercentChange = round(sum(PercentChange), digits = -1), .groups = "drop") )), "%")))),
                fill = F, 
                color = "yellow") 
        
    })
    
    
    
    output$PlotTest1 <- renderPlotly({
        
        file %>% 
            filter(DISTRICT %in% input$district) %>%
            filter(TEHSIL %in% input$tehsil) %>% 
            filter(AREA %in% input$deh) %>% 
            rename_at(c("TOTAL_MALE", 
                        "TOTAL_FEMALE", 
                        "TOTAL_MALE_12_17", 
                        "TOTAL_FEMALE_12_17",	
                        "FULL_VAC_MALE", 
                        "FULL_VAC_MALE_YD", 
                        "FULL_VAC_FEMALE", 
                        "FULL_VAC_FEMALE_YD", 
                        "PART_VAC_MALE", 
                        "PART_VAC_MALE_YD", 
                        "PART_VAC_FEMALE", 
                        "PART_VAC_FEMALE_YD"), 
                      list(~c("Total\nMale", 
                              "Total\nFemale", 
                              "Total Male\n12 to 17", 
                              "Total\nFemale\n12 to 17",	
                              "Full\nVaccine\nMale", 
                              "Full\nVaccine\nMale\nYesterday", 
                              "Full\nVaccine\nFemale", 
                              "Full\nVaccine\nFemale\nYesterday", 
                              "Partial\nVaccine\nMale", 
                              "Partial\nVaccine\nMale\nYesterday", 
                              "Partial\nVaccine\nFemale", 
                              "Partial\nVaccine\nFemale\nYesterday"))) %>%
            pivot_longer(cols = c(5:16), 
                         names_to = "Partial_Vaccination", values_to = "Partial_Vaccination_Values") %>% 
            select(PROVINCE, DISTRICT, TEHSIL, AREA, Partial_Vaccination, Partial_Vaccination_Values) %>% 
            
            group_by(Partial_Vaccination) %>% 
            summarise(Partial_Vaccination_Values = sum(Partial_Vaccination_Values)) %>% 
            
            plot_ly(type = 'bar', x = ~Partial_Vaccination, y = ~Partial_Vaccination_Values, marker = list(color = 'orange', line = list(color = 'blue', width = 1.5))) %>% 
            
            add_text(text=~Partial_Vaccination_Values, textposition = 'top', cliponaxis = FALSE, showlegend = FALSE, textfont=list(size="auto", color="black")) %>%
            
            layout(xaxis = list(showticklabels = T, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = 0),
                   yaxis = list(showticklabels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12)),
                   margin = list(l = 0, r = 0, b = 0, t = 8, pad = 4),
                   barmode = 'group') %>%
            style(hoverinfo = "none") %>%
            config(displayModeBar = F)
        
        
    })
    
    
    output$PlotTest2 <- renderPlotly({
        
        file18 %>% 
            filter(DISTRICT %in% input$district2) %>%
            filter(TEHSIL %in% input$tehsil2) %>% 
            filter(AREA %in% input$deh2) %>% 
            rename_at(c("TOTAL_MALE", 
                        "TOTAL_FEMALE",
                        "FULL_VAC_MALE", 
                        "FULL_VAC_MALE_YD", 
                        "FULL_VAC_FEMALE", 
                        "FULL_VAC_FEMALE_YD", 
                        "PART_VAC_MALE", 
                        "PART_VAC_MALE_YD", 
                        "PART_VAC_FEMALE", 
                        "PART_VAC_FEMALE_YD"), 
                      list(~c("Total\nMale", 
                              "Total\nFemale", 
                              "Full\nVaccine\nMale", 
                              "Full\nVaccine\nMale\nYesterday", 
                              "Full\nVaccine\nFemale", 
                              "Full\nVaccine\nFemale\nYesterday", 
                              "Partial\nVaccine\nMale", 
                              "Partial\nVaccine\nMale\nYesterday", 
                              "Partial\nVaccine\nFemale", 
                              "Partial\nVaccine\nFemale\nYesterday"))) %>%
            pivot_longer(cols = c(5:14), 
                         names_to = "Partial_Vaccination", values_to = "Partial_Vaccination_Values") %>% 
            select(PROVINCE, DISTRICT, TEHSIL, AREA, Partial_Vaccination, Partial_Vaccination_Values) %>% 
            
            group_by(Partial_Vaccination) %>% 
            summarise(Partial_Vaccination_Values = sum(Partial_Vaccination_Values)) %>% 
            
            plot_ly(type = 'bar', x = ~Partial_Vaccination, y = ~Partial_Vaccination_Values, marker = list(color = 'orange', line = list(color = 'blue', width = 1.5))) %>% 
            
            add_text(text=~Partial_Vaccination_Values, textposition = 'top', cliponaxis = FALSE, showlegend = FALSE, textfont=list(size="auto", color="black")) %>%
            
            layout(xaxis = list(showticklabels = T, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12), tickangle = 0),
                   yaxis = list(showticklabels = FALSE, zeroline = F, showgrid = F, title = "", tickfont = list(size = 12)),
                   margin = list(l = 0, r = 0, b = 0, t = 8, pad = 4),
                   barmode = 'group') %>%
            style(hoverinfo = "none") %>%
            config(displayModeBar = F)
        
        
    })
    
    
    
    
    output$mymapB <- renderLeaflet({
        
        file %>% 
            filter(DISTRICT == input$district) %>% 
            leaflet() %>%
            setView(lng = 68.5247, lat = 26.55, zoom = 7) %>%
            addTiles() %>%
            addMarkers(lng = ~long, lat = ~lat, popup = ~paste(DISTRICT))     })
    
    output$mymap2B <- renderLeaflet({
        
        file18 %>% 
            filter(DISTRICT == input$district2) %>% 
            leaflet() %>%
            setView(lng = 68.5247, lat = 26.55, zoom = 7) %>%
            addTiles() %>%
            addMarkers(lng = ~long, lat = ~lat, popup = ~paste(DISTRICT))     })
    
    
    output$mapx <- renderLeaflet({
        
        leaflet(sindhmap_shp) %>%
            setView(lng = 69, lat = 26, zoom = 7) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = fvm.min.long, lat = fvm.min.lat, fill = T, fillColor ="yellow", popup = fvm.min) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = fvm.max.long, lat = fvm.max.lat, fill = T, fillColor ="purple", popup = fvm.max) %>% 
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = fvf.min.long, lat = fvf.min.lat, fill = T, fillColor ="red", popup = fvf.min) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = fvf.max.long, lat = fvf.max.lat, fill = T, fillColor ="green", popup = fvf.max) %>% 
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = pvm.min.long, lat = pvm.min.lat, fill = T, fillColor ="blue", popup = pvm.min) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = pvm.max.long, lat = pvm.max.lat, fill = T, fillColor ="orange", popup = pvm.max) %>% 
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = pvf.min.long, lat = pvf.min.lat, fill = T, fillColor ="black", popup = pvf.min) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = pvf.max.long, lat = pvf.max.lat, fill = T, fillColor ="maroon", popup = pvf.max) %>%
            addPolygons(
                stroke = TRUE, 
                opacity = 1,
                color = 'grey',
                weight = 2,
                highlight = highlightOptions(
                    weight = 3,
                    fillOpacity = 1,
                    color = "#666",
                    opacity = 1,
                    bringToFront = TRUE,
                    sendToBack = TRUE)) 
        
        
    })
    
    output$mapx18 <- renderLeaflet({
        
        leaflet(sindhmap_shp) %>%
            setView(lng = 69, lat = 26, zoom = 7) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = fvm18.min.long, lat = fvm18.min.lat, fill = T, fillColor ="yellow", popup = fvm18.min) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = fvm18.max.long, lat = fvm18.max.lat, fill = T, fillColor ="purple", popup = fvm18.max) %>% 
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = fvf18.min.long, lat = fvf18.min.lat, fill = T, fillColor ="red", popup = fvf18.min) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = fvf18.max.long, lat = fvf18.max.lat, fill = T, fillColor ="green", popup = fvf18.max) %>% 
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = pvm18.min.long, lat = pvm18.min.lat, fill = T, fillColor ="blue", popup = pvm18.min) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = pvm18.max.long, lat = pvm18.max.lat, fill = T, fillColor ="orange", popup = pvm18.max) %>% 
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = pvf18.min.long, lat = pvf18.min.lat, fill = T, fillColor ="black", popup = pvf18.min) %>%
            addCircleMarkers(fillOpacity = 1, weight = 1, lng = pvf18.max.long, lat = pvf18.max.lat, fill = T, fillColor ="maroon", popup = pvf18.max) %>%
            addPolygons(
                stroke = TRUE, 
                opacity = 1,
                color = 'grey',
                weight = 2,
                highlight = highlightOptions(
                    weight = 3,
                    fillOpacity = 1,
                    color = "#666",
                    opacity = 1,
                    bringToFront = TRUE,
                    sendToBack = TRUE)) 
        
        
    })
    
    
    output$T1 <- function(){
        eo.table %>%
            kbl(align = "c") %>%
            kable_styling(full_width = T) 
        
    }
    
    output$T2 <- function(){
        eo.table18 %>%
            kbl(align = "c") %>%
            kable_styling(full_width = T)
        
    }
    
    
    output$my_test1 <- renderPlotly({
        
        district.df$Group.1 <- factor(district.df$Group.1, levels = district.df[["Group.1"]])
        
        plot_ly(district.df, x = ~district.df$Group.1, y = ~district.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district.df$TOTAL_MALE_12_17, type = 'bar',name = "Male Target - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test2 <- renderPlotly({
        
        district.df$Group.1 <- factor(district.df$Group.1, levels = district.df[["Group.1"]])
        
        plot_ly(district.df, x = ~district.df$Group.1, y = ~district.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district.df$TOTAL_FEMALE_12_17, type = 'bar',name = "Female Target - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test3 <- renderPlotly({
        
        district18.df$Group.1 <- factor(district18.df$Group.1, levels = district18.df[["Group.1"]])
        
        plot_ly(district18.df, x = ~district18.df$Group.1, y = ~district18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district18.df$TOTAL_MALE, type = 'bar',name = "Total Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test4 <- renderPlotly({
        
        district18.df$Group.1 <- factor(district18.df$Group.1, levels = district18.df[["Group.1"]])
        
        plot_ly(district18.df, x = ~district18.df$Group.1, y = ~district18.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district18.df$TOTAL_FEMALE, type = 'bar',name = "Total Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    
    output$my_test5 <- renderPlotly({
        
        tehsil.df$Group.1 <- factor(tehsil.df$Group.1, levels = tehsil.df[["Group.1"]])
        
        plot_ly(tehsil.df, x = ~tehsil.df$Group.1, y = ~tehsil.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil.df$TOTAL_MALE_12_17, type = 'bar',name = "Male Target - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test6 <- renderPlotly({
        
        tehsil.df$Group.1 <- factor(tehsil.df$Group.1, levels = tehsil.df[["Group.1"]])
        
        plot_ly(tehsil.df, x = ~tehsil.df$Group.1, y = ~tehsil.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil.df$TOTAL_FEMALE_12_17, type = 'bar',name = "Female Target - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test7 <- renderPlotly({
        
        tehsil18.df$Group.1 <- factor(tehsil18.df$Group.1, levels = tehsil18.df[["Group.1"]])
        
        plot_ly(tehsil18.df, x = ~tehsil18.df$Group.1, y = ~tehsil18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil18.df$TOTAL_MALE, type = 'bar',name = "Total Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test8 <- renderPlotly({
        
        tehsil18.df$Group.1 <- factor(tehsil18.df$Group.1, levels = tehsil18.df[["Group.1"]])
        
        plot_ly(tehsil18.df, x = ~tehsil18.df$Group.1, y = ~tehsil18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil18.df$TOTAL_FEMALE, type = 'bar',name = "Total Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    
    
    output$my_test9 <- renderPlotly({
        
        All.Area$AREA <- factor(All.Area$AREA, levels = All.Area[["AREA"]])
        
        plot_ly(All.Area, x = ~All.Area$AREA, y = ~All.Area$FULL_VAC_MALE, type = 'scatter', mode = "lines", name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area$TOTAL_MALE_12_17, type = 'scatter', mode = "lines", name = "Male Target - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test10 <- renderPlotly({
        
        All.Area$AREA <- factor(All.Area$AREA, levels = All.Area[["AREA"]])
        
        plot_ly(All.Area, x = ~All.Area$AREA, y = ~All.Area$FULL_VAC_FEMALE, type = 'scatter', mode = "lines", name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area$TOTAL_FEMALE_12_17, type = 'scatter', mode = "lines", name = "Male Target - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test11 <- renderPlotly({
        
        All.Area18$AREA <- factor(All.Area18$AREA, levels = All.Area18[["AREA"]])
        
        plot_ly(All.Area18, x = ~All.Area18$AREA, y = ~All.Area18$FULL_VAC_MALE, type = 'scatter', mode = "lines", name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area18$TOTAL_MALE, type = 'scatter', mode = "lines", name = "Total Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test12 <- renderPlotly({
        
        All.Area18$AREA <- factor(All.Area18$AREA, levels = All.Area18[["AREA"]])
        
        plot_ly(All.Area18, x = ~All.Area18$AREA, y = ~All.Area18$FULL_VAC_FEMALE, type = 'scatter', mode = "lines", name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area18$TOTAL_FEMALE, type = 'scatter', mode = "lines", name = "Total Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    
    output$my_test13 <- renderPlotly({
        
        district.df$Group.1 <- factor(district.df$Group.1, levels = district.df[["Group.1"]])
        
        plot_ly(district.df, x = ~district.df$Group.1, y = ~district.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district.df$PART_VAC_MALE, type = 'bar',name = "Partial Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test14 <- renderPlotly({
        
        district.df$Group.1 <- factor(district.df$Group.1, levels = district.df[["Group.1"]])
        
        plot_ly(district.df, x = ~district.df$Group.1, y = ~district.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district.df$PART_VAC_FEMALE, type = 'bar',name = "Partial Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test15 <- renderPlotly({
        
        district18.df$Group.1 <- factor(district18.df$Group.1, levels = district18.df[["Group.1"]])
        
        plot_ly(district18.df, x = ~district18.df$Group.1, y = ~district18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district18.df$PART_VAC_MALE, type = 'bar',name = "Partial Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test16 <- renderPlotly({
        
        district18.df$Group.1 <- factor(district18.df$Group.1, levels = district18.df[["Group.1"]])
        
        plot_ly(district18.df, x = ~district18.df$Group.1, y = ~district18.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district18.df$PART_VAC_FEMALE, type = 'bar',name = "Partial Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    
    output$my_test17 <- renderPlotly({
        
        tehsil.df$Group.1 <- factor(tehsil.df$Group.1, levels = tehsil.df[["Group.1"]])
        
        plot_ly(tehsil.df, x = ~tehsil.df$Group.1, y = ~tehsil.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil.df$PART_VAC_MALE, type = 'bar',name = "Partial Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test18 <- renderPlotly({
        
        tehsil.df$Group.1 <- factor(tehsil.df$Group.1, levels = tehsil.df[["Group.1"]])
        
        plot_ly(tehsil.df, x = ~tehsil.df$Group.1, y = ~tehsil.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil.df$PART_VAC_FEMALE, type = 'bar',name = "Partial Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test19 <- renderPlotly({
        
        tehsil18.df$Group.1 <- factor(tehsil18.df$Group.1, levels = tehsil18.df[["Group.1"]])
        
        plot_ly(tehsil18.df, x = ~tehsil18.df$Group.1, y = ~tehsil18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil18.df$PART_VAC_MALE, type = 'bar',name = "Partial Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test20 <- renderPlotly({
        
        tehsil18.df$Group.1 <- factor(tehsil18.df$Group.1, levels = tehsil18.df[["Group.1"]])
        
        plot_ly(tehsil18.df, x = ~tehsil18.df$Group.1, y = ~tehsil18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil18.df$PART_VAC_FEMALE, type = 'bar',name = "Partial Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    
    output$my_test21 <- renderPlotly({
        
        All.Area$AREA <- factor(All.Area$AREA, levels = All.Area[["AREA"]])
        
        plot_ly(All.Area, x = ~All.Area$AREA, y = ~All.Area$FULL_VAC_MALE, type = 'scatter', mode = "lines", name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area$PART_VAC_MALE, type = 'scatter', mode = "lines", name = "Partial Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test22 <- renderPlotly({
        
        All.Area$AREA <- factor(All.Area$AREA, levels = All.Area[["AREA"]])
        
        plot_ly(All.Area, x = ~All.Area$AREA, y = ~All.Area$FULL_VAC_FEMALE, type = 'scatter', mode = "lines", name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area$PART_VAC_FEMALE, type = 'scatter', mode = "lines", name = "Partial Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test23 <- renderPlotly({
        
        All.Area18$AREA <- factor(All.Area18$AREA, levels = All.Area18[["AREA"]])
        
        plot_ly(All.Area18, x = ~All.Area18$AREA, y = ~All.Area18$FULL_VAC_MALE, type = 'scatter', mode = "lines", name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area18$PART_VAC_MALE, type = 'scatter', mode = "lines", name = "Partial Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test24 <- renderPlotly({
        
        All.Area18$AREA <- factor(All.Area18$AREA, levels = All.Area18[["AREA"]])
        
        plot_ly(All.Area18, x = ~All.Area18$AREA, y = ~All.Area18$FULL_VAC_FEMALE, type = 'scatter', mode = "lines", name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area18$PART_VAC_FEMALE, type = 'scatter', mode = "lines", name = "Partial Vaccine - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    
    
    output$my_test25 <- renderPlotly({
        
        district.df$Group.1 <- factor(district.df$Group.1, levels = district.df[["Group.1"]])
        
        plot_ly(district.df, x = ~district.df$Group.1, y = ~district.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test26 <- renderPlotly({
        
        district18.df$Group.1 <- factor(district18.df$Group.1, levels = district18.df[["Group.1"]])
        
        plot_ly(district18.df, x = ~district18.df$Group.1, y = ~district18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~district18.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test27 <- renderPlotly({
        
        plot_ly(x = ~district.df$Group.1, y = ~district.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(x = ~district18.df$Group.1, y = ~district18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test28 <- renderPlotly({
        
        plot_ly(x = ~district.df$Group.1, y = ~district.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(x = ~district18.df$Group.1, y = ~district18.df$TOTAL_FEMALE, type = 'bar',name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(tickfont = list(size = 8), zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    
    output$my_test29 <- renderPlotly({
        
        tehsil.df$Group.1 <- factor(tehsil.df$Group.1, levels = tehsil.df[["Group.1"]])
        
        plot_ly(tehsil.df, x = ~tehsil.df$Group.1, y = ~tehsil.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test30 <- renderPlotly({
        
        tehsil18.df$Group.1 <- factor(tehsil18.df$Group.1, levels = tehsil18.df[["Group.1"]])
        
        plot_ly(tehsil18.df, x = ~tehsil18.df$Group.1, y = ~tehsil18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~tehsil18.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test31 <- renderPlotly({
        
        plot_ly(x = ~tehsil.df$Group.1, y = ~tehsil.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(x = ~tehsil18.df$Group.1, y = ~tehsil18.df$FULL_VAC_MALE, type = 'bar',name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test32 <- renderPlotly({
        
        plot_ly(x = ~tehsil.df$Group.1, y = ~tehsil.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(x = ~tehsil18.df$Group.1, y = ~tehsil18.df$FULL_VAC_FEMALE, type = 'bar',name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = ""), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 2, t = 0, pad = 4),
                   barmode = 'group') %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    
    
    output$my_test33 <- renderPlotly({
        
        All.Area$AREA <- factor(All.Area$AREA, levels = All.Area[["AREA"]])
        
        plot_ly(All.Area, x = ~All.Area$AREA, y = ~All.Area$FULL_VAC_MALE, type = 'scatter', mode = "lines", name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area$FULL_VAC_FEMALE, type = 'scatter', mode = "lines", name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test34 <- renderPlotly({
        
        All.Area18$AREA <- factor(All.Area18$AREA, levels = All.Area18[["AREA"]])
        
        plot_ly(All.Area18, x = ~All.Area18$AREA, y = ~All.Area18$FULL_VAC_MALE, type = 'scatter', mode = "lines", name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~All.Area18$FULL_VAC_FEMALE, type = 'scatter', mode = "lines", name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test35 <- renderPlotly({
        
        m.Area$AREA <- factor(m.Area$AREA, levels = m.Area[["AREA"]])
        
        plot_ly(m.Area, x = ~m.Area$AREA, y = ~m.Area$FULL_VAC_MALE.x, type = 'scatter', mode = "lines", name = "Full Vaccine Male - Aged 12 to 17", cliponaxis = FALSE, marker = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~m.Area$FULL_VAC_MALE.y, type = 'scatter', mode = "lines", name = "Full Vaccine Male - Above 18 Population", cliponaxis = FALSE, marker = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   type = 'scatter', mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$my_test36 <- renderPlotly({
        
        m.Area$AREA <- factor(m.Area$AREA, levels = m.Area[["AREA"]])
        
        plot_ly(m.Area, x = ~m.Area$AREA, y = ~m.Area$FULL_VAC_FEMALE.x, type = 'scatter', mode = "lines", name = "Full Vaccine Female - Aged 12 to 17", cliponaxis = FALSE, color = list(color = 'rgba(222,45,38,0.8)')) %>% 
            
            add_trace(y = ~m.Area$FULL_VAC_FEMALE.y, type = 'scatter', mode = "lines", name = "Full Vaccine Female - Above 18 Population", cliponaxis = FALSE, color = list(color = 'rgba(204,204,204,1)')) %>% 
            
            layout(legend = l, xaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", tickangle = -30),
                   yaxis = list(showticklabels = F, zeroline = F, showgrid = F, title = "", range="auto"), showlegend = TRUE,
                   margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
                   mode = "lines") %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.5)) %>%
            layout(hovermode = "x unified") %>%
            config(displayModeBar = F)
    })
    
    output$ftable <- DT::renderDataTable({
        DT::datatable(file, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': 'navy', 'color': '#fff'});",
            "}"), orderClasses = TRUE, autoWidth = TRUE))
    })
    
    output$f18table <- DT::renderDataTable({
        DT::datatable(file18, options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': 'navy', 'color': '#fff'});",
            "}"), orderClasses = TRUE, autoWidth = TRUE))  })
    
}

shinyApp(ui = ui, server = server)