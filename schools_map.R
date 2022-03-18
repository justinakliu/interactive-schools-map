#### SCRIPT INFORMATION ####
# File:         recruitment_map.R
# Author:       Justina Liu
# Last updated: 3/17/22
# 
# Purpose:
# To create an interactive map of the recruited schools and update the file on Box
# This script uses fake data
# Some parts specific to Box (reading in and uploading files) are commented out


##### SET UP ENVIRONMENT #####
library(boxr)
library(stringr)
library(dplyr)
library(miscTools)
library(openxlsx)
library(tidyr)
library(data.table)
library(ggplot2)
library(maps)
library(mapdata)
library(usmap)
library(plotly)
library(readr)
library(leaflet)
library(tigris)
library(htmlwidgets)
box_auth()

#### READ FILES  ####

# read in a file containing the average longitude/lattitude coordinates for each state
# state_coord <- box_read_excel(#fileid) 
# state_coord <- subset(state_coord, !State %in% c("AK", "HI"))

# read in a file of recruited schools, containing school info, recruitment status, and demographic information
# this script uses fake data
# schools.orig <- box_read_csv(#fileid)

# read in a file exported from NCES website containing list of schools and their location data (longitude & latitude)
# nces_location_data.orig <- box_read_excel(#fileid)

#### PREPARE NCES LOCATION DATA ####

# clean the location data 
nces_location_data <- nces_location_data.orig %>%
  rename(nces.id = `School ID - NCES Assigned [Public School] Latest available year`,
         latitude = `Latitude [Public School] 2018-19`,
         longitude = `Longitude [Public School] 2018-19`,
         state = `Location State Abbr [Public School] 2018-19`) %>%
  mutate(nces.id = as.numeric(nces.id))

#### DATA CLEANING ####

# the NCES data sometimes leaves data on certain races as NA instead of "0", if they do not
# have students of a certain race but still reported percentages for other races
# this section finds which schools have NAs that need to be replaced with "0"

schools.no.race.data <- schools.orig %>%
  filter(
    is.na(`pct.race.two (from NCES ID)`) &
            is.na(`pct.race.black (from NCES ID)`) &
                    is.na(`pct.race.hispanic (from NCES ID)`) &
                            is.na(`pct.race.white (from NCES ID)`) &
                                    is.na(`pct.race.asian.pi (from NCES ID)`)&
                                            is.na(`pct.race.native (from NCES ID)`) 
  )

schools.complete.race.data <- schools.orig %>%
  filter(
    !is.na(`pct.race.two (from NCES ID)`) &
      !is.na(`pct.race.black (from NCES ID)`) &
      !is.na(`pct.race.hispanic (from NCES ID)`) &
      !is.na(`pct.race.white (from NCES ID)`) &
      !is.na(`pct.race.asian.pi (from NCES ID)`)&
      !is.na(`pct.race.native (from NCES ID)`) 
  )

temp <- schools.orig %>%
  anti_join(rbind(schools.complete.race.data, schools.no.race.data))

fill.zeros <- temp$SchoolName

#### PREPARE SCHOOL DATA FRAME ####  

# this preps the school data for leaflet
schools <- schools.orig %>%
  rename(nces.id=`NCES School Code`)%>%
  mutate(nces.id = as.numeric(nces.id)) %>%
  mutate(num.comp.tea.app = str_count(`Teacher App Status (from Application-Teacher)`, "Complete")) %>%
  mutate(study.app.comp = case_when(
    str_count(`Study Status`, "Study App Complete") > 0 &
      str_count(`Study Status`, "Ineligible") < 1 ~ "Yes",
    TRUE ~ "No"
  )) %>%
  mutate(school.app.comp.temp = str_count(`School Appl  Status (from Application-School)`, "Complete")) %>%
  mutate(school.app.inelig = str_count(`School Appl  Status (from Application-School)`, "Ineligible")) %>%
  mutate(school.app.comp = case_when(
    school.app.comp.temp == 1 & school.app.inelig != 1 ~ "Yes",
    TRUE ~ "No"
  )) %>%
  mutate(study.app.inelig = str_count(`Study Status`, "Ineligible")) %>%
  mutate(ineligible = case_when(
    school.app.inelig == 1 | study.app.inelig == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  left_join(nces_location_data) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))  %>%
  mutate(`pct.race.black (from NCES ID)` = ifelse(((is.na(`pct.race.black (from NCES ID)`) & SchoolName %in% fill.zeros)), 0, `pct.race.black (from NCES ID)`)) %>%
  mutate(`pct.race.hispanic (from NCES ID)` = ifelse(((is.na(`pct.race.hispanic (from NCES ID)`) & SchoolName %in% fill.zeros)), 0, `pct.race.hispanic (from NCES ID)`)) %>%
  mutate(`pct.race.native (from NCES ID)` = ifelse(((is.na(`pct.race.native (from NCES ID)`) & SchoolName %in% fill.zeros)), 0, `pct.race.native (from NCES ID)`)) %>%
  mutate(pct.diverse = as.numeric((`pct.race.black (from NCES ID)`+`pct.race.hispanic (from NCES ID)`+`pct.race.native (from NCES ID)`))) %>%
  select(nces.id, SchoolName, `orgName (from District/Organization)`,`charter (from NCES ID)`, Rural, `# Teachers (Manual)`,longitude,latitude,state, pct.diverse, num.comp.tea.app, study.app.comp, school.app.comp, ineligible) %>%
  mutate(`charter (from NCES ID)` = case_when(
    `charter (from NCES ID)` == "1 checked out of 1" ~ "Yes",
    `charter (from NCES ID)` == "0 checked out of 1" ~ "No")) %>%
  mutate(Rural = case_when(
    Rural == "YES" ~ "Rural",
    Rural == "NO" ~ "Not Rural")) %>%
  filter(ineligible != 1) %>%
  mutate(Rural = as.factor(Rural)) %>%
  filter(!is.na(longitude)) %>%
  mutate(recruitment_category = case_when(
    (Rural == "Rural" & pct.diverse >= 50) ~ "Rural & Diverse",
    (Rural == "Rural" & pct.diverse < 50) ~ "Rural & Not Diverse",
    (Rural == "Not Rural" & pct.diverse >= 50) ~ "Not Rural & Diverse",
    (Rural == "Not Rural" & pct.diverse < 50) ~ "Not Rural & Not Diverse",)) %>%
  mutate(recruitment_category = as.factor(recruitment_category)) %>%
  mutate(diversity = case_when(
    pct.diverse >= 50 ~ "Diverse",
    pct.diverse < 50 ~ "Not Diverse"
  )) %>%
  mutate(diversity = as.factor(diversity)) %>%
  mutate(Rural = relevel(Rural, "Rural"))


#### PREPARE STATE DATA FRAME FOR CHOLOROPLETH LAYER ####
highlight_states <- schools %>%
  count(state)

states <- states(cb=T)

state_app_count <- geo_join(states, highlight_states, "STUSPS", "state") 
state_app_count <- subset(state_app_count, !is.na(n))


#### COLOR PALETTES AND MARKERS FOR ENTIRE SCHOOLS DATA FRAME####
pal_state <- colorNumeric("Blues", domain=state_app_count$n)

popup_state <- paste0("Schools: ", as.character(state_app_count$n))

popup_marker <- paste0("<strong>District: </strong>", 
                       schools$`orgName (from District/Organization)`,
                       "</br>",
                       "<strong>Charter: </strong>",
                       schools$`charter (from NCES ID)`,
                       "</br>",
                       "<strong>Study App Complete: </strong>", 
                       schools$study.app.comp,
                       "</br>",
                       "<strong>School App Complete: </strong>", 
                       schools$school.app.comp,
                       "</br>",
                       "<strong>Complete Teacher Apps: </strong>", 
                       schools$num.comp.tea.app)

pal_category <- colorFactor(palette = c("#16E4E1", "#4EE416", "#E47D16", "#E41645"),
                   levels = c("Rural & Diverse", "Rural & Not Diverse", "Not Rural & Diverse", "Not Rural & Not Diverse"))
                  
pal_rural <- colorFactor(palette = c("#1654E4","#CE16E4"),
                            levels = c("Not Rural","Rural"))

pal_diversity <- colorFactor(palette = c("#F3B24D", "#50F12C"),
                         levels = c("Diverse", "Not Diverse"))



#### COLOR PALETTES AND MARKERS FOR "STUDY APP COMPLETE" SUBSET OF SCHOOLS DATA FRAME####
schools2 <- schools[schools$study.app.comp=="Yes",]
highlight_states2 <- schools2 %>%
  count(state)

states <- states(cb=T)

state_app_count2 <- geo_join(states, highlight_states2, "STUSPS", "state") 
state_app_count2 <- subset(state_app_count2, !is.na(n))


pal_state2 <- colorNumeric("Blues", domain=state_app_count2$n)

popup_state2 <- paste0("Schools: ", as.character(state_app_count2$n))

popup_marker2 <- paste0("<strong>District: </strong>", 
                        schools2$`orgName (from District/Organization)`,
                        "</br>",
                        "<strong>Charter: </strong>",
                        schools2$`charter (from NCES ID)`,
                        "</br>",
                        "<strong>Study App Complete: </strong>", 
                        schools2$study.app.comp,
                        "</br>",
                        "<strong>School App Complete: </strong>", 
                        schools2$school.app.comp,
                        "</br>",
                        "<strong>Complete Teacher Apps: </strong>", 
                        schools2$num.comp.tea.app)

pal_category2 <- colorFactor(palette = c("#16E4E1", "#4EE416", "#E47D16", "#E41645"),
                             levels = c("Rural & Diverse", "Rural & Not Diverse", "Not Rural & Diverse", "Not Rural & Not Diverse"))

pal_rural2 <- colorFactor(palette = c("#1654E4","#CE16E4"),
                          levels = c("Not Rural","Rural"))

pal_diversity2 <- colorFactor(palette = c("#F3B24D", "#50F12C"),
                              levels = c("Diverse", "Not Diverse"))


#### MAKING ICONS  ####

location_marker <- makeIcon(
  iconUrl = "~/Desktop/location.svg",
  iconWidth = 20, iconHeight = 40,
  iconAnchorX = 10, iconAnchorY = 30
)

#### PUTTING MAP TOGETHER ####
schools.complete.app <- schools[schools$study.app.comp == "Yes",]

map_v1 <- leaflet(schools) %>% 
  setView(lng = -98.5795, lat = 39.8283, zoom = 3) %>%
  addTiles() %>%
  #chloropleth
  addMapPane("level 1", zIndex = 410) %>%
  addMapPane("level 2", zIndex = 420) %>%
  addMapPane("level 3", zIndex = 430) %>%
  addMapPane("level 4", zIndex = 440) %>%
  addPolygons(data = state_app_count , 
              fillColor = ~pal_state(state_app_count$n), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_state,
              group = "# Recruited Schools per State",
              options = pathOptions(pane = "level 1")) %>%
  #chlorpleth legend
  addLegend(pal = pal_state, 
            values = state_app_count$n, 
            position = "bottomright", 
            title = "# Recruited Schools per State",
            group = "Legends") %>%
  #diversity color markers
  addCircleMarkers(
    radius = 8,
    color = ~pal_diversity(diversity),
    stroke = FALSE, fillOpacity = 1,
    label= ~SchoolName,
    popup = popup_marker,
    popupOptions = popupOptions(),
    group = "Diversity",
    options = pathOptions(pane = "level 2") )%>%
  #diversity color markers legend
  addLegend("bottomright", pal = pal_diversity, values = ~diversity,
            title = "Recruited Schools",
            opacity = 1,
            group = "Legends") %>%
  addCircleMarkers(
    radius = 4,
    color = ~pal_rural(Rural),
    stroke = FALSE, fillOpacity = 1,
    label= ~SchoolName,
    popup = popup_marker,
    popupOptions = popupOptions(),
    group = "Rural" ,
    options = pathOptions(pane = "level 3"))%>%
  #rural legend
  addLegend("bottomright", pal = pal_rural, values = ~Rural,
            title = "Recruited Schools",
            opacity = 1,
            group = "Legends") %>%
  
  addMarkers(
    data = schools.complete.app,
    icon = location_marker,
    label= ~SchoolName,
    popup = popup_marker2,
    group = "Study App Complete Marker" ,
    options = pathOptions(pane = "level 4")
    
  ) %>%

  addLayersControl(
    overlayGroups = c("Study App Complete Marker", "Rural", "Diversity", "# Recruited Schools per State", "Legends"),
    options = layersControlOptions(collapsed = FALSE)
  )


#### SAVE AND UPLOAD HTML FILE TO BOX ####

# saveWidget(map_v1, file = "recruitment_map_v1.html")
# box_ul(#folderdirectory, "recruitment_map_v1.html")
