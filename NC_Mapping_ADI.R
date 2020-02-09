library(tidyverse)
library(tigris)
library(leaflet)


# read in and save the census block groups for the 5 counties to look at..
# Alamance	37001
# Forsyth	37067
# Guilford	37081
# Randolph	37151
# Rockingham	37157

Counties <- c("37001", "37067", "37081", "37151", "37157")

# NC_bg <- block_groups(state = "NC", county = c("001", "067", "081", "151", "157"), cb = FALSE)
# saveRDS(NC_bg, here::here("data", "NC_blockgroups.rds"))
NC_bg <- readRDS(here::here("data", "NC_blockgroups.rds"))


leaflet(NC_bg) %>%
  addTiles() %>%
  addPolygons(fillColor = "grey50",
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = ~NAMELSAD,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))


ADI <- read_csv(here::here("data", "NC_blockgroup_15.txt")) %>% 
  mutate(fips_char = as.character(fips))

ADI_filter <- 
  ADI %>% 
  filter(str_detect(fips, Counties)) 

  
BL_ADI <- geo_join(NC_bg, ADI, 'GEOID', 'fips_char', how = 'left')
  
# Distribution of the population per country?
BL_ADI@data %>% 
  ggplot( aes(x=as.numeric(adi_staternk))) + 
  geom_bar(bins=20, fill='#69b3a2', color='white') +
  xlab("ADI State Rank") + 
  scale_x_continuous(breaks = seq(0,10,1), labels = seq(0,10,1)) +
  theme_bw()  


pal <- colorNumeric("Greens", domain=BL_ADI$adi_staternk)

# Setting up the pop up text
popup_sb <- paste0("ADI State Rank: ", as.character(BL_ADI$adi_staternk))

popup_sb <- paste(
  "ADI State Rank: ", BL_ADI@data$adi_staternk,"<br/>", 
  "County: ", BL_ADI@data$COUNTYFP, "<br/>", 
  "Block Group: ", BL_ADI@data$NAMELSAD, 
  sep="") %>%
  lapply(htmltools::HTML)



leaflet(BL_ADI) %>%
  addTiles() %>%
  # setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(fillColor = ~pal(BL_ADI$adi_staternk), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_sb) %>%
  addLegend(pal = pal, 
            values = BL_ADI$adi_staternk, 
            position = "bottomright", 
            title = "ADI State Rank")











  
  
  
  
  
