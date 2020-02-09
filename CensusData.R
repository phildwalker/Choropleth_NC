library(censusapi)

key <- c("xxxxxxxxx")


# Add key to .Renviron
Sys.setenv(CENSUS_KEY=key)
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

apis <- listCensusApis()


sahie_vars <- listCensusMetadata(name = "timeseries/healthins/sahie", 
                                 type = "variables")
head(sahie_vars)


acs_income_group <- getCensus(name = "acs/acs5", 
                              vintage = 2017, 
                              vars = c("NAME", "group(B19013)"), 
                              region = "tract:*", 
                              regionin = "state:37")



state_pop <-  getCensus(name="acs/acs5", 
                        vintage=2015,
                        # key=CENSUS_KEY, C
                        vars=c("NAME", "B01003_001E"), 
                        region="state:*37")

head(state_pop)

#-----------------------------
library(tidycensus)

census_api_key(key, install= TRUE)

# m90 <- get_decennial(geography = "state", variables = "H043A001", year = 1990)


censusVar <- load_variables(2017, "acs5", cache = TRUE)

# B00001_001 // UNWEIGHTED SAMPLE COUNT OF THE POPULATION
# B01003_001 // TOTAL POPULATION

# Alamance	37001
# Forsyth	37067
# Guilford	37081
# Randolph	37151
# Rockingham	37157


popVars <- c("B00001_001", "B01003_001")
counties <- c("Alamance", "Forsyth", "Guilford", "Randolph", "Rockingham")



bg.popn.acs <- get_acs(geography = "block group", variables = c(PopESt = "B01003_001"), 
                         state = "NC", county = counties, year = 2018)


bg_pop <- 
  bg.popn.acs %>% 
  separate(NAME, into = c("BG", "Tract", "County", "State"), sep = ",")


saveRDS(bg_pop, here::here("data", "ACS_PopEst.rds"))



