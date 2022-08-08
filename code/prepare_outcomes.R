

##### 0. PACKAGES, WD, OBJECTS #####


### WORKING DIRECTORY SHOULD BE CORRECT IF THIS SCRIPT IS RUN WITHIN R_project_for_individual_runs
### OR CALLED FROM LUCFP PROJECT master.do FILE.
### IN ANY CASE IT SHOULD BE (~/LUCFP/data_processing) 


### PACKAGES ###
# see this project's README for a better understanding of how packages are handled in this project. 

# These are the packages needed in this particular script. *** these are those that we now not install: "rlist","lwgeom","htmltools", "iterators", 
neededPackages <- c("fixest", # for some reason, it's necessary that fixest is loaded before other packages (I don"t know exactly which ones)
                    "data.table", "plyr", "tidyr", "dplyr",  "Hmisc", "sjmisc", "stringr",
                    "here", "foreign", "readxl", "writexl",
                    "raster", "rgdal", "sp", "sf", 
                    "knitr", "kableExtra",
                    "DataCombine", 
                    "boot",  "sandwich",# "fwildclusterboot",
                    "ggplot2", "dotwhisker", "leaflet", "htmltools", "viridis", "hrbrthemes")
# Install them in their project-specific versions
renv::restore(packages = neededPackages)

# Load them
lapply(neededPackages, library, character.only = TRUE)

# /!\/!\ IF renv::restore(neededPackages) FAILS TO INSTALL SOME PACKAGES /!\/!\ 

# For instance sf could cause trouble https://github.com/r-spatial/sf/issues/921 
# or magick, as a dependency of raster and rgdal. 

# FOLLOW THESE STEPS:
# 1. Remove these package names from neededPackages above, and rerun renv::restore(packages = neededPackages)
# 2. Write them in troublePackages below, uncomment, and run the following code chunk: 

# # /!\ THIS BREAKS THE PROJECT REPRODUCIBILITY GUARANTY /!\
# troublePackages <- c() 
# # Attempt to load packages from user's default libraries.
# lapply(troublePackages, library, lib.loc = default_libraries, character.only = TRUE)

# 3. If the troubling packages could not be loaded ("there is no package called ...") 
#   you should try to install them, preferably in their versions stated in the renv.lock file. 
#   see in particular https://rstudio.github.io/renv/articles/renv.html 


# # # /!\ THIS BREAKS THE PROJECT REPRODUCIBILITY GUARANTY /!\
# troublePackages <- c("leaflet", "leaflet.providers", "png")
# # Attempt to load packages from user's default libraries.
# lapply(troublePackages, library, lib.loc = default_libraries, character.only = TRUE)

### WORKING DIRECTORY SHOULD BE CORRECT IF THIS SCRIPT IS RUN WITHIN R_project_for_individual_runs
### OR CALLED FROM LUCFP PROJECT master.do FILE.
### IN ANY CASE IT SHOULD BE (~/LUCFP/data_processing

### NEW FOLDERS USED IN THIS SCRIPT 
dir.create(here("temp_data"))
dir.create(here("temp_data","country_nourishment"))
# dir.create(here("temp_data","country_population"))





# ------------------------- POPULATION -------------------------------------------------
pop <- read_xlsx(here("input_data", "outcome_variables", "WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx"), 
                 skip = 16) %>% as.data.frame()

pop_save <- pop 

names(pop)[names(pop)=="Region, subregion, country or area *"] <- "country"
names(pop)[names(pop)=="Total Population, as of 1 July (thousands)"] <- "population_kcapita"
names(pop)[names(pop) == "Year"] <- "year"


# trim 
# unique(pop$Index)
# unique(pop$Variant) # only Estimates
# unique(pop$Notes)
# unique(pop$Type)
# unique(pop$`Parent code`) 
pop <- pop[, c("country", "Type", "year", "population_kcapita")]
pop <- dplyr::filter(pop, Type == "Country/Area") # this removes NAs from year 
pop <- dplyr::select(pop, -Type)
unique(pop$year) # 1950 - 2021  
pop <- dplyr::filter(pop, (year %in% c(2000:2022) | is.na(year)))
unique(pop$year) 
head(pop)

# the data is balanced and misses no value 
length(unique(pop$country)) == nrow(pop)/length(unique(pop$year))
anyNA(pop)


# lengths_years <- ddply(pop, "country", summarise, 
#                         lgt_y = length(unique(year)))
# lengths_years[lengths_years$lgt_y!=21,]

# Make pop var numeric 
pop$population_kcapita <- as.numeric(pop$population_kcapita)

# Make pre-treatment period population 
pop_2007 <- dplyr::filter(pop, year == 2007)
names(pop_2007)[names(pop_2007)=="population_kcapita"] <- "population_kcapita_2007"
pop <- left_join(pop, pop_2007[,c("country", "population_kcapita_2007")], by = "country")

####  UNDERNOURISHMENT #### 
# Share of people who are undernourished in the total population
# "Undernourishment measures the share of the population that has a caloric intake which is insufficient to meet the
# minimum energy requirements necessary for a given individual." 
# See FAOSTAT-foodsecurity_Descriptions_and_Metadata.xlsx for detailed definitions
undn <- read.csv(here("input_data", "outcome_variables", "FAOSTAT-foodsecurity_FAOall_value_20002021.csv"))
head(undn)

# the domain is the common to the whole data set "Food Balances (-2013, old methodology and population)" so we can remove it
unique(undn$Domain) 

undn <- undn[, !grepl("Domain", names(undn))]

## Area and area code are bijective 
length(unique(undn$Area)) == length(unique(undn$Area.Code..FAO.)) 
undn <- dplyr::select(undn, -Area.Code..FAO.)

names(undn)[names(undn)=="Area"] <- "country"

## Element is Value
unique(undn$Element)
undn <- undn[, !grepl("Element", names(undn))]

## Item 
unique(undn$Item)
# [1] "Prevalence of undernourishment (percent) (3-year average)"                                                 
# [2] "Prevalence of severe food insecurity in the total population (percent) (3-year average)"                   
# [3] "Prevalence of severe food insecurity in the male adult population (percent) (3-year average)"              
# [4] "Prevalence of severe food insecurity in the female adult population (percent) (3-year average)"            
# [5] "Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)"       
# [6] "Prevalence of moderate or severe food insecurity in the male adult population (percent) (3-year average)"  
# [7] "Prevalence of moderate or severe food insecurity in the female adult population (percent) (3-year average)"
# [8] "Prevalence of undernourishment (percent) (annual value)" 

# Items and Item.Code are bijective
length(unique(undn$Item)) == length(unique(undn$Item.Code))
undn <- dplyr::select(undn, -Item.Code)


# We keep only [1] [2] and [5] ([8] is feature but actually there is no data for it, only China has it, and its empty)
undn$Item[undn$Item=="Prevalence of undernourishment (percent) (3-year average)"] <- "undernourished"
undn$Item[undn$Item=="Prevalence of severe food insecurity in the total population (percent) (3-year average)"] <- "foodinsecu_severe"
undn$Item[undn$Item=="Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)"] <- "foodinsecu_modsevere"

undn <- dplyr::filter(undn, Item %in% c("undernourished", "foodinsecu_severe", "foodinsecu_modsevere"))#

head(undn)

## Year
undn <- dplyr::select(undn, -Year.Code)

names(undn)[names(undn) == "Year"] <- "year"

unique(undn$year)
for(year in 2000:2021){ 
  # determine moving average bounds
  mab <- paste0(year-1,"-",year+1)
  undn$year[undn$year == mab] <- year
}
undn$year <- as.numeric(undn$year)
unique(undn$year)

## Unit 
unique(undn$Unit)
undn <- dplyr::select(undn, -Unit)

## For the moment, do not bother Flags,nor notes
undn <- undn[, !grepl("Flag", names(undn))]
undn <- dplyr::select(undn, -Note)


# Now split data by Item
unique(undn$Item)

wide_ds <- stats::reshape(undn,
                          # varying = unique(long_ds$Item),
                          # v.names = c("Value"),
                          sep = ".",
                          timevar = "Item",
                          idvar = c("country", "year"),
                          direction = "wide",
                          new.row.names = NULL)  

vars_slct <- grepl("Value.", names(wide_ds))

# those variables that have been reshaped, give the Item identifier to their names
names(wide_ds)[vars_slct] <- gsub(pattern = "Value.", replacement = "", names(wide_ds)[vars_slct])

head(undn, 20)
head(wide_ds, 20)


## Handle missing values 
wide_ds[grepl("NA", wide_ds$undernourished), "undernourished"] <- ""
wide_ds[grepl("NA", wide_ds$foodinsecu_severe), "foodinsecu_severe"] <- ""
wide_ds[grepl("NA", wide_ds$foodinsecu_modsevere), "foodinsecu_modsevere"] <- ""

wide_ds[is.na(wide_ds$undernourished), "undernourished"] <- ""
wide_ds[is.na(wide_ds$foodinsecu_severe), "foodinsecu_severe"] <- ""
wide_ds[is.na(wide_ds$foodinsecu_modsevere), "foodinsecu_modsevere"] <- ""


## Coerce character strings into numeric, and in particular "<2.5". 
# Most countries that have <2.5 have no temporal variation in it. Therefore, they will be removed automatically from analysis. 
# Hence, the choice of the value given to 2.5 is not very important. We attribute the mid-point 1.25 by default. 
wide_ds[wide_ds$undernourished=="<2.5", "undernourished"] <- "1.25"
wide_ds[wide_ds$foodinsecu_severe=="<2.5", "foodinsecu_severe"] <- "1.25"
wide_ds[wide_ds$foodinsecu_modsevere=="<2.5", "foodinsecu_modsevere"] <- "1.25"

wide_ds$undernourished <- as.numeric(wide_ds$undernourished)
wide_ds$foodinsecu_severe <- as.numeric(wide_ds$foodinsecu_severe)
wide_ds$foodinsecu_modsevere <- as.numeric(wide_ds$foodinsecu_modsevere)

summary(wide_ds$foodinsecu_modsevere)


# Explore those that are always missing
# avg_undn <- ddply(wide_ds, "country", summarise, avg_undn = mean(undernourished, na.rm = TRUE))
# always_na <- avg_undn[is.na(avg_undn$avg_undn), "country"] %>% unique()

# add mozambique data from World Bank manually, as it is available there but not from FAO 
# (I have added the NAs)
moz_undn_preval <- c(NA, 36.5,	35.2,	34.1,	34.1,	33.3,	32.4,	29.9,	28.6,	25.8,	24.1,	21.8,	21,	22.4,	25.7,	29.7,	31.5,	31.4,	31,	31.2, NA, NA)
names(moz_undn_preval) <- 2000:2021
wide_ds[wide_ds$country=="Mozambique", "undernourished"] <- moz_undn_preval
# Add Somalia as well, although exposure data is missing for this country
som_undn_preval <- c(NA, 57.9,	57.9,	58,	58.1,	58.2,	58.2,	58.2,	58.2,	67.1,	75.6,	81.7,	79.7,	71.3,	65.7,	60.3,	57.8,	58.7,	57.4,	59.5, NA, NA)
names(som_undn_preval) <- 2000:2021
wide_ds[wide_ds$country=="Somalia", "undernourished"] <- som_undn_preval


#### STUNTING #### 
# https://ourworldindata.org/hunger-and-undernourishment#too-little-height-for-age-stunting
# "Children who are stunted are determined as having a height which falls two standard deviations below 
# the median height-for-age of the World Health Organization’s Child Growth Standards.
# Stunting is an indicator of severe malnutrition. Unlike wasting and low weight-for-age, the impacts of stunting on 
# child development are considered to be largely irreversible beyond the first 1000 days of a child’s life."

# Here there is only one variable: the share of the under-5 population who are defined as stunted. 
# "Note that many countries report stunting prevalence through periodic health and demographic surveys, 
# meaning that this data is often not available on an annual basis." 

stun <- read.csv(here("input_data", "outcome_variables", "share-of-children-younger-than-5-who-suffer-from-stunting.csv"))

head(stun)

names(stun) <- c("Area", "Code", "year", 
                 "stunting")

names(stun)[names(stun)=="Area"] <- "country"

stun$year <- as.numeric(stun$year)
unique(stun$year) %>% sort()
stun <- dplyr::filter(stun, year >= 2000)
length_ctry <- ddply(stun, "country", summarise, length_ctry=length(unique(year)))
summary(length_ctry$length_ctry)

stun <- dplyr::select(stun, -Code)


#### WASTING #### 
# https://ourworldindata.org/hunger-and-undernourishment#too-little-weight-for-height-wasting
# "Wasting is defined as being dangerously thin for one’s height, and is generally a sign (especially in children) of rapid weight loss."
# Here there is only one variable: share of children under-5 suffering from wasting.  

wast <- read.csv(here("input_data", "outcome_variables", "share-of-children-with-a-weight-too-low-for-their-height-wasting.csv"))

head(wast)

names(wast) <- c("Area", "Code", "year", 
                "wasting")

names(wast)[names(wast)=="Area"] <- "country"

wast$year <- as.numeric(wast$year)

unique(wast$year) %>% sort()
wast <- dplyr::filter(wast, year >= 2000)
length_ctry <- ddply(wast, "country", summarise, length_ctry=length(unique(year)))
summary(length_ctry$length_ctry)

wast <- dplyr::select(wast, -Code)



#### MERGE OUTCOME VARIABLES ----------------------------------------------------
# Requires to handle matching key: country names
# 1. match modif made in prepare_exposures too 
wide_ds$country[wide_ds$country=="CÃ´te d'Ivoire"] <- "Ivory Coast" 
wide_ds$country[wide_ds$country=="C?te d'Ivoire"] <- "Ivory Coast"
wide_ds$country[wide_ds$country=="Côte d'Ivoire"] <- "Ivory Coast"
wide_ds$country[wide_ds$country=="TÃ¼rkiye"] <- "Turkey"
wide_ds$country[wide_ds$country=="T?rkiye"] <- "Turkey" 
wide_ds$country[wide_ds$country=="Türkiye"] <- "Turkey"
wide_ds$country[wide_ds$country=="Congo"] <- "Republic of the Congo"

# China 
wide_ds$country[wide_ds$country == "China, Taiwan Province of"] <- "Taiwan"
# Remove China (which aggregates China mainland and Taiwan), and keep only China mainland 
wide_ds <- dplyr::filter(wide_ds, country != "China")
# and remove Hong Kong and Macao 
wide_ds <- dplyr::filter(wide_ds, country != "China, Hong Kong SAR")
wide_ds <- dplyr::filter(wide_ds, country != "China, Macao SAR")

wide_ds <- dplyr::filter(wide_ds, country != "French Polynesia")
wide_ds <- dplyr::filter(wide_ds, country != "Netherlands Antilles (former)")
wide_ds <- dplyr::filter(wide_ds, country != "Bermuda")
wide_ds <- dplyr::filter(wide_ds, country != "New Caledonia")

# the only difference between these two are the consequence of the changes just above 
target_c <- unique(wide_ds$country)
undn_c <- unique(undn$country)

pop_c <- unique(pop$country)
stun_c <- unique(stun$country)
wast_c <- unique(wast$country)

length(target_c)
length(undn_c) # 204
length(pop_c) # 237
length(stun_c) # 138
length(wast_c) # 150

length(unique(c(stun_c, wast_c))) # 151 



# Match UN POPULATION to FAO 
pop_c[!(pop_c %in% target_c)] # mostly islands (UN and FAO have mostly similar naming)

target_c[grepl("ambique", target_c)]
pop_c[grepl("ambique", pop_c)]

pop$country[pop$country=="CÃ´te d'Ivoire"] <- "Ivory Coast" 
pop$country[pop$country=="C?te d'Ivoire"] <- "Ivory Coast"
pop$country[pop$country=="Côte d'Ivoire"] <- "Ivory Coast"
pop$country[pop$country=="TÃ¼rkiye"] <- "Turkey"
pop$country[pop$country=="T?rkiye"] <- "Turkey" 
pop$country[pop$country=="Türkiye"] <- "Turkey"
pop$country[pop$country=="China"] <- "China, mainland"
pop$country[pop$country=="China, Taiwan Province of China"] <- "Taiwan"
pop$country[pop$country=="Congo"] <- "Republic of the Congo"
pop$country[pop$country=="Dem. People's Republic of Korea"] <- "Democratic People's Republic of Korea"
pop$country[pop$country=="Micronesia (Fed. States of)"] <- "Micronesia (Federated States of)"
pop$country[pop$country=="State of Palestine"] <- "Palestine"
pop$country[pop$country=="United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland"

pop[pop$country == "Western Sahara", "population_kcapita"] %>% as.numeric() %>% summary() # it's >~1% of the population of Morocco (without WS). 
# I don't know how it is counted in terms of food outcomes by FAOSTAT, so let's just ignore (i.e. leave it like this)
pop[pop$country == "Morocco", "population_kcapita"] %>% as.numeric() %>% summary()


stun_c[!(stun_c %in% target_c)]
wast_c[!(wast_c %in% target_c)]

# adjust stunting data country names
stun$country[stun$country=="China"] <- "China, mainland"
stun$country[stun$country=="Bolivia"] <- "Bolivia (Plurinational State of)"
stun$country[stun$country=="Brunei"] <- "Brunei Darussalam"
stun$country[stun$country=="Cote d'Ivoire"] <- "Ivory Coast"
stun$country[stun$country=="Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
stun$country[stun$country=="Congo"] <- "Republic of the Congo"
stun$country[stun$country=="Iran"] <- "Iran (Islamic Republic of)"
stun$country[stun$country=="Laos"] <- "Lao People's Democratic Republic"
stun$country[stun$country=="Moldova"] <- "Republic of Moldova"
stun$country[stun$country=="North Korea"] <- "Democratic People's Republic of Korea"
stun$country[stun$country=="South Korea"] <- "Republic of Korea"
stun$country[stun$country=="Syria"] <- "Syrian Arab Republic"
stun$country[stun$country=="Tanzania"] <- "United Republic of Tanzania"
stun$country[stun$country=="Timor"] <- "Timor-Leste"
stun$country[stun$country=="United States"] <- "United States of America"
stun$country[stun$country=="Venezuela"] <- "Venezuela (Bolivarian Republic of)"
stun$country[stun$country=="Vietnam"] <- "Viet Nam"

# adjust wasting data country names
wast$country[wast$country=="China"] <- "China, mainland"
wast$country[wast$country=="Bolivia"] <- "Bolivia (Plurinational State of)"
wast$country[wast$country=="Brunei"] <- "Brunei Darussalam"
wast$country[wast$country=="Cote d'Ivoire"] <- "Ivory Coast"
wast$country[wast$country=="Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
wast$country[wast$country=="Congo"] <- "Republic of the Congo"
wast$country[wast$country=="Iran"] <- "Iran (Islamic Republic of)"
wast$country[wast$country=="Laos"] <- "Lao People's Democratic Republic"
wast$country[wast$country=="Moldova"] <- "Republic of Moldova"
wast$country[wast$country=="North Korea"] <- "Democratic People's Republic of Korea"
wast$country[wast$country=="South Korea"] <- "Republic of Korea"
wast$country[wast$country=="Syria"] <- "Syrian Arab Republic"
wast$country[wast$country=="Tanzania"] <- "United Republic of Tanzania"
wast$country[wast$country=="Timor"] <- "Timor-Leste"
wast$country[wast$country=="United States"] <- "United States of America"
wast$country[wast$country=="Venezuela"] <- "Venezuela (Bolivarian Republic of)"
wast$country[wast$country=="Vietnam"] <- "Viet Nam"



stun_c <- unique(stun$country)
wast_c <- unique(wast$country)
pop_c <- unique(pop$country)
# the discrepancies that remain are only those with regions of the world, or small islands / territories for UN pop data 
pop_c[!(pop_c %in% target_c)] 
stun_c[!(stun_c %in% target_c)]
wast_c[!(wast_c %in% target_c)]

# the inner join with UN pop data gives the same number of observations  as the wide_ds (target) data. 
nrow(wide_ds) == inner_join(wide_ds, pop, by = c("country", "year")) %>% nrow()


outcomes <- left_join(wide_ds, pop, by = c("country", "year")) 

outcomes <- left_join(outcomes, stun, by = c("country", "year")) 

outcomes <- left_join(outcomes, wast, by = c("country", "year"))

nrow(outcomes) == nrow(wide_ds)


# those have undernourished data neither here nor in OWID
outcomes[grepl("babwe",outcomes$country),]
outcomes[grepl("ganda",outcomes$country),]
wide_ds[grepl("ganda",wide_ds$country),]
outcomes[outcomes$country=="Guinea",] 

outcomes[grepl("Zambia",outcomes$country),] # this has data, but not in OWID, and not in avg_preval
wide_ds[grepl("Zambia",wide_ds$country),] 

outcomes[grepl("lgeria",outcomes$country),] # this has data, but not in OWID
outcomes[grepl("iger",outcomes$country),] # this has data, but not in OWID




#### Handle Serbia and Montenegro and Sudan and South Sudan ####
# We aggregate these countries because they were not distinct in the pre-treatment period from which we build the exposures
# We aggregate by average with population weights (with population sizes taken from Google searches for same years). 
# For Serbia and Montenegro, set weights at 9/10 and 1/10 resp. 
sm <- outcomes[outcomes$country%in%c("Serbia", "Montenegro"),] 
sm <- dplyr::mutate(sm, 
                     w_undernourished = if_else(country=="Montenegro", 
                                                true = undernourished*0.1, 
                                                false = undernourished*0.9), 
                     w_foodinsecu_severe = if_else(country=="Montenegro", 
                                                       true = foodinsecu_severe*0.1, 
                                                       false = foodinsecu_severe*0.9), 
                     w_foodinsecu_modsevere = if_else(country=="Montenegro", 
                                                          true = foodinsecu_modsevere*0.1, 
                                                          false = foodinsecu_modsevere*0.9), 
                     w_stunting = if_else(country=="Montenegro", 
                                          true = stunting*0.1, 
                                          false = stunting*0.9), 
                     w_wasting = if_else(country=="Montenegro", 
                                         true = wasting*0.1, 
                                         false = wasting*0.9))


# Aggregate within years
sm <- ddply(.data = sm, .variables = "year", summarise, 
             undernourished = if_else(is.na(sum(undernourished, na.rm = TRUE)),  # if one of the two country is NA this year
                                      true = mean(undernourished, na.rm = FALSE), # then take the value from the non-NA one (if any)
                                      false = sum(w_undernourished, na.rm = TRUE)), # else (none is NA), make the weighted average
             
             foodinsecu_severe = if_else(is.na(sum(foodinsecu_severe, na.rm = TRUE)),  
                                             true = mean(foodinsecu_severe, na.rm = FALSE), 
                                             false = sum(w_foodinsecu_severe, na.rm = TRUE)), 
             
             foodinsecu_modsevere = if_else(is.na(sum(foodinsecu_modsevere, na.rm = TRUE)), 
                                                true = mean(foodinsecu_modsevere, na.rm = FALSE), 
                                                false = sum(w_foodinsecu_modsevere, na.rm = TRUE)), 
             
             stunting = if_else(is.na(sum(stunting, na.rm = TRUE)),  
                                true = mean(stunting, na.rm = FALSE), 
                                false = sum(w_stunting, na.rm = TRUE)), 
             
             wasting = if_else(is.na(sum(wasting, na.rm = TRUE)),
                               true = mean(wasting, na.rm = FALSE), 
                               false = sum(w_wasting, na.rm = TRUE)), 
            
            # And make the sum of their populations
            population_kcapita = sum(population_kcapita, na.rm = TRUE), # there is no NA in pop data anyway
            population_kcapita_2007 = sum(population_kcapita_2007, na.rm = TRUE) 
)

# replace the two countries by the group of them two
sm$country <- "Serbia and Montenegro" # rbind "matches columns by name (rather than by position)." so it does not matter that country is not at the same position
outcomes <- rbind(outcomes, sm) 
outcomes <- dplyr::filter(outcomes, 
                          country != "Serbia" & country != "Montenegro")

# For Sudan and South Sudan, set weights at 34.5/44 and 9.5/44 resp. 
sss <- outcomes[outcomes$country%in%c("Sudan", "South Sudan"),] 
sss <- dplyr::mutate(sss, 
                     w_undernourished = if_else(country=="South Sudan", 
                                                     true = undernourished*9.5/44, 
                                                     false = undernourished*34.5/44), 
                     w_foodinsecu_severe = if_else(country=="South Sudan", 
                                                true = foodinsecu_severe*9.5/44, 
                                                false = foodinsecu_severe*34.5/44), 
                     w_foodinsecu_modsevere = if_else(country=="South Sudan", 
                                                true = foodinsecu_modsevere*9.5/44, 
                                                false = foodinsecu_modsevere*34.5/44), 
                     w_stunting = if_else(country=="South Sudan", 
                                                true = stunting*9.5/44, 
                                                false = stunting*34.5/44), 
                     w_wasting = if_else(country=="South Sudan", 
                                                true = wasting*9.5/44, 
                                                false = wasting*34.5/44))

# Aggregate within years
sss <- ddply(.data = sss, .variables = "year", summarise, 
            undernourished = if_else(is.na(sum(undernourished, na.rm = TRUE)),  # if one of the two country is NA this year
                                     true = mean(undernourished, na.rm = FALSE), # then take the value from the non-NA one (if any)
                                     false = sum(w_undernourished, na.rm = TRUE)), # else (none is NA), make the weighted average

            foodinsecu_severe = if_else(is.na(sum(foodinsecu_severe, na.rm = TRUE)),  
                                            true = mean(foodinsecu_severe, na.rm = FALSE), 
                                            false = sum(w_foodinsecu_severe, na.rm = TRUE)), 

            foodinsecu_modsevere = if_else(is.na(sum(foodinsecu_modsevere, na.rm = TRUE)), 
                                               true = mean(foodinsecu_modsevere, na.rm = FALSE), 
                                               false = sum(w_foodinsecu_modsevere, na.rm = TRUE)), 

            stunting = if_else(is.na(sum(stunting, na.rm = TRUE)),  
                               true = mean(stunting, na.rm = FALSE), 
                               false = sum(w_stunting, na.rm = TRUE)), 

            wasting = if_else(is.na(sum(wasting, na.rm = TRUE)),
                              true = mean(wasting, na.rm = FALSE), 
                              false = sum(w_wasting, na.rm = TRUE)), 
            
            # And make the sum of their populations
            population_kcapita = sum(population_kcapita, na.rm = TRUE), # there is no NA in pop data anyway
            population_kcapita_2007 = sum(population_kcapita_2007, na.rm = TRUE)
            )


# replace the two countries by the group of them two
sss$country <- "Sudan (former)"
outcomes <- rbind(outcomes, sss)
outcomes <- dplyr::filter(outcomes, 
                          country != "Sudan" & country != "South Sudan")

# ---------------------------- MAKE ABSOLUTE VARIABLES ----------------------------------------------------------------------------- 
summary(outcomes$undernourished)
summary(outcomes$foodinsecu_severe)
summary(outcomes$foodinsecu_modsevere)
summary(outcomes$stunting)
summary(outcomes$wasting)

# Rename prevalence vars 
names(outcomes)[names(outcomes)=="undernourished"] <- "undernourished_preval"
names(outcomes)[names(outcomes)=="foodinsecu_severe"] <- "foodinsecu_severe_preval"
names(outcomes)[names(outcomes)=="foodinsecu_modsevere"] <- "foodinsecu_modsevere_preval"
names(outcomes)[names(outcomes)=="stunting"] <- "stunting_preval"
names(outcomes)[names(outcomes)=="wasting"] <- "wasting_preval"



outcomes <- dplyr::mutate(outcomes, across(.cols = contains("_preval"),
                            .fns = ~.*0.01*population_kcapita, 
                            .names = paste0("{col}", "_kcapita")))

names(outcomes) <- gsub(pattern = "_preval_kcapita", replacement = "_kcapita", names(outcomes))



saveRDS(outcomes, file = here("temp_data", "country_nourishment", paste0("undernourished_stun_wast_",
                                                                         min(unique(outcomes$year)),
                                                                         max(unique(outcomes$year)),
                                                                         ".Rdata")))




#### OLD STUFFS, NOT USED -------------------------------------------------------
#### depth of the food deficit #### 
dfd <- read.csv(here("input_data", "outcome_variables", "depth-of-the-food-deficit.csv"))
# Here there is only one variable: the depth of the food deficit in kilocalories per person per day. 
# "The depth of the food deficit indicates how many calories would be needed to lift all undernourished people from their
# status, everything else being constant."
head(dfd)

names(dfd) <- c("Entity", "Code", "year", 
                "deficit_kcal_cpt_day")

length(unique(dfd$Entity))
unique(dfd$year)

#### supply of cereal kcal/capita/day #### 
cereals <- read.csv(here("input_data", "outcome_variables", "per-capita-consumption-of-cereals-by-commodity-type-daily-kilocalories.csv"))
# Variable names are "Food Balance Sheets: Oats - Food supply (kcal/capita/day) (FAO (2017))" 
# with the cereal taking values: Oats, Rye and products, Barley and products, Sorghum and products, Maize and products, Wheat and products, Rice Milled Equivalent
# They re all expressed in kcal/capita/day.
# " Breakdown of the average per capita intake of cereals by specific cereal-based commodity types, measured in kilocalories per
# person per day. This figure measures the primary equivalent of all food products derived from a given commodity (e.g. "wheat"
# represents the primary equivalent of its derived products). Data refers to cereal food supply at the consumer level but does not
# account for consumer wastage."

names(cereals)

unique(cereals$Entity)

names(cereals) <- c("Entity", "Code", "year", 
                    "oat_kcal_cpt_day",
                    "rye_kcal_cpt_day",
                    "barley_kcal_cpt_day",
                    "sorghum_kcal_cpt_day",
                    "maize_kcal_cpt_day",
                    "wheat_kcal_cpt_day", 
                    "rice_kcal_cpt_day")

cereals <- mutate(cereals, cereal_kcal_cpt_day = rowSums(across(.cols = contains("kcal_cpt_day")), na.rm = TRUE))

# cereal_kcal_cpt_day is an outcome to be used as such. Particular intakes from maize, for instance, can be looked at to see whether the effect hits directly through corn market



#### global hunger index ####
# The index score comprises of four key hunger indicators: prevalence of undernourishment; childhood wasting; childhood
# stunting; and child mortality. It is measured on a 100-point scale where 0 is the best score (no hunger) and 100 the worst.
ghi <- read.csv(here("input_data", "outcome_variables", "global-hunger-index.csv"))

head(ghi)
length(unique(ghi$Entity))
unique(ghi$Year) # there are only 4 years of data for every country 

names(ghi) <- c("Entity", "Code", "year", "GHI", "annotations")

unique(ghi$annotations)
ghi[ghi$annotations != "", ] # only 2021 observations

ghi <- dplyr::select(ghi, annotations)


#### staple food prices #### 
sfp <- read_excel(here("input_data", "FEWS_NET_Staple_Food_Price_Data.xlsx"))
head(sfp)
class(sfp)
sfp <- as.data.frame(sfp)
head(sfp)
length(unique(sfp$country))
# IT'S ONLY 13 COUNTRIES 

sfp <- dplyr::select(sfp, -source_document, -longitude, -latitude, admin_1)
unique(sfp$unit_type) # Weight, Itam, Volume 
unique(sfp$product_source) # Local, Import
unique(sfp$price_type) # Retail, Wage, Wholesale 




# manage time var
# sub(pattern = "\-(.*)", replacement = "", x= sfp$period_date)

nrow(sfp)
sfp$year <- sapply(str_split( sfp$period_date, pattern = "-"), FUN = function(date){date[1]}) 
head(sfp)
sfp$value <- as.numeric(sfp$value)
summary(sfp$value)
# this averages local prices over months and markets within (i.e. for every) a country, year, product type, currency, unit and source. 
sfp2 <- ddply(sfp, c("country", "year", "cpcv2", "currency", "unit", "product_source"), summarise, 
              staple_price = mean(value, na.rm = TRUE))

head(sfp2)
summary(sfp2$staple_price)

multicurrency <- ddply(sfp2, c("country", "cpcv2"), summarise, 
                       ncurrency = length(unique(currency)))


# there may be different 










