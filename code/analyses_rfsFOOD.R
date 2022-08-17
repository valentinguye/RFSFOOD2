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
# "pglm", "multiwayvcov", "clusterSEs", "alpaca", "clubSandwich",

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
dir.create(here("temp_data","reg_results"))


### GLOBAL CRS USED ### 
mercator_world_crs <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

### OBJECTS USED IN RFS PROCESSES ###

### Outcomes data set 
outcomes <- readRDS(here("temp_data", "country_nourishment", "undernourished_stun_wast_20002021.Rdata"))



### Exposures data set 
# store them in a list to call either one easily
pretreat_year_sets <- list(`2001_2007` = c(2001:2007), 
                           `2004_2007` = c(2004:2007),
                           `2006_2007` = c(2006:2007))
exposures_list <- list()

for(period_name in names(pretreat_year_sets)){
  exposures_list[[period_name]] <- readRDS(here("temp_data", 
                                                "exposures", 
                                                paste0("dependency_",
                                                       min(pretreat_year_sets[[period_name]]),
                                                       max(pretreat_year_sets[[period_name]]),".Rdata")))
}

# Check that outcomes and exposures are ready to be merged (based on country names)
expo <- exposures_list[[1]]
expo_c <- unique(expo$country)
outcomes_c <- unique(outcomes$country)

# countries that are in outcome variables that are not in exposure variables 
outcomes_c[!(outcomes_c %in% expo_c)] # some small countries, + the important ones that are missing from food balance sheets. 
# countries that are in exposure, that are not in outcome
expo_c[!(expo_c %in% outcomes_c)]
# This should be empty, i.e. all countries in exposure data have a match in outcome data

# expo_c[grepl("onga", expo_c)]

class(outcomes$year)


####  MATCHING OBJECTS (Commodities -> Dependencies -> Outcomes) --------------------------------------------------------------

category_names <- c("cereals", "oilcrops", "vegetable_oils", "meat", "fish")
nutrients <- c("calorie", "protein", "fat")

# organize exposure variables (matching commodity names to dependencies)
exposures_names_list <- list(total = paste0("dependency_",nutrients,"_total"),
                             
                             cereals = paste0("dependency_",nutrients,"_cereals"), 
                             oilcrops = paste0("dependency_",nutrients,"_oilcrops"), 
                             vegetable_oils = paste0("dependency_",nutrients,"_vegetable_oils"), 
                             meat = paste0("dependency_",nutrients,"_meat"), 
                             fish = paste0("dependency_",nutrients,"_fish"), 
                             
                             wheat = paste0("dependency_",nutrients,"_wheat"), 
                             rice = paste0("dependency_",nutrients,"_rice"), 
                             maize = paste0("dependency_",nutrients,"_maize"), 
                             soybeans = paste0("dependency_",nutrients,"_soybeans"), 
                             soybean_oil = paste0("dependency_",nutrients,"_soybean_oil"), 
                             palm_oil = paste0("dependency_",nutrients,"_palm_oil"), 
                             sunflowerseed_oil = paste0("dependency_",nutrients,"_sunflowerseed_oil"), 
                             rapeseed_oil = paste0("dependency_",nutrients,"_rapeseed_oil") )

foodinsecu_exposures <- paste0("dependency_",category_names)

## matching exposures (dependencies) to outcomes
undernourished_exposures_sets <- list()
for(item in names(exposures_names_list)){
  undernourished_exposures_sets[[item]] <- paste0("dependency_",nutrients,"_",item)
} 

ch_malnutrition_exposures_sets <- list()
for(item in names(exposures_names_list)){
  ch_malnutrition_exposures_sets[[item]] <- paste0("dependency_",nutrients,"_",item)
}

# All elements are named lists themselves
exposure_outcome_map <- list(foodinsecu_modsevere_kcapita = list(categories = foodinsecu_exposures), 
                             foodinsecu_modsevere_preval = list(categories = foodinsecu_exposures), 
                             foodinsecu_severe_kcapita = list(categories = foodinsecu_exposures), 
                             foodinsecu_severe_preval = list(categories = foodinsecu_exposures), 
                             
                             undernourished_kcapita = undernourished_exposures_sets, 
                             undernourished_preval = undernourished_exposures_sets, 
                             
                             ch_malnutrition_kcapita = ch_malnutrition_exposures_sets, 
                             ch_malnutrition_preval = ch_malnutrition_exposures_sets, 
                             stunting_kcapita = ch_malnutrition_exposures_sets, 
                             stunting_preval = ch_malnutrition_exposures_sets, 
                             wasting_kcapita = ch_malnutrition_exposures_sets, 
                             wasting_preval = ch_malnutrition_exposures_sets, 
                             overweight_kcapita = ch_malnutrition_exposures_sets,
                             overweight_preval = ch_malnutrition_exposures_sets)

#all_dep_vars <- names(exposures_list[[1]])[grepl("dependency_", names(exposures_list[[1]]))]
# the variables mapped to every outcome are the POTENTIAL variables that are relevant in attempting to explain the outcome 
# (or more precisely: that can capture the outcome's exposure to the RFS shock)# Now, we specify vectors of variable names, that feature a breakdown of a category
# cereals_detail <- c("wheat", "rice", "barley", "maize", "other_cereals", "oilcrops", "vegetable_oils", "meat", "fish")
# oilcrops_detail <- c("soybeans", "other_oilcrops", "cereals", "vegetable_oils", "meat", "fish")
# vegetable_oils_detail <- c("soybean_oil", "sunflowerseed_oil", "rapeseed_oil", "palm_oil", "other_vegetable_oils", "cereals", "oilcrops", "meat", "fish")

# but actually, we don't use this for food security regressions, we stick to category level exposures, to contain dimensionality 


#### RFS DATA -----------------------------------------------------------------------
# /!\ THIS IS CONSTRUCTED DIFFERENTLY THAN IN THE ILUC PROJECT CURRENTLY (FITS DIFFERENT NEEDS)

# renewable fuel standards, as from https://www.epa.gov/renewable-fuel-standard-program/renewable-fuel-annual-standards
# remember: biodiesel are nested within advanced biofuels, which are themeselves nested within total
# conventional biofuels are the part of the total that is not "advanced"
rfs <- data.frame(year = 1994:2022, # taking from 1994 just so that even with 6-year lag there is no NA removing year 2000, which may be used as the first year with undernourishment outcome for the pre-treatment period. 
                  statute_total = 0, final_total = 0, 
                  statute_advanced = 0, final_advanced = 0, 
                  statute_biodiesel = 0, final_biodiesel = 0, 
                  statute_conv_earliest = 0)

rfs <- dplyr::arrange(rfs, year)
# this is if RFS2 takes over as soon as 2008 (9bgal). 
# The two first years of NAs represent the fact that these mandates are endogenous and should thus not be used 
# /!\ THIS IS DIFFERENT FROM THE ILUC PROJECT /!\
rfs[rfs$year >= 2006 & rfs$year <= 2022, c("statute_total")] <- c(NA, NA, 9, 11.1, 12.95,	13.95,	15.2,	16.55,	18.15,	20.5,	22.25,	24.0,	26.0,	28.0, 30.0, 33.0, 36.0)
rfs[rfs$year >= 2006 & rfs$year <= 2022, c("final_total")] <- c(NA, NA, 9, 11.1, 12.95,	13.95,	15.2,	16.55,	16.28,	16.93,	18.11,	19.28,	19.29,	19.92, 20.09, NA, NA)

rfs[rfs$year >= 2006 & rfs$year <= 2022, c("statute_advanced")] <- c(NA, NA, 0, 0.6, 0.95,	1.35,	2.0,	2.75,	3.75,	5.5,	7.25,	9.0,	11.0,	13.0, 15.0, 18.0, 21.0)
rfs[rfs$year >= 2006 & rfs$year <= 2022, c("final_advanced")] <- c(NA, NA, 0, 0.6, 0.95,	1.35,	2.0,	2.75,	2.67,	2.88,	3.61,	4.28,	4.29,	4.92, 5.09, NA, NA)

rfs[rfs$year >= 2009 & rfs$year <= 2022, c("statute_biodiesel")] <- c(0.5, 0.65, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
rfs[rfs$year >= 2009 & rfs$year <= 2022, c("final_biodiesel")] <- c(0.5, 1.15, 0.8, 1.0, 1.28, 1.63, 1.73, 1.9, 2.0, 2.1, 2.1, 2.43, 2.43, NA)

rfs <- mutate(rfs, statute_conv = statute_total - statute_advanced)
rfs <- mutate(rfs, final_conv = final_total - final_advanced)
# RFS mandates with for each year, the earliest set target (by RFS1 up to 2012 included, and by RFS2 afterwards)
rfs[rfs$year >= 2006 & rfs$year <= 2022, c("statute_conv_earliest")] <- c(4, 4.7, 5.4, 6.1, 6.8, 7.4, 7.5, 13.8, 14.4, 15, 15, 15, 15, 15, 15, 15, 15)


# make lags and leads
rfs_vars <- c("statute_conv", "statute_conv_earliest")#, "amendments", "blendwall"
for(voi in rfs_vars){
  for(lead in c(1:6)){
    rfs <- dplyr::arrange(rfs, year)
    rfs <- DataCombine::slide(rfs,
                                 Var = voi, 
                                 TimeVar = "year",
                                 NewVar = paste0(voi,"_lead",lead),
                                 slideBy = lead, 
                                 keepInvalid = FALSE)
    rfs <- dplyr::arrange(rfs, year)
    
    # up to 2005, leads are actually 0 because mandates were not known/disclosed
    rfs[rfs$year <= 2005, paste0(voi,"_lead", lead)] <- 0
    # and in 2006 the expected mandates are those of the RFS1 (the EISA was discussed in Senate as of January 2007)
    rfs[rfs$year == 2006, paste0(voi,"_lead", lead)] <- rfs[rfs$year == 2006+lead, "statute_conv_earliest"]
  }  
  
  for(lag in c(1:6)){
    rfs <- dplyr::arrange(rfs, year)
    rfs <- DataCombine::slide(rfs,
                                 Var = voi, 
                                 TimeVar = "year",
                                 NewVar = paste0(voi,"_lag",lag),
                                 slideBy = -lag, 
                                 keepInvalid = FALSE)
    rfs <- dplyr::arrange(rfs, year)
  }  
  
  
  for(py in c(1:6)){
    ## Future-year averages (1, 2, 3, and 4 years, after and EXCLUDING contemporaneous)  
    # note that we DON'T add voi column (not lagged) in the row mean
    rfs <- mutate(rfs, 
                     !!as.symbol(paste0(voi,"_",py,"fya")) := round(rowMeans(across(.cols = any_of(paste0(voi,"_lead",c(1:py)))), 
                                                                             na.rm = FALSE), 2) 
    )
    # NOTE ALSO that we remove NAs (not the case in ILUC currently), as they represent endogenous mandates 
    ## Past-year averages (1, 2, 3, and 4 years, before and INCLUDING contemporaneous)  
    
    # note that we DO add the voi column (not lagged) in the row mean 
    rfs <- mutate(rfs, 
                     !!as.symbol(paste0(voi,"_",py,"pya")) := round(rowMeans(across(.cols = any_of (c(voi, paste0(voi,"_lag",c(1:py))))), 
                                                                             na.rm = FALSE), 2)
   )
  }
}
all_rfs_treatments <- grep(pattern = "statute_conv", names(rfs), value = TRUE)



## Merge to outcomes 
main_data <- left_join(outcomes, rfs, by = "year")



#### PRE- VS. POST- DEPENDENCY PLOT ---------------------------------------------------------------------------------------
postdep <- readRDS(here("temp_data", "exposures_20102019", "dependency_20102019.Rdata"))
predep <- exposures_list[["2001_2007"]]

postdep <- dplyr::select(postdep, country, dependency_calorie_total)
predep <- dplyr::select(predep, country, dependency_calorie_total)

names(postdep) <- c("country", "Actual study period exposure")
names(predep) <- c("country", "Pre-treatment period exposure")


prepostdep <- inner_join(postdep, predep, by = "country")

post_c <- unique(postdep$country)
pre_c <- unique(exposures_list[["2001_2007"]]$country)
post_c[!(post_c %in% pre_c)] # 3 countries that are indeed absent in pre-period exposure data

prepostdep <- dplyr::mutate(prepostdep, 
                            growth_rate = ( `Actual study period exposure` - `Pre-treatment period exposure` ) / 
                                            `Pre-treatment period exposure` )
prepostdep <- dplyr::mutate(prepostdep, 
                            ratio = ( `Actual study period exposure`) / 
                              `Pre-treatment period exposure` )

quantile(prepostdep$growth_rate, seq(0,1,0.1))

prepostdep[prepostdep$growth_rate > 0.7, ]

ggplot(prepostdep, aes(x = `Pre-treatment period exposure`, y = `Actual study period exposure`)) +
  geom_point() +
  # geom_smooth(method = "lm", alpha = 0.05, aes(fill = `Pre-treatment period exposure`)) +
  geom_abline(slope=1, intercept=0) +
  geom_label(aes(label="x = y", 
                 x=1.3,
                 y=1.3)) +
  coord_cartesian() +
  geom_label( 
    data=prepostdep %>% filter(`Actual study period exposure` > 1), # Filter data first
    aes(label=country), nudge_y = 0.1
  ) + 
  theme_minimal() 
  
# aes(color = factor(gear))







#### DES STATS RFS --------------------------------------------------------------------------------------------------
# add PSD data to the chart 
psd <- readRDS(here("input_data", "prepared_psd.Rdata"))
# UNITS
# from https://apps.fas.usda.gov/psdonline/app/index.html#/app/about#G5
# Production, Trade, & Use: 1000 metric tons.
# So once divided by 1000, it's expressed in million tons 
psd$us_dc_maize <- psd$UnitedStates.Domestic_Consumption.Maize / 1000
psd$us_ts_maize <- psd$UnitedStates.Total_Supply.Maize / 1000
psd$us_im_maize <- psd$UnitedStates.Imports.Maize / 1000

#head(psd)
w_rfs <- left_join(rfs, psd[,c("year", "us_dc_maize")], by = "year")

# w_rfs <- dplyr::mutate(w_rfs, us_ex_maize = us_ts_maize - us_dc_maize)

# give back RFS1 values, to display them
w_rfs[w_rfs$year>=2006 & w_rfs$year<=2007, ] <- w_rfs %>% 
                                                dplyr::filter(year>=2006 & year<=2007) %>% 
                                                dplyr::mutate(statute_total = c(0, 0), # give zeros for total, because this is going to represent RFS2
                                                              final_total = c(0, 0), 
                                                              statute_advanced = c(0, 0), 
                                                              final_advanced = c(0, 0))
# reconstitute conventional
w_rfs[w_rfs$year>=2006 & w_rfs$year<=2007, ] <- w_rfs %>% 
                                                dplyr::filter(year>=2006 & year<=2007) %>% 
                                                dplyr::mutate(statute_conv = statute_total - statute_advanced, 
                                                              final_conv = final_total - final_advanced)

# continue final_conv
w_rfs[is.na(w_rfs$final_conv), "final_conv"] <- 15
w_rfs[is.na(w_rfs$us_dc_maize), "us_dc_maize"] <- 309.132

# group biodiesel and (other) advanced, not relevant to show distinctively
w_rfs <- w_rfs %>% 
  dplyr::mutate(statute_alladvanced = statute_advanced + statute_biodiesel, 
                final_alladvanced = final_advanced + final_biodiesel)


# take what's needed
w_rfs <- w_rfs %>% 
  dplyr::filter(year>=2000) %>% 
  dplyr::select(year, statute_conv, final_conv, us_dc_maize) #  statute_alladvanced, final_alladvanced,

# add RFS1 (2000-2012 only)
w_rfs$statute_rfs1 <- c(0, 0, 0, 0, 0, 0, 4, 4.7, 5.4, 6.1, 6.8, 7.4, 7.5, rep(NA, 10) ) 

# convert mandates from billion gallons to tons maize 
# According to USDA, 1 bushel maize gives 2.7 gallons ethanol https://www.ers.usda.gov/about-ers/partnerships/strengthening-statistics-through-the-icars/biofuels-data-sources/
# Given that 1 bushel maize is 0.0254 metric ton maize https://www.sagis.org.za/conversion_table.html
# 1 gallon ethanol = (1/2.7) * 0.0254 metric ton
# 1bgal = (1/2.7) * 0.0254 billion metric tons = (1/2.7) * 0.0254 * 1000 million metric tons
w_rfs <- dplyr::mutate(w_rfs, across(.cols = contains("_conv") | contains("_rfs1"), .fns = ~.*(1/2.7) * 0.0254 * 1000) )

# pile up 
l_rfs <- pivot_longer(w_rfs, cols = c("statute_conv", "final_conv", "statute_rfs1"), names_to = "mandates", values_to = "maize_milton") %>% as.data.frame() %>% arrange(mandates)


ggplot(l_rfs, aes(x = year, y = maize_milton, group = mandates)) +
  geom_line(aes(linetype=mandates)) + 
  geom_line(aes(x = year, y = us_dc_maize)) +
  geom_label(aes(label="US domestic consumption", 
                 x=2019,
                 y=290)) +
  
  geom_label(aes(label="RFS2 (statutory)", 
                 x=2020,
                 y=150)) +
  
  geom_label(aes(label="RFS2 (final rule)", 
                 x=2016,
                 y=120)) +
  
  geom_label(aes(label="RFS1 (statutory)", 
                 x=2014,
                 y=70)) +
  scale_linetype_manual(breaks=c("statute_conv", "final_conv", "statute_rfs1"),
                        values=c("solid", "dotted", "twodash"),
                        labels=c("RFS2 (statutory)", "RFS2 (final)", "RFS1 (statutory)"),
                        name="Mandates") +
  
  annotate("rect", xmin = 2005, xmax = 2008, ymin = 0, ymax = 150, 
           alpha = .2, col = "lightgrey") + 
  geom_label(aes(label="Possibly endogenous mandates", 
                 x=2006.5,
                 y=160)) +

  scale_x_continuous(breaks = c(2000, 2005, 2006, 2008, 2012, 2015, 2022)) +
  scale_y_continuous(name = "Million tonnes maize") + 
  theme_minimal() +
  theme(legend.position="none", 
        plot.title = element_text(size = 10, face = "bold"), 
        axis.title.y.left=element_text(size=10,face="bold", hjust = 1),
        axis.title.y.right=element_text(size=10,face="bold", hjust = 1),
        axis.title.x=element_blank(), #element_text(size=10,face="bold", hjust = 0.5), 
        panel.grid = element_line(inherit.blank = TRUE))  





#### MAP EXPOSURES -------------------------------------------------------------------------------------------------- 

sffb <- st_read(here("input_data", "Global_LSIB_Polygons_Detailed"))

# necessary for simplifying below, and better for unioning 
sffb <- st_transform(sffb, crs = 4088)

names(sffb)[names(sffb) == "COUNTRY_NA"] <- "country"
# sffb[grepl("ongo", sffb$country), ]
# pfb[grepl("ongo", pfb$country), "country"]

### Match country names to those from FAOSTAT 
## /!\ NEXT TIME CONSIDER USING A FAOSTAT ADMIN BOUNDARIES MAP DIRECTLY ! 
# (but some advantages to adjust manually some things here)

# Note that some FAOSTAT names have been modified above already (they are on top here)
# Note also that we do not associate islands to their main country, for simplicity (by commenting their section out)
sffb$country[sffb$country=="Cote d'Ivoire"] <- "Ivory Coast" # NOTICE THIS (FAOSTAT name is Côte d'Ivoire)
sffb$country[sffb$country=="Congo, Rep of the"] <- "Republic of the Congo" # This name is not from FAOSTAT, I changed it above

# Political discrepancies
sffb$country[sffb$country=="China"] <- "China, mainland" # NOTICE THIS 
sffb$country[sffb$country=="Hong Kong (Ch)"] <- "China, Hong Kong SAR"
sffb$country[sffb$country=="Macau (Ch)"] <- "China, Macao SAR"
# sffb$country[sffb$country=="Taiwan"] 

# for Sudan and South Sudan, they will have exactly the same treatment. So we can treat them as within the same cluster, 
# or merge them into a single country, with average outcome. 
sffb[sffb$country=="Sudan", "geometry"] <- st_union(sffb[sffb$country == "Sudan", "geometry"], 
                                                    sffb[sffb$count == "South Sudan", "geometry"]) # %>% dplyr::select(geometry)
sffb$country[sffb$country=="Sudan"] <- "Sudan (former)" # South Sudan independent since 2005, after our data period

sffb[sffb$country=="Serbia", "geometry"] <- st_union(sffb[sffb$country == "Serbia", "geometry"], 
                                                     sffb[sffb$country == "Montenegro", "geometry"]) # %>% dplyr::select(geometry)
sffb$country[sffb$country=="Serbia"] <- "Serbia and Montenegro" # Montenegro independent since 2006, after our data period

# Countries for which it's simply a matter of different way to write the name down
sffb$country[sffb$country=="Congo, Dem Rep of the"] <- "Democratic Republic of the Congo"
sffb$country[sffb$country=="Macedonia"] <- "North Macedonia"
sffb$country[sffb$country=="United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland"
sffb$country[sffb$country=="United States"] <- "United States of America"
sffb$country[sffb$country=="Russia"] <- "Russian Federation"
sffb$country[sffb$country=="Syria"] <- "Syrian Arab Republic"
sffb$country[sffb$country=="Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
sffb$country[sffb$country=="Korea, North"] <- "Democratic People's Republic of Korea"
sffb$country[sffb$country=="Korea, South"] <- "Republic of Korea"
sffb$country[sffb$country=="Moldova"] <- "Republic of Moldova"
sffb$country[sffb$country=="Gambia, The"] <- "Gambia"
sffb$country[sffb$country=="Swaziland"] <- "Eswatini"
sffb$country[sffb$country=="Iran"] <- "Iran (Islamic Republic of)"
sffb$country[sffb$country=="Burma"] <- "Myanmar"
sffb$country[sffb$country=="Bahamas, The"] <- "Bahamas"
sffb$country[sffb$country=="Vietnam"] <- "Viet Nam"
sffb$country[sffb$country=="Laos"] <- "Lao People's Democratic Republic"
sffb$country[sffb$country=="Antigua & Barbuda"] <- "Antigua and Barbuda"
sffb$country[sffb$country=="St Kitts & Nevis"] <- "Saint Kitts and Nevis"
sffb$country[sffb$country=="St Lucia"] <- "Saint Lucia"
sffb$country[sffb$country=="St Vincent & the Grenadines"] <- "Saint Vincent and the Grenadines"
sffb$country[sffb$country=="Venezuela"] <- "Venezuela (Bolivarian Republic of)"
sffb$country[sffb$country=="Trinidad & Tobago"] <- "Trinidad and Tobago"
sffb$country[sffb$country=="Central African Rep"] <- "Central African Republic"
sffb$country[sffb$country=="Brunei"] <- "Brunei Darussalam"
sffb$country[sffb$country=="Sao Tome & Principe"] <- "Sao Tome and Principe"
sffb$country[sffb$country=="Tanzania"] <- "United Republic of Tanzania"
sffb$country[sffb$country=="Solomon Is"] <- "Solomon Islands"
sffb$country[sffb$country=="Bolivia"] <- "Bolivia (Plurinational State of)"


# simplify before plotting
sffb$geometry <- st_simplify(sffb$geometry, dTolerance = 1500)

sffb <- st_transform(sffb, crs = 4326)

# add 2010-2019 exposures
postdep <- readRDS(here("temp_data", "exposures_20102019", "dependency_20102019.Rdata"))
postdep <- dplyr::select(postdep, country, dependency_calorie_total)
names(postdep) <- c("country", "dependency_calorie_total_20102019")

prepostdep <- left_join(exposures_list[["2001_2007"]], postdep, by = "country")

csfb_sf <- left_join(prepostdep, sffb, by = "country") %>% st_as_sf()

# if st_simplify is too "strong", geometries become "empty" 
csfb_sf$country[st_is_empty(csfb_sf$geometry)]



##### Calorific import dependency ####
pal_dep <- colorNumeric("viridis", # "viridis" (green-purple), "magma" (yellow-purple), "inferno" (like magma), or "plasma", "BuPu", "Greens"
                        domain = st_drop_geometry(csfb_sf[,"dependency_calorie_total"]),
                        #bins = 4, 
                        na.color = "transparent", 
                        reverse = F)


# popup
csfb_sf$popup_total <- paste0("<b>",csfb_sf$country,"</b>", "<br/>",
                              "2001-2007 annual averages:", "<br/>",
                              " &nbsp;&nbsp;&nbsp;&nbsp; Calorific import dependency: ", formatC(csfb_sf$dependency_calorie_total, format = "f", digits = 2), "<br/>", 
                              " &nbsp;&nbsp;&nbsp;&nbsp; Imports: ", formatC(csfb_sf$import_kcal_total/1e6, format = "e", digits = 2), " bn cal", "<br/>",
                              " &nbsp;&nbsp;&nbsp;&nbsp; Supply + Exports: ", formatC(csfb_sf$gross_supply_kcal_total/1e6,format = "e", digits = 2), " bn cal", "<br/>", 
                              "<br/>", 
                              "2010-2019 calorific import dependency: ", formatC(csfb_sf$dependency_calorie_total_20102019, format = "f", digits = 2)
)



leaflet() %>% 
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group ="ESRI") %>%
  setView(lat = 0, 
          lng = 0, 
          zoom = 1) %>% 
  addPolygons(data = csfb_sf, 
              opacity = 0, color = "black", weight = 2, 
              fill = TRUE, fillColor = ~pal_dep(csfb_sf$dependency_calorie_total), fillOpacity = 0.5,
              popup = ~csfb_sf$popup_total, 
              popupOptions = popupOptions(riseOnHover = TRUE, 
                                          bringToFront = TRUE),
              highlightOptions = highlightOptions(bringToFront = TRUE)
  ) %>% 
  # addMarkers(data = csfb_sf, 
  #            popup = ~csfb_sf$popup_total,
  #            options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegend(pal = pal_dep,  
            values = csfb_sf$dependency_calorie_total, 
            bins = 5, opacity = 0.4,
            title = "Calorific import dependency",
            position = "bottomright") 

#### Inverse GDP weighted calorific import dependency ####

pal_dep <- colorNumeric("viridis", # "viridis" (green-purple), "magma" (yellow-purple), "inferno" (like magma), or "plasma", "BuPu", "Greens"
                        domain = st_drop_geometry(csfb_sf[,"gdp_dependency_calorie_total"]),
                        #bins = 4, 
                        na.color = "transparent", 
                        reverse = F)


# popup
csfb_sf$popup_total <- paste0("<b>",csfb_sf$country,"</b>", "<br/>",
                              "2001-2007 annual averages:", "<br/>",
                              " &nbsp;&nbsp;&nbsp;&nbsp; Calorific import dependency weighted by inverse GDP per capita: ", formatC(csfb_sf$gdp_dependency_calorie_total, format = "f", digits = 2), "<br/>", 
                              " &nbsp;&nbsp;&nbsp;&nbsp; Imports: ", formatC(csfb_sf$import_kcal_total/1e6, format = "e", digits = 2), " bn cal", "<br/>",
                              " &nbsp;&nbsp;&nbsp;&nbsp; Supply + Exports: ", formatC(csfb_sf$gross_supply_kcal_total/1e6,format = "e", digits = 2), " bn cal", "<br/>", 
                              "<br/>", 
                              "2010-2019 calorific import dependency: ", formatC(csfb_sf$dependency_calorie_total_20102019, format = "f", digits = 2)
)



leaflet() %>% 
  addTiles()%>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group ="ESRI") %>%
  setView(lat = 0, 
          lng = 0, 
          zoom = 1) %>% 
  addPolygons(data = csfb_sf, 
              opacity = 0, color = "black", weight = 2, 
              fill = TRUE, fillColor = ~pal_dep(csfb_sf$gdp_dependency_calorie_total), fillOpacity = 0.5,
              popup = ~csfb_sf$popup_total, 
              popupOptions = popupOptions(riseOnHover = TRUE, 
                                          bringToFront = TRUE),
              highlightOptions = highlightOptions(bringToFront = TRUE)
  ) %>% 
  # addMarkers(data = csfb_sf, 
  #            popup = ~csfb_sf$popup_total,
  #            options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegend(pal = pal_dep,  
            values = csfb_sf$gdp_dependency_calorie_total, 
            bins = 5, opacity = 0.4,
            title = "Calorific import dependency <br/> weighted by inverse GDP per capita",
            position = "bottomright") 

#### REGRESSION FUNCTION --------------------------------------------------------------------------------------------------

### TEMPORARY OBJECTS 
outcome_variable = "undernourished_kcapita" #   # c("foodinsecu_modsevere_kcapita", "foodinsecu_severe_kcapita", "undernourished_kcapita", "stunting_kcapita", "wasting_kcapita")
offset = "population_kcapita"
weights = FALSE
preperiod_end = 2004
exclude_bad_expo_proxy = FALSE # if TRUE, removes countries with an outlying absolute ratio of post-exposure/pre-exposure (>1.5*IQR)

# exposure 
expo_measure_period = "2001_2007" # names(exposures_list)
calorie_only = TRUE
commodities = "total"
gdp_weighting = FALSE
# exposure_rfs = exposures_names_list[c("total", "cereals", "oilcrops", "vegetable_oils")] %>% unlist() %>% unname()
remove_wellnourished = TRUE
original_rfs_treatments = c("statute_conv")
start_year = 2009 
end_year = 2020 


# dynamics 
rfs_lead = 3
rfs_lag = 3
rfs_fya = 0
rm(rfs_pya) # needs to be reinitialized every time
lag_controls = NULL
aggr_dyn = TRUE 

# heterogeneity control
pre_trend = TRUE 
post_trend = TRUE
overall_trend = FALSE
s_trend = TRUE
s_trend_sqrt = FALSE
s_trend_sq = FALSE
s_trend_log = FALSE
fe = "country + year" #  
control_remaining_dependency = TRUE

# estimation 
distribution = "quasipoisson"
invhypsin = FALSE 
preclean_level = "FE" 
clustering = "twoway"
cluster_var1 = "country" 
cluster_var2 = "year"
glm_iter = 25

output = "coef_table" 



rm(d, d_clean, outcome_variable, offset, weights, commodities, original_rfs_treatments, start_year, end_year, remove_wellnourished,
   rfs_lead, rfs_lag, rfs_fya, rfs_pya, lag_controls, aggr_dyn, 
   s_trend, s_trend_log, fe, 
   distribution, invhypsin, preclean_level, clustering, cluster_var1, cluster_var2, glm_iter,
   output)

make_main_reg <- function(# outcome 
                          outcome_variable = "undernourished_kcapita",
                          offset = "population_kcapita", # should the log of the annual ("population_kcapita") or of the pre-period ("population_kcapita_2007") population size be added as an offset (i.e. linearly to the fml element of fixest)? Any positive constant number yields no offset
                          # sample
                          start_year = 2009, # nto much useful, as this is mostly driven by the choice of rfs_lag
                          end_year = 2020, # not much useful, as this is mostly driven by the choice of rfs_lead
                          preperiod_end = 2004, # after which year to stop assuming that RFS1 could not anticipated (i.e. until when to assume it could not be anticipated). This trims the data after this year, and until start_year. 
                          remove_wellnourished = TRUE, # removes countries which undernourishment prevalence is "<2.5%" every year of the study period (according to FAOSTAT)
                          exclude_bad_expo_proxy = FALSE, # if TRUE, removes countries with an outliing absolute ratio of post-exposure/pre-exposure (>1.5*IQR)
                          weights = FALSE, # should population weights be used in the regression
                          
                          # exposure
                          commodities = "total", # if TRUE, then all prepared regression sets are run for the chosen outcome. Otherwise, commodities should be a character vector containing one or several elements of names(exposures_names_list). May not be available for a given outcome, depending on exposure_outcome_map. If "" or NULL, all regressions allowed by exposure_outcome_map are run
                          gdp_weighting = FALSE,
                          expo_measure_period = "2001_2007", # one of names(exposures_list)
                          calorie_only = TRUE, # this should be left TRUE if outcome is undernourished. Set to FALSE to estimate conditional effects through
                          # all nutrient-specific exposures (calorie, protein and fat). Inconsequential if outcome is foodinsecu_*
                          # nutrient = "calorie", currently not used, because either all nutrients are included, or only calorie, or it's exposure is not in nutrient terms. # one of "calorie", "protein", or "fat". This is used only when studying childhood malnutrition or nutrient supply
                          
                          # shocks
                          original_rfs_treatments = c("statute_conv"),
                          
                          # dynamics 
                          rfs_lead = 3,
                          rfs_lag = 3,
                          rfs_fya = 0, 
                          # rfs_pya = 0, # this is determined as a function of rfs_fya, in code
                          lag_controls = NULL, # which of the lags specified above should be CONTROLLED for rather than counted in the cumulative effect
                          aggr_dyn = TRUE, # whether to report aggregate coefficients of all leads and lags ("all") or leads and lags separately ("leadlag"), or no aggregate (any other string)
                          
                          # heterogeneity control
                          pre_trend = TRUE, 
                          post_trend = TRUE,
                          overall_trend = FALSE,
                          s_trend = TRUE,
                          s_trend_sqrt = FALSE,
                          s_trend_sq = FALSE,
                          s_trend_log = FALSE,
                          fe = "country + year", 
                          control_remaining_dependency = TRUE,  
                          
                          # estimation 
                          distribution = "quasipoisson",#  "quasipoisson", "poisson", or "gaussian". If gaussian, the outcome is logged and the population offset is added (in log) such that there is equivalence with poisson estimations
                          invhypsin = FALSE, # currently not used. if distribution is gaussian, should the dep. var. be transformed to inverse hyperbolic sine? If FALSE, and gaussian, outcome is transformed in log
                          preclean_level = "FE", 
                          clustering = "twoway", # either "oneway" or "twoway". If oneway, it clusters on cluster_var1. 
                          cluster_var1 = "country", # this can be a numeric, in which case this number is interpreted as the number of quantiles of the exposure variable by which to cluster in this dimension. For instance, 20 makes 20 tiles of dependency var
                          cluster_var2 = "year",
                          glm_iter = 25,

                          output = "coef_table", # one of est_object, "coef_table", or "everything"
                          
                          rfs_rando = "" # either "between", "within", or any other string. If one of the former two, randomization inference of the type is performed
){
  #### INTRODUCE DATA 
  
  # manipulate a different data set so that original one can be provided to all functions and not read again every time. 

  # merge exposures average over the specified pre-treatment period
  d <- inner_join(main_data, exposures_list[[expo_measure_period]], by = "country") # exposure data is a cross section 
  # d <- left_join(main_data, exposures_list[[expo_measure_period]], by = "country") # this should yield the same number of rows
  
  # grep("enegro", unique(d$country), value = TRUE)
  
  
  ### SPECIFICATION #### 
  
  # This is used to collect formula of every regression to run 
  regression_sets <- list()


  #### SET NAMES BASED ON SPECIFIED DYNAMICS
  # this does not involve data, just arguments of the make_reg function
  # original_ names are used to get generic covariate names in coefficient tables (irrespective of modelling choices)
  
  # set past year average to one year length less, because it includes the contemporaneous mandate, and thus 3pya averages over one year more than 3fya 
  rfs_pya <- max(0, rfs_fya - 1) 
  # and for security, force annual dynamics to be null when they are already accounted for by average
  if(rfs_pya>0){rfs_lag <- 0}
  if(rfs_fya>0){rfs_lead <- 0}
  
  # collect names of contemporaneous, lead and lag RFS 
  rfs_treatments <- original_rfs_treatments
  
  if(rfs_lead >= 1){
    for(rfs_var in original_rfs_treatments){
      rfs_treatments <- c(rfs_treatments, paste0(rfs_var, "_lead", 1:rfs_lead))
    }
  }
  if(rfs_lag >= 1){
    for(rfs_var in original_rfs_treatments){
      rfs_treatments <- c(rfs_treatments, paste0(rfs_var, "_lag", 1:rfs_lag))
    }
  }
  
  # Or they are past or future values averaged over a certain amount of time
  if(rfs_fya >= 1 & rfs_pya == 0){
    for(rfs_var in original_rfs_treatments){
      rfs_treatments <- paste0(rfs_var, "_", rfs_fya, "fya") 
    }
  }  
  if(rfs_pya >= 1 & rfs_fya == 0){
    for(rfs_var in original_rfs_treatments){
      rfs_treatments <- paste0(rfs_var, "_", rfs_pya, "pya") 
    }
  }
  if(rfs_fya >=1 & rfs_pya >=1){
    for(rfs_var in original_rfs_treatments){
      rfs_treatments <- c(paste0(rfs_var, "_", rfs_pya, "pya"), paste0(rfs_var, "_", rfs_fya, "fya"))
    }
  }
  
  # # or they are a combination of averaged past treatments, and annual future ones
  # if(rfs_pya > 0 & rfs_fya == 0 & rfs_lead > 0 & rfs_lag == 0){
  #   for(rfs_var in original_rfs_treatments){
  #     rfs_treatments <- c(paste0(rfs_var, "_lead", 1:rfs_lead), paste0(rfs_var, "_", rfs_pya, "pya") )
  #   }
  # }
  

  ## Handling some forbidden parameter combinations
  
  # 2008 is the first year of pre-determined (thus quasi-experimental, thus "allowed") RFS mandates
  # start_year must be sufficiently late for the use of lags not to leverage older mandates than 2008
  if(start_year - rfs_lag < 2008 | start_year - rfs_pya < 2008 ){
    start_year <- 2008+max(rfs_lag, rfs_pya)
  }
  
  # Some changes are needed in the data, such that years under RFS1 are not included
  # the years under the influence of RFS1 depend on our assumption on lag length (and hence depends on start_year as set above)
  # (but the RFS1 mandates being set to NAs would have had these years removed too)
  # and on our assumption on how early the RFS1 was anticipated. Default is 2005, but this can be flexibly made earlier.
  d[d$year > preperiod_end & d$year < start_year, outcome_variable] <- NA
  
  # no safety measure for end_year, as it cannot go too late, there is no endogenous rfs after 2022 in our current data.
  
  # If the outcome is a prevalence, then there should be no offset. This is relevant only when distribution = "gaussian", but not impossible if it's poisson
  if(grepl("preval", outcome_variable)){
    offset <- "constant_offset"
    d$constant_offset <- 1
  }
  

  # We don't want calorie exposures if we investigate food insecurity, and total is not available
  if(grepl("foodinsecu_", outcome_variable)){
    calorie_only <- FALSE
    if(commodities == "total"){commodities <- TRUE}
  }
  
  # we can't have both overall trends and pre or post trends
  if(overall_trend){ 
    pre_trend <- FALSE
    post_trend <- FALSE
  }
  
  
  # This is the set of exposures for every regression ordered for the present outcome (written flexibly to accomodate different outcome forms)
  # There is one regression to run per element of exposure_outcome_map[[outcome_variable]]
  # each regression features all the exposures contained in the element 
  exposures_sets <- exposure_outcome_map[str_contains(x = outcome_variable, pattern = names(exposure_outcome_map))]
  exposures_sets <- exposures_sets[[outcome_variable]] # this necessary for list issues
  # subset to only those specified 
  exposures_sets <- exposures_sets[commodities]
  # also, we purge non-calorie exposures from every set if this is specified
  if(calorie_only){
    exposures_sets <- lapply(exposures_sets, FUN = grep, pattern = "calorie", value = TRUE) 
  } else{
    exposures_sets <- lapply(exposures_sets, FUN = identity) 
  }
  
  # and edit import dependency names to their GDP weighted versions, if required
  if(gdp_weighting){
    exposures_sets <- lapply(exposures_sets, FUN = function(x){paste0("gdp_",x)} )
  }  
  
  ### Regressors ####

  # the set of all exposure variables that will be leveraged for the regressions of the chosen outcome
  potential_exp <- exposures_sets %>% unlist() %>% unname() # unlist necessary for foodinsecu_ exposures, but inconsequentiel for other outcomes (already unlisted)

  
  # DATA: construct the interactions with all these potential exposures, 
  # they will all be needed by at least one regression ordered for the present outcome
  potential_regressors <- c()
  for(rfs_var in rfs_treatments){
    for(pot_exp in potential_exp){
      # make regressors of interest
      varname <- paste0(pot_exp, "_X_", rfs_var)
      potential_regressors <- c(potential_regressors, varname)
      d <- dplyr::mutate(d, !!as.symbol(varname) := !!as.symbol(pot_exp) * !!as.symbol(rfs_var))
    }
  }
  

  ## FORMULA: construct regressors part
  for(exp_set_name in names(exposures_sets)){
    # single set of exposures 
    exp_set <- exposures_sets[[exp_set_name]]

    # the double paste0 is necessary when there are several exposures  
    regression_sets[[exp_set_name]]$formula <- sapply(exp_set, paste0, "_X_",rfs_treatments, collapse = " + ") %>% paste0(collapse = " + ")
  } 
  
  ### Controls #### 
  # record all potential controls, needed in at least one regression ordered for the present outcome 
  potential_controls <- c()
  # it's important that this is not conditioned on anything so these objects exist
  # transfer lags of the treatment of interest from regressors to controls
  if(length(lag_controls)>0){
    lags_to_transfer <- c()
    for(rfs_var in original_rfs_treatments){
      lags_to_transfer <- c(lags_to_transfer, paste0(exposure_rfs, "_X_", rfs_var, "_lag", lag_controls))
    }
    
    regressors <- regressors[regressors != lags_to_transfer]
    potential_controls <- c(potential_controls, lags_to_transfer)
    # no need to change their names, they are not captured by cumulative effects as long as they are in controls. 
  }
  
  if(control_remaining_dependency){
    
    ## FOOD SECURITY
    # in this case, dependency is not expressed in nutrient, then there is no total available, and we thus control for 
    # all dependencies, but this is done via the use of commodities argument, and thus embedded in regressors
    
    ## UNDERNOURISHMENT or CHILDHOOD MALNUTRITION
    # if outcome is undernourished or ch_malnutrition, then dependency is expressed in calorie or any nutrient, either total or for a particular item
    # if it's total, we don't want to control for the dependency on "other" commodities (there is no "other")
    if(!grepl("foodinsecu_", outcome_variable)){
      # create all potential remaining dependency variables in the data 
      
      # per regression  
      for(exp_set_name in names(exposures_sets)[names(exposures_sets)!="total"]){
        exp_set_controls <- c()
        # this allows flexibility in nutrient(s) considered 
        for(nutr in nutrients[str_contains(pattern = nutrients, x=exposures_sets)]){      
          # interact each with the RFS shocks
          for(rfs_var in rfs_treatments){
            remctrl_name <- paste0("remdep_",nutr,"_",exp_set_name,"_X_", rfs_var)
            exp_set_controls <- c(exp_set_controls, remctrl_name)
            if(gdp_weighting){
              d <- dplyr::mutate(d, !!as.symbol(remctrl_name) := ( !!as.symbol(paste0("gdp_dependency_",nutr,"_total")) - 
                                                                   !!as.symbol(paste0("gdp_dependency_",nutr,"_",exp_set_name)) ) *
                                                                   !!as.symbol(rfs_var))
            } else{
               d <- dplyr::mutate(d, !!as.symbol(remctrl_name) := ( !!as.symbol(paste0("dependency_",nutr,"_total")) - 
                                                                    !!as.symbol(paste0("dependency_",nutr,"_",exp_set_name)) ) *
                                                                    !!as.symbol(rfs_var))
            }
           
          }
        }
        
        potential_controls <- unique(c(potential_controls, exp_set_controls))
        # if we are looking at calorie exposure only, then control for remaining dependency for calorie only 
        # this is in particular necessary if the outcome is "undernourished"
        # if(calorie_only){
        #   exp_set_controls <- exp_set_controls[grepl("calorie", exp_set_controls)]
        # }
        
        # For this specific set of exposures, we now have the specific set of controls for remaining dependencies
        fml_controls_part <- paste0(exp_set_controls, collapse = " + ")

        # add them to the regressors part of the formula, within each regression set 
        regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " + ", fml_controls_part)
      
      } # closes loop on regressions
    } # closes condition on not being in food insecurity part 

  } # closes condition on control for remaining dep 
  
  
  ### Trends & FE #### 
  for(exp_set_name in names(exposures_sets)){
    
    # it's important that this is renewed for every regression (exposures_sets loop)
    pre_trends_lin <- c()
    pre_trends_log <- c() 
    post_trends_lin <- c()
    post_trends_log <- c()
    overall_trends_lin <- c()
    overall_trends_log <- c() 
    
    # single set of exposures 
    exp_set <- exposures_sets[[exp_set_name]]
    
    if(pre_trend){
      d <- dplyr::mutate(d, pre_trend_dummy = (year<=preperiod_end))

      if(s_trend){
        for(exp_ in exp_set){
          pre_trend_name <- paste0(exp_,"_pretrend_lin")
          
          pre_trends_lin <- c(pre_trends_lin, pre_trend_name)
          
          d <- mutate(d, !!as.symbol(pre_trend_name) := !!as.symbol(exp_) * (year) * pre_trend_dummy) 
        }  
        fml_lintrends_part <- paste0(pre_trends_lin, collapse = " + ")
        # add them to the regressors part of the formula, within each regression set 
        regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " + ", fml_lintrends_part)
      }
      
      
      if(s_trend_log){
        for(exp_ in exp_set){
          pre_trend_name <- paste0(exp_, "_pretrend_log")
          pre_trends_log <- c(pre_trends_log, pre_trend_name)
          
          # make the logged value start from 1 for the first year in the data - which is not the same depending on whether we include the pre-treatment period
          d <- mutate(d, !!as.symbol(pre_trend_name) := !!as.symbol(exp_) * log(year - min(year) + 1) * pre_trend_dummy)
          
        }  
        fml_logtrends_part <- paste0(pre_trends_log, collapse = " + ")
        # add them to the regressors part of the formula, within each regression set 
        regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " + ", fml_logtrends_part)
      }
      
    } 
    
    if(post_trend){
      d <- dplyr::mutate(d, post_trend_dummy = (year>=start_year))
      
      if(s_trend){
        for(exp_ in exp_set){
          post_trend_name <- paste0(exp_,"_posttrend_lin")
          
          post_trends_lin <- c(post_trends_lin, post_trend_name)
          
          d <- mutate(d, !!as.symbol(post_trend_name) := !!as.symbol(exp_) * (year) * post_trend_dummy) 
        }  
        fml_lintrends_part <- paste0(post_trends_lin, collapse = " + ")
        # add them to the regressors part of the formula, within each regression set 
        regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " + ", fml_lintrends_part)
      }
      
      
      if(s_trend_log){
        for(exp_ in exp_set){
          post_trend_name <- paste0(exp_, "_posttrend_log")
          post_trends_log <- c(post_trends_log, post_trend_name)
          
          d <- mutate(d, !!as.symbol(post_trend_name) := !!as.symbol(exp_) * log(year - min(year) + 1) * post_trend_dummy )
          
        }  
        fml_logtrends_part <- paste0(post_trends_log, collapse = " + ")
        # add them to the regressors part of the formula, within each regression set 
        regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " + ", fml_logtrends_part)
      }
      
    } 
    
    if(overall_trend){
      if(s_trend){
        for(exp_ in exp_set){
          overall_trend_name <- paste0(exp_,"_overalltrend_lin")
          
          overall_trends_lin <- c(overall_trends_lin, overall_trend_name)
          
          d <- mutate(d, !!as.symbol(overall_trend_name) := !!as.symbol(exp_) * (year)) 
        }  
        fml_lintrends_part <- paste0(overall_trends_lin, collapse = " + ")
        # add them to the regressors part of the formula, within each regression set 
        regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " + ", fml_lintrends_part)
      }
      
      
      if(s_trend_log){
        for(exp_ in exp_set){
          overall_trend_name <- paste0(exp_, "_overalltrend_log")
          overall_trends_log <- c(overall_trends_log, overall_trend_name)
          
          # make the logged value start from 1 for the first year POST TREATMENT, notice the difference with log trend in pre_trend above
          d <- mutate(d, !!as.symbol(overall_trend_name) := !!as.symbol(exp_) * log(year - min(year) + 1))
          
        }  
        fml_logtrends_part <- paste0(overall_trends_log, collapse = " + ")
        # add them to the regressors part of the formula, within each regression set 
        regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " + ", fml_logtrends_part)
      }

    } 

    if(s_trend_sqrt){
      for(exp_ in exp_set){
        trendname <- paste0(exp_, "_trend_sqrt")
        sqrt_trends <- c(sqrt_trends, trendname)

        # make the logged value start from 1 for the first year in the data - which is not the same depending on whether we include the pre-treatment period
        d <- mutate(d, !!as.symbol(trendname) := !!as.symbol(exp_) * sqrt((year - min(year))))

      }
      fml_sqrttrends_part <- paste0(sqrt_trends, collapse = " + ")
      # add them to the regressors part of the formula, within each regression set
      regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " + ", fml_sqrttrends_part)
    }

    if(s_trend_sq){
      for(exp_ in exp_set){
        trendname <- paste0(exp_, "_trend_sq")
        sq_trends <- c(sq_trends, trendname)

        # make the logged value start from 1 for the first year in the data - which is not the same depending on whether we include the pre-treatment period
        d <- mutate(d, !!as.symbol(trendname) := !!as.symbol(exp_) * ((year - min(year))^2))

      }
      fml_sqtrends_part <- paste0(sq_trends, collapse = " + ")
      # add them to the regressors part of the formula, within each regression set
      regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " + ", fml_sqtrends_part)
    }
    

    potential_controls <- unique(c(potential_controls, 
                                   pre_trends_lin, pre_trends_log, 
                                   post_trends_lin, post_trends_log, 
                                   overall_trends_lin, overall_trends_log))
    
    ## FIXED EFFECTS
    # simply add them at the end
    regression_sets[[exp_set_name]]$formula <- paste0(regression_sets[[exp_set_name]]$formula, " | ", fe) 
    
    ## FORMULA 
    # make it an actual formula obect
    regression_sets[[exp_set_name]]$formula <- as.formula(paste0(outcome_variable, " ~ ", regression_sets[[exp_set_name]]$formula))
    
    
  }
  

### KEEP OBSERVATIONS THAT: ####
  # construct vector of potential variables used 
  # does not matter that regressions of different outcomes are not estimated on the same sample, because they are not meant to be compared
  
  # not really useful, as the end of the data is trimmed by the farthest lead variable in mani spec. 
  d <- dplyr::filter(d, year <= end_year)
  
  # remove those countries that are always under the 2.5% undernourishment prevalence threshold
  if(grepl("undernourished", outcome_variable) & remove_wellnourished){
    
     avg_preval <- ddply(d, "country", summarise, avg_preval = mean(undernourished_preval, na.rm = TRUE))
     well_nourished_c <- avg_preval[avg_preval$avg_preval == 1.25, "country"] %>% unique() # the value of 1.25 was given in prepare_outcomes.R to observations equal to "<2.5"  
      
     d <- dplyr::filter(d, !(country %in% c(well_nourished_c)))

  }
  
  d_save <- d 
  
  # Remove countries with an outliing absolute ratio of post-exposure/pre-exposure (>1.5*IQR)
  if(exclude_bad_expo_proxy){
    
    postdep <- readRDS(here("temp_data", "exposures_20102019", "dependency_20102019.Rdata"))

    postdep <- dplyr::select(postdep, country, dependency_calorie_total)
    
    names(postdep) <- c("country", "dependency_calorie_total_post")
  
    d <- left_join(d, postdep, by = "country")
    
    d <- dplyr::mutate(d, ratio = dependency_calorie_total_post/dependency_calorie_total)
    d <- dplyr::select(d, -dependency_calorie_total_post)
    # ratio_iqr <- IQR(d$ratio, na.rm = TRUE)
    otl_values <- boxplot.stats(d$ratio)$out %>% unique
    
    d <- dplyr::filter(d, !(ratio %in% otl_values))
    
  }
  
  d_save <- d 
  
  used_vars <- unique(c("country", "year", "population_kcapita", "population_kcapita_2007", offset, # "undernourished_preval", 
                        outcome_variable, potential_regressors, potential_controls, # it is not mor restrictive to add potential_controls, but we need to keep these vars in the data, and we restrict the data set to have an NA-free data set 
                        potential_exp, original_rfs_treatments, rfs_treatments)) # this is necessary to reconstruct variables in randomization inference processes
  
  # - have no NA nor INF on any of the variables used (otherwise they get removed by {fixest})
  # for instance, there are some NAs in the suitability index (places in water that we kept while processing other variables...) 
  usable <- lapply(used_vars, FUN = function(var){is.finite(d[,var]) | is.character(d[,var])})
  # used_vars[!(used_vars%in%names(d))]
  names(usable) <- used_vars            
  usable <- bind_cols(usable)
  filter_vec <- base::rowSums(usable)
  filter_vec <- filter_vec == length(used_vars)
  d <- d[filter_vec, c(used_vars)]
  if(anyNA(d)){stop()}
  rm(filter_vec, usable)
  
  # note removing NAs also makes sure that 2005, 2006 and 2007 get removed eventhough sufficient leads can given them values, because for short leads, they take in RFS1 period that has NAs 
  # (in the same time, even with long leads, *expected* (i.e. lead) mandatesare 0 up to 2005)
  
  # are in countries that experience variation in their undernourishment outcome at least once 
  # (automatically removed if distribution is quasipoisson, but not if it's gaussian, though we want 
  # identical samples to compare distributional assumptions, or FE specifications
  if(preclean_level == "FE"){
    preclean_level <- fe
  }
  temp_est <- feglm(fml = as.formula(paste0(outcome_variable, " ~ 1 | ", preclean_level)),
                    data = d,
                    family = "poisson")
  # it's possible that the removal of always zero dep.var in some FE dimensions above is equal to with the FE currently implemented
  if(length(temp_est$obs_selection)>0){
    d_clean <- d[unlist(temp_est$obs_selection),]
  }  else { 
    d_clean <- d
  }
  
  rm(d)
  
  ## Outcome transformation 
  # put this after removing always zero, because the removing zero assesses non-transformed outcomes (and we want the same sample irrespective of the distributional choice)
  if((distribution == "gaussian") & invhypsin){
    # transform dependent variable, if gaussian GLM 
    d_clean <- dplyr::mutate(d_clean, !!as.symbol(outcome_variable) := asinh(!!as.symbol(outcome_variable)))
  }
  
  if((distribution == "gaussian") & !invhypsin & !grepl("_preval", outcome_variable)){
    d_clean <- dplyr::mutate(d_clean, !!as.symbol(outcome_variable) := log(!!as.symbol(outcome_variable)))
  }
  
  
  ### REGRESSIONS ####
  
  # Store only information necessary, in a dataframe. otherwise the output of fixest estimation is large and we can't collect too many at the same time (over loops)  
  # either there are several elemnts in regressors, and then we want to aggregate them, or there is only one. 
  # In both cases, we are interested in a one-line output
  
  # handle SE computation flexibly within feglm now, through argument vcov
  if(is.numeric(cluster_var1)){
    d_clean <- mutate(d_clean, !!as.symbol(paste0("expo_",cluster_var1,"tiles")) := cut_number(dependency_calorie_total, n = cluster_var1, labels = paste0("exposure_Q", 1:cluster_var1)))
    cluster_var1 <- paste0("expo_",cluster_var1,"tiles")
  }
  
  if(clustering =="twoway"){
    se <- as.formula(paste0("~ ", paste0(c(cluster_var1, cluster_var2), collapse = "+")))
  }
  if(clustering =="oneway"){
    se <- as.formula(paste0("~ ", cluster_var1))
  }

  ## Make the offset variable ## 
  d_clean <- dplyr::mutate(d_clean, log_offset := log(!!as.symbol(offset)))

  ## Make weights (never used actually)
  if(weights){
    pop_w <- d_clean$population_kcapita
  } else { 
    pop_w <- 1 # this is equivalent to not giving the weights option in feglm
  }
  
  # d <- dplyr::mutate(d, test = round(undernourished_kcapita/population_kcapita, 0) == round(undernourished_preval/100, 0))
  # d[!d$test, c("country", "year", "undernourished_kcapita", "undernourished_preval", "new_preval", "population_kcapita", "test")] 
  
  exp_set_name <- "total" # just for convenience when running manually
  
  # run regressions for each formula 
  for(exp_set_name in names(exposures_sets)){
    
    reg_res <- fixest::feglm(fml = regression_sets[[exp_set_name]]$formula,
                             offset = d_clean$log_offset,
                             weights = pop_w,
                             data = d_clean, 
                             family = distribution,
                             vcov = se,
                             # this is just to get the same p value by recomputing by hand below. See: https://cran.r-project.org/web/packages/fixest/vignettes/standard_errors.html
                             # ssc = ssc(cluster.df = "conventional", t.df = "conventional"),
                             nthreads = 3,
                             fixef.rm = "perfect",
                             glm.iter = glm_iter,
                             notes = TRUE, 
                             verbose = 0) 
    
    # if its total (commodities) regression, store it
    if(exp_set_name=="total"){
      reg_res_main <- reg_res
    }
    
    df_res <- summary(reg_res)$coeftable#[paste0(original_sj, "_X_", original_Pk), ]
    
    fixest_df <- degrees_freedom(reg_res, type = "t")
    ## MAKE AGGREGATE RESULTS 
    # Aggregate all contemporaneous, lead and lag effects together
    
    # we probably don't need to condition on dynamic parameters, the grep used in making the all_roi object does the job flexibly
    
    # if(aggr_dyn & rfs_lead > 0 & rfs_lag > 0 & rfs_fya == 0 & rfs_pya == 0){
      # In this case, we are interested in LEAD AND LAG effects, aggregated separately, and all together.
      
      # identify explanatory variables that contain the exposures 
      exp_set <- exposures_sets[[exp_set_name]]      
      
      # the loop handles flexibly that there is one or more commodities, in any case it aggregates only over dynamics, not over commodities or nutrients
      for(EOI in exp_set){

        df_res <- rbind(rep(NA, ncol(df_res)), df_res)
        
        aggr_names <- paste0(EOI, c("_X_aggrall"))
        row.names(df_res)[1] <- aggr_names
        
        # name of contemporaneous regressor
        base_reg_name <- paste0(EOI,"_X_",original_rfs_treatments)
        
        # Contemporaneous, lead, and lag values
        # Order does NOT matter currently
        if(rfs_pya==0 & rfs_fya==0){
          all_roi <- c(base_reg_name, 
                       grep(pattern = paste0(base_reg_name,"_lag"), 
                            names(reg_res$coefficients), value = TRUE),
                       grep(pattern = paste0(base_reg_name,"_lead"), 
                            names(reg_res$coefficients), value = TRUE))
        }
        if(rfs_pya > 0 & rfs_fya > 0){
          all_roi <- c(grep(pattern = paste0(base_reg_name,"_",rfs_pya,"pya"), 
                            names(reg_res$coefficients), value = TRUE),
                       grep(pattern = paste0(base_reg_name,"_",rfs_fya,"fya"), 
                            names(reg_res$coefficients), value = TRUE))
        }
          
        df_res[aggr_names[1],"Estimate"] <- reg_res$coefficients[all_roi] %>% sum()
        
        # select the part of the VCOV matrix that is to be used to compute the standard error of the sum
        # use formula for variance of sum of random variables : https://en.wikipedia.org/wiki/Variance#Sum_of_correlated_variables
        df_res[aggr_names[1],"Std. Error"] <- reg_res$cov.scaled[all_roi, all_roi] %>% as.matrix() %>% sum() %>% sqrt()
        
        df_res[aggr_names[1],"t value"]  <- (df_res[aggr_names[1],"Estimate"] - 0)/(df_res[aggr_names[1],"Std. Error"])
        
        # use t distribution with degrees of freedom equal to that used by fixest, i.e. after two way cluster adjustment.
        # does not make a significant difference given sample size
        df_res[aggr_names[1],"Pr(>|t|)"]  <- (2*pt(abs(df_res[aggr_names[1],"t value"]), 
                                                   lower.tail = FALSE, 
                                                   df = fixest_df)) 
      
      } # closes loop on exposures within the set used for this specific regression
    # } # closes condition on type of dynamic aggregation 
      
    regression_sets[[exp_set_name]]$df_res <- df_res
    
  } # closes loop on regressions to perform for this particular outcome
  
  
  
  
  # output wanted
  if(output == "est_object"){
    toreturn <- reg_res_main
  }
  
  if(output == "coef_table"){
    toreturn <- regression_sets
  }
  
  if(output == "everything"){
    toreturn <- list(reg_res_main, d_clean, regression_sets)
  }
  
  
  rm(d_clean, df_res)
  return(toreturn)
  rm(toreturn)
}

#### FIGURE HETEROGENEOUS TRENDS --------------------------------------------------------
# we just run the regression to extract the data used under the preferred specification
d_clean_out <- make_main_reg(outcome_variable = "undernourished_kcapita", 
                              preperiod_end = 2004, 
                              rfs_lead = 3, 
                              rfs_lag = 3, 
                              output = "everything")

d_clean_out[[1]]
expdec <- d_clean_out[[2]] 

# just redo prevalence, as this is what we want to aggregate over countries, but it's not in the data because not used per se in regression
# expdec <- dplyr::mutate(expdec, undernourished_kcapita = undernourished_kcapita / population_kcapita)
expdec <- dplyr::filter(expdec, year <= 2004 | year >= 2011)

# demean, using convenient fixest fnct
expdec$undernourished_kcapita_dm <- fixest::demean(X = as.formula("undernourished_kcapita ~ country + year"), data = expdec, as.matrix = TRUE) %>% unname()


# excluded_outcomes <- outcomes[outcomes$year %in% c(2005:2010),c("country", "year", "undernourished_kcapita", "population_kcapita")]
# expdecfill <- how_to_join(expdec, excluded_outcomes, by = c("country"))

# make a variable for quantiles of import dependency
Q <- 10 # define quantile
expdec <- mutate(expdec, exposure_Q = cut_number(dependency_calorie_total, n = Q, labels = paste0("exposure_Q", 1:Q)))
# check that it worked out
#quantile(expdec$dependency_calorie_total, seq(0, 1, 1/Q))
#head(expdec[!duplicated(expdec$country),c("country", "year", "dependency_calorie_total", "exposure_Q")])

yeardec <- ddply(expdec, c("year", "exposure_Q"), summarise, 
                 undernourished_kcapita = mean(undernourished_kcapita), 
                 undernourished_kcapita_dm = mean(undernourished_kcapita_dm))

ggplot(yeardec, aes(x = year, y = undernourished_kcapita_dm, group = exposure_Q)) +
  geom_line(aes( col = exposure_Q)) + # linetype=exposure_Q,
  
  # scale_linetype_manual(breaks=paste0("exposure_Q", 1:Q),
  #                       values=c("solid", "dotted", "twodash", "longdash", "dotdash"),
  #                       labels=c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")[1:Q],
  #                       name="Calorific import \n dependency quintiles") +
  scale_colour_brewer(breaks=paste0("exposure_Q", 1:Q), 
                      palette = "Paired", 
                      labels=c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")[1:Q], # needs to be repeated so that legend isn't doubled... 
                      name="Calorific import \n dependency quintiles") +
  
  annotate("rect", xmin = 2004, xmax = 2011, 
           ymin = min(yeardec$undernourished_kcapita_dm), 
           ymax = max(yeardec$undernourished_kcapita_dm),
           alpha = .3, col = "lightgrey") +
  geom_label(aes(label="Years excluded from analysis",
                 x=2007.5,
                 y=max(undernourished_kcapita_dm)), alpha = 0.8, col = "black",) +
  scale_x_continuous(breaks = c(2001, 2004, 2011, 2014, 2022)) +
  scale_y_continuous(name = "Thousand undernourished people, country & year demeaned") + 
  theme_minimal() +
  theme(legend.position="right", 
        plot.title = element_text(size = 10, face = "bold"), 
        axis.title.y.left=element_text(size=10,face="bold", hjust = 1),
        axis.title.y.right=element_text(size=10,face="bold", hjust = 1),
        axis.title.x=element_blank(), #element_text(size=10,face="bold", hjust = 0.5), 
        panel.grid = element_line(inherit.blank = TRUE))  

# and extract number of people comprised in the sample in 2011
d_clean_out %>% dplyr::filter(year == 2011) %>% dplyr::select(population_kcapita) %>% sum()

#### TABLE 1 - REGRESSION --------------------------------------------------------------

est_data_obj_list <- list()
i <- 1
for(LINTREND in c(FALSE, TRUE)){
  #for(SQRTTREND in c(FALSE, TRUE)){
  for(LOGTREND in c(FALSE, TRUE)){
    est_data_obj_list[[i]] <- make_main_reg(outcome_variable = "undernourished_kcapita", 
                                      preperiod_end = 2004, 
                                      rfs_lead = 3, 
                                      rfs_lag = 3, 
                                      s_trend = LINTREND, 
                                      #s_trend_sq = SQRTTREND,
                                      s_trend_log = LOGTREND,
                                      output = "everything"
                                      )
    
    i <- i + 1
  #}
  }
}

# extract aggregated effects, in the coef table put in 3rd position in the list
coef_table_list <- lapply(est_data_obj_list, FUN = function(x){coef_df <- x[[3]][["total"]][["df_res"]]
                                                               est <- coef_df[grepl("_aggrall", row.names(coef_df)), "Estimate"]%>%formatC(format = "f", digits = 3)
                                                               SE <- coef_df[grepl("_aggrall", row.names(coef_df)), "Std. Error"]%>%formatC(format = "f", digits = 3)
                                                               paste0(est," \n (",SE,")")})



# each element of est_obj_list is a list with the fixest object in the first element, and the data d_clean in the second
# extract a list of estimation objects only 
est_obj_list <- lapply(est_data_obj_list, FUN = function(x){x[[1]]})

fixest_dict=c(undernourished_kcapita = "# people undernourished \n offset by total population",
               dependency_calorie_total_X_statute_conv = "$\\hat{\\beta}^{0}$",
               dependency_calorie_total_X_statute_conv_lead1 = "$\\hat{\\beta}^{1}$",
               dependency_calorie_total_X_statute_conv_lead2 = "$\\hat{\\beta}^{2}$",
               dependency_calorie_total_X_statute_conv_lead3 = "$\\hat{\\beta}^{3}$",
              dependency_calorie_total_X_statute_conv_lead4 = "$\\hat{\\beta}^{4}$",
              dependency_calorie_total_X_statute_conv_lag1 = "$\\hat{\\beta}^{-1}$",
               dependency_calorie_total_X_statute_conv_lag2 = "$\\hat{\\beta}^{-2}$",
               dependency_calorie_total_X_statute_conv_lag3 = "$\\hat{\\beta}^{-3}$", 
              dependency_calorie_total_X_statute_conv_lag4 = "$\\hat{\\beta}^{-4}$", 
              dependency_calorie_total_trend_lin = "$\\hat{\\tau}$", 
              dependency_calorie_total_trend_log = "$\\hat{\\theta}$"
              )


etable(est_obj_list, 
       tex = TRUE,
       title = "Dynamic coefficients of quasi-poisson regressions",
       label = "table1",
       # tabular = "X",
       dict = fixest_dict,
       depvar = FALSE,
       style.tex = style.tex(var.title = "\\midrule \\emph{Coefficients}"), # for some reason I need to add back the black line between headers and coefficients part
       headers = list("Dep. var." = list("# people undernourished \n (offset by total population)" = length(est_obj_list)),
                      "_Full effects:" = list(coef_table_list)), # the underscore places this head below the model header
       order =  c("-3", "-2", "-1", "{0}", "{1}", "{2}", "{3}"),
       # extralines = list("^^Calorific import dependency $*$ \n RFS2 mandates in year:" = rep(" ", length(est_obj_list))),
       digits = 3,
       signif.code = NA,
       tpt = TRUE,
       # file = here("temp_data", "reg_results", "table1"), 
       # replace = TRUE
       placement = "H")


#### ROBUSTNESS CHECKS ------------------------------------------------------------------------------
# The function is written in base R and exactly copied from https://github.com/ArielOrtizBobea/spec_chart/blob/master/spec_chart_function.R
schart <- function(data, labels=NA, highlight=NA, n=1, index.est=1, index.se=2, index.ci=NA,
                   order="asis", ci=.95, ylim=NA, axes=T, heights=c(1,1), leftmargin=11, offset=c(0,0), ylab="Coefficient", lwd.border=1,
                   lwd.est=4, pch.est=21, lwd.symbol=2, ref=0, lwd.ref=1, lty.ref=2, col.ref="black", band.ref=NA, col.band.ref=NA,length=0,
                   col.est=c("grey60", "red3"), col.est2=c("grey80","lightcoral"), bg.est=c("white", "white"),
                   col.dot=c("grey60","grey95","grey95","red3"),
                   bg.dot=c("grey60","grey95","grey95","white"),
                   pch.dot=c(22,22,22,22), fonts=c(2,1), adj=c(1,1),cex=c(1,1)) {
  
  # Authors: Ariel Ortiz-Bobea (ao332@cornell.edu).
  # Version: March 10, 2020
  # If you like this function and use it, please send me a note. It might motivate
  # me to imporove it or write new ones to share.
  
  # Description of arguments
  
  # Data:
  # data: data.frame with data, ideally with columns 1-2 with coef and SE, then logical variables.
  # labels: list of labels by group. Can also be a character vector if no groups. Default is rownames of data.
  # index.est: numeric indicating position of the coefficient column.
  # index.se: numeric indicating position of the SE column.
  # index.ci: numeric vector indicating position of low-high bars for SE. Can take up to 2 CI, so vector can be up to length 4
  
  # Arrangement and basic setup:
  # highlight: numeric indicating position(s) of models (row) to highlight in original dataframe.
  # n: size of model grouping. n=1 removes groupings. A vector yields arbitrary groupings.
  # order: whether models should be sorted or not. Options: "asis", "increasing", "decreasing"
  # ci: numeric indicating level(s) of confidence. 2 values can be indicated.
  # ylim: if one wants to set an arbitrary range for Y-axis
  
  # Figure layout:
  # heights: Ratio of top/bottom panel. Default is c(1,1) for 1 50/50 split
  # leftmargin: amount of space on the left margin
  # offset: vector of numeric with offset for the group and specific labels
  # ylab: Label on the y-axis of top panel. Default is "Coefficient"
  # lwd.border: width of border and other lines
  
  # Line and symbol styles and colors:
  # lwd.est: numeric indicating the width of lines in the top panel
  # ref: numeric vector indicating horizontal reference lines(s) Default is 0.
  # lty.ref: Style of reference lines. Default is dash line (lty=2).
  # lwd.ref. Width of reference lines. Default is 1.
  # col.ref: vector of colors of reference lines. Default is black.
  # band.ref: vector of 2 numerics indicating upper abdn lower height for a band
  # col.band.ref: color of this band
  # col.est: vector of 2 colors indicating for "other" and "highlighted" models
  # col.est2: same for outer confidence interval if more than 1 confidence interval
  # col.dot: vector of 4 colors indicating colors for borders of symbol in bottom panel for "yes", "no", "NA", and "yes for highlighted model"
  # bg.dot : vector of 4 colors indicating colors for background of symbol in bottom panel for "yes", "no", "NA", and "yes for highlighted model"
  # pch.dot: style of symbols in bottom panel for "yes", "no", "NA", and "yes for highlighted model"
  # length: length of the upper notch on th vertical lines. default is 0.
  
  # Letter styles
  # fonts: numeric vector indicating font type for group (first) and other labels (second) (e.g. 1:normal, 2:bold, 3:italic)
  # adj: numeric vector indicating alignment adjustment for text label: 0 is left, .5 is center, 1 is right.
  # cex: numeric vector for size of fonts for top panel (first) and bottom panel (Second)
  
  # 1. Set up
  if (T) {
    # Arrange data
    d <- data
    rownames(d) <- 1:nrow(d)
    
    # Create ordering vector
    if (order=="asis")       o <- 1:length(d[,index.est])
    if (order=="increasing") o <- order(d[,index.est])
    if (order=="decreasing") o <- order(-d[,index.est])
    if (!is.numeric(d[,index.est])) {warning("index.est does not point to a numeric vector.") ; break}
    d <- d[o,]
    est <- d[,index.est] # Estimate
    if (length(index.ci)>1) {
      l1 <- d[,index.ci[1]]
      h1 <- d[,index.ci[2]]
      if (length(index.ci)>2) {
        l2 <- d[,index.ci[3]]
        h2 <- d[,index.ci[4]]
      }
    } else {
      if (!is.numeric(d[,index.se]))  {warning("index.se does not point to a numeric vector.") ; break}
      se  <- d[,index.se] # Std error
      ci <- sort(ci)
      a <- qnorm(1-(1-ci)/2)
      l1 <- est - a[1]*se
      h1 <- est + a[1]*se
      if (length(ci)>1) {
        l2 <- est - a[2]*se
        h2 <- est + a[2]*se
      }
    }
    
    # Table
    if (length(index.ci)>1) remove.index <- c(index.est,index.ci) else remove.index <- c(index.est,index.se)
    remove.index <- remove.index[!is.na(remove.index)]
    tab <- t(d[,-remove.index]) # get only the relevant info for bottom panel
    if (!is.list(labels) & !is.character(labels)) labels <- rownames(tab)
    
    # Double check we have enough labels
    if ( nrow(tab) != length(unlist(labels))) {
      print("Warning: number of labels don't match number of models.")
      labels <- rownames(tab)
    }
    
    # Plotting objects
    xs <- 1:nrow(d) # the Xs for bars and dots
    if (n[1]>1 & length(n)==1) xs <- xs + ceiling(seq_along(xs)/n) - 1 # group models by n
    if (length(n)>1) {
      if (sum(n) != nrow(d) ) {
        warning("Group sizes don't add up.")
      } else {
        idx <- unlist(lapply(1:length(n), function(i) rep(i,n[i])))
        xs <- xs + idx - 1
      }
    }
    h <- nrow(tab) + ifelse(is.list(labels),length(labels),0) # number of rows in table
    # Location of data and labels
    if (is.list(labels)) {
      index <- unlist(lapply(1:length(labels), function(i) rep(i, length(labels[[i]])) ))
      locs <- split(1:length(index),index)
      locs <- lapply(unique(index), function(i) {
        x <- locs[[i]]+i-1
        x <- c(x,max(x)+1)
      })
      yloc  <- unlist(lapply(locs, function(i) i[-1])) # rows where data points are located
      yloc2 <- sapply(locs, function(i) i[1]) # rows where group lables are located
    } else {
      yloc <- 1:length(labels)
    }
    
    # Range
    if (is.na(ylim[1]) | length(ylim)!=2) {
      if (length(index.ci)>2 | length(ci)>1) {
        ylim <- range(c(l2,h2,ref)) # range that includes reference lines
      } else {
        ylim <- range(c(l1,h1,ref))
      }
      ylim <- ylim + diff(ylim)/10*c(-1,1) # and a bit more
    }
    xlim <- range(xs) #+ c(1,-1)
  }
  
  # 2. Plot
  if (T) {
    #par(mfrow=c(2,1), mar=c(0,leftmargin,0,0), oma=oma, xpd=F, family=family)
    layout(t(t(2:1)), height=heights, widths=1)
    par(mar=c(0,leftmargin,0,0), xpd=F)
    
    # Bottom panel (plotted first)
    plot(est, xlab="", ylab="", axes=F, type="n", ylim=c(h,1), xlim=xlim)
    lapply(1:nrow(tab), function(i) {
      # Get colors and point type
      type <- ifelse(is.na(tab[i,]),3,ifelse(tab[i,]==TRUE,1, ifelse(tab[i,]==FALSE,2,NA)))
      type <- ifelse(names(type) %in% paste(highlight) & type==1,4,type) # replace colors for baseline model
      col <- col.dot[type]
      bg  <- bg.dot[type]
      pch <- as.numeric(pch.dot[type])
      sel <- is.na(pch)
      # Plot points
      points(xs, rep(yloc[i],length(xs)), col=col, bg=bg, pch=pch, lwd=lwd.symbol)
      points(xs[sel], rep(yloc[i],length(xs))[sel], col=col[sel], bg=bg[sel], pch=pch.dot[3]) # symbol for missing value
      
    })
    par(xpd=T)
    if (is.list(labels)) text(-offset[1], yloc2, labels=names(labels), adj=adj[1], font=fonts[1], cex=cex[2])
    # Does not accomodate subscripts
    text(-rev(offset)[1], yloc , labels=unlist(labels), adj=rev(adj)[1], font=fonts[2], cex=cex[2])
    # Accomodates subscripts at the end of each string
    if (F) {
      labels1 <- unlist(labels)
      lapply(1:length(labels1), function(i) {
        a  <- labels1[i]
        a1 <- strsplit(a,"\\[|\\]")[[1]][1]
        a2 <- rev(strsplit(a,"\\[|\\]")[[1]])[1]
        if (identical(a1,a2))  a2 <- NULL
        text(-rev(offset)[1], yloc[i], labels=bquote(.(a1)[.(a2)]), adj=adj[2], font=fonts[2], cex=cex[2])
      })
    }
    par(xpd=F)
    
    # Top panel (plotted second)
    colvec  <- ifelse(colnames(tab) %in% paste(highlight), col.est[2], col.est[1])
    bg.colvec  <- ifelse(colnames(tab) %in% paste(highlight), bg.est[2], bg.est[1])
    colvec2 <- ifelse(colnames(tab) %in% paste(highlight),col.est2[2], col.est2[1])
    plot(est, xlab="", ylab="", axes=F, type="n", ylim=ylim, xlim=xlim)
    # Band if present
    if (!is.na(band.ref[1])) {
      rect(min(xlim)-diff(xlim)/10, band.ref[1], max(xlim)+diff(xlim)/10, band.ref[2], col=col.band.ref, border=NA)
    }
    # Reference lines
    abline(h=ref, lty=lty.ref, lwd=lwd.ref, col=col.ref)
    # Vertical bars
    if (length(ci)>1 | length(index.ci)>2) arrows(x0=xs, y0=l2, x1=xs, y1=h2, length=length, code=3, lwd=rev(lwd.est)[1], col=colvec2, angle=90)
    arrows(x0=xs, y0=l1, x1=xs, y1=h1, length=length, code=3, lwd=lwd.est[1]     , col=colvec, angle=90)
    points(xs, est, pch=pch.est, lwd=lwd.symbol, col=colvec, bg=bg.colvec)
    # Axes
    if (axes) {
      axis(2, las=2, cex.axis=cex[1], lwd=lwd.border)
      axis(4, labels=NA, lwd=lwd.border)
    }
    mtext(ylab, side=2, line=3.5, cex=cex[1])
    box(lwd=lwd.border)
    
  }
  
} 

# order from more important in terms of possible change in coefficient, to specifications that change only the SEs. 
schart_labels <- list(
  # with trends in first position
  "Exposure trend" = c("linear", 
                       "logarithmic"),

  # then choices that properly may introduce bias 
  "Pre-treatment period:" = c("2001-2002",
                              "2001-2003", 
                              "2001-2004"),
  
  "Farther dynamic effects" = c("Lags up to 4 years", 
                                "Leads up to 4 years"),
  
  # sliding to choices affecting measurement error (not necessarily the good kind)
  "Year fixed effects (in addition to country FE)" = c(""), 
  
  "Exposure measurement over:" = c("2001-2007", 
                                   "2004-2007",
                                   "2006-2007"),
  
  "W/o countries outlying in exposure time variation" = c(""),
  
  "Offset population:" = c("Constant (2007)",
                           "Running (contemporaneous)"),
  
  # and choices affecting only SEs
  "Two-way clustering, at year level and:" = c("country", 
                                               "exposure's 50% quantiles", 
                                               "exposure's 20% quantiles", 
                                               "exposure's 10% quantiles (deciles)"),
  
  "Distribution assumption:" = c("Poisson",
                                 "quasi-Poisson")
  
)

# Need to embed in a function because we then loop over some arguments passed to this function to make different specifications
# this function has the same arguments as those of make_main_reg that we want to test, + possibly others
make_spec_chart_df <- function(outcome_variable = "undernourished_kcapita",
                               offset = "population_kcapita", 
                               preperiod_end = 2004, 
                               exclude_bad_expo_proxy = FALSE,
                               # exposure 
                               expo_measure_period = "2001_2007", 
                               # dynamics 
                               rfs_lead = 0,
                               rfs_lag = 0,
                               # heterogeneity control
                               s_trend = TRUE,
                               s_trend_log = TRUE,
                               fe = "country + year", 
                               # estimation 
                               distribution = "quasipoisson",
                               clustering = "twoway", 
                               cluster_var1 = "country", 
                               cluster_var2 = "year"
                               
){
  # run regression and extract aggregated effect 
  regression_sets <- make_main_reg(outcome_variable = outcome_variable,
                                   offset = offset,
                                   preperiod_end = preperiod_end,
                                   exclude_bad_expo_proxy = exclude_bad_expo_proxy,
                                   # exposure 
                                   expo_measure_period = expo_measure_period,
                                   # dynamics 
                                   rfs_lead = rfs_lead,
                                   rfs_lag = rfs_lag,
                                   # heterogeneity control
                                   s_trend = s_trend,
                                   s_trend_log = s_trend_log,
                                   fe = fe,
                                   # estimation 
                                   distribution = distribution,
                                   clustering = clustering,
                                   cluster_var1 = cluster_var1,
                                   cluster_var2 = cluster_var2,
                                   # these two last arguments determine the type of the output non flexibly
                                   commodities = "total",
                                   output = "coef_table")
  
  coef_df <- regression_sets[["total"]][["df_res"]]                            
  aggr_coef_df <- coef_df[grepl("_aggrall", row.names(coef_df)), ]
  
  
  ### make indicator variables that will be used to label specifications. 
  # Set everything to FALSE, and switch them on afterwards, according to specification passed to make_spec_chart_df
  # /!\ THE ORDER HERE MATTERS ! IT MUST MATCH THE ORDER IN schart_labels   BELOW
  ind_var <- data.frame(
    #"Exposure trend" 
    "s_trend" = FALSE,
    "s_trend_log" = FALSE,
    # "Pre-treatment period:"
    "preperiod_end_2002" = FALSE,
    "preperiod_end_2003" = FALSE,
    "preperiod_end_2004" = FALSE,
    # "Farther dynamic effects" 
    "lag4" = FALSE,
    "lead4" = FALSE,
    
    # "Year fixed effects (in addition to country FE)" 
    "year_FE" = FALSE,
    # "Exposure measurement over:" 
    "expo_measure_period_2001_2007" = FALSE,
    "expo_measure_period_2004_2007" = FALSE,
    "expo_measure_period_2006_2007" = FALSE,
    # "W/o countries outlying in exposure time variation"
    "exclude_bad_expo_proxy" = FALSE,
    # "Offset population:" 
    "population_kcapita_2007" = FALSE,
    "population_kcapita" = FALSE,

    
    # "Two-way clustering, at year level and:"
    "country" = FALSE, 
    "expo_50tiles" = FALSE,
    "expo_20tiles" = FALSE,
    "expo_10tiles" = FALSE,
    # "Distribution assumption:"
    "poisson" = FALSE,
    "quasipoisson" = FALSE
  )
  
  
  ## Change the indicator variable to TRUE, for specification being run
  
  # trends 
  # the condition makes sure that if a trend is removed because of perfect colinearity 
  # (which occurs when long annual dynamics are specified), then the regression is not marked as featuring it. 
  if(any(grepl("trend_lin", row.names(coef_df)))){
    ind_var[,"s_trend"] <- s_trend
  }
  if(any(grepl("trend_log", row.names(coef_df)))){
    ind_var[,"s_trend_log"] <- s_trend_log
  }
  # end of pre-treatment period
  ind_var[,paste0("preperiod_end_",preperiod_end)] <- TRUE
  # farther lag and lead
  if(rfs_lag == 4){ind_var[,"lag4"] <- TRUE}
  if(rfs_lead == 4){ind_var[,"lead4"] <- TRUE}
  
  # FE
  if(grepl("year", fe)){ind_var[,"year_FE"] <- TRUE}
  # exposure measurement period
  ind_var[,paste0("expo_measure_period_",expo_measure_period)] <- TRUE
  # outlying exposure variation
  ind_var[,"exclude_bad_expo_proxy"] <- exclude_bad_expo_proxy
  # offset
  ind_var[,offset] <- TRUE
  
  # clustering 
  if(cluster_var1=="country"){ind_var[,cluster_var1] <- TRUE}
  if(is.numeric(cluster_var1)){ind_var[,paste0("expo_",cluster_var1,"tiles")] <- TRUE}
  # distribution 
  ind_var[,distribution] <- TRUE
  
  
  ### Bind together coeff, SE, and specification labels
  spec_df <- cbind(data.frame(Estimate = unname(aggr_coef_df["Estimate"]), 
                              SE = unname(aggr_coef_df["Std. Error"])), # the names do not matter, Est and SE are found with column indexes 1 and 2 resp. in schart. 
                   ind_var)
  
  return(spec_df)
}


reg_stats_indvar_list <- list()
i <- 1
# run loops from the first elements of schart_labels to the last
# run trend loops at the smallest level, i.e. closest one to each others, so they are next to each others on the chart, within each alternative spec. 

for(PREEND in c(2002, 2003, 2004)){
for(LAG in c(3,4)){
for(LEAD in c(3,4)){
if(!(LAG==4 & LEAD==4)){ # prevent proceeding to loop if too many dynamics, one will be dropped bc of perfect colinearity anyway
for(FE in c("country", "country + year")){
for(EXPPER in c("2001_2007", "2006_2007")){
for(WOOTL in c(FALSE, TRUE)){
for(OFFSET in c("population_kcapita_2007", "population_kcapita")){
for(CLT in list("country", 50, 20, 10)){
for(DISTR in c("poisson", "quasipoisson")){
for(LOGTREND in c(FALSE, TRUE)){ # log trend first, to match order in Table 1. 
for(LINTREND in c(FALSE, TRUE)){
reg_stats_indvar_list[[i]] <- make_spec_chart_df(outcome_variable = "undernourished_kcapita",
                                             preperiod_end = PREEND,
                                             rfs_lag = LAG, 
                                             rfs_lead = LEAD,
                                             fe = FE,
                                             expo_measure_period = EXPPER, 
                                             exclude_bad_expo_proxy = WOOTL,
                                             offset = OFFSET, 
                                             cluster_var1 = CLT,
                                             distribution = DISTR,
                                             s_trend_log = LOGTREND,
                                             s_trend = LINTREND)
i <- i + 1
}}}}}}}}}}}}

# convert to dataframe to be able to chart
reg_stats_indvar <- bind_rows(reg_stats_indvar_list)

scdf <- reg_stats_indvar
# remove all those regressions that have both lag4 and lead4, as this is not compatible
scdf <- scdf[!(reg_stats_indvar$lag4 & reg_stats_indvar$lead4),]

# save it 
if(sum(duplicated(reg_stats_indvar))==0 ){ # i.e. 50 currently & nrow(reg_stats_indvar)+1 == i
  saveRDS(reg_stats_indvar, file.path(paste0("temp_data/reg_results/spec_chart_df_robustness",Sys.Date())))
} else{print(paste0("SOMETHING WENT WRONG in spec_chart_df"))}


scdf <- readRDS(file.path(paste0("temp_data/reg_results/spec_chart_df_robustness2022-08-12")))

# are there duplicated estimates (this is not expected) 
scdf[duplicated(scdf[,c(1,2)]), ] 

large <- scdf[scdf$Estimate>10,]
summary(large$preperiod_end_2002)
summary(large$lead4)
summary(large$lag4)
summary(large$year_FE)
# so those very large estimates are driven by 2002 pre period only, without year FE ever, either lag4 or lead4, and whatever the other spec
# there is probably a very large idiosynratic error involved in this case, that is not captured by year FE. 
scdf <- scdf[!(scdf$preperiod_end_2002 & !(scdf$year_FE) & (scdf$lag4 | scdf$lead4)), ]
  

# Unsurprisingly, year-FE-less estimates are very wide (either positively or negatively)
# remove them for clarity 
scdf <- scdf[scdf$year_FE,]

# further reduce rows for visibiity, by keeping only preferred trends spec 
scdf <- scdf[scdf$s_trend & scdf$s_trend_log,]

#### Full as is -------------------------------------------------------------------
scdf_sig_idx <- dplyr::transmute(scdf, is_sig = abs(Estimate) < 1.96*SE) %>% pull(is_sig) %>% which()
schart(scdf, 
       labels = schart_labels,
       order="asis", # "increasing",# 
       highlight=scdf_sig_idx, 
       heights=c(1,1.5),
       pch.dot=c(20,20,20,20), # it's necessary that a length 4 vector is given, for bottom panel to show highlighted models
       ci=c(.95),
       col.est=c("grey70", "red3"),
       col.dot=c("grey70","grey95","grey95","red3"),
       bg.dot=c("grey60","grey95","grey95","white"),
       leftmargin = 12,
       ylab = "Estimates",
       lwd.est = 3,
       lwd.symbol = 1, 
       fonts=c(2,1), adj=c(1,1), cex=c(0.6,0.6)
)


#### COUNTERFACTUAL MAGNITUDE SIMULATIONS -----------------------------------------------------------

# implied by coefficient, for undernourishment outcome and total commodity dependency 
# The aim is to estimate the annually averaged count of undernourished people avoided globally
magnitudes_list <- list()
MDL <- 1
set.seed(8888)
for(EST_DATA_OBJ in est_data_obj_list){
  
  EST_OBJ <- EST_DATA_OBJ[[1]]
  d_clean <- EST_DATA_OBJ[[2]]
  
  # coefficients and vcov matrix
  beta <- EST_OBJ$coefficients
  beta_cov <- vcov(EST_OBJ)
  
  # fitted values
  nrow(d_clean) == length(EST_OBJ$fitted.values)
  all(EST_OBJ$fitted.values == fitted(EST_OBJ))
  
  d_clean$fv <- EST_OBJ$fitted.values  
  d_clean$lp <- EST_OBJ$linear.predictors
  summary(d_clean$fv)
  # summary(d_clean[,outcome_variable])
  
  
  ##### MONTE CARLO SIMULATIONS ####
  # 3 steps, repeated for each draw
  # 1. Sum dynamic coefficients --> cumulative coefficients (common to all obs.)
  # 2. Multiply these cumulative coefficients by the dependency exposure --> scaled effects (common within countries)
  # 3. Insert these in the counterfactual formula (which features the fitted values, that are country-year specific)
  
  # Then, for each draw, we output different quantities: 
  # - Country averages over time to see which countries were hit the worst
  # - Annual sums over countries to see which years were the worst 
  # - The annually averaged count of undernourished people avoided globally
  # - The global and period avergae of the undernourishment prevalence  
  
  # collected here 
  sim_outputs <- list(periodavg_i = NULL, 
                      globalsum_t = NULL, 
                      globalsum_periodavg = NULL)
  
  # In preparation for step 2, make a cross-section of the countries, with their time invariant dependency 
  d_clean_cs <- d_clean[!duplicated(d_clean$country), c("country", "dependency_calorie_total")]
  
  # And for step 3, subset the unique identifiers of panel d_clean and the fitted values
  # FOR THE YEARS POST TREATMENT! 
  d_clean_base <- d_clean[d_clean$year>=2008 ,c("country", "year", "fv", "population_kcapita")]
  
  n <- 1000
  # store in a single row the aggregated effect (of dependency_calorie_total, aggregated over treatment window)
  rep_effects <- data.frame(matrix(ncol = n, nrow = 1)) 
  
  row.names(rep_effects) <- c("total")
  
  # mod_adj <- reg_res_main
  # each column is one replication. Each row is one beta (coefficient). 
  beta_draw <- t(MASS::mvrnorm(n, mu = beta, Sigma = beta_cov))
  
  for(draw in 1:n){
    
    beta <- beta_draw[,draw]
    
    # Step 1. Make cumulative coefficients
    
    annual_coeff_names <- grep(pattern = "dependency_calorie_total_X_statute_conv", names(coef(EST_OBJ)), value = TRUE)
    
    cum_beta <- beta[annual_coeff_names] %>% sum() # this is a scalar in the present case
    
    
    # Step 2. Make scaled effects
    
    # important that the scaled effect df be "reinitialized" at each replication 
    # the cross-section of exposure data does not change with every replication
    scaled_effects_i <- d_clean_cs
    scaled_effects_i$scaled_effects_i <- d_clean_cs[,"dependency_calorie_total"] * cum_beta
    
    
    # Step 3. Make the country-year predicted factual - counterfactual difference 
    ctfl_it <- left_join(d_clean_base, scaled_effects_i, by = "country")
    
    #  # NOTE THE MINUS SCALED EFFECTS: it represents the counterfactual scenarios of mandates 1bgal lower every year
    # Also, the 1 - exp() reflects that we difference factual - counterfactual, and not the other way round (just a matter of interpretation eventually)
    ctfl_it <- dplyr::mutate(ctfl_it, ctfl_diff = fv * (1 - exp(-scaled_effects_i))) # NOTE THE MINUS SCALED EFFECTS
    
    
    # return outputs of interest
    sim_outputs[["periodavg_i"]][[draw]] <- ddply(ctfl_it, "country", summarise, 
                                                  kcapita_diff_periodavg = mean(ctfl_diff, na.rm = TRUE))
    
    sim_outputs[["globalsum_t"]][[draw]] <- ddply(ctfl_it, "year", summarise, 
                                                  kcapita_diff_globalsum = sum(ctfl_diff, na.rm = TRUE), 
                                                  annual_pop = sum(population_kcapita, na.rm = TRUE)) # this would be constant, if offset was pop in 2007
    
    # within each year, divide the global count of undernourished people by the total population that year, to get the annual global prevalence effect
    sim_outputs[["globalsum_t"]][[draw]] <- dplyr::mutate(sim_outputs[["globalsum_t"]][[draw]], 
                                                          preval_diff_globalsum = kcapita_diff_globalsum/annual_pop)
    
    # we do averages of already computed, annual global sums
    sim_outputs[["globalsum_periodavg"]][[draw]] <- data.frame(kcapita_diff = mean(sim_outputs[["globalsum_t"]][[draw]][,"kcapita_diff_globalsum"], na.rm = TRUE), 
                                                               preval_diff = mean(sim_outputs[["globalsum_t"]][[draw]][,"preval_diff_globalsum"], na.rm = TRUE))
    
  }
  
  sim_summary <- list()
  
  sim_summary[["periodavg_i"]] <- sim_outputs[["periodavg_i"]] %>% bind_rows() %>% ddply("country", summarise,
                                                                                         est_kcapita_diff_periodavg = mean(kcapita_diff_periodavg, na.rm = T),
                                                                                         se_kcapita_diff_periodavg = sd(kcapita_diff_periodavg, na.rm = T))
  
  sim_summary[["globalsum_t"]] <- sim_outputs[["globalsum_t"]] %>% bind_rows() %>% ddply("year", summarise,
                                                                                         est_kcapita_diff_globalsum = mean(kcapita_diff_globalsum, na.rm = T),
                                                                                         se_kcapita_diff_globalsum = sd(kcapita_diff_globalsum, na.rm = T), 
                                                                                         est_preval_diff_globalsum = mean(preval_diff_globalsum, na.rm = T),
                                                                                         se_preval_diff_globalsum = sd(preval_diff_globalsum, na.rm = T))
  
  sim_summary[["globalsum_periodavg"]] <- sim_outputs[["globalsum_periodavg"]] %>% bind_rows() %>% summarise( 
    est_kcapita_diff_gspa = mean(kcapita_diff, na.rm = TRUE), 
    se_kcapita_diff_gspa = sd(kcapita_diff, na.rm = TRUE), 
    est_preval_diff_gspa = mean(preval_diff, na.rm = TRUE), 
    se_preval_diff_gspa = sd(preval_diff, na.rm = TRUE))
  
  # ON EN EST LAAAAAAAAAAA
  # effects_stats <- dplyr::mutate(effects_stats, ci95lb = (avg - qt(0.975, df = n-1)*std_dev/sqrt(n)))
  # effects_stats <- dplyr::mutate(effects_stats, ci95hb = (avg + qt(0.975, df = n-1)*std_dev/sqrt(n)))
  # 
  # row.names(effects_stats) <- row.names(rep_effects)
  # 
  # # return 
  # effects_list[[CNT]][["crop_effects"]] <- crop_effects
  # effects_list[[CNT]][["total_effect"]] <- total_effect
  
  # Print figures features in the text: 
  trend_names <- c("dependency_calorie_total_trend_lin", "dependency_calorie_total_trend_log")
  coef_names <- names(beta)
  
  if(!any(grepl("trend_lin", coef_names)) & !any(grepl("trend_log", coef_names)) ){
    print(paste0("Not controlling for exposure trends: ", 
                 round(sim_summary[["globalsum_periodavg"]]$est_kcapita_diff_gspa, 0), 
                 " (+/- ", round(sim_summary[["globalsum_periodavg"]]$se_kcapita_diff_gspa, 0), ") thousand people"))
  }
  if(any(grepl("trend_lin", coef_names)) & !any(grepl("trend_log", coef_names)) ){
    print(paste0("Controlling for exposure level, linear trends: ", 
                 round(sim_summary[["globalsum_periodavg"]]$est_kcapita_diff_gspa, 0), 
                 " (+/- ", round(sim_summary[["globalsum_periodavg"]]$se_kcapita_diff_gspa, 0), ") thousand people"))
  }
  if(!any(grepl("trend_lin", coef_names)) & any(grepl("trend_log", coef_names)) ){
    print(paste0("Controlling for exposure level, logarithmic trends: ", 
                 round(sim_summary[["globalsum_periodavg"]]$est_kcapita_diff_gspa, 0), 
                 " (+/- ", round(sim_summary[["globalsum_periodavg"]]$se_kcapita_diff_gspa, 0), ") thousand people"))
  }
  if(any(grepl("trend_lin", coef_names)) & any(grepl("trend_log", coef_names)) ){
    print(paste0("Controlling for exposure level, linear and logarithmic trends: ", 
                 round(sim_summary[["globalsum_periodavg"]]$est_kcapita_diff_gspa, 0), 
                 " (+/- ", round(sim_summary[["globalsum_periodavg"]]$se_kcapita_diff_gspa, 0), ") thousand people"))
  }
    
    
  
  magnitudes_list[[MDL]] <- sim_summary
  MDL <- MDL + 1
}
print("The # of undernourished people that could have been avoided annually, over the hundred countries in the sample, had the mandates been 1bgal lower than what they actually were, every year :") 


magnitudes_list[[1]]

saveRDS(magnitudes_list, here("temp_data", "reg_results", "magnitudes_list.Rdata"))




#### EXPLORATORY ANALYSIS  -----------------------------------------------------------------------------
schart_labels <- list(
  "Including 2000-2004 pre-RFS period" = c(""),
  
  # "Weighting exposure by inverse GDP per capita" = c(""),
  
  "Exposure period:" = c("2001-2007", 
                         "2004-2007",
                         "2006-2007"),
  # "Relative to population:" = c("initial (2007)", 
  #                               "contemporaneous"), 
  
  # "Lags and leads each averaged over:" = c("3 years", 
  #                                          "4 years"),
  
  "With annual lags up to:" = c("none",
                                "1 year",
                                 "2 years",
                                 "3 years"), 
  
  "With annual leads up to:" = c("none",
                                 "1 year",
                                  "2 years",
                                  "3 years"),
  
  "Exposure trend" = c("linear", 
                       "logarithmic")
  
  # "Fixed effects" = c("country", 
  #                     "year"), 
  
  # "Distribution assumption:" = c(# "Poisson",
  #                                "quasi-Poisson", 
  #                                "gaussian")
  
)

# Need to embed in a function because we then loop over some arguments passed to this function to make different specifications
# this function has the same arguments as those of make_main_reg that we want to test, + possibly others
make_spec_chart_df <- function(outcome_variable = "undernourished_kcapita",
                               offset = "population_kcapita", 
                               weights = FALSE,
                               start_year = 2009,
                               end_year = 2020, 
                               preperiod_end = 2004, 
                               # exposure 
                               gdp_weighting = TRUE,
                               expo_measure_period = "2001_2007", 
                               # dynamics 
                               rfs_lead = 0,
                               rfs_lag = 0,
                               rfs_fya = 0,
                               # heterogeneity control
                               control_remaining_dependency = TRUE,
                               s_trend = FALSE,
                               s_trend_log = FALSE,
                               fe = "country + year", 
                               # estimation 
                               distribution = "quasipoisson",
                               clustering = "twoway", 
                               cluster_var1 = "country", 
                               cluster_var2 = "year"

){
  
  regression_sets <- make_main_reg(outcome_variable = outcome_variable, 
                                 offset = offset, 
                                 weights = weights,
                                 start_year = start_year, 
                                 end_year = end_year, 
                                 preperiod_end = preperiod_end, 
                                 gdp_weighting = gdp_weighting,
                                 expo_measure_period = expo_measure_period, 
                                 rfs_lead = rfs_lead, 
                                 rfs_lag = rfs_lag, 
                                 rfs_fya = rfs_fya,
                                 control_remaining_dependency = control_remaining_dependency, 
                                 s_trend = s_trend, 
                                 s_trend_log = s_trend_log, 
                                 fe = fe,
                                 distribution = distribution, 
                                 clustering = clustering, 
                                 cluster_var1 = cluster_var1, 
                                 cluster_var2 = cluster_var2, 
                                 # these two last arguments determine the type of the output non flexibly
                                 commodities = "total",
                                 output = "coef_table")
  
  
   coef_df <- regression_sets[["total"]][["df_res"]]                            
                                 
   aggr_coef_df <- coef_df[grepl("_aggrall", row.names(coef_df)), ]
  
   
  ### make indicator variables that will be used to label specifications. 
  # /!\ THE ORDER HERE MATTERS ! IT MUST MATCH THE ORDER IN schart_labels   BELOW
  ind_var <- data.frame(
    # include pre-treatment period observations (2000-2004)

    # "gdp_weighting" = FALSE,
    
    # pre-treament period for dependency measurement
    "expo_measure_period_2001_2007" = FALSE,
    "expo_measure_period_2004_2007" = FALSE,
    "expo_measure_period_2006_2007" = FALSE,
    
    # offset type
    # "population_kcapita_2007" = FALSE,
    # "population_kcapita" = FALSE,
    
    # dynamics 
    # "ya3" = FALSE,
    # "ya4" = FALSE,
    "lag0" = FALSE,
    "lag1" = FALSE,
    "lag2" = FALSE,
    "lag3" = FALSE,
    "lead0" = FALSE,
    "lead1" = FALSE,
    "lead2" = FALSE,
    "lead3" = FALSE,

    
    # trends
    "s_trend" = FALSE,
    "s_trend_log" = FALSE
    
    # FE
    # "country_FE" = FALSE,
    # "year_FE" = FALSE,
    
  # distribution
    # "quasipoisson" = FALSE,
    # "poisson" = FALSE,
    # "gaussian" = FALSE
  )
  
  ## Change the indicator variable to TRUE, for specification being run
  
  # GDP weighting
  # ind_var[,"gdp_weighting"] <- gdp_weighting

  # pretreat period for dependency
  ind_var[,paste0("expo_measure_period_",expo_measure_period)] <- TRUE
  
  # ind_var[,offset] <- TRUE

  # dynamics
  ind_var[,paste0("lag",rfs_lag)] <- TRUE
  ind_var[,paste0("lead",rfs_lead)] <- TRUE
  # if(rfs_fya>0){ind_var[,paste0("ya",rfs_fya)] <- TRUE} # NOTE that only rfs_fya is a parameter now

  # trends 
  # the condition makes sure that if a trend is removed because of perfect colinearity 
  # (which occurs when long annual dynamics are specified), then the regression is not marked as featuring it. 
  if(any(grepl("trend_lin", row.names(coef_df)))){
    ind_var[,"s_trend"] <- s_trend
  }
  if(any(grepl("trend_log", row.names(coef_df)))){
    ind_var[,"s_trend_log"] <- s_trend_log
  }
  
  # FE
  # if(fe == "country"){ind_var[,"country_FE"] <- TRUE}
  # if(fe == "year"){ind_var[,"year_FE"] <- TRUE}
  # if(fe == "country + year"){ind_var[,"country_FE"] <- TRUE
  #                        ind_var[,"year_FE"] <- TRUE}
  
  # distribution 
  ind_var[,distribution] <- TRUE
  
  ### Bind together coeff, SE, and specification labels
  spec_df <- cbind(data.frame(Estimate = unname(aggr_coef_df["Estimate"]), 
                              SE = unname(aggr_coef_df["Std. Error"])), # the names do not matter, Est and SE are found with column indexes 1 and 2 resp. in schart. 
                   ind_var)
  
  return(spec_df)
}


### Add to the list the specifications that are to be compared in the chart. 
# These are particular departures from the preferred specification (which arguments are already set by default)
# Specifying only arguments for which it's changing + those defining the estimate we are interested in plotting the spec chart. 
# ## the main specification
# reg_stats_indvar_list[["main"]] <- make_spec_chart_df(island = ISL,
#                                                       outcome_variable = paste0("lucpf",SIZE,"p_pixelcount"))
# i <- i+1

PREOBS <- TRUE
PREPER <- "2004_2007"
OFFSET <-  "population_kcapita"
LINTREND <- FALSE  
LOGTREND <- TRUE
DISTR <- "quasipoisson"
FYA <- 3
LAG <- 3
LEAD <- 3

reg_stats_indvar_list <- list()
i <- 1
for(PREOBS in c(FALSE, TRUE)){
  # for(GDPW in c(FALSE, TRUE)){
    for(PREPER in names(exposures_list)){
      #for(OFFSET in c("population_kcapita", "population_kcapita_2007")){
        for(LINTREND in c(FALSE, TRUE)){
          for(LOGTREND in c(FALSE, TRUE)){
            for(DISTR in c("quasipoisson")){# "quasipoisson", , "gaussian"
              # all the specifications above this point are compatible in any combination
              # However, annual LEAD AND LAG are allowed only when FYA is 0 (and therefore rfs_pya too)
              #for(FYA in c(0,3,4)){
               # if(FYA==0){
                  for(LAG in 0:3){
                    for(LEAD in 0:3){
                      reg_stats_indvar_list[[i]] <- make_spec_chart_df(outcome_variable = "undernourished_kcapita",
                                                                       weights = TRUE,
                                                                       preperiod_end = PREOBS,
                                                                       # gdp_weighting = GDPW,
                                                                       expo_measure_period = PREPER, 
                                                                       #offset = OFFSET, 
                                                                       rfs_lag = LAG, 
                                                                       rfs_lead = LEAD,
                                                                       #rfs_fya <- FYA,
                                                                       s_trend = LINTREND, 
                                                                       s_trend_log = LOGTREND, 
                                                                       distribution = DISTR, 
                                                                       clustering = "twoway"
                                                                       )
                      i <- i + 1
                    }
                  }
               # } else { # i.e. FYA > 0
                  # LAG <- 0
                  # LEAD <- 0
                  # reg_stats_indvar_list[[i]] <- make_spec_chart_df(outcome_variable = "undernourished_kcapita",
                  #                                                preperiod_end = PREOBS,
                  #                                                expo_measure_period = PREPER, 
                  #                                                offset = OFFSET, 
                  #                                                rfs_lag = LAG, 
                  #                                                rfs_lead = LEAD,
                  #                                                rfs_fya = FYA,
                  #                                                s_trend = LINTREND, 
                  #                                                s_trend_log = LOGTREND, 
                  #                                                distribution = DISTR
                  #                                                )
                  # i <- i + 1
                  
              #   }
              # }
            }
          }
        }
      #}
    }  
  #}
}


# convert to dataframe to be able to chart
reg_stats_indvar <- bind_rows(reg_stats_indvar_list)

# save it 
if(sum(duplicated(reg_stats_indvar))==0 ){ # i.e. 50 currently & nrow(reg_stats_indvar)+1 == i
  saveRDS(reg_stats_indvar, file.path(paste0("temp_data/reg_results/spec_chart_df_2waycluster_weighted",Sys.Date())))
} else{print(paste0("SOMETHING WENT WRONG in spec_chart_df"))}

scdf <- reg_stats_indvar

scdf <- readRDS(file.path(paste0("temp_data/reg_results/spec_chart_df_2waycluster_weighted",Sys.Date())))

# are there duplicated estimates (this is not expected) 
scdf[duplicated(scdf[,c(1,2)]), ] # Poisson may be exactly the same as quasipoisson at the level of rounding CIs

head(scdf)

scdf <- scdf[!duplicated(scdf),]
scdf <- dplyr::select(scdf, -quasipoisson)
scdf <- dplyr::select(scdf, -gaussian)



### SCHART PLOTS ------------------------------------------------------------------------
#### Full as is -------------------------------------------------------------------
scdf_sig_idx <- dplyr::transmute(scdf, is_sig = abs(Estimate) > 1.96*SE) %>% pull(is_sig) %>% which()
schart(scdf, 
       labels = schart_labels,
       order="asis", # "increasing",# 
       highlight=scdf_sig_idx, 
       heights=c(1,1.5),
       pch.dot=c(20,20,20,20), # it's necessary that a length 4 vector is given, for bottom panel to show highlighted models
       ci=c(.95),
       col.est=c("grey70", "red3"),
       col.dot=c("grey70","grey95","grey95","red3"),
       bg.dot=c("grey60","grey95","grey95","white"),
       leftmargin = 12,
       ylab = "Estimates",
       lwd.est = 3,
       lwd.symbol = 1, 
       fonts=c(2,1), adj=c(1,1), cex=c(0.6,0.6)
)

#### Focusing on dynamics ----------------------------------------------------------
scdf1 <- dplyr::filter(scdf, expo_measure_period_2001_2007) # , population_kcapita
# Lighten labels in bottom panel 
scdf1 <- dplyr::select(scdf1, -expo_measure_period_2001_2007, -expo_measure_period_2004_2007, -expo_measure_period_2006_2007) #, -population_kcapita, -population_kcapita_2007
schart_labels1 <- schart_labels[!(names(schart_labels) %in% c("Exposure period:", "Relative to population:"))]

##### Without pre-treatment period ---------------------------------------------
scdf1post <- dplyr::filter(scdf1, !include_preperiod)

scdf1post_sig_idx <- dplyr::transmute(scdf1post, is_sig = abs(Estimate) > 1.96*SE) %>% pull(is_sig) %>% which()

schart(scdf1post, 
       labels = schart_labels1,
       order="asis", #"increasing",#  
       highlight=scdf1post_sig_idx, 
       heights=c(1,1.5),
       pch.dot= c(20,20,20,20),
       ci=c(.95),
       col.est=c("grey70", "red3"),
       col.dot=c("grey70","grey95","grey95","red3"),
       bg.dot=c("grey60","grey95","grey95","red3"),
       leftmargin = 12,
       ylab = "Estimates",
       lwd.est = 3,
       lwd.symbol = 1, 
       fonts=c(2,1), adj=c(1,1), cex=c(0.6,0.6)
)

##### Including pre-treatment period ---------------------------------------------
scdf1pre <- dplyr::filter(scdf1, include_preperiod)

scdf1pre_sig_idx <- dplyr::transmute(scdf1pre, is_sig = abs(Estimate) > 1.96*SE) %>% pull(is_sig) %>% which()

schart(scdf1pre, 
       labels = schart_labels1,
       order="asis", #"increasing",#  
       highlight=scdf1pre_sig_idx, 
       heights=c(1,1.5),
       pch.dot= c(20,20,20,20),
       ci=c(.95),
       col.est=c("grey70", "red3"),
       col.dot=c("grey70","grey95","grey95","red3"),
       bg.dot=c("grey60","grey95","grey95","red3"),
       leftmargin = 12,
       ylab = "Estimates",
       lwd.est = 3,
       lwd.symbol = 1, 
       fonts=c(2,1), adj=c(1,1), cex=c(0.6,0.6)
)



# dplyr::filter(scdf, abs(Estimate) > 1.96*SE)

scdf1post[sig_idx,]




