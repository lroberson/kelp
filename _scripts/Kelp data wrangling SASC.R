#####<><><><><><><><><><><><><<><><><><><><><><><><><><><><<><>
## Same thing but way more work for SASC data #<><><>###
#####<><><><><><><><><><><><><<><><><><><><><><><><><><><><<><>

library(readr)
fish <- read_csv("SASC_BRUVs_data_kelppaper.csv", col_types = cols(
  File_name = col_character(),
  SASC_Sample_number = col_character(),
  Site_name = col_character(),
  Sample_ID = col_character(),
  lat = col_number(),
  lon = col_number(),
  FOV = col_integer(),
  Common_name = col_character(),
  Species = col_character(),
  ToFS_clip = col_character(),
  ToFS_totalvideo = col_character(),
  Time_MaxN_minutes = col_character(),
  MaxN = col_integer(),
  Depth_m = col_number(),
  Habitat = col_character(),
  Date = col_character(),
  Year = col_integer(),
  Sampling_period = col_character()
))

names(fish)
head(fish)
class(fish) #this conversion happens implicitly once oyu start using the verbs in dplyr.

fish.SASC <- select(tbl_df(fish), SASC_Sample_number, Sample_ID, Depth_m, Species, MaxN, Habitat, Year, Sampling_period,lat,lon) 
# make a new table keeping only the relevant columns --> still have some manupulation to do though and need to rename some
# Will have to change "Area" because they're all spread out...

#Rename to match BB and Dyer data
fish.SASC <- rename(fish.SASC, Depth = Depth_m, Latitude = lat, Longitude = lon)

nrow(fish.SASC)
#1338 samples
names(fish.SASC)
# Remove all samples where Depth > 26 m
max(fish.SASC$Depth) #have up to 51m
min(fish.SASC$Depth) # min is 2

# na.test <- subset(fish.SASC,is.na(Depth))
## only one site missing a Depth reading
## Changed it to 4 and reimported the csv (the other Bientangs cave sights were recorded as 4)

fish.SASC <- filter(fish.SASC, Depth < 26) 
nrow(fish.SASC)

# convert all habitat types to CAPS 
## Don't want problems with kelp not matching Kelp
fish.SASC$Habitat <- toupper(fish.SASC$Habitat)

#Check for blanks
is.na(fish.SASC) # returns logical values (FALSE or TRUE)
# list rows of data that have missing values 
fish.SASC[!complete.cases(fish.SASC),]
# one sample only had 1 spp record and no habitat --> delete it
fish.SASC <- filter(fish.SASC, Sample_ID != "14_Old Harbour_Winter_2015")

## Add a depth category factor (even though it probably won't mean much)
levels <- c(0,15,26) #less than 15m and 15-25m
labels <- c("Shallow", "Deep")
fish.SASC <- mutate(fish.SASC, Depthcat = cut(Depth, levels, labels = labels))

## <><><><>< Check that all spp records are spelled correctly ####
distinct_df <- fish.SASC %>% distinct(Species)

distinct_df <- distinct_df[order(distinct_df$Species),] #order alphabetically (could also use Arrange or Sort)
View(distinct_df)
### alot of capitalization mistakes --> toupper
fish.SASC$Species <- toupper(fish.SASC$Species)
### fix all the spelling mistakes in fish.SASC
fish.SASC$Species[ fish.SASC$Species == "CHEILODACTYLUS_ FASCIATUS" ] <- "CHEILODACTYLUS FASCIATUS"
fish.SASC$Species[ fish.SASC$Species == "CHEILODACTYLUS_ PIXI" ] <- "CHEILODACTYLUS PIXI"
fish.SASC$Species[ fish.SASC$Species == "CHIRODACTYLUS SP." ] <- "CHIRODACTYLUS BRACHYDACTYLUS"
fish.SASC$Species[ fish.SASC$Species == "CHRYSOBLEPHUS LARICEPS" ] <- "CHRYSOBLEPHUS LATICEPS"
fish.SASC$Species[ fish.SASC$Species == "CINUS COTTOIDES" ] <- "CLINUS COTTOIDES"
fish.SASC$Species[ fish.SASC$Species == "CLINUS SUPERCILIOSIS" ] <- "CLINUS SUPERCILIOSUS"
fish.SASC$Species[ fish.SASC$Species == "CLINUS SUPERCILIOCUS" ] <- "CLINUS SUPERCILIOSUS"
fish.SASC$Species[ fish.SASC$Species == "DIPLODUS HOTENTOTUS" ] <- "DIPLODUS HOTTENTOTUS"
fish.SASC$Species[ fish.SASC$Species == "EPATATRETUS HEXATREMA" ] <- "EPTATRETUS HEXATREMA"
fish.SASC$Species[ fish.SASC$Species == "EPATRETUS HEXATREMA" ] <- "EPTATRETUS HEXATREMA"
fish.SASC$Species[ fish.SASC$Species == "HABLOBLEPHARUS PICTUS" ] <- "HAPLOBEPHARUS PICTUS"
fish.SASC$Species[ fish.SASC$Species == "HAPLOBEPHARUS EWARDSII" ] <- "HAPLOBLEPHARUS EDWARDSII"
fish.SASC$Species[ fish.SASC$Species == "HAPLOBLEPHARUS SP" ] <- "HAPLOBLEPHARUS EDWARDSII"
fish.SASC$Species[ fish.SASC$Species == "LONGSNOUT PIPEFISH" ] <- "SYNGNATHUS ACUS"
fish.SASC$Species[ fish.SASC$Species == "NOTORYNCHUS CEPEDINUS" ] <- "NOTORYNCHUS CEPEDIANUS"
fish.SASC$Species[ fish.SASC$Species == "PORODERMAN AFRICANUM" ] <- "PORODERMA AFRICANUM"
fish.SASC$Species[ fish.SASC$Species == "SARPA SARPA" ] <- "SARPA SALPA"
fish.SASC$Species[ fish.SASC$Species == "POLYAMBLYODON GERMANUM" ] <- "PACHYMETOPON GRANDE"
# The german seabream is an east coast spp frequently confused with P. grande... 
fish.SASC$Species[ fish.SASC$Species == "ABUDEFDUF SORDIDUS" ] <- "DIPLODUS SARGUS"
# A. sordidus is east coast fish, looks like D. sargus so I replaced it (also there were no records of D. sargus, which is unlikely)
# couldn't figure it out with dplyr so used base R....


### <><><><>> Remove rows with non-fish spp records <><><>><#####

fish.SASC <- filter(fish.SASC, Species != "NUCELLA DUBIA")
fish.SASC <- filter(fish.SASC, Species != "NUCELLA SQUAMOSA")
fish.SASC <- filter(fish.SASC, Species != "OXYSTELE SINENSIS")
fish.SASC <- filter(fish.SASC, Species != "MATHASTERIAS GLACIALIS") #actually spelled MARTHASTERIAS GLACIALIS... it's a starfish
fish.SASC <- filter(fish.SASC, Species != "MACROPETASMA AFRICANA") #shrimp 
# Can't figure how to do this in one line, tried "OR" and "," and got errors

#### <><><><><> Delete duplicate species records in a Sample_ID <><><><#
# Remove duplicates (for MaxN = 1)
fish.SASC <- fish.SASC %>% distinct(SASC_Sample_number, Sample_ID, Depth, Species, MaxN, Habitat, Year, Sampling_period, Latitude, Longitude, Depthcat)
nrow(fish.SASC)
# 256

#### Keep only max(MaxN) for each spp-sample 
fish.SASC <- fish.SASC %>% group_by(Sample_ID, Species) %>% slice(which.max(MaxN)) 
nrow(fish.SASC)
# 176 unique 
names(fish.SASC) # Yup, kept all the columns this time
# Check if this worked... 
# Sample_ID = "3_Bientangs Cave_Spring_2013" had a lot of duplicates
test.df <- filter(fish.SASC.test, Sample_ID == "3_Bientangs Cave_Spring_2013")
View(test.df)
#Yup all good
n_distinct(fish.SASC$Sample_ID)
# 32 SASC samples with 176 species records

#### <><><> Add a species richness column in a new data frame (that I'll make into a pivot table later)
fish.SASC <- fish.SASC %>%
  group_by(Sample_ID) %>% #pivots the df based on the column name passed to it (Sample_number in this case)
  mutate(S=length(unique(Species))) #add species richenss (S) column that counts number of unique rows in each sample (all rows should be unique)
head(fish.SASC)

write.csv(fish.SASC, file = "SASC_cleaned.csv")
save(fish.SASC,file="SASC_cleaneddf.Rda")
saveRDS(fish.SASC, file = "SASC_cleaneddf.RDS")
## From here I can just load the cleaned df and the BB&Dyer cleaned df and merge dfs from there


