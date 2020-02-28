# SIMPER 

library(vegan) 
browseVignettes("vegan")
# Discriminating species between two groups using Bray-Curtis dissimilarities

# <><><>< ARGUMENTS<><><><><><

# comm : Community data matrix.
# group : Factor describing the group structure. Must have at least 2 levels.
# permutations : a list of control values for the permutations as returned by the function how, or the number of permutations required, or a permutation matrix where each row gives the permuted indices.
# trace : Trace permutations.
# object : an object returned by simper.
# ordered : Logical; Should the species be ordered by their average contribution?
# digits : Number of digits in output.
# parallel : Number of parallel processes or a predefined socket cluster. With parallel = 1 uses ordinary, non-parallel processing.
## ....Parameters passed to other functions. In simper the extra parameters are passed to shuffleSet if permutations are used.
# <><><><><><><><><><><

# <><><><>><<> RESULTING VALUES <><><><><><><><><

# A list of class "simper" with following items:

# species : The species names.
# average : Average contribution to overall dissimilarity.
# overall : The overall between-group dissimilarity.
# sd : Standard deviation of contribution.
# ratio : Average to sd ratio.
# ava, avb : Average abundances per group.
# ord : An index vector to order vectors by their contribution or order cusum back to the original data order.
# cusum : Ordered cumulative contribution.
# p : Permutation p-value. Probability of getting a larger or equal average contribution in random permutation of the group factor.

#<><><><><><><><><<><><><<><><><><><><><><><><><><>####
#<><><><><><><><><<><><><<><><><><><><><><><><><><>####
# <><><><><><> DYER FISH DATA <><><><><><><>< #
# Comparison between reef and kelp communities, and between reef and kelp communities at Dyer, Betty's Bay and TMNP ##
## All sand sites were excluded ### 
#<><><><><><><><><<><><><<><><><><><><><><><><><><>####
#<><><><><><><><><<><><><<><><><><><><><><><><><><>####

getwd()
setwd("/Volumes/JUST NOW/R/Dyer BRUVs publication") #If you've opened the project then it's already set
library(readr)
fish <- read_csv("Dyer_BB_TMNP.csv")
names(fish)
# "Sample_Number" refers to the combined dataset
# "Survey_sampleID" refers to the sample ID in the individual dataset
# Depth is in m
# Sand habitats are already removed from dataset
head(fish)
###   DATA PREP  ###

library(magrittr)
#introduces a convenient piping syntax %>%, which is reminiscent of the | operator in the UNIX shell. 
library(dplyr)
# key verbs in dplyr: 
## group and ungroup,
## select,
## filter,
## arrange,
## mutate and transmute, and
## summarise (or aggregate).
fish <- tbl_df(fish)
class(fish) #this conversion happens implicitly once oyu start using the verbs in dplyr.
fish.comp <- select(tbl_df(fish), Area, Sample_number, Depth, Species, MaxN, Habitat, Year, Sampling_period,Latitude,Longitude) 
# make a new table keeping only the relevant columns
head(fish.comp)

# Make sure there are no duplicates
distinct(fish.comp)
nrow(fish.comp)

# Remove all samples where Depth > 26 m
max(fish.comp$Depth) #have up to 39m
nrow(fish.comp)
fish.comp <- filter(fish.comp, Depth < 26) 
nrow(fish.comp)
# 996 rows

# convert all habitat types to CAPS 
## Don't want problems with kelp not matching Kelp
fish.comp$Habitat <- toupper(fish.comp$Habitat) # Ugh can't figure out how to do this with piping..

### Remove all the TMNP samples :/
fish.comp <- filter(fish.comp, Area != "TMNP")

#Check for blanks
is.na(fish.comp) # returns logical values (FALSE or TRUE)
# list rows of data that have missing values 
fish.comp[!complete.cases(fish.comp),]
# All the BB and Dyer samples are complete (including lat and long --> TMNP didn't have coords)

## Add a depth category factor (even though it probably won't mean much)
levels <- c(4,15,26)
labels <- c("Shallow", "Deep")
fish.comp <- mutate(fish.comp, Depthcat = cut(Depth, levels, labels = labels))
# return new column but dont permanently alter the fish.comp data frame:
# fish.comp %>% mutate(Depthcat = cut(Depth, levels, labels = labels))

#### <><><> Add a species richness column in a new data frame (that I'll make into a pivot table later)
fish.comp <- fish.comp %>%
  group_by(Sample_number) %>% #pivots the df based on the column name passed to it (Sample_number in this case)
  mutate(S=length(unique(Species))) #add species richenss (S) column that counts number of unique rows in each sample (all rows should be unique)
#Example I followed... http://trendct.org/2015/08/21/tutorial-pivot-tables-with-r/
head(fish.comp)

write.csv(fish.comp, file = "Dyer_BB_cleaned.csv")

DyerBB.clean <- read_csv("Dyer_BB_cleaned.csv", col_types = cols(
  Area = col_character(),
  Sample_number = col_character(),
  Depth = col_number(),
  Species = col_character(),
  MaxN = col_integer(),
  Habitat = col_character(),
  Year = col_integer(),
  Sampling_period = col_character(),
  Latitude = col_number(),
  Longitude = col_number(),
  Depthcat = col_character(),
  S = col_integer()
))
names(DyerBB.clean)
# will have to delete that added "X1" column
DyerBB.clean <- select(DyerBB.clean, -X1)

saveRDS(DyerBB.clean, file = "Dyer_BB_cleaneddf.RDS")

### mini data exploration.. delete this later
summary(fish.comp)
#119 samples from BB and Dyer; 996 species records
n_distinct(fish.comp$Sample_number)
#103 samples
max(fish.comp$Sample_number)
#119 ... uh oh, need to redo the numbering (TMNP was in the middle)

#####<><><><><><><><><><><><><<><><><><><><><><><><><><><><<><>
## Same thing but way more work for SASC data #<><><>###

# see code in "Kelp data wrangling SASC.R" 
#####<><><><><><><><><><><><><<><><><><><><><><><><><><><><<><>

#####<><><><><><><><><><><><><<><><><><><><><><><><><><><><<><>
## COMBINE BOTH DATA FRAMES 

SASC.clean <- readRDS("SASC_cleaneddf.RDS") #the Rda approach wasn't working, this loads as a data frame

names(SASC.clean)

arrange(SASC.clean, desc(Latitude))
#Some of the latitudes somehow don't have a minus 
## some sort of ifelse statement... 
SASC.clean <- rename(SASC.clean, LatitudeWRONG = Latitude) # make a temp column

SASC.clean <- mutate(SASC.clean, Latitude = ifelse(LatitudeWRONG > 0, -LatitudeWRONG, LatitudeWRONG))
#Drop the temp column
SASC.clean <- select(SASC.clean, Sample_ID, Depth, Habitat, Year, Sampling_period, Latitude, Longitude, Depthcat, S, Species, MaxN)

# <><><><><><><><>< Add area column --> Three main clumps: Betty's, Hermanus, Dyer/Gansbaai
# Boundaries: 
## 1. Dyer: < -34.516995 (meaning more negative/more South)
## 2. Hermanus: > 19.050110 (East of) and > -34.516995 (North of)
## 3. BB: < 19.050110 (West of)

# Add an area column to SASC.clean
class(SASC.clean$Latitude)
#numeric, just checking

SASC.clean <- SASC.clean %>% mutate(Area = 
    ifelse(Latitude < -34.51699, "DYER", 
    ifelse(Longitude > 19.050110 & Latitude > -34.51699, "HERMANUS", "BB")))

View(SASC.clean)

# I did the depthcat wrong so drop that column and make a new one
SASC.clean <- select(SASC.clean, -Depthcat)

levels <- c(0,15,26) #less than 15m and 15-25m
labels <- c("Shallow", "Deep")
SASC.clean <- mutate(SASC.clean, Depthcat = cut(Depth, levels, labels = labels))

# Reorder the columns
SASC.clean <- select(SASC.clean, Area, Sample_ID, Depth, Habitat, Year, Sampling_period, Latitude, Longitude, Depthcat, S, Species, MaxN)

######<><><><><><><><><>>< Wrangle DyerBB.clean to match SASC data <><><>><><#####
DyerBB.clean <- readRDS("Dyer_BB_cleaneddf.RDS")
names(DyerBB.clean)
## The sample_number is already arbitrary (different from individual studies) so just change that to Sample_ID
DyerBB.clean <- rename(DyerBB.clean, Sample_ID = Sample_number)
# Put in same order as SASC data
DyerBB.clean <- select(DyerBB.clean, Area, Sample_ID, Depth, Habitat, Year, Sampling_period, Latitude, Longitude, Depthcat, S, Species, MaxN)

####<><><>>< Bind the two data frames <><><><><> #

df <- bind_rows(SASC.clean, DyerBB.clean)

saveRDS(df, file = "df.RDS")

#### <><><><> Reshape approach using melt() and cast () 
library(reshape2)
# basically making a pivot table
# melt() transforms df from wide (each species is a separate column) to long (each species/MaxN for a site is a separate observation)
# dcast() transforms from long to wide 

#### NOW WORKING WITH "df"


pivot.fish <- dcast(fish.comp, Sample_number + Area + Depth + Habitat + Depthcat + Sampling_period + S ~ Species, value.var = "MaxN", fun.aggregate=sum)
# + for all the id vars you want to keep, ~ Species to pivot the species column to rows

# reshape(fish, direction = "long") # reverts to original shape

#### <><><><>

### calculate Shannon Weiner diversity index 

# library(BiodiversityR) # the BiodiversityRGUI() returns an error, but I can do the same things in vegan using diversity()
# alpha.div(x,index)
# x = A vector or matrix of species abundances (e.g. counts). 
## Foo assumes that species are in columns and sites are in rows
# index =  either simp, inv.simp, or shan
nrow(pivot.fish)
#105
ncol(pivot.fish)
#57
diversity.subset <- select(pivot.fish,8:57) 
# select all the species columns, refer to index instead of typing the species name
Hprime <- diversity(diversity.subset,index="shannon") # returns a vector
data_frame(Hprime)
# add Hprime vector as a column to the pivot df
pivot.fish <- mutate(pivot.fish, H = round(Hprime, digits =3))
# reorder columns so all the id vars come before the species columns
pivot.fish <- select(pivot.fish, Sample_number, Area, Depth, Habitat, Depthcat, Sampling_period, S, H, everything()) 

# Group and summarize

by_Area <- group_by(pivot.fish, Area, Habitat)
by_Area <- summarise(by_Area, 
                     S_area = mean(S, na.rm = TRUE),
                     H_area = mean(H, na.rm = TRUE),
                     Number_samples = length(unique(Sample_number)))

by_Area <- by_Area %>% mutate_each(funs(round(.,2)), S_area, H_area) # round to 2 digits

# Visualize

library(ggplot2)

# Q: Is there a difference in H' between the 3 areas? Between Reef and Kelp habitats?
boxplot(H~Area, data=pivot.fish, ylab = "H'", xlab = "Area") # but first need to group to get average H'
boxplot(H~Habitat, data=pivot.fish, ylab = "H'", xlab = "Habitat")
boxplot(H~Depthcat, data=pivot.fish, ylab = "H'", xlab = "Depth category")
boxplot(H~Sampling_period, data=pivot.fish, ylab = "H'", xlab = "Sampling period") 
# This doesn't mean anything by itself, same as comparing the 3 areas because there was no overlap (maybe Shark Conservancy data will solve this)

# Kelp is slightly higher H', Dyer is slightly greater H' than BB and TMNP (in that order, those 2 are very close)
# Shallow and Deep are very close (makes sense, all the samples are relatively shallow)


#<><><><><><><><>><><><><><><>><><><><><><>><><><><><><>><><><><><><>><><><><><><>><><><><><><>
### <><>< That's data wrangling! now for ANOVAs <><><><>
#<><><><><><><><>><><><><><><>><><><><><><>><><><><><><>><><><><><><>><><><><><><>><><><><><><>

# Test for significant differences in H' between habitats, Area, Depthcats
# compare kelps between each area, and kelp-reef within each area

### check out this package! Maybe it's helpful
library(help=mvabund)
# Statistical Methods for Analysing Multivariate Abundance Data
## not sure this will work... "no ordination function." What does that mean?

### ><><><>><><><> 4TH ROOT TRANSFORMATION ><><><>><><><>><><><>><><><>><><><> (continuing without library(mvabund) )
data.dat$trans_Y <- (data.dat$Y)^(1/4) #transforms one column and makes a new variable

pivot.sqrt <- select(pivot.fish, everything()) ## Why isn't this working?s
pivot.sqrt %>% mutate_each(funs(sqrt),9:58)
# Maybe try a 4th root?
root4 <- function(x^(1/4)) # There's something very obvious that I'm doing wrong...
  pivot.root4 %>% mutate_each(funs(root4),9:58) #apply to columns index 9:58 (the species counts)
# df %>% mutate_each(funs(as.character), v1, v2)
#mutate_each(iris, funs(min_rank))

### <><><><><><Nonmetric MDS (nMDS) performed using the isoMDS( ) foo in the MASS package.<><><><><><><>
# represent the distances among the objects in a parsimonious (and visual) way
library(MASS)

#<><><> EXAMPLE: (from http://www.statmethods.net/advstats/mds.html)
# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name
d <- dist(mydata) # euclidean distances between the rows
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(mydata), cex=.7)
###### 

#<><><><><<>>< ANOTHER nMDS EXAMPLE<><><><><>< From http://userweb.eng.gla.ac.uk/umer.ijaz/bioinformatics/ecological.html

fish.mds <- metaMDS(abund_table,distance = "bray", k = 2, trymax = 50) # k is number of dims

#Make a new data frame, and put country, latrine, and depth information there, to be useful for coloring, and shape of points
NMDS = data.frame(x=sol$point[,1],y=sol$point[,2],Country=as.factor(grouping_info[,1]),Latrine=as.factor(grouping_info[,2]),Depth=as.factor(grouping_info[,3]))
NMDS = data.frame() # not sure how to do the rest of this
#<><><><><><><<><><><><><<><><><><><<><><><><><<><><><><><<><><><><><
