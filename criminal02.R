# CA for NI crime data
# NI Crime dataset
# GMD

# Read in the data
crimes_in <- read.csv("data/NI Crime Data/2015-01/2015-01-northern-ireland-street.csv", header=TRUE, sep=",")

# dataset review
str(crimes_in)

# Remove specified columns
crimes_in <- subset(crimes_in, select = -c(Crime.ID, 
                              Reported.by, Falls.within, LSOA.code, LSOA.name, 
                              Last.outcome.category, 
                              Context) )
# Confirm new structure
str(crimes_in)

# Abbreviate crime types 
library(dplyr)

# Import dplyr library for manipulation functions
library(dplyr)

# Reconfigure values in the crime type field
crimes_in <- crimes_in %>% mutate(Crime.type=recode(Crime.type, 
                                       "Anti-social behaviour" = "ASBO",
                                       "Bicycle theft" = "BITH",
                                       "Burglary" = "BURG",
                                       "Criminal damage and arson" = "CDAR",
                                       "Drugs" = "DRUG",
                                       "Other theft" = "OTTH",
                                       "Public order" = "PUBO",
                                       "Robbery" = "ROBY",
                                       "Shoplifting" = "SHOP",
                                       "Theft from the person" = "THPR",
                                       "Vehicle crime" = "VECR",
                                       "Violence and sexual offences" = "VISO",
                                       "Other crime" = "OTCR",
                                       ))


# Align all factors
crimes_in <- crimes_in %>% mutate(Crime.type=recode(Crime.type, 
                                                    "Possession of weapons" = "POSW"
                                                    ))
# Re-check
levels(crimes_in$Crime.type)

# All input crime factor descriptions now shortened


# Data reduced and tidyied
# Now to move to plotting the data

# Store default plot settings
before_plt <- par()

# Add colour package
# install.packages("viridis")  # Install
library("viridis")

plot_table <- table(crimes_in$Crime.type)
# plot(crimes_in)
str(plot_table)

barplot(plot_table, 
        main = "NI Crime Statistics", 
        xlab = "Crime Type", 
        ylab = "Frequency",
        col = viridis(length(levels(crimes_in$Crime.type))),
        cex.names = 0.75,
        )

# Can see high incidences of ASBOs and violent and sexual offences
# Criminal damage, other theft and burglary are also highly prevalent


# Remove "On or near" substring  from location
crimes_in$Location <- gsub( "On or near", "", crimes_in$Location)
crimes_in$Location <- trimws(crimes_in$Location)

# View small sample of the data
sample_n(crimes_in, 20)

# Set empty locations to NA
crimes_in$Location[crimes_in$Location == ""] <- NA

# Select a random sample of 5000, ensuring 5000 different entries, with no NA values
set.seed(100)
random_crime_sample <- sample_n(na.omit(crimes_in), 5000, replace = FALSE)

samp <- sample_n(random_crime_sample, 10, replace = TRUE)


#Re-read Postcode data from previously
postcodes_clean <- read.csv("data/CleanNIPostcodeData.csv", header=TRUE, sep=",")

# Indexing postcode data by street name, ie primary thouroughfare

# Create function find_a_town
# function is to find, for an individual entry, a value for town based on location
library("data.table")
postcodes_table <- data.table(postcodes_clean)

# Create the function
# Removes blank town values and returns only the first entry, obviously leads to possibly incorrect
# data, eg consider case of "man street" which is in partically every town in the country
find_a_town <- function(row_in)
{
  loc_in <- row_in[c("Location")]

  # Tidy function input eg remove case sensitivity
  loc_in <- toupper(loc_in)
  
  # Do lookup
  retVal <- postcodes_table[ Primary_Thorfare %in% c(loc_in)] 
  
  # Remove blank town values
  # retVal <- retVal[complete.cases(retVal[,c(10)]),]
  retVal <- na.omit(retVal$Town)
  
  # Only return the first entry found
  return(retVal[1])
}

# Add new town column to dataset
samp$Town <- apply(samp, 1, find_a_town)


# Read in villages data containing population data
villages <- read.csv("data/VillageList.csv", header=TRUE, sep=",")

colnames(villages)[1] <- "CITY.TOWN.VILLAGE"
villages$CITY.TOWN.VILLAGE <- toupper(villages$CITY.TOWN.VILLAGE)


str(villages)
villages_table <- data.table(villages)

str(villages_table)

# Lookup population function
add_town_data <- function(row_in)
{
  loc_in <- row_in[c("Town")]
  
  cat(loc_in)
  if (is.na(loc_in) | loc_in == "")
  {
    return(NA)
  }

  # Deal with Derry  
  if (loc_in == "LONDONDERRY" )
  {
     loc_in <- "DERRY"
  }
  
  # Tidy function input eg remove case sensitivity
  loc_in <- toupper(loc_in)
  
  # Do lookup
  retVal <- villages_table[ CITY.TOWN.VILLAGE %in% c(loc_in)] 

  # Return values
  retVal <- as.character(na.omit(retVal$POPULATION))

  # Only return the first entry found
  return(retVal[1])
}

str(villages)

samp <- sample_n(na.omit(random_crime_sample), 10, replace = TRUE)
samp$Town <- apply(samp, 1, find_a_town)
str(samp)

#samp$Town <- 
samp$Population <- apply(samp, 1, add_town_data)

add_town_data("Beflast")

str(samp)

structure(samp)
