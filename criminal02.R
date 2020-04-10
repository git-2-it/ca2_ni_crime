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

sample(random_crime_sample, 10, replace = TRUE)

#Re-read Postcode data from previously
postcodes_clean <- read.csv("data/CleanNIPostcodeData.csv", header=TRUE, sep=",")

# Indexing postcode data by street name, ie primary thouroughfare

# Create function find_a_town
# function is to able to find, for an individual entry, a value for town
# Note, trying the use of  datatables, reading that these are a more efficient way of lookups over 
# large amounts of data library(data.table)
library("data.table")

postcodes_table <- data.table(postcodes_clean)

# Create the function
# Removes blank town values and returns only the first entry, obviously leads to possibly incorrect
# data, eg consider case of "man street" which is in partically every town in the country
find_a_town <- function(loc_in)
{
  # Tidy function input eg remove case sensitivity
  loc_in <- toupper(loc_in)

    # Do a lookup
  retVal <- postcodes_table[ Primary_Thorfare %in% c(loc_in)] 
  
  # Remove blank town values
  # retVal <- retVal[complete.cases(retVal[,c(10)]),]
  retVal <- na.omit(retVal$Town)

  # Only return the first entry found
  return(retVal[1])
}

fred <- random_crime_sample # $Location
locs <- data.frame(fred$Location)
fred$Town <- find_a_town(locs)

str(locs)
by(fred, 1:nrow(fred), find_a_town ) # sets all to fucking portstewart


apply(locs[,c("Location")], 1, function(y) testFunc(y["Location"]))

mapply(find_a_town, fred[, "Location"])


str(fred)
levels(fred$Town)
summary(fred)

fred$Town <- sapply(fred[4], 2, find_a_town) 

fred$Town <- apply(fred[,c(4)], 2, find_a_town) 


find_a_town <- function(loc_in)
{
  cat (loc_in)
  
  # Tidy function input eg remove case sensitivity
  loc_in <- toupper(loc_in)
  
  # Do a lookup
  retVal <- postcodes_table[ Primary_Thorfare %in% c(loc_in)] 
  
  # Remove blank town values
  # retVal <- retVal[complete.cases(retVal[,c(10)]),]
  retVal <- na.omit(retVal$Town)
  
  # Only return the first entry found
  return(retVal[1])
}


samp <- sample_n(na.omit(crimes_in), 5, replace = FALSE)

fred <- samp

fred$Town <- apply(fred, 1, find_a_town)




