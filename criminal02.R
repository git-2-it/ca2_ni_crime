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

# View small sample of the data to confirm the removal
sample_n(crimes_in, 20)

# Set empty locations to NA
crimes_in$Location[crimes_in$Location == ""] <- NA

# Select a random sample of 5000, ensuring 5000 different entries, with no NA values
set.seed(100)
random_crime_sample <- sample_n(na.omit(crimes_in), 5000, replace = FALSE)

# Re-read Postcode data created previously
postcodes_clean <- read.csv("data/CleanNIPostcodeData.csv", header=TRUE, sep=",")

# Create function find_a_town
# function is to find, for an individual entry, a value for town based on location

library("data.table")
# Create data table
postcodes_table <- data.table(postcodes_clean)

# Indexing postcode data by street name, ie primary thouroughfare
setkey(postcodes_table, Primary_Thorfare)

# Now create the function
find_a_town <- function(row_in)
{
  # Removes blank town values and returns only the first entry, 
  # Naive function, could lead to possibly incorrect
  # data, eg consider case of "man street" which is in pratically 
  # every town in the country
  # Expects data row containing a Location field as input
  
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
# samp$Town <- apply(samp, 1, find_a_town)
random_crime_sample$Town <- apply(random_crime_sample, 1, find_a_town)


# Read in villages data containing population figures
villages <- read.csv("data/VillageList.csv", header=TRUE, sep=",")
str(villages)

# Change odd looking column name and set values to uppercase for convienience
colnames(villages)[1] <- "CITY.TOWN.VILLAGE"
villages$CITY.TOWN.VILLAGE <- toupper(villages$CITY.TOWN.VILLAGE)

# Create a data.table for search
villages_table <- data.table(villages)

# Set an index in data table
setkey(villages_table, CITY.TOWN.VILLAGE)

# View new table
str(villages_table)


# Lookup population function
add_town_data <- function(row_in)
{
  # Expects data row containing a Location field as input
  # Retuns either a value or an NA if bad input or data not found
  
  loc_in <- row_in[c("Town")]
  
  retVal <- NA
  
  if (is.na(loc_in) | loc_in == "")
  {
    return(retVal)
  }

  # Deal with Derry naming
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

random_crime_sample$Population <- apply(random_crime_sample, 1, add_town_data)



# Add town population data to the sample 
random_crime_sample$Population <- apply(random_crime_sample, 1, add_town_data)

# Save sample dataset file
write.csv(file="data/random_crime_sample.csv", x=random_crime_sample, quote=TRUE, row.names = FALSE)


# Charting data figures for Belfast and Derry

# Extract belfast/Derry data 
# First, clean up the NA Town values
random_crime_sample <- na.omit(random_crime_sample)

# Need to add new level in factors for Derry  before can assign it
levels(random_crime_sample$Town) <- c(levels(random_crime_sample$Town), "DERRY")
random_crime_sample$Town[random_crime_sample$Town == "LONDONDERRY"] <- "DERRY"

# Filter based on Town Values
belderr <- random_crime_sample %>% filter(Town == "BELFAST" | Town == "DERRY")
str(belderr)

# Drop unnecessary factors levels to make plotting etc simpler
belderr <- droplevels(belderr)

# Store the current  par setup before next set of visuals
init_par <- par()

# Create table of data for plots
plot_table <- table(belderr$Town, belderr$Crime.type, exclude = TRUE)

str(plot_table)

# Create side-by-side bar chart to allow comparison between the two cities
barplot(plot_table, 
        main = "NI Crime Statistics: Belfast and Derry",
        xlab = "Crime Type", 
        ylab = "Frequency",
        col = viridis(2),
        cex.names = 0.75,
        legend = TRUE , 
        beside = TRUE
)

