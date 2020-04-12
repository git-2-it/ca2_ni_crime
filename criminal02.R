# CA for NI crime data
# NI Crime dataset
# GMD

# --------------------------------------------------
# Read and review the data 
# --------------------------------------------------

# Init required libraries
library("plyr")
library("dplyr") 
library("readr") 
library("viridis")
library("data.table")

# set the path to the top level data directory
datapath <- "data/NI Crime Data"

crimes_in <- data.frame()

# List of expected columns and structure 
coltypes <- list(
  col_character(), col_character(), col_character(), col_character(), col_character(), col_character(),
  col_character(), col_character(), col_character(), col_factor(), col_character(), col_character()
)

colnames <- c(
  "Crime.ID", 
  "Month", 
  "Reported.by", 
  "Falls.within" ,
  "Longitude" ,
  "Latitude" ,
  "Location",
  "LSOA.code" ,
  "LSOA.name" ,
  "Crime.type",
  "Last.outcome.category" ,
  "Context"
)


# Read in the data
crimes_in <- list.files(path=datapath, full.names=TRUE, recursive = TRUE) %>% 
  lapply(read_csv, col_types = coltypes) %>%   bind_rows

colnames(crimes_in) <- colnames

# dataset review
str(crimes_in)


write.csv(file="data/AllNICrimeData.csv", x=crimes_in, quote=TRUE, row.names = FALSE)

# --------------------------------------------------
# Remove unnecessary columns
# --------------------------------------------------

crimes_in <- subset(crimes_in, select = -c(Crime.ID, 
                              Reported.by, Falls.within, LSOA.code, LSOA.name, 
                              Last.outcome.category, 
                              Context) )
# Confirm new structure
str(crimes_in)

# --------------------------------------------------
# Abbreviate crime types
# --------------------------------------------------

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

# Check the levels
levels(crimes_in$Crime.type)

# One was missed, align all factors
crimes_in <- crimes_in %>% mutate(Crime.type=recode(Crime.type, 
                                                    "Possession of weapons" = "POSW"
                                                    ))
# Re-check
levels(crimes_in$Crime.type)

# All input crime factor descriptions now shortened
# Data reduced and tidyied

write.csv(file="data/AllNICrimeData.csv", x=crimes_in, quote=TRUE, row.names = FALSE)

# --------------------------------------------------
# Now to move to plotting the data
# --------------------------------------------------

# Store default plot settings
before_plt <- par()

plot_table <- table(crimes_in$Crime.type)
str(plot_table)

barplot(plot_table, 
        main = "NI Crime Statistics", 
        xlab = "Crime Type", 
        ylab = "Frequency",
        col = viridis(length(levels(crimes_in$Crime.type))),
        cex.names = 0.75,
        )

structure(plot_table)

# Can see high incidences of ASBOs and violent and sexual offences
# Criminal damage, other theft and burglary are also highly prevalent


# --------------------------------------------------
# Remove "On or near" substring  from location
# --------------------------------------------------

crimes_in$Location <- gsub( "On or near", "", crimes_in$Location)
crimes_in$Location <- trimws(crimes_in$Location)

# View small sample of the data to confirm the removal
sample_n(crimes_in, 10)

# Set empty locations to NA
crimes_in$Location[crimes_in$Location == ""] <- NA

str(crimes_in)

# --------------------------------------------------
# Create random sample
# --------------------------------------------------

# Select a random sample of 5000, ensuring 5000 different entries, with no NA values
set.seed(100)
random_crime_sample <- sample_n(na.omit(crimes_in), 5000, replace = FALSE)
str(random_crime_sample)

# Re-read Postcode data created previously
postcodes_clean <- read.csv("data/CleanNIPostcodeData.csv", header=TRUE, sep=",")

# --------------------------------------------------
# Create function find_a_town
# function is to find, for an individual entry, a value for town based on location
# --------------------------------------------------


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
random_crime_sample$Town <- apply(random_crime_sample, 1, find_a_town)

str(random_crime_sample)

# --------------------------------------------------
# Read in villages data containing population figures
# --------------------------------------------------

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


# --------------------------------------------------
# Lookup population function
# --------------------------------------------------
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

# --------------------------------------------------
# Use the function to add town population data to the sample 
# --------------------------------------------------
random_crime_sample$Population <- apply(random_crime_sample, 1, add_town_data)

# --------------------------------------------------
# Save sample dataset file
# --------------------------------------------------
write.csv(file="data/random_crime_sample.csv", x=random_crime_sample, quote=TRUE, row.names = FALSE)


# --------------------------------------------------
# Charting data figures for Belfast and Derry 
# --------------------------------------------------

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
belderr <- belderr[order(belderr$Town, belderr$Crime.type),]

str(belderr)

# --------------------------------------------------
# Visuals 
# --------------------------------------------------

# Store the current  par setup before next set of visuals
init_par <- par()

# Create table of data for plots
plot_table <- table(belderr$Town, belderr$Crime.type, exclude = TRUE, 
                    dnn = c("Town", "Crime.Type"))

str(plot_table)

# Create side-by-side bar chart to allow comparison between the two cities
barplot(plot_table, 
        main = "NI Crime Statistics: Belfast and Derry",
        xlab = "Crime Type", 
        ylab = "Frequency",
        col = viridis(2),
        cex.names = 0.75,
        legend = TRUE, 
        beside = TRUE,
        args.legend = list(x = "topright") 
) 

library(lattice)
histogram(~ Crime.type | Town, data=belderr,
          type = c("count"),
          xlab=list(label="Crime Type",cex=0.75),
          ylab=list(label="Frequency",cex=0.75),
          main = "NI Crime Statistics: Belfast and Derry",
          col = viridis(length(levels(belderr$Crime.type))),
          scales=list(cex=0.5),
)


# --------------------------------------------------
# Build and view crime figures relative to population 
# --------------------------------------------------

plot_hist <-data.frame(plot_table)

plot_hist$Population <- as.numeric(gsub(",", "", apply(plot_hist, 1, add_town_data)))

# Large number (100k) used for indicative purposes and scaling
plot_hist$Per <- 100000 * plot_hist[, "Freq"] / plot_hist[, "Population"]

str(plot_hist)

library(ggplot2)
side_plot <- ggplot(data=plot_hist, 
                    aes(x=Crime.Type, y=Per, fill = Town ,
                    colour = Town) ,
                    main = "NI Crime Statistics: Belfast and Derry",
                    xlab = "Crime Type", 
                    ylab = "Relative Frequency",
                    col = viridis(2))
side_plot <- side_plot + geom_bar(stat="identity", position=position_dodge()) 
side_plot <- side_plot + ggtitle("Relative crime rate\n by population") +
  xlab("Crime") + ylab("Relative rate")
side_plot

