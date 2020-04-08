# CA for NI crime data
# NI Postcode data
# GMD

# Read data file from data directory, a subdir of the code storage
# no header, defined seperator, (blanks set as NA?)
postcodes_in <- read.csv("data/NIPostcodes.csv", header=FALSE, sep=",")

# show structure
str(postcodes_in)

# Actual data structure to be applied
# Organisation Name
# Sub-building Name
# Building Name
# Number
# Primary Thorfare
# Alt Thorfare
# Secondary Thorfare
# Locality
# Townland
# Town
# County
# Postcode
# x-coordinates
# y-coordinates
# Primary Key (identifier)

# Build and assign column names replacing spaces with underscores to make field handing easier
column_names <- c("Organisation","Sub-building", "Building", "Number",
                  "Primary_Thorfare", "Alt_Thorfare", "Secondary_Thorfare",
                  "Locality", "Townland", "Town","County","Postcode",
                  "x-coordinates", "y-coordinates","Primary_Key")

colnames(postcodes_in) <- column_names

# Check named columns
str(postcodes_in)

# Confirm the number of raw rows (943 034)
nrow(postcodes_in)

# First 10 complete rows
# (really complete, na on all blanks is 0?)
head(postcodes_in[complete.cases(postcodes_in),], n=10)

# Begin to deal with missing data, filtering what is missing and necessary, 
# vs missing and unnecessary
# This will help decide how to manage blanks values in the dataset
str(postcodes_in)

# Based on reasonable assumptions the fields can be seperated into required and non-required fields
# eg this is postcode data, therefore  missing postcode is a problem, on the other hand
# organisation is not so relevant
#
#First, run through the data to see where a choice should be made

# No empty values - 
# Primary Key
# x- and y- coordinates
# townland
# County

# Not strictly required -
# Organsiation
# Building
# sub-building
# Locality
# Secondary Thorfare
# Alt Thorfare
# Number (not all addresses have a street number)

# Possilbly required  -
# Town
# postcode
# Primary Thorfare

# subset those columns to deal only with them, inclulding primary key
sub_post <- postcodes_in[ c(5,10,12,15) ]

str(sub_post)

# Check a summary view of the data, noting total records count is 943 034
summary(sub_post)

# On checking if there are records with required data missing, ie blank/empty factor values
# postcode - empty values
# Town - empty values
# Primary Thorfare - empty values

# Town is arguably categorical data, categorical data should not be removed
# Check on number of potentially affected records none the less
# set the blanks to NA and check the resulting NA count
sub_post$Town[sub_post$Town == ""] <- NA
sum(is.na(sub_post$Town))

# Approx 20k missing town records is substantial, missing > 20%
# Therefore  leave it in place, besides its categorical data

# Checking Postcode
# Summary shows 8 900 blanks out of total, missing < 1%
# Therefore they could be removed
# If, in later analysis, the issing values were important, its possible 
# the x/y co-ordinatres could be used to guess the missing values
sub_post$Postcode[sub_post$Postcode == ""] <- NA
sum(is.na(sub_post$Postcode))

# Check Primary_Thorfare
# set the blanks to NA and check the resulting NA count
sub_post$Primary_Thorfare[sub_post$Primary_Thorfare == ""] <- NA
sum(is.na(sub_post$Primary_Thorfare))

# approx 470 missing Primary_Thorfare records not substantial, missing < 0.1%

# Therefore, it should be possible to remove blank Primary_Thorfare and Postcode
# Examine if thw missing data aligns  by charting the missing values
# This will show how the missing values are spread across the columns
# and indicate the number of total rows  that will be deleted

library(mice)
md.pattern(sub_post, rotate.names = FALSE)

library(VIM)
missing_values <- aggr(sub_post, prop = c(FALSE), numbers = TRUE,
                       labels=names(data),
                       combined = TRUE
                       )

# Show summary of the contents of missing_values
summary(missing_values)


# 8900 + 438 = 9338 to go, 933 696 records should remain
# Removing the blank postcode and primary_thorfare
postcodes_in$Primary_Thorfare[postcodes_in$Primary_Thorfare == ""] <- NA
postcodes_in$Postcode[postcodes_in$Postcode == ""] <- NA

postcodes_in <- postcodes_in[complete.cases(postcodes_in[,c(5,12)]),]
nrow(postcodes_in)

# New number of rows co-incides with expected result

# Column reorder, placing primary key in column 1
postcodes_in <- postcodes_in[c(15,1:14) ]

str(postcodes_in)

# Save newly cleaned and reorganised complete data to output file
write.csv(file="data/CleanNIPostcodeData.csv", x=postcodes_in, quote=FALSE, row.names = FALSE)

# Create data subset for Limavady
# Where Limavady (LIMAVADY) is either in the town, townland or locality
Limavady_data <- subset(postcodes_in,  
                        postcodes_in$Town == "LIMAVADY" 
                        | postcodes_in$Locality == "LIMAVADY" 
                        | postcodes_in$Townland == "LIMAVADY" 
)
str(Limavady_data)

# Save Limavady data to output file
write.csv(file="data/Limavady.csv", x=Limavady_data, quote=FALSE, row.names = FALSE)


