# CA for NI crime data
# GMD

# Read data file from data directory
# no header, defined seperator, blanks set as NA
postcodes_in <- read.csv("data/NIPostcodes.csv", header=FALSE, sep=",")
#postcodes_in <- read.csv("data/NIPostcodes.csv", header=FALSE, sep=",", na = c(" ",""))

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

# Build column names
column_names <- c("Organisation","Sub-building", "Building", "Number",
                  "Primary Thorfare", "Alt Thorfare", "Secondary Thorfare",
                  "Locality", "Townland", "Town","County","Postcode",
                  "x-coordinates", "y-coordinates","Primary Key")

colnames(postcodes_in) <- column_names

# Check named columns
str(postcodes_in)

# What is the number of raw rows
nrow(postcodes_in)

# First 10 complete rows
# (really complete, na on all blanks is 0?)
head(postcodes_in[complete.cases(postcodes_in),], n=10)

# Begin to deal with missing data, filtering what is missing and necessary, vs missing and unnecessary
str(postcodes_in)

# Based on reasonable assumption the fields can be seperated into required and non-required fields
# eg this postcode data, therefore  missing postcode is a problem
# Required  -
# Key
# postcode
# x and y- coordinates
# Country
# Town
# Primary Thorfare

# No empty values - 
# townland

# Not strictly required -
# Organsiation
# sub-building
# Primary Thorfare
# Alt Thorfare
# Number


# Check numbers of records with required data missing



