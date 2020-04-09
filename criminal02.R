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

mutate(crimes_in$Crime.type, )

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

# Can se high incidences of ASBOs and violent and sexual offences
# Criminal damage, other theft and burglary are also highly prevalent


