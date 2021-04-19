#### Some definitions
# string = sequence of characters, could be a word, in R we distinguish these by putting parenthesis around them e.g. "this_is_a_string"
# vector = list of values, defined using "c", e.g. creating a vector of values:  vec <- c(1,2,3,4,5)
# index = using a number position to select a row(s) or column(s), or in the case of our vector to select the nth element. e.g. if we want the "3" in out vec, we'd use vec[3], or vec[3]


######
## Introduction to R

# First we need some data to play with! We'll read in some example blood serum spectra from 10 patient in total; 5 
# of these have colorectal cancer, and 5 are considered controls after a negative colonoscopy.

# The structure of the data is a .csv file where each row contains an independent spectra, each column an intensity 
# reading at a wavenumber, and the last two columns contain the patient's diagnosis and a patient ID. There are 5 repeats per patient.

# Now lets read it in, first we'll look at raw spectra:

######

# read in our data from the csv file
raw_spectra <- read.csv("Raw_IOP_Data.csv", header = FALSE)
# View the whole thing?
View(raw_spectra)
# What about the dimensions of our data
dim(raw_spectra)


######

# The first row in our data frame contains the wavenumber, so we need to take this out of the data. 
# We'll also take out the Diagnosis column and the ID columns and store them as vectors.

######

# Selecting the first row of the raw_spectra data frame, and removing the last two rows
wn <- raw_spectra[1,-c(1016, 1017)]

# now let's remove the first row
raw_dat <- raw_spectra[-1,]

# the first 1015 columns are intensities, and V1016=Diagnosis and V1017=ID
Diagnosis <- raw_dat$V1016
ID <- raw_dat$V1017

# And we'll get rid of the last two columns now so we only have the intensity data in one data frame
raw <- raw_dat[,-c(1016, 1017)]

######

# Now we've sorted out our input data a little, let's take a look at how many of each we had, and how to produce a basic plot in R.

######

# How many cancers and controls do we have?
table(Diagnosis)

# How about of each patient?
table(ID)

# Now lets plot one of the spectra - what exactly are we looking at?
# plot() takes a bunch of arguments, but we'll just look at x, y, and type for now

plot(as.numeric(wn), as.numeric(raw[1,]), type = "l")

# and there we have the first spectra in the dataset, how do we spruce this up a bit, tidy up the plot
plot(as.numeric(wn), as.numeric(raw[1,]), type = "l", xlab = expression(Wavenumber / cm^-1),
     ylab = "Intensity / a.u.")

# how about we plot all the spectra? Want to use the matplot function
matplot(x=as.numeric(wn), y=t(raw), type = "l", xlab = expression(Wavenumber / cm^-1),
        ylab = "Intensity / a.u.")




####### 
# END PART I CODING SEGMENT
######
