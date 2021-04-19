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

# Now that we've had a look at our raw spectra, now let's read in some data that has been processed. 
# In Raman spectroscopy we have a background which causes the spectrum to lay on a slant. 
# We remove this by using various background removal techniques, in this case a polynomial has been fitted to the background and removed. 
# We also want to normalise our data to account for fluctuations in serum concentration, laser fluctuations etc. 
# This has been done using min/max normalisation scaling between 0 and 1.
# Now we just need to repeat the same process as before; separating the labels, and diagnosis, and wavenumber.

######

# read in our data from the csv file
processed_spectra <- read.csv("Pre-Processed_IOP_Data.csv", header = FALSE)

# Selecting the first row of the processed_spectra data frame, and removing the last two rows
wn <- processed_spectra[1,-c(1016, 1017)]

# now let's remove the first row
processed_dat <- processed_spectra[-1,]

# the first 1015 columns are intensities, and V1016=Diagnosis and V1017=ID
Diagnosis <- processed_dat$V1016
ID <- processed_dat$V1017

# And we'll get rid of the last two columns now so we only have the intensity data in one data frame
processed <- processed_dat[,-c(1016, 1017)]

# And now we'll plot them all again
matplot(x=as.numeric(wn), y=t(processed), type = "l", xlab = expression(Wavenumber / cm^-1),
        ylab = "Intensity / a.u.")

# Now let's say we want to colour them based on the diagnosis? we use an ifelse statement
matplot(x=as.numeric(wn), y=t(processed), type = "l", xlab = expression(Wavenumber / cm^-1),
        ylab = "Intensity / a.u.", col = ifelse(Diagnosis == "Cancer", "red", "black"))



######

# So, we have 5 repeats per patient, and it would be useful to compute the mean per patient. R has a very useful function called aggregate which we'll utilise.

######

# first we need to re-add the patient ids to the processed spectra
proc_with_id <- cbind(processed, "ID" = ID, "Diagnosis"= ifelse(Diagnosis =="Cancer", 1, 0)) # converting the diagnosis into a number because aggregate is taking a mean

# Aggregate uses the .~ notation, which essentially says I want to use all the columns, aggregating on the ID, and computing the mean
mean_patients <- aggregate(.~ID, proc_with_id, mean)

#Now let's store the IDs and diagnosis again
mean_id <- mean_patients$ID
mean_diagnosis <- mean_patients$Diagnosis

#and again remove them from the dataset
mean_pats <- mean_patients[,-c(1, ncol(mean_patients))]

# now let's plot the means and colour based on diagnosis
matplot(x=as.numeric(wn), y=t(mean_pats), type = "l", xlab = expression(Wavenumber / cm^-1),
        ylab = "Intensity / a.u.", col = ifelse(mean_diagnosis == 1, "red", "black"))

######

# A useful tool when dealing with data with high dimensionality (i.e. many variables), principle component analysis (PCA) 
# can help us reduce the number of variables whilst still keeping a large amount of variance contained in the data. 
# It does this by extracting "principle components" from the data, each of which is orthogonal to one another, 
# and PC1 contains the most variance, PC2 contains the next highest, and so on.

######

# Compute PCA on the data using R's built in prcomp function.
pca <- prcomp(mean_pats)

# summarise the PCA object
summary(pca)
str(pca)

# Store the PC scores - and let's take a look at PC1 vs. PC2 i.e. the most variance contained in principle components.
scores <- pca$x

# quick plot of PC1 ad PC2, which are the first and second columns
plot(scores[,1], scores[,2])

plot(scores[,1], scores[,2], col = ifelse(mean_diagnosis == 1, "red", "black"), 
     xlab = "PC1", ylab = "PC2", pch = 19)

# Now a pairs plot to look rapidly at a bunch of variables compared to others
pairs(scores[,1:6], col = ifelse(mean_diagnosis == 1, "red", "black"), pch = 19)

# Now the loading on PC1 - representation of how much variance at each wavenumber goes into PC1
plot(as.numeric(wn), pca$rotation[,1], type = "l")

####### 
# END PART 2 CODING SEGMENT
######

# Now that we have reduced our data, let's try building a model to separate these. We'll use a k-means clustering technique to see if our PCs cluster.

######

# K-Means Cluster Analysis
fit <- kmeans(scores[,1:2], 2) # 2 cluster solution
str(fit)
# get cluster means
aggregate(scores[,1:2],by=list(fit$cluster),FUN=mean)

mydata <- data.frame(scores[,1:2], fit$cluster)

# Plain PC plot
plot(scores[,1], scores[,2], col = ifelse(mean_diagnosis == 1, "red", "black"), 
     xlab = "PC1", ylab = "PC2", pch = 19)

#Cluster prediction
plot(scores[,1], scores[,2], col = ifelse(mydata$fit.cluster == 1, "blue", "black"), 
     xlab = "PC1", ylab = "PC2", pch = 19)

# Not a great model! can see the clustering on the cancer side is a bit too large size wise
# perhaps some other distance metrics/optimisations may be able to produce a better fit?
# Maybe try to create a linear Support Vector Machine
# or a Logistic regression - see what works!

####### 
# END PART 3 CODING SEGMENT