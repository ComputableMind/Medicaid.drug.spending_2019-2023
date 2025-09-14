##Installing packages##
install.packages("dplyr")
install.packages("psych")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(psych)
library(tidyr)


set.seed(1)

######Data set cleaning#######
##############################

spending <- read.csv("medicaid_spending_by_drug_data_dictionary.csv")
drug.spending <-  spending %>%
  na.omit(spending) %>%
  filter(Mftr_Name == "Overall") %>%
  arrange(desc(Tot_Mftr)) %>%
  filter(duplicated(Gnrc_Name))
drug.spending <- drug.spending[!duplicated(drug.spending$Gnrc_Name),]

View(drug.spending)

# Keep only 'Brand' and 'Avg_Spending_Per_Unit' columns
data_subset <- subset(drug.spending, select = c("Brnd_Name","Tot_Mftr","Avg_Spnd_Per_Dsg_Unt_Wghtd_2018","Avg_Spnd_Per_Dsg_Unt_Wghtd_2019", "Avg_Spnd_Per_Dsg_Unt_Wghtd_2020","Avg_Spnd_Per_Dsg_Unt_Wghtd_2021","Avg_Spnd_Per_Dsg_Unt_Wghtd_2022"))

print(data_subset)

# Calculate CAGR for each consecutive pair of 2018 to 2019
  cagr_values_2018 <- numeric(length = nrow(data_subset))
  for (i in 1:(nrow(data_subset))) {
    beginning_value <- data_subset$Avg_Spnd_Per_Dsg_Unt_Wghtd_2018[i]
    ending_value <- data_subset$Avg_Spnd_Per_Dsg_Unt_Wghtd_2019[i]
    cagr_values_2018[i] <- (ending_value / beginning_value) - 1
  }
  
  # Add CAGR values to the dataset
  drug.spending_mod <- cbind(data_subset, CAGR_2018_to_2019 = c(cagr_values_2018))
  
  # Calculate CAGR for each consecutive pair of 2019 to 2020
  cagr_values_2019 <- numeric(length = nrow(data_subset))
  for (i in 1:(nrow(data_subset))) {
    beginning_value <- data_subset$Avg_Spnd_Per_Dsg_Unt_Wghtd_2019[i]
    ending_value <- data_subset$Avg_Spnd_Per_Dsg_Unt_Wghtd_2020[i]
    cagr_values_2019[i] <- (ending_value / beginning_value) - 1
  }
  
  # Add CAGR values to the dataset
  drug.spending_mod <- cbind(drug.spending_mod, CAGR_2019_to_2020 = c(cagr_values_2019))
  
  # Calculate CAGR for each consecutive pair of 2020 to 2021
  cagr_values_2020 <- numeric(length = nrow(data_subset))
  for (i in 1:(nrow(data_subset))) {
    beginning_value <- data_subset$Avg_Spnd_Per_Dsg_Unt_Wghtd_2020[i]
    ending_value <- data_subset$Avg_Spnd_Per_Dsg_Unt_Wghtd_2021[i]
    cagr_values_2020[i] <- (ending_value / beginning_value) - 1
  }
  
  # Add CAGR values to the dataset
  drug.spending_mod <- cbind(drug.spending_mod, CAGR_2020_to_2021 = c(cagr_values_2020))
  
  # Calculate CAGR for each consecutive pair of 2021 to 2022
  cagr_values_2021 <- numeric(length = nrow(data_subset))
  for (i in 1:(nrow(data_subset))) {
    beginning_value <- data_subset$Avg_Spnd_Per_Dsg_Unt_Wghtd_2021[i]
    ending_value <- data_subset$Avg_Spnd_Per_Dsg_Unt_Wghtd_2022[i]
    cagr_values_2021[i] <- (ending_value / beginning_value) - 1
  }
  
# Add CAGR values to the dataset
drug.spending_mod <- cbind(drug.spending_mod, CAGR_2021_to_2022 = c(cagr_values_2021))

#View Modified Dataset with added columns for CAGR
View(drug.spending_mod)

#######Making the Data smaller to focus on the extremes####
###########################################################

# Sort the dataset based on manufacturing numbers
drug.spending_mod_subset <- drug.spending_mod[order(drug.spending_mod$Tot_Mftr), ]

# Calculate the number of rows to keep for the top and bottom quantiles
num_rows <- nrow(drug.spending_mod_subset)
top_quantile_size <- ceiling(0.10 * num_rows)
bottom_quantile_size <- floor(0.10 * num_rows)

# Select the top and bottom quantiles
top_quantile <- drug.spending_mod_subset[(num_rows - top_quantile_size + 1):num_rows, ]
bottom_quantile <- drug.spending_mod_subset[1:bottom_quantile_size, ]

# Combine the top and bottom quantiles to create smaller dataset
selected_data <- rbind(top_quantile, bottom_quantile)

# View New Dataset
View(selected_data)

# Keep only CAGR columns
selected_data_subset <- subset(selected_data, select = c("Brnd_Name","Tot_Mftr","CAGR_2018_to_2019","CAGR_2019_to_2020","CAGR_2020_to_2021","CAGR_2021_to_2022"))

# View New Dataset
View(selected_data_subset)


############Clustering#####
###########################

# run hierarchical clustering, using complete as your measure of similarity
str(selected_data_subset) 
numeric_data <- selected_data_subset[, sapply(selected_data_subset, is.numeric)]
character_data <- selected_data_subset[, !sapply(selected_data_subset, is.numeric)]

# Remove rows with missing values
numeric_data <- na.omit(numeric_data)

# store the results in a variable called hc.complete
hc.complete <- hclust(dist(numeric_data),method="complete")

# plot the dendrogram
plot(hc.complete)

# cut the dendrogram to obtain three clusters
# notice the output, which tells you the the cluster for each observation
# you can also assign the output to variable, say hc.out
cutree(hc.complete,5)
hc.out <- cutree(hc.complete,5)

# plot each Brand, showing CAGR_2018_to_2019 on x-axis and CAGR_2019_to_2020 on y-axis 
plot(numeric_data$CAGR_2018_to_2019,numeric_data$CAGR_2019_to_2020,col=hc.out,xlab= "CAGR_2018_to_2019", ylab="CAGR_2019_to_2020")

#display the Brand names on the plot
text(x=numeric_data$CAGR_2018_to_2019, y=numeric_data$CAGR_2019_to_2020, labels =selected_data_subset$Brnd_Name, col=hc.out)

# plot each Brand, showing Tot_Mftr on x-axis and CAGR_2018_to_2019 on y-axis 
plot(numeric_data$Tot_Mftr,numeric_data$CAGR_2018_to_2019,col=hc.out, xlab="Tot_Mftr", ylab= "CAGR_2018_to_2019")

#display the Brand names on the plot
text(x=numeric_data$Tot_Mftr, y=numeric_data$CAGR_2018_to_2019, labels =selected_data_subset$Brnd_Name, col=hc.out)


# plot each Brand, showing Tot_Mftr on x-axis and CAGR_2019_to_2020 on y-axis 
plot(numeric_data$Tot_Mftr,numeric_data$CAGR_2019_to_2020,col=hc.out, xlab="Tot_Mftr", ylab= "CAGR_2019_to_2020")

#display the Brand names on the plot
text(x=numeric_data$Tot_Mftr, y=numeric_data$CAGR_2019_to_2020, labels =selected_data_subset$Brnd_Name, col=hc.out)


# plot each Brand, showing Tot_Mftr on x-axis and CAGR_2020_to_2021 on y-axis 
plot(numeric_data$Tot_Mftr,numeric_data$CAGR_2020_to_2021,col=hc.out, xlab="Tot_Mftr", ylab= "CAGR_2020_to_2021")

#display the Brand names on the plot
text(x=numeric_data$Tot_Mftr, y=numeric_data$CAGR_2020_to_2021, labels =selected_data_subset$Brnd_Name, col=hc.out)

# plot each Brand, showing Tot_Mftr on x-axis and CAGR_2021_to_2022 on y-axis 
plot(numeric_data$Tot_Mftr,numeric_data$CAGR_2021_to_2022,col=hc.out, xlab="Tot_Mftr", ylab= "CAGR_2021_to_2022")

#display the Brand names on the plot
text(x=numeric_data$Tot_Mftr, y=numeric_data$CAGR_2021_to_2022, labels =selected_data_subset$Brnd_Name, col=hc.out)


############Clustering2#####
###########################

# run hierarchical clustering, using complete as your measure of similarity
str(selected_data_subset2) 
numeric_data <- selected_data_subset2[, sapply(selected_data_subset2, is.numeric)]
character_data <- selected_data_subset2[, !sapply(selected_data_subset2, is.numeric)]

# Remove rows with missing values
numeric_data <- na.omit(numeric_data)

# store the results in a variable called hc.complete
hc.complete <- hclust(dist(numeric_data),method="complete")

# plot the dendrogram
plot(hc.complete)

# cut the dendrogram to obtain three clusters
# notice the output, which tells you the the cluster for each observation
# you can also assign the output to variable, say hc.out
cutree(hc.complete,3)
hc.out <- cutree(hc.complete,3)

# plot the dendrogram
plot(cutree)