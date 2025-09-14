### NOTE: The code in this script was intended to clean and filter the data for analysis as a pre-requisite/guide for use in the other scripts during the project. ###

# Identify the most utilized drugs under the Medicaid program.

library(dplyr)
library(psych)
library(tidyr)

set.seed(1)

utilization <- read.csv("StateDrugUtilization2023.csv")
drug.utilization <- utilization %>% 
                    na.omit(utilization) %>%
                    arrange(desc(units_reimbursed)) %>%
                    filter(duplicated(product_name == FALSE)) 

drug.utilization <- drug.utilization[!duplicated(drug.utilization$product_name),]
View(drug.utilization)


# Test whether manufacturer count significantly affects Medicaid spending


spending <- read.csv("medicaid_spending_by_drug_data_dictionary.csv")
drug.spending <-  spending %>%
                  na.omit(spending) %>%
                  filter(Mftr_Name == "Overall") %>%
                  arrange(desc(Tot_Mftr)) %>%
                  filter(duplicated(Gnrc_Name))
drug.spending <- drug.spending[!duplicated(drug.spending$Gnrc_Name),]
View(drug.spending)


# Test whether there is a significant change in CAGR from avg total spending per
# dosage unit by Medicaid.
