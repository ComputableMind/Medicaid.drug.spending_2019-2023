library(dplyr)
library(tidyr)
library(psych)
library(leaps)
set.seed(1)

spending <- read.csv("medicaid_spending_by_drug_data_dictionary.csv")
drug.spending <-  spending %>%
  na.omit(spending) %>%
  filter(Mftr_Name == "Overall") %>%
  arrange(desc(Tot_Mftr)) %>%
  filter(duplicated(Gnrc_Name))
drug.spending <- drug.spending[!duplicated(drug.spending$Gnrc_Name),]
View(drug.spending)

# subset selection for logistic regression
regfit.full=regsubsets(CAGR_Avg_Spnd_Per_Dsg_Unt_18_22 ~ Avg_Spnd_Per_Dsg_Unt_Wghtd_2018 +
                         Avg_Spnd_Per_Dsg_Unt_Wghtd_2019 + Avg_Spnd_Per_Dsg_Unt_Wghtd_2020 +
                       Avg_Spnd_Per_Dsg_Unt_Wghtd_2021 +  Avg_Spnd_Per_Dsg_Unt_Wghtd_2022
                       , data= drug.spending, nvmax = 5)
summary(regfit.full)
reg.summary=summary(regfit.full)


names(reg.summary)
which.min(reg.summary$cp)
plot(reg.summary$cp, xlab="Number of predictors", ylab="Cp")
regfit.bwd=regsubsets(CAGR_Avg_Spnd_Per_Dsg_Unt_18_22 ~ Avg_Spnd_Per_Dsg_Unt_Wghtd_2018 +
                        Avg_Spnd_Per_Dsg_Unt_Wghtd_2019 + Avg_Spnd_Per_Dsg_Unt_Wghtd_2020 +
                        Avg_Spnd_Per_Dsg_Unt_Wghtd_2021 +  Avg_Spnd_Per_Dsg_Unt_Wghtd_2022
                      , data= drug.spending, nvmax = 5, method = "backward")
summary(regfit.bwd)
coef(regfit.bwd,3) # Avg_Spnd_Per_Dsg_Unt_Wghtd_2019, Avg_Spnd_Per_Dsg_Unt_Wghtd_2020, Avg_Spnd_Per_Dsg_Unt_Wghtd_2021

# Logistic regression on CAGR Total spending per unit, based on annual total spending 

attach(drug.spending)
summary(drug.spending)

drug.spending <- drug.spending %>%
  mutate(CAGR_Direction = ifelse(CAGR_Avg_Spnd_Per_Dsg_Unt_18_22 > 0, 'Up', 'Down'))
drug.spending$CAGR_Direction <- factor(drug.spending$CAGR_Direction, levels = c('Down', 'Up'))


glm.fit=glm(drug.spending$CAGR_Direction ~ Avg_Spnd_Per_Dsg_Unt_Wghtd_2019 +
              Avg_Spnd_Per_Dsg_Unt_Wghtd_2020 + Avg_Spnd_Per_Dsg_Unt_Wghtd_2021,
            family = binomial, data = drug.spending)

glm.probs <- predict(glm.fit,type="response")

options("digits"=2)
contrasts(drug.spending$CAGR_Direction)
dim(drug.spending)

### 2018-2022 CAGR mean, 712 observations
mean(drug.spending$CAGR_Avg_Spnd_Per_Dsg_Unt_18_22)
glm.probs.df <- data.frame(glm.probs)
geometric.mean(glm.probs.df) # 0.5375695 


glm.pred=rep("Down", 712)
glm.pred[glm.probs>0.54]="Up"
table(glm.pred, drug.spending$CAGR_Direction)
mean(glm.pred == drug.spending$CAGR_Direction)


######### improve prediciton accuracy #########
train <- sample(nrow(drug.spending), 712/2, replace = FALSE)
test <- !(1:nrow(drug.spending) %in% train)
drug.spending.test.data <- drug.spending[test, ]
CAGR_Direction.test <- drug.spending.test.data$CAGR_Direction

# backup training codes (unresolved)
# train <- sample(nrow(drug.spending), 712/2, replace = FALSE)
# drug.spending.test.data <- drug.spending[!train,]

glm.fit <- glm(drug.spending$CAGR_Direction ~ Avg_Spnd_Per_Dsg_Unt_Wghtd_2019 +
              Avg_Spnd_Per_Dsg_Unt_Wghtd_2020 + Avg_Spnd_Per_Dsg_Unt_Wghtd_2021,
            family = binomial, data = drug.spending, subset = train)
glm.probs <- predict(glm.fit, drug.spending.test.data, type = "response")
glm.pred <- rep("Down", 356)
glm.pred[glm.probs>0.54]="Up"
table(glm.pred,CAGR_Direction.test)
mean(glm.pred==CAGR_Direction.test)
