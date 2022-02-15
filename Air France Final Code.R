## -----------------------------------------------------------------------------
##
## Script name: Air France Digital Marketing Analysis
##
## Purpose of script: Preparing and analyzing digital campaign data from Air France
##                    business case published by Kellogg School of Management in order
##                    to make recommendations for a future digital ad strategy.
##
## Authors: Team 11 // Alexey Pchelintsev, Carmen Neier, Mary Stanly, Xingyu Wang 
##
## Date Created: 2022-02-13
##
## MSBA Assignment for HULT International Business School
##
##
## -----------------------------------------------------------------------------

################################################################################
##                                                                            ##
##                    Exploring the data                                      ##
##                                                                            ##
################################################################################

# importing all packages and libraries needed to the analysis
#installation code is commented out, uncomment in need of installation
#install.packages("lattice")
#install.packages("ggplot2")
#install.packages("stats")
#install.packages("graphics")

#calling libraries
library(readxl)     # *.xlsx (MS Excel) import
library(plotly)     # visualization
library(ggplot2)    # visualization
library(caret)      # confusion matrix
library(ROCR)       # AUC/ROC
library(rpart)      # GINI Trees
library(rpart.plot) # GINI Trees

# importing the Air France data
# saving the data to data frame af
# change file path accordingly
# initial data frame location
af <- read_excel("C:/Users/Carmen/OneDrive - Hult Students/HULT/Business Analytics/Term 2/R/Team Assignment/Air France Case Spreadsheet.xls", 
                 sheet = "DoubleClick")

# importing second data sheet of data saving it to dataframe kayak
kayak <- read_excel("C:/Users/Carmen/OneDrive - Hult Students/HULT/Business Analytics/Term 2/R/Team Assignment/Air France Case Spreadsheet.xls",
                    sheet = "Kayakclean")

# taking a first look at the data and what it is made up of
View(af)
View(kayak)

num_obs <- nrow(af) 
num_obs #4509 observations (ad campaigns) in data set 

################################################################################
##                                                                            ##
##                    Massaging the data                                      ##
##                                                                            ##
################################################################################

# cleaning up missing values
is.na(af) # bid strategy has missing values

af %>%
  summarise(count = sum(is.na(`Bid Strategy`)))
# 1224 missing values in Bid Strategy

# converting NAs in Bid Strategy to string
af[is.na(af)] <- 'undefined'

table(af$`Bid Strategy`)
# conversion worked, NAs are now "undefined"

# making a new column of return of advertising
af$ROA    <- round(af$Amount / af$`Total Cost`,2)

kayak$ROA <- round(kayak$`Total Revenue`/kayak$`Media Cost`,2)

# cleaning up infinite value in ROA
af %>%
  summarise(count = sum(is.infinite(ROA)))

# finding what row the single infinite value is in
is.infinite(af$ROA) # Infinite value is value 338
inf_row <- af[338,]

# deleting a single value where total cost is 0 causing an infinite value
af <- af[-c(338),]
# deleting this data is necessary in order to use ROA for analysis (which is
# an industry standard)

# ROA alone is not sufficient to determine the success of an ad we would like
# to see ads that have both a positive ROA and a large number of impressions

# making a label based on Impressions and ROA
af$label <- c()

for(i in 1:nrow(af)){
  if(af$Impressions[i] > 1000 & af$ROA[i] > 0){
    af$label[i] <- "good"
  }  else {af$label[i] <- "bad"} 
}

table(af$label)
summary(af$ROA)
# observations above 1000 is an industry benchmark*
# *see the presentation materials for the source
# this label column will be the target variable 

# making a binary column for label to use as target variable for modeling
af$sufficient <- gsub("good", "1", af$label)
af$sufficient <- gsub("bad", "0", af$sufficient)
af$sufficient <- as.numeric(af$sufficient)

# creating a data frame with only sufficient ads for further analysis use
af_sufficient <- af[which(af$sufficient == 1),]
summary(af_sufficient)

# preparing key words for analysis
# separating top 3 Air France key words into separate data frames 
af_airfrance       <- af[which(af$`Keyword Group` == 'Air France'),]
af_airfrance_brand <- af[which(af$`Keyword Group` == 'Air France Brand'),]
af_airfrance_web   <- af[which(af$`Keyword Group` == 'Air France Website'),]

af_airfrance_total <- rbind(af_airfrance, af_airfrance_brand, af_airfrance_web)
summary(af_airfrance_total)

# making new column with top 3 Air France keywords as binary
af$Keyword_Airfrance <- grepl('Air France', af$`Keyword Group`)
af$Keyword_Airfrance <- as.numeric(af$Keyword_Airfrance)

table(af$`Publisher Name`)

# converting Publisher Name to dummies
# We'll start with substituting Brands on numbers (as char datatype at first)
af$Publisher_dummies <- gsub("Google - Global",   "1", af$`Publisher Name`)
af$Publisher_dummies <- gsub("Google - US",       "2", af$Publisher_dummies)
af$Publisher_dummies <- gsub("MSN - Global",      "3", af$Publisher_dummies)
af$Publisher_dummies <- gsub("MSN - US",          "4", af$Publisher_dummies)
af$Publisher_dummies <- gsub("Overture - Global", "5", af$Publisher_dummies)
af$Publisher_dummies <- gsub("Overture - US",     "6", af$Publisher_dummies)
af$Publisher_dummies <- gsub("Yahoo - US",        "7", af$Publisher_dummies)

# after all the publishers are indicated with numbers, let's convert it in a
# proper numeric data type
af$Publisher_dummies <- as.numeric(af$Publisher_dummies)

# checking result
table(af$Publisher_dummies) 

# converting match type to dummies
# We'll start with substituting Brands on numbers (as char datatype at first)
af$MT_dummies <- gsub("Advanced", "1", af$`Match Type`)
af$MT_dummies <- gsub("Broad",    "2", af$MT_dummies)
af$MT_dummies <- gsub("Exact",    "3", af$MT_dummies)
af$MT_dummies <- gsub("N/A",      "4", af$MT_dummies)# creating separate group for NAs
af$MT_dummies <- gsub("Standard", "5", af$MT_dummies)

# after all the Match Types are indicated with numbers, let's convert it in a
# proper numeric data type
af$MT_dummies <- as.numeric(af$MT_dummies)

# checking result
table(af$MT_dummies) 

# converting Status to numeric
af$Status_dummies <- gsub("Deactivated", "1", af$Status)
af$Status_dummies <- gsub("Live",        "2", af$Status_dummies)
af$Status_dummies <- gsub("Paused",      "3", af$Status_dummies)
af$Status_dummies <- gsub("Sent",        "4", af$Status_dummies)
af$Status_dummies <- gsub("Unavailable", "5", af$Status_dummies)

# after all the Status are indicated with numbers, let's convert it in a
# proper numeric data type
af$Status_dummies <- as.numeric(af$Status_dummies)

# checking result
table(af$Status_dummies) 

# cleaning Bid Strategy variable and converting to numeric groups
af$Bid_dummies <- gsub("Pos 3-6",                    "1", af$`Bid Strategy`)
af$Bid_dummies <- gsub("Position 1 -2 Target",       "2", af$Bid_dummies)
af$Bid_dummies <- gsub("Position 1-2 Target",        "2", af$Bid_dummies)
af$Bid_dummies <- gsub("Position 1- 3",              "3", af$Bid_dummies)
af$Bid_dummies <- gsub("Position 1-4 Bid Strategy",  "4", af$Bid_dummies)
af$Bid_dummies <- gsub("Postiion 1-4 Bid Strategy",  "4", af$Bid_dummies)
af$Bid_dummies <- gsub("Position 2-5 Bid Strategy",  "5", af$Bid_dummies)
af$Bid_dummies <- gsub("Position 5-10 Bid Strategy", "6", af$Bid_dummies)
af$Bid_dummies <- gsub("undefined",                  "7", af$Bid_dummies)

# after all the Bids are indicated with numbers, let's convert it 
# in a proper numeric data type
af$Bid_dummies <- as.numeric(af$Bid_dummies)

# checking result
table(af$Bid_dummies) 

################################################################################
##                                                                            ##
##                     Descriptive Statistics                                 ##
##                                                                            ##
################################################################################

# doing descriptive statistics on data
summary(af)
summary(kayak)

summary(af$ROA)     # mean is 4.4
summary(kayak$ROA)  # mean is 65
# INSIGHT: difference in ROA between kayak and the rest is 15x

# checking the distribution of data from publisher
tbl      <- table(af$`Publisher Name`) 
tbl_perc <- (tbl / sum(tbl))*100
tbl_perc
# google has biggest representation by far making up over 50% of the data
#will need to use medians and means for comparing amongst publisher

# differences in Air France keyword and others
summary(af_airfrance_total)
summary(af)

# creating scatter plots to see correlation between variables and keyword
# Air France
ggplot(data=af, aes(x=Keyword_Airfrance, y=Amount))+
  geom_jitter()
# higher amounts are correlated with keyword Air France

ggplot(data=af, aes(x=Keyword_Airfrance, y=Clicks))+
  geom_jitter()
# slightly more high click amounts correlating with Air France keyword

# plotting graphs to show comparison of KPI performance

# make graph including all publishers, also Kayak for ROA
x <- c('Kayak',
       'MSN-Global',
       'MSN - US',
       'Yahoo-US',
       'Google-Global',
       'Overture-Global',
       'Google-US',
       'Overture-US')

y <- c(64.5,
       11.6,
       2.18,
       11.3,
       5.69,
       5.48,
       2.22,
       2.22)
# ROA averages across channels

data <- data.frame(x, y)

# recoding the automatic alphabetic order
data$x <- factor(data$x,
                 levels = data[["x"]])

# plotting ROA
p <- plot_ly(data,
             x= ~x,
             y= ~y,
             type = "bar",
             name = "Return on Advertising",
             color = x,
             alpha = 0.5) %>%
  layout(title = "Return On Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         showlegend = FALSE)
p


#plotting comparison of conversion rate
x2 <- c('MSN-Global',
        'MSN - US',
        'Yahoo-US',
        'Google-Global',
        'Overture-Global',
        'Google-US',
        'Overture-US')

y2 <- c(1.1,
        0.73,
        1.8,
        0.4,
        0.2,
        0.4,
        0.09)
#conversion across channels

data2 <- data.frame(x2, y2)

# recoding the automatic alphabetic order
data2$x2 <- factor(data2$x2,
                  levels = data2[["x2"]])

# plotting conversion
p2 <- plot_ly(data2,
              x= ~x2,
              y= ~y2,
              type = "bar",
              name = "Transaction Conversion Rate",
              color = x2,
              alpha = 0.5) %>%
  
  layout(title = "Transaction Conversion Rate",
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         showlegend = FALSE)
p2

################################################################################
##                                                                            ##
##                    Predictive Modeling                                     ##
##                                                                            ##
################################################################################

# Normalizing Data for use in modeling
# Creating a UDF to normalize with min-max rescaling
normalize <- function(var){
  norm <- (var-min(var))/(max(var)-min(var))
  return(norm)
  
} # closing the normalize UDF

# using our UDF going throughout all the numeric variables and checking the results
af$SEB_norm              <- normalize(var=af$`Search Engine Bid`)
head(af$SEB_norm)

af$Clicks_norm           <- normalize(var=af$Clicks)
head(af$Clicks_norm)

af$Click_Charges_norm    <- normalize(var=af$`Click Charges`)
head(af$Click_Charges_norm)

af$AVG_CpC_norm          <- normalize(var=af$`Avg. Cost per Click`)
head(af$AVG_CpC_norm)

af$Impressions_norm      <- normalize(var=af$Impressions)
head(af$Impressions_norm)

af$ECT_norm              <- normalize(var=af$`Engine Click Thru %`)
head(af$ECT_norm)

af$AVG_Pos_norm          <- normalize(var=af$`Avg. Pos.`)
head(af$AVG_Pos_norm)

af$Trans_Conv_norm       <- normalize(var=af$`Trans. Conv. %`)
head(af$Trans_Conv_norm)

af$Total_Cost_Trans_norm <- normalize(var=af$`Total Cost/ Trans.`)
head(af$Total_Cost_Trans_norm)

af$Amount_norm           <- normalize(var=af$Amount)
head(af$Amount_norm)

af$Total_Cost_norm       <- normalize(var=af$`Total Cost`)
head(af$Total_Cost_norm)

# Creating Random Sampling into Training and Testing
# Step One: get the random vertical index with 80% representatives
training_idx <- sample(1:nrow(af), size=0.8*nrow(af))

# Step Two: new data frame for training
af_train <- af[training_idx,]
af_test  <- af[-training_idx,]

# logistic regressions with normalized variables excl. variables of target
# (Impressions, Cost, Amount)
af_norm_logit <- glm(sufficient ~ MT_dummies+
                       Status_dummies+
                       SEB_norm+
                       Clicks_norm+
                       Click_Charges_norm+
                       AVG_CpC_norm+
                       ECT_norm+
                       AVG_Pos_norm+
                       Trans_Conv_norm+
                       Publisher_dummies,
                     data   = af_train,
                     family = 'binomial')

summary(af_norm_logit)
# status dummies, avg cpc, avg pos are insignificant

# running logit regression without insignificant variables
af_norm_logit <- glm(sufficient ~ MT_dummies+
                       SEB_norm+
                       Clicks_norm+
                       Click_Charges_norm+
                       ECT_norm+
                       Trans_Conv_norm+
                       Publisher_dummies,
                     data   = af_train,
                     family = 'binomial')

summary(af_norm_logit)
# the coefficient with the biggest positive impact on successful ad is number
# of clicks, followed by publisher as second highest
# the coefficient with the biggest negative impact on successful ad is click
# charges followed by click through rate

# Here we'll go with a complete set of everything we have in original (not
# normalized) values for numeric, but also numeric/dummies for categories we've
# defined before for factors
#conducting a logit regression on variables and dummies
af_logit <- glm(sufficient ~ MT_dummies+
                  Clicks+
                  Status_dummies+
                  `Avg. Cost per Click`+
                  `Click Charges`+
                  `Engine Click Thru %`+
                  `Search Engine Bid`+
                  `Total Cost/ Trans.`+
                  `Trans. Conv. %`+
                  Bid_dummies+
                  `Avg. Pos.`+
                  Publisher_dummies,
                data   = af_train,
                family = 'binomial')
summary(af_logit)

#conducting the same regression without the insignificant variables
af_logit <- glm(sufficient ~ Clicks+
                  `Avg. Cost per Click`+
                  `Click Charges`+
                  `Engine Click Thru %`+
                  `Total Cost/ Trans.`+
                  `Trans. Conv. %`+
                  Bid_dummies+
                  Publisher_dummies,
                data   = af_train,
                family = 'binomial')
summary(af_logit)

# building an odds calculating function
# UDF for log transformation
odds <- function(x){
  result <- exp(x)-1
  return(result)
}

# gaining business insights through odds calculations
odds(0.1122397)
#with every % increase of conversion odds of business success increase by 11%

odds(0.0142862) #with every additional click odds of business success increase
# by 1.4%

odds(-1.2890982) # with every additional dollar of av cost per click odds of
# business success decrease by 72%

# INSIGHT: focus on reducing average cost of click and increasing conversion rate

# Developing Confusion Matrix. UNITS
# the numbers represent the probability of business success. above 0.5 means
# business success, below means failure. Step 1 - TEST Set
prediction_testing <- predict(af_logit,
                              af_test,
                              type="response")

confusionMatrix(data = as.factor(as.numeric(prediction_testing > 0.5)),
                reference = as.factor(as.numeric(af_test$sufficient)))

# Step 2 - TRAIN Set
prediction_training <- predict(af_logit,
                               af_train,
                               type="response")

confusionMatrix(data = as.factor(as.numeric(prediction_training > 0.5)),
                reference = as.factor(as.numeric(af_train$sufficient)))
# ACCURACY 0.9933 for TEST and 0.997 for TRAIN. This extremely high value of
# accuracy can't allow to consider the model as adequate.

# Developing Confusion Matrix. UNITLESS NORMALIZED. Step 1 - TEST Set
prediction_testing <- predict(af_norm_logit,
                              af_test,
                              type="response")

confusionMatrix(data = as.factor(as.numeric(prediction_testing > 0.5)),
                reference = as.factor(as.numeric(af_test$sufficient)))

# Step 2 - TRAIN Set
prediction_training <- predict(af_norm_logit,
                               af_train,
                               type="response")

confusionMatrix(data = as.factor(as.numeric(prediction_training>0.5)),
                reference = as.factor(as.numeric(af_train$sufficient)))
# ACCURACY 0.7838 for TEST and 0.7824 for TRAIN.
# In this case we can suppose at least that the model make sence in terms of
# accuracy.

# creating an AUC/ROC for logistic regression
pred_val_logit <- prediction(prediction_testing, af_test$sufficient)

perf_logit <- performance(pred_val_logit,
                          "tpr",
                          "fpr")

# Visualization AUC and ROC
plot(perf_logit)
 
# As an alternative approach and double-check, we'll try to run model using the
# GINI trees

af_tree <- rpart(sufficient ~ Clicks +
                             `Click Charges`+
                             `Avg. Cost per Click`+
                             `Engine Click Thru %`+
                             `Trans. Conv. %`+
                             `Total Cost/ Trans.`+
                             `Total Volume of Bookings`, 
                  data   = af_train,
                  method = "class",
                  cp     = 0.001)

rpart.plot(af_tree,
           type  = 1,
           extra = 1)
# We see now high levels of filtering the observations during the classification
# including the filter factors and its values

# using this tree to predict on testing customers

af_tree_predict <- predict(af_tree,
                           af_test,
                           type = 'prob')
# outputs probability of success and failure

af_tree_prediction <- prediction(af_tree_predict[,2], 
                                  af_test$sufficient)
# only taking the second variable which represents successes

tree_perf <- performance(af_tree_prediction,"tpr", "fpr")  

plot(perf_logit,
     col="blue")

plot(tree_perf,
     col="green4",
     add=TRUE)
#primary decision node is cost per transaction, aligning with our previous findings

################################################################################
##                                                                            ##
##                    Final Recommendations                                   ##
##                                                                            ##
################################################################################

#descriptive statistics have show that kayak and yahoo are the most effective channels
#air france has been predominantly using google ads which is expensive and lacks conversion
#branded keywords with the word "airfrance" yield better ROA than others
#regression shows that cost and conversion is the biggest influence on business success or failure
