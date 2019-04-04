#####################
# Loading Libraries #
#####################

## Function - To Install & Load Packages ##
package_install_load <- function(package) {
  
  if (!require(package, character.only = TRUE)) {
    invisible(install.packages(package, dep = TRUE,force= TRUE))
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package)
      if (!require(package, character.only = TRUE)) {
        print(paste("Package Not Found : ", package))
      }
    }
  }
  else{
    print(paste("Package Loaded : ", package ))
  }
}

## Function - Set Working Directory ##
set_working_directory <- function(dir_path) {
  
  if(!file.exists(dir_path)){ # Check If Directory Exist
    dir.create(dir_path) # Create Directory
  }
  
  setwd(dir_path) #Set Working Directory
  print(paste("Working Directory Set to :",dir_path)) # Print Message
}



## Block - Load Required Packages ##
install.packages("kernlab")
packages <- c("devtools","rlang","caret","ggplot2","plyr","dplyr", "e1071","mlr","randomForest","sqldf","rpart","corrplot","pROC","gmodels"
              ,"adabag","kernlab","class","caTools","ROCR","zipcode","reshape2","tidyverse","maps","viridis","albersusa","RColorBrewer","ModelMetrics")
devtools::install_github("hrbrmstr/albersusa")

## Execute - The Package Load Install Function ##
invisible(lapply(packages, package_install_load))

## Block - Setting Working Directory ##
set_working_directory("C:/Users/aggarwal.k/Downloads/Yelp");

##-------------------------------------------------------------------------------
##-------------------------------------------------------------------------------
# 1. Data Acquisition
##-------------------------------------------------------------------------------

business_raw <- read.csv("yelp_business.csv")
review_raw <- read.csv("yelp_review_wo_user.csv")
pop_raw <- read.csv("pop-by-zip-code.csv")
business_hrs_raw <- read.csv("yelp_business_hours.csv")
geo <- read.csv("Citystate.csv")
zipcodeimp <- read.csv("zipcodeimp.csv")

#Extracting restaurants details from all files within United States

business <- sqldf("SELECT distinct business_id,name,b.city,b.state,postal_code,latitude,longitude,stars,review_count,is_open 
                  FROM business_raw b
                  INNER JOIN geo g ON g.state = b.state
                  WHERE b.categories LIKE '%Restaurant%'
                  AND business_id NOT IN ('fh4hlhzpwQRPg82gFLDLdg','RBOXMNb_gzdmy1CRPxv_4A')")

review <- sqldf("SELECT distinct review_id,r.business_id,r.stars,date,useful,funny,cool
                FROM review_raw r
                INNER JOIN business b ON b.business_id = r.business_id")

review$date <- as.integer(review$date)

#Changes to the format were made in the excel sheet (to get the total number of hours open on each day)
business_hrs <- sqldf("SELECT distinct bh.* FROM business_hrs_raw bh
                      INNER JOIN business b ON b.business_id = bh.business_id")


##-------------------------------------------------------------------------------
# 2. Data Cleaning
##-------------------------------------------------------------------------------

#Cleaning postal codes and imputing missing values (searched manually from the internet against latitudes and longitudes)

data("zipcode")
business$postal_code <- clean.zipcodes(business$postal_code)
sapply(business, function(x) sum(is.na(x)))

business %>%
  filter(is.na(postal_code))

business[is.na(business)] <- 0

zipcodeimp$postal_code <- as.character(zipcodeimp$postal_code)

business <- sqldf("select business_id,name,city,state
                  ,CASE WHEN b.postal_code == '0' 
                        THEN imp.postal_code 
                        ELSE b.postal_code
                  END AS postal_code
                  ,b.latitude,b.longitude,stars,review_count,is_open
                  FROM business b
                  LEFT JOIN zipcodeimp imp ON imp.latitude = b.latitude AND imp.longitude = b.longitude")


pop_raw$zip_code <- clean.zipcodes(pop_raw$zip_code)
sapply(pop_raw, function(x) sum(is.na(x)))

pop_raw$population <- as.integer((pop_raw$y.2016+pop_raw$y.2015+pop_raw$y.2014+pop_raw$y.2013+pop_raw$y.2012+pop_raw$y.2011+pop_raw$y.2010)/7)
pop <- pop_raw[-c(2:8)]

##-------------------------------------------------------------------------------
# 3. Data Exploration and New Feature Designing
##-------------------------------------------------------------------------------

##Business Exploratory Analysis

str(business)

categories <- as.data.frame(strsplit(paste(business_raw$categories, collapse=";"), split = ";"))
names(categories)[1] <- "category"

#Summarized Categories (number of types each cateogry was listed Top 10)

business_cat <- sqldf("SELECT category, count(category) AS count FROM
               categories
               GROUP BY category 
               ORDER BY 2 DESC 
               LIMIT 10")

ggplot(data=business_cat, aes(x=factor(category, levels = category[order(count)]),y=count,fill=count,label = count)) +
  geom_col() + geom_text(nudge_y = 2000) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,60000,10000)) +
  labs(x="Business Category",y="Count of Establishments", title="Top 10 Business Categories on Yelp")+
  theme(plot.title = element_text(hjust = 0.5))

#Number of restaurants per city (Top 20)

business_city <- sqldf("SELECT city, count(business_id) AS count FROM
               business
               GROUP BY city 
               ORDER BY 2 DESC 
               LIMIT 20")

ggplot(data=business_city, aes(x=factor(city, levels = city[order(count)]),y=count,fill=count,label = count)) +
  geom_col() + geom_text(nudge_y = 200) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,6000,1000)) +
  labs(x="City",y="Count of Restaurants", title="Top 20 Cities with Restaurants")+
  theme(plot.title = element_text(hjust = 0.5))

#Number of restaurants by star rating

business_star <- sqldf("SELECT stars, count(business_id) AS count FROM
                       business
                       GROUP BY stars 
                       ORDER BY 2 DESC 
                       ")

ggplot(data=business_star, aes(x=stars,y=count,fill=count,label = count)) +
  geom_col() + geom_text(nudge_y = 200) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(0,5,.5)) +
  labs(x="Star Rating",y="Count of Restaurants", title="Restaurants by Star Rating")+
  theme(plot.title = element_text(hjust = 0.5))

postal_res <- sqldf("select postal_code
                    ,count(business_id) AS postal_res
                    FROM business
                    GROUP BY postal_code
                    ORDER BY 2 DESC")

business <- sqldf("select business_id,name,b.city,b.state,b.postal_code,p.postal_res,is_open,z.longitude,z.latitude,stars,review_count
                  FROM business b
                  INNER JOIN postal_res p on p.postal_code = b.postal_code
                  INNER JOIN zipcode z on z.zip = b.postal_code AND b.city = z.city")


#Plotting coordinates on the US geographical map
us <- map_data('state')

ggplot(business,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = postal_res),size=3,alpha=0.25) +
  xlim(-125,-65)+ylim(20,50)


##Reviews Exploratory Analysis

str(review)
summary(review)

#There seems to be outliers in review

#We will aggregate our data to useful features and then remove any outliers
review <- sqldf("SELECT business_id
                ,count(review_id) review_count
                ,median(stars) AS Stars
                ,CASE WHEN (max(date)-min(date)) <= 7
                      THEN 1
                      ELSE CAST(((max(date)-min(date))/7) AS FLOAT)
                 END AS review_age_wks
                ,CASE WHEN (max(date)-min(date)) <= 7 
                      THEN CAST((count(review_id)) AS FLOAT)
                      ELSE count(review_id)/CAST(((max(date)-min(date))/7) AS FLOAT)
                 END AS review_per_wk
                ,sum(useful)+sum(funny)+sum(cool) AS reactions
                ,sum(useful)+sum(funny)+sum(cool)/count(review_id)  AS reactions_per_wk 
                FROM review
                GROUP BY business_id")


sqldf("select * from review order by 6 desc limit 10")

#We will be removing the ids "DN0b4Un8--Uf6SEWLeh0UA" as they have a substantial higher reactions as compared to the other restaurants.
review <- review %>% filter(review$reactions < 20000)


#Top 10 Restaurants by reviews

review_cnt <- sqldf("SELECT name,r.review_count AS count 
                    FROM review r
                    INNER JOIN business b on r.business_id = b.business_id
                    ORDER BY 2 desc
                    LIMIT 10")

ggplot(data=review_cnt, aes(x=factor(name, levels = name[order(count)]),y=count,fill=count,label = count)) +
  geom_col() + geom_text(nudge_y = 200) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,10000,2000)) +
  labs(x="Restaurants",y="Count of Reviews", title="Top 10 Restaurants by Reviews")+
  theme(plot.title = element_text(hjust = 0.5))

#Top 10 Restaurants by reactions

review_reactions <- sqldf("SELECT name,r.reactions AS count 
                    FROM review r
                    INNER JOIN business b on r.business_id = b.business_id
                    ORDER BY 2 desc
                    LIMIT 10")

ggplot(data=review_reactions, aes(x=factor(name, levels = name[order(count)]),y=count,fill=count,label = count)) +
  geom_col() + geom_text(nudge_y = 500) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,18000,3000)) +
  labs(x="Restaurants",y="Count of Reactions", title="Top 10 Restaurants by Reaction")+
  theme(plot.title = element_text(hjust = 0.5))


#Star Rating Histogram

review_star <- sqldf("SELECT r.business_id,r.stars,b.is_open,review_age_wks 
                    FROM review r
                    INNER JOIN business b on r.business_id = b.business_id
                    ORDER BY 2 desc")



ggplot(review_star, aes(x=Stars,fill=factor(is_open),color=factor(is_open))) + 
  geom_histogram(position="dodge", binwidth=.5) + 
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  geom_vline(aes(xintercept=mean(review_star$Stars,na.rm = 1)),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(review_star$Stars,na.rm = 1)),
             color="blue", linetype="dashed", size=1) +
  labs(y="Frequency", x="Star Rating", title="Histogram - Star Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#Age of Reviews Histogram

ggplot(review_star, aes(x=review_age_wks,fill=factor(is_open),color=factor(is_open))) + 
  geom_histogram(position="dodge", binwidth=100) + 
  geom_vline(aes(xintercept=mean(review_star$review_age_wks,na.rm = 1)),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(review_star$review_age_wks,na.rm = 1)),
             color="blue", linetype="dashed", size=1) +
  labs(y="Frequency", x="Review Age", title="Histogram - Age of Reviews") +
  theme(plot.title = element_text(hjust = 0.5))



##business_hrs Exploratory Analysis

str(business_hrs)
summary(business_hrs)

#Average hours of operation day wise

hrs_avg <- sqldf("SELECT avg(monday) AS Monday
                  ,avg(tuesday) AS Tuesday
                  ,avg(wednesday) AS Wednesday
                  ,avg(thursday) AS Thursday
                  ,avg(friday) AS Friday
                  ,avg(saturday) AS Saturday
                  ,avg(sunday) AS Sunday
                  FROM business_hrs bh
                  INNER JOIN business b on bh.business_id = b.business_id")

hrs_avg <- melt(hrs_avg)
hrs_avg$value <- round(hrs_avg$value,2)


ggplot(data=hrs_avg, aes(x=variable,y=value,fill=value,label = value)) +
  geom_col() + geom_text(nudge_y = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(x="Day",y="Hours", title="Avg Operational Hours by Day")+
  theme(plot.title = element_text(hjust = 0.5))

#Assigning dummy code to consider if the restaurant is open on Weekends and Sundays

business_hrs <- sqldf("SELECT b.business_id
                       ,monday
                       ,tuesday
                       ,wednesday
                       ,thursday
                       ,friday
                       ,saturday
                       ,sunday
                       ,CASE WHEN sunday > 0
                             THEN 1
                             ELSE 0
                       END is_sunday
                       ,CASE WHEN sunday > 0 AND saturday > 0
                             THEN 1
                             ELSE 0
                       END is_weekend
                       ,CASE WHEN monday > 0 AND tuesday > 0 AND wednesday > 0 AND thursday > 0 AND friday > 0
                             THEN 1
                             ELSE 0
                       END is_weekday
                       FROM business_hrs bh
                       INNER JOIN business b on bh.business_id = b.business_id")


##-------------------------------------------------------------------------------
# 4. Data Consolidation
##-------------------------------------------------------------------------------

str(business) 
str(review)
str(business_hrs)

business_final <- sqldf("select b.business_id,postal_code,b.postal_res,r.review_count,r.Stars,r.review_age_wks,r.review_per_wk,reactions_per_wk,reactions
                        ,monday+tuesday+wednesday+thursday+friday+saturday+sunday AS total_open_hrs
                        ,is_weekend,is_weekday,is_sunday,is_open
                        FROM business b
                        INNER JOIN review r ON b.business_id = r.business_id
                        INNER JOIN business_hrs bh ON b.business_id = bh.business_id")

#Aggregating average reactions by Postal Code

postal_reac <- sqldf("select postal_code
                    ,avg(reactions) AS postal_reac
                    FROM business_final
                    GROUP BY postal_code
                    ORDER BY 2 DESC")

#Final Consolidation

business_final <- sqldf("select postal_res,review_count,Stars,review_age_wks,review_per_wk,reactions_per_wk,reactions,postal_reac
                        ,total_open_hrs,population,is_weekend,is_weekday,is_sunday,is_open
                        FROM business_final b
                        INNER JOIN postal_reac p ON b.postal_code = p.postal_code
                        INNER JOIN pop po ON po.zip_code = b.postal_code")

str(business_final)
business_final <- as.data.frame(sapply( business_final, as.integer ))


##correlation/collinearity analysis

corMatrix <- cor(business_final)

corrplot(corMatrix, method="square", col = brewer.pal(n=8, name="RdYlBu"),  
         type="lower", order="hclust", number.font = 3,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         diag=TRUE, sig.level = 0.05, insig = "blank")


# Density Plot
business_final %>% 
  select(-c(is_open,is_weekday,is_weekend,is_sunday)) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free") +
  ggtitle("Density Plot of Features") +
  theme(plot.title = element_text(hjust = 0.5))



#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
rm(business)
rm(business_cat)
rm(business_city)
rm(business_hrs)
rm(business_hrs_raw)
rm(business_star)
rm(categories)
rm(pop)
rm(postal_res)
rm(us)
rm(corMatrix)
rm(hrs_avg)
rm(review)
rm(review_cnt)
rm(review_reactions)
rm(review_star)
rm(postal_reac)

##-------------------------------------------------------------------------------
# 5. Model Preperation
##-------------------------------------------------------------------------------

#Spliting data set into training and validation

set.seed(123)
spec <- c(train = .7, validation = .3)

g <- sample(cut(
  seq(nrow(business_final)), 
  nrow(business_final)*cumsum(c(0,spec)),
  labels = names(spec)
))

set.seed(123)
res <- split(business_final, g)
train_set <- res$train
val_set <- res$validation

ctrl <- trainControl(method = "cv", number = 10)

#Normalization

train_set[, 1:13] <- lapply(train_set[, 1:13], function(x) (x-min(x))/(max(x)-min(x)))
train_set_fac <- train_set
train_set_fac$is_open <- as.factor(train_set_fac$is_open)

val_set[, 1:13] <- lapply(val_set[, 1:13], function(x) (x-min(x))/(max(x)-min(x)))
val_set_fac <- val_set
val_set_fac$is_open <- as.factor(val_set_fac$is_open)

#Implementation of Principal Component Analysis

prin_comp <- prcomp(train_set[, 1:13], scale. = T)
names(prin_comp)

#loadings
prin_comp$rotation

std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


train_set_pca <- data.frame(is_open = train_set$is_open,prin_comp$x)

#we are interested in first 6 PCA explaining more than 90% variance

train_set_pca  <- train_set_pca [,1:7]


val_set_pca <- predict(prin_comp, newdata = val_set[,1:13])
val_set_pca <- as.data.frame(val_set_pca)
val_set_pca <- val_set_pca[,1:7]


##-------------------------------------------------------------------------------
# 5. Model Construction
##-------------------------------------------------------------------------------


##Logistic Regression
#Train Accuracy: 73.72%
#Test Accuracy: 73.36%
#AUC: 0.71

log_model_raw <- glm(is_open~., data=train_set,  family=binomial )
summary(log_model_raw)

#Removing is_sunday
log_model <- glm(is_open~postal_res+review_count+Stars+review_age_wks+review_per_wk+reactions_per_wk+reactions+
                   postal_reac+total_open_hrs+population+is_weekend+is_weekday, data=train_set,  family=binomial )
summary(log_model)

#Removing postal_res
log_model <- glm(is_open~review_count+Stars+review_age_wks+review_per_wk+reactions_per_wk+reactions+
                   postal_reac+total_open_hrs+population+is_weekend+is_weekday, data=train_set,  family=binomial )
summary(log_model)

#Removing is_weekday
log_model <- glm(is_open~review_count+Stars+review_age_wks+review_per_wk+reactions_per_wk+reactions+
                   postal_reac+total_open_hrs+population+is_weekend, data=train_set,  family=binomial )
summary(log_model)

#Removing is_weekend
log_model <- glm(is_open~review_count+Stars+review_age_wks+review_per_wk+reactions_per_wk+reactions+
                   postal_reac+total_open_hrs+population, data=train_set,  family=binomial )
summary(log_model)

#Removing review_per_wk
log_model <- glm(is_open~review_count+Stars+review_age_wks+reactions_per_wk+reactions+
                   postal_reac+total_open_hrs+population, data=train_set,  family=binomial)
summary(log_model)

#Train Accuracy
pred <- predict(log_model,train_set,type = "response")
pred <- ifelse(pred > 0.5,1,0)

caret::confusionMatrix(data = as.factor(pred), reference = as.factor(train_set$is_open))

#Validation Accuracy
pred <- predict(log_model,val_set,type = "response")
pred <- ifelse(pred > 0.5,1,0)


caret::confusionMatrix(data = as.factor(pred), reference = as.factor(val_set$is_open))

#ROC
ROCRpred <- prediction(pred, val_set$is_open)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#AUC
#We can label this a an fairly good classisfier but it doing a good job in the classification with AUC of 0.71 
#which is significanlty higher than the other models
auc(log_model)


#With PCA
#Accuracy: 71.71%

log_model_pca_raw <- glm(is_open~., data=train_set_pca,  family=binomial)
summary(log_model_pca_raw)

#Removing PC2
log_model_pca <- glm(is_open~PC1+PC3+PC4+PC5+PC6, data=train_set_pca,  family=binomial)
summary(log_model_pca)

#Removing PC5
log_model_pca <- glm(is_open~PC1+PC3+PC4+PC6, data=train_set_pca,  family=binomial)
summary(log_model_pca)

pred_pca <- predict(log_model_pca_raw,val_set_pca)
pred_pca <- ifelse(pred_pca > 0.5,1,0)

caret::confusionMatrix(data = as.factor(pred_pca), reference = as.factor(val_set$is_open))

#ROC
ROCRpred_pca <- prediction(pred_pca, val_set$is_open)
ROCRperf_pca <- performance(ROCRpred_pca, 'tpr','fpr')
plot(ROCRperf_pca, colorize = TRUE, text.adj = c(-0.2,1.7))

#AUC
auc(log_model_pca)




##Random Forest
#Accuracy: 75.41%
#AUC: 0.59

#Here we will be tuning the model by varying the value mtry. The value for trees (ntree) of 50 and 500 did not give a 
#very big differnce in AUC and Accuracy, so 50 has been taken for computional ease.

set.seed(123)

accuracy <- 0
for (i in 1:6) {
  model <- randomForest(is_open ~ ., data = train_set, ntree = 50, mtry = i, importance = TRUE)
  predValid <- predict(model, val_set, type = "response")
  predValid <- ifelse(predValid > 0.5,1,0)
  accuracy_test = mean(predValid == val_set$is_open)
  if (accuracy_test>accuracy) {
    accuracy <- accuracy_test 
    optimal_mtry <- i
  }
}

set.seed(123)
rfmodel <- randomForest(is_open ~ ., data = train_set, ntree = 50, mtry = optimal_mtry, importance = TRUE)
rfmodel

predValid <- predict(rfmodel, val_set, type = "response")
predValid <- ifelse(predValid > 0.5,1,0)

caret::confusionMatrix(data = as.factor(predValid), reference = as.factor(val_set$is_open))

#ROC
roc_rf <- roc(val_set$is_open,predValid)
plot(roc_rf)

#AUC
#We can label this a an excellent classisfier but it doing a fair job in the classification with AUC of 0.59
roc_rf


#With PCA, optimal mtry was 2 for the pca data set as well
#Accuracy: 72.74%

set.seed(123)
rfmodel_pca <- randomForest(is_open ~ ., data = train_set_pca, ntree = 50, mtry = optimal_mtry, importance = TRUE)
rfmodel_pca

predValid_pca <- predict(rfmodel_pca, val_set_pca, type = "response")
predValid_pca <- ifelse(predValid_pca > 0.5,1,0)

caret::confusionMatrix(data = as.factor(predValid_pca), reference = as.factor(val_set$is_open))

#ROC
roc_rf_pca <- roc(val_set$is_open,predValid_pca)
plot(roc_rf_pca)



#SVM
#Accuracy: rbfdot -> 55.71%
#Accuracy: polydot -> 39.18%
#Accuracy: laplacedot -> 70.89% (AUC: 0.61)

#Here we will be tuning the model by varying the kernels
#By varying the value of cost of constraints violation, there was very little difference in the training error. 
#For laplacedot, C values of 10 and 10000, even though the training error reduced  with higher C value it was only 0.000053.

set.seed(123)
sv_rf <- ksvm(is_open ~ ., data = train_set,kernel = "rbfdot",type="nu-svc")

pred_svm_rf <- predict(sv_rf, val_set)
caret::confusionMatrix(data = as.factor(pred_svm_rf), reference = as.factor(val_set$is_open))


sv_py <- ksvm(is_open ~ ., data = train_set,kernel = "polydot",type="nu-svc")

pred_svm_py <- predict(sv_py, val_set)
caret::confusionMatrix(data = as.factor(pred_svm_py), reference = as.factor(val_set$is_open))


sv_ld <- ksvm(is_open ~ ., data = train_set,kernel = "laplacedot",type="nu-svc",prob.model = TRUE)

pred_svm_ld <- predict(sv_ld, val_set,type = "prob")
pred_svm_ld <- ifelse(pred_svm_ld[,2] > 0.5,1,0)
caret::confusionMatrix(data = as.factor(pred_svm_ld), reference = as.factor(val_set$is_open))


#ROC
#We can label this a an excellent classisfier but it doing a fair job in the classification with AUC of 0.56
roc_svm <- roc(val_set$is_open,pred_svm_ld)
plot(roc_svm)

#AUC
roc_svm


#With PCA
#Accuracy: rbfdot -> 54.06%
#Accuracy: polydot -> 45.11%
#Accuracy: laplacedot -> 69.71%


sv_rf_pca <- ksvm(is_open ~ ., data = train_set_pca,kernel = "rbfdot",type="nu-svc")

pred_svm_rf_pca <- predict(sv_rf_pca, val_set_pca)
caret::confusionMatrix(data = as.factor(pred_svm_rf_pca), reference = as.factor(val_set$is_open))


sv_py_pca <- ksvm(is_open ~ ., data = train_set_pca,kernel = "polydot",type="nu-svc")

pred_svm_py_pca <- predict(sv_py_pca, val_set_pca)
caret::confusionMatrix(data = as.factor(pred_svm_py_pca), reference = as.factor(val_set$is_open))


sv_ld_pca <- ksvm(is_open ~ ., data = train_set_pca,kernel = "laplacedot",type="nu-svc")

pred_svm_ld_pca <- predict(sv_ld_pca, val_set_pca)
caret::confusionMatrix(data = as.factor(pred_svm_ld_pca), reference = as.factor(val_set$is_open))

#Bagging on SVM was not performed as it was taking very long to process
# set.seed(123)
# bagctrl <- bagControl(fit = svmBag$fit,predict = svmBag$pred, aggregate = svmBag$aggregate)
# svmbag <- caret::train(is_open ~ ., data = business_final, "bag", trControl = ctrl, bagControl = bagctrl)



#The 3 models i.e. SVM, Random Forest and Logistic Regression have been implemented with and without PCA and taken into account 
#6 principal compenents (thereby having dimensionality reduction). But we see that using PCA has not helped our models,
#all the implementation had lower accuracy (not a big margin). The accuracy along with AUC values are:

##Logistic Regression
#Accuracy: 73.36%
#AUC: 0.71

##Random Forest
#Accuracy: 75.41%
#AUC: 0.59

##SVM (laplacedot)
#Accuracy: 70.89%
#AUC: 0.61

#Ideally we would be picking Random Forest model as our final model, but we will combining all the three models
#on a weighted average basis into an ensemble model and evaluating it.


#Implementation of Bagging
#Train Accuracy: 74.4%

caret::train(as.factor(is_open) ~ ., data = business_final, method = "treebag",trControl = ctrl)


#Implmentation of Boosting
#Train Accuracy: 76.1%
#Validation Accuracy: 75.19%

business_final_fac <- business_final
business_final_fac$is_open <- as.factor(business_final_fac$is_open)
adaboost <- adabag::boosting(is_open ~ ., data = train_set_fac)

pred_adaboost <- predict(adaboost, val_set_fac)
pred_adaboost <- ifelse(pred_adaboost$prob[,2] > 0.5,1,0)
caret::confusionMatrix(data = as.factor(pred_adaboost), reference = as.factor(val_set_fac$is_open))


#Ensemble Model
#Accuracy: 64.43%
#AUC: 0.62

#Assigning weights to the Logistic, Random Forest and SVM as 0.15,0.6 and 0.25 respectively
train_set$pred_lr_prob <- predict(log_model,train_set,type = "response")*0.15
train_set$pred_rf_prob <- predict(rfmodel, train_set,type = "response")*0.6
train_set$pred_svm_prob <- predict(sv_ld, train_set,type = "prob")[,2]*0.25


val_set$pred_lr_prob <- predict(log_model,val_set,type = "response")*0.15
val_set$pred_rf_prob <- predict(rfmodel, val_set,type = "response")*0.6
val_set$pred_svm_prob <- predict(sv_ld, val_set,type = "prob")[,2]*0.25


train_set$open <- ifelse(train_set$is_open > 0,'Y','N')
val_set$open <- ifelse(val_set$is_open > 0,'Y','N')

#Top Layer as Bagged AdaBoost
model_bag <- caret::train(train_set[,15:17],train_set$open,method='AdaBag',trControl=ctrl,tuneLength=3)

#Predict using Bagged AdaBoost top layer model
bag_stacked <- predict(model_bag,val_set[,15:17])
caret::confusionMatrix(bag_stacked, as.factor(val_set[,18]),positive='Y')

#ROC
roc_stacked <- roc(val_set$is_open,ifelse(bag_stacked != 'Y',0,1))
plot(roc_stacked) 

#AUC
roc_stacked