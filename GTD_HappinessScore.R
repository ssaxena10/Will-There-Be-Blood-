#library 
install.packages("corrplot")
install.packages("leaflet")
install.packages("rworldmap")
install.packages("knitr")
install.packages("kableExtra")
install.packages("formattable")
install.packages("dplyr")
install.packages("tm")
install.packages("corpus")
install.packages("tidytext")
install.packages("tidyr")
install.packages("wordcloud")
install.packages("olsrr")
install.packages('randomForest')
install.packages('rsq')
install.packages('leaps')
library(leaps)
library(randomForest)
library(Metrics)
library(rsq)
library(rworldmap)
library(ggplot2)
library(leaflet)
library(treemap)
library(corrplot)
library(tm)
library(corpus)
library(tidytext)
library(tidyr)
library(wordcloud)
library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)
library(olsrr)
library(caret)
library(pROC)



#load dataset
terrorism_data_old <- read.csv(file="/Users/aashka/Desktop/Sem4/DPA/project/globalterrorism_hs_db.csv",
                               header=TRUE, sep=",", na.strings = "",fileEncoding="latin1")

terrorism_data <- read.csv(file="/Users/aashka/Desktop/Sem4/DPA/project/globalterrorism_hs_db_final.csv",
                           header=TRUE, sep=",", na.strings = "")



terrorism_data$number_kill[terrorism_data$number_kill == -9 || terrorism_data$number_kill == -99] <- as.integer("0")
terrorism_data$number_kill[is.na(terrorism_data$number_kill)] <- as.integer("0")
terrorism_data$ransomamt[is.na(terrorism_data$ransomamt) || terrorism_data$ransomamt == -9 || terrorism_data$ransomamt == -99] <- as.integer("0")

#box plots
ggplot(terrorism_data, mapping = aes(country_name,year)) + geom_boxplot(fill="blue", color="yellow", outlier.color="red", stat = "boxplot") + labs(y = "Year")
ggplot(terrorism_data, mapping = aes(x=number_kill,y=year,group =1)) + geom_boxplot(fill="blue", color="yellow", outlier.color="red", stat = "boxplot") + labs(y = "Year")
ggplot(terrorism_data, mapping = aes(x=ransomamt,y=year, group= 2)) + geom_boxplot(fill="blue", color="yellow", outlier.color="red", stat = "boxplot") + labs(y = "Year")

#Plotting the distributions
#year
x <- terrorism_data$year
x <- na.omit(x)
plot(density(x), col = "red", xlab = "Year", main = "Distribution of the number of attacks per year")

#number of kills
x <- terrorism_data$number_kill
x <- na.omit(x)
x <- x[!x %in% c(-99, -9, 0)]
#rem <- c(-99, -9, 0)
#x <- setdiff(x, rem)
d <- density(x, from = min(x), to = max(x))
plot(d, col = "red", xlab = "Number of kills", main = "Distribution of the number of kills")

#ransom amount
x <- terrorism_data$ransomamt
x <- na.omit(x)
x <- x[!x %in% c(-99.00, -9.00, 0.00)]
d <- density(x, from = min(x), to = max(x))
plot(d, col = "red", xlab = "Ransom", main = "Distribution of the ransom asked")

#Happiness score
x <- terrorism_data$Happiness_Score
x <- na.omit(x)
d <- density(x, from = min(x), to = max(x))
plot(d, col = "red", xlab = "Happiness Score", main = "Distribution of the Happiness Score")
max(x)



#correlation matrix
#"iyear","imonth","iday","nkill","ransompaid","ransompaidus","nhostkidus","nhours","ndays","Happiness_Rank","Happiness_Score","Dystopia.Residual
terrorCor <- terrorism_data_old[,c("iyear","imonth","iday","nkill","ransomamt","ransomamtus","ransompaid","ransompaidus","nhostkid","nhours","ndays","Happiness_Rank","Happiness_Score","Dystopia.Residual")]
terrorCor <- na.omit(terrorCor)
correlations <- cor(terrorCor)
p <- corrplot(correlations, method="circle")


#summary
summary(terrorism_data)

#World Map
leaflet(data = terrorism_data) %>%
  addTiles() %>%
  addMarkers(lat=terrorism_data$latitude, lng=terrorism_data$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", terrorism_data$iday,"/",terrorism_data$imonth,"/", terrorism_data$iyear,
                          "<br><br><strong>Place: </strong>", terrorism_data$city,"-",terrorism_data$country_name,
                          "<br><strong>Killed: </strong>", terrorism_data$nkill,
                          "<br><strong>Wounded: </strong>", terrorism_data$nwound
             ))

#Killings per year per country
terrorism_data %>% group_by(year,region_name) %>% summarise(nkills = sum(number_kill)) %>% ungroup() -> dfyr

colnames(dfyr)<-c("Year","Region","Killed")
ggplot(data = dfyr, aes(x = Year, y = Killed, colour = Region)) +       
  geom_line() + geom_point() + theme_bw()

View(dfyr)
min(dfyr$Killed, na.rm = TRUE)

#Common types of attack
terrorism_data %>% group_by(year,attackmode_desc) %>% summarise(n = length(year)) %>% ungroup() -> dfya

colnames(dfya)<-c("Year","Type of attack","Number of events")
ggplot(data = dfya, aes(x = Year, y = `Number of events`, colour = `Type of attack`)) + 
  geom_line() + geom_point() + theme_bw()

max(dfya$`Number of events`, na.rm = TRUE)
View(dfya)


#Groups with maximum killings

terrorism_data %>% group_by(group_name) %>% summarise(nkills = sum(number_kill)) %>% top_n(n=10) %>% ungroup() -> dfg
dfg %>% filter(group_name != "Unknown") -> fGroup
ggplot(data = fGroup, aes(x = reorder(group_name,nkills), y = nkills)) +  
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8)  +
  labs(title="", x ="Group", y = "Killed")

#Common Weapons
terrorism_data %>% group_by(weaptype_desc) %>% summarise(nweapons = length(weaptype_desc)) %>% ungroup() -> dfw

ggplot(data = dfw, aes(x = reorder(weaptype_desc,nweapons), y = nweapons)) +  
  geom_bar(stat="identity", fill="violet", colour="black") +
  coord_flip() + theme_bw(base_size = 8)  +
  labs(title="", x ="Type of weapon used", y = "Number of events")

#Common Motives
terrorism_data %>% filter(!is.na(motive)) -> dfm0
dfm0 %>% filter(motive != "") -> dfm
text <- sample(dfm$motive, nrow(dfm)/50)
specificWords <- c("The", "Unknown", "attack", "specific", "motive", "sources", "unknown", "claimed", "targeted",
                   "carried", "noted", "incident", "stated", "responsibility", "the")

text<-sapply(text, function(x) gsub("\n"," ",x))
myCorpus<-VCorpus(VectorSource(text))
myCorpusClean <- myCorpus %>% 
  
tm_map(content_transformer(removeNumbers)) %>% 
tm_map(content_transformer(removePunctuation)) %>%
tm_map(content_transformer(removeWords),tidytext::stop_words$word) %>%
tm_map(content_transformer(removeWords),specificWords)

myDtm = TermDocumentMatrix(myCorpusClean,
                           control = list(minWordLength = 3))

freqTerms <- findFreqTerms(myDtm, lowfreq=1)

m <- as.matrix(myDtm)
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wctop <-wordcloud(d$word, d$freq,scale = c(3.6,0.5), rot.per=0.20, min.freq=5, colors=brewer.pal(9,"Set1"))

#Correlation of Happiness Score with its factors
terrorism_data %>% group_by(country_name,Happiness_Score,Economy_GDP_per_Capita.,Family,Health.Life.Expectancy.,Freedom,Trust.Government.Corruption.,Generosity) %>% summarise(Terror_attack = length(eventid)) %>% ungroup() -> hcor
hcor <- na.omit(hcor)
hcor <- hcor[,-1]
happinessCorrelations <- cor(hcor)
happy <- corrplot(happinessCorrelations, method="square")

#Correlation on the world map
happiness <- terrorism_data[,c("country_name", "Happiness_Score")]
happiness_df <- unique(happiness)
target_citizen <- subset(terrorism_data,targsubtype==75|targsubtype==67)
#target_citizen<- terrorism_data[terrorism_data$targsubtype==75 || terrorism_data$targsubtype==67,]


q<-map_data("world")
colnames(q)[5] <- "country_name"
df<- left_join(q,happiness_df)
ggplot()  +
  geom_polygon( aes(x = df$long, y = df$lat, group = df$group,fill= df$Happiness_Score)) + 
  coord_equal() +scale_fill_gradient(breaks=c(3,5,7,9)) +
  geom_point(aes(target_citizen$longitude,target_citizen$latitude,shape="."))+
  ggtitle("Happiness score vs Number of terror attacks on civilians (2017)")+
  xlab("") + ylab("") + guides(shape=FALSE) + labs(fill="Happiness Score")

#variable selection backwards
terrorism_model = lm(Happiness_Score ~ attackmode+targettype+targsubtype+weaptype+number_kill+property_value+Economy_GDP_per_Capita.+Family+Health.Life.Expectancy.+Freedom+Generosity+Trust.Government.Corruption., data = terrorism_data)
ols_step_backward_p(terrorism_model)

#variable forward slection

ols_step_backward_p(terrorism_model)

#Mallows Cp
cols_yo <- c(13, 15:18, 20, 23, 27, 30, 31, 33, 37)
#data = mutate_each(data, funs(as.factor), cols_yo)
for(i in cols_yo){
  class(terrorism_data[ ,i]) = "factor"
}
sapply(terrorism_data, class)
regsubsets.out <-
  regsubsets(Happiness_Score ~ attackmode +  targettype + weaptype + number_kill+property_value+Economy_GDP_per_Capita.+Family+Health.Life.Expectancy.+Freedom+Generosity+Trust.Government.Corruption.,
             data = terrorism_data,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary(regsubsets.out)
par(cex.axis=0.5)
plot(regsubsets.out, scale = "Cp")
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
#install.packages("car")
library(car)
layout(matrix(1:2, ncol = 2))
res.legend <-
  subsets(regsubsets.out, statistic="cp", legend = FALSE, min.size = 5, main = "Mallow Cp")

#Plotting the distribution
attach(terrorism_data)

#Sampling for Future Attack
terrorism_FA<- terrorism_data[!(is.na(terrorism_data$Happiness_Rank) | terrorism_data$Happiness_Rank==""), ]


terrorism_FA$FutureAttack <- (terrorism_FA$Happiness_Rank>100) 
terrorism_FA$FutureAttack=factor(terrorism_FA$FutureAttack)


trainIndexTerr <- createDataPartition(terrorism_FA$FutureAttack, p = .8, list = FALSE, times = 1)
trainingTerr <- terrorism_FA[trainIndexTerr,]
testingTerr <- terrorism_FA[-trainIndexTerr,]
ctrlTerr <- trainControl(method = "repeatedcv",
                     repeats = 5)

#LDA
lda1_cv_terr <- train(FutureAttack~attackmode+Economy_GDP_per_Capita.+Family+Health.Life.Expectancy.+Freedom+Generosity+Trust.Government.Corruption.+Trust.Government.Corruption., data = terrorism_FA, method = "lda",
                     trControl = ctrlTerr)
ldapredTestTerror <- predict(lda1_cv_terr,type="raw", newdata = testingTerr)

#lm1_cv_terr <- train(as.numeric(FutureAttack)~attackmode+Economy_GDP_per_Capita.+Family+Health.Life.Expectancy.+Freedom+Generosity+Trust.Government.Corruption.+Trust.Government.Corruption., data = terrorism_FA, method = "lm",
#                     trControl = ctrlTerr)
#lmpredTestTerror <- predict(lm1_cv_terr,type="raw", newdata = testingTerr)
confusionMatrix(data=ldapredTestTerror, testingTerr$FutureAttack)
lmpredTestTerror_i <- as.integer(ldapredTestTerror)
rocPred_lda <- roc(FutureAttack~ldapredTestTerror_i, data= testingTerr)
plot(rocPred_lda,col="red")
rocPred_lda
lda1_cv_terr


#Logistic Regression
glm1_cv_terr <- train(FutureAttack~attackmode+Economy_GDP_per_Capita.+Family+Health.Life.Expectancy.+Freedom+Generosity+Trust.Government.Corruption.+Trust.Government.Corruption., data = terrorism_FA, method = "glm",
                 trControl = ctrlTerr)

predTestTerror <- predict(glm1_cv_terr,type="raw", newdata = testingTerr)
confusionMatrix(data=predTestTerror, testingTerr$FutureAttack)
predTestTerror_i <- as.integer(predTestTerror)
rocPred_glm <- roc(FutureAttack~predTestTerror_i, data= testingTerr)
plot(rocPred_glm,col="red")
rocPred_glm
# Random forest algm Regression
RF_Terr_fit <- randomForest(FutureAttack ~ attackmode+Economy_GDP_per_Capita.+Family+Health.Life.Expectancy.+Freedom+Generosity+Trust.Government.Corruption.+Trust.Government.Corruption.,
                    data=trainingTerr, 
                    importance=TRUE, 
                    ntree=200)

varImpPlot(RF_Terr_fit)
predTestTerror_RF <- predict(RF_Terr_fit, testingTerr)
confusionMatrix(data=predTestTerror_RF, testingTerr$FutureAttack)
predTestTerrorRF_i <- as.integer(predTestTerror_RF)
rocPred_RF <- roc(FutureAttack~predTestTerrorRF_i, data= testingTerr)
plot(rocPred_RF,col="blue")
rocPred_RF

#Sampling for for casualties
R2<- function(y,model){
  R2<- 1-(model$deviance/model$null.deviance)
  return(R2)
}

terrorism_C<- terrorism_data

trainIndexTerr_C <- createDataPartition(terrorism_C$number_kill, p = .8, list = FALSE, times = 1)
trainingTerr_C <- terrorism_C[trainIndexTerr_C,]
testingTerr_C <- terrorism_C[-trainIndexTerr_C,]
ctrlTerr_C <- trainControl(method = "repeatedcv",
                         repeats = 5)
#Linear regression for casualties
lm1_cv <- train(number_kill~success+multiple_attack+attackmode+targettype+weaptype, data = trainingTerr_C, method = "lm",
                trControl = ctrlTerr_C,na.action = na.pass)
lm1_cv$results
lm_resample <- lm1_cv$resample
predTestTerror_lm_C <- predict(lm1_cv, testingTerr_C)
rmse(testingTerr_C$number_kill,predTestTerror_lm_C)
R2_C <- R2(testingTerr_C$number_kill,lm1_cv)
rocPred_C <- roc(number_kill~predTestTerror_lm_C, data= testingTerr_C)
plot(rocPred_C,col="green")
rocPred_C$auc

#Logistic Regression for casualties
glm1_cv_C <- train(number_kill~success+multiple_attack+attackmode+targettype+weaptype, data = trainingTerr_C, method = "glm",
                      trControl = ctrlTerr_C, na.action = na.pass)
glm1_summary_c <- summary(glm1_cv_C)
predTestTerror_C <- predict(glm1_cv_C,type="raw", newdata = testingTerr_C)
rmse(testingTerr_C$number_kill,predTestTerror_C)

R2_C <- 1-(glm1_summary_c$deviance/glm1_summary_c$null.deviance)
rocPred_C_glm <- roc(number_kill~predTestTerror_C, data= testingTerr_C)
plot(rocPred_C_glm,col="blue")
rocPred_C$auc


# Random forest algm Regression for casualties
RF_Terr_fit_C <- randomForest(number_kill~success+multiple_attack+attackmode+targettype+weaptype,
                            data=trainingTerr_C, 
                            importance=TRUE, 
                            ntree=50)

varImpPlot(RF_Terr_fit_C)
predTestTerror_RF_C <- predict(RF_Terr_fit_C, testingTerr_C)
rmse(testingTerr_C$number_kill,predTestTerror_RF_C)
R2_C <- R2(testingTerr_C$number_kill,RF_Terr_fit_C)
rocPred_C_RF <- roc(number_kill~predTestTerror_RF_C, data= testingTerr_C)
plot(rocPred_C,col="red")
rocPred_C$auc

