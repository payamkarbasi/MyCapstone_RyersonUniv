library(lubridate)  #To change the Timestamps
library(dplyr)      #To use group_by functions
library(gmp)        #To show big integers using as.bigq(int)
library(stringi)    #To remove NAs and shift cells to the left
library(reshape2)   #To reshape DF by dcast
#library(tidyr)     #To reshape DF by spread
library(data.table) #To add rownames into the DF as of the first column
library(clickstream)#The picked package for handling clickstreams
library(stringr)    #To count a char like "," in a String of Characters, or to run gsub()
library(fpc)        #To determine how many Clusters needed for converting Numeric to Categorical, pamk()
library(cluster)    #For Clustering Numeric to Categorical, pam()
library(ggplot2)    #For demonstrating date set

#install.packages("pander")
library(pander)     #Better Univariate/Bivariate 

#install.packages("roperators")
library(roperators) #To remove a char from a String by command: %-% using regular expression
detach("package:dplyr", character.only = TRUE)
library("dplyr", character.only = TRUE)
#install.packages("ggcorrplot")
library(ggcorrplot) #For vitualization

#install.packages("sqldf")
library(sqldf)      #For executing SQL queries in R

#install.packages("StatMatch")
library(StatMatch)  #For reviewing Clusters and finding the best "K" in different Clustering Algorithms

#############################################################################
###                                                                       ###
###                       1. Importing Data File                          ###
###                                                                       ###
#############################################################################
rfile <- function(path1){
  clicksF <- read.csv(path1, header=T, sep=",")
  print(str(clicksF))
  print(head(clicksF))
  clicksF$session_start <-as_datetime(clicksF$session_start/1000)
  clicksF$click_timestamp <-as_datetime(clicksF$click_timestamp/1000)
  summary(clicksF)
  return(clicksF)
}

#############################################################################
###                                                                       ###
###                       2. Data Set Review                              ###
###                                                                       ###
#############################################################################
descriptiveStats <- function(df){

  df %>%
    group_by(user_id, session_id) %>%
    summarize(frequency = n()) %>%
    arrange(desc(frequency)) %>%
    head() %>%
    pander
  
  df %>%
    group_by(click_article_id) %>%
    summarize(frequency = n()) %>%
    arrange(desc(frequency)) %>%
    head() %>%
    pander
  
  #####A useless plot showing Bivariate relations one-by-one between ARTs and SESs
  df %>%
    ggplot(aes(session_id, click_article_id)) +
    geom_point() +
    theme_minimal() +
    labs(title = "Relationship between sessions and Articles") +
    geom_smooth(method = "lm", se = F)
  
  #####A Plot showing there are no Correlations between variables
  df %>%
    select_if(is.numeric) %>%
    cor %>% 
    ggcorrplot()
  
  #####To show Histogram of Number of Clicks per Article Distribution
  clicksTemp <- sqldf("select count(click_article_id) num_of_Art, click_article_id from df group by click_article_id")
  clicksF <- merge(clicks,clicksTemp,by.x = "click_article_id", by.y="click_article_id", all.x=T)
  clicksTemp1 <- sqldf("select num_of_Art, count(num_of_Art) freq_of_Art from clicksF group by num_of_Art")
  clicksAF <- merge(clicksF,clicksTemp1,by.x = "num_of_Art", by.y="num_of_Art", all.x=T)
  hist(clicksAF$num_of_Art, xlab = "Number of clicks per Article", ylab="Frequency", col="Orange", breaks = 20, main="The Frequency of Number of Clicks per Distinct Article", labels = T)
  rm("clicksTemp")
  rm("clicksF")
  rm("clicksTemp1")
  
  
  #####To show Histogram of Number of Clicks per Session Distribution
  clicksTempS <- sqldf("select count(session_id) num_of_Clicks_per_Session, session_id from df group by session_id")
  clicksSF <- merge(clicks,clicksTempS,by.x = "session_id", by.y="session_id", all.x=T)
  clicksTemp2 <- sqldf("select num_of_Clicks_per_Session, count(num_of_Clicks_per_Session) freq_of_Sessions from clicksSF group by num_of_Clicks_per_Session")
  clicksSSF <- merge(clicksSF,clicksTemp2,by.x = "num_of_Clicks_per_Session", by.y="num_of_Clicks_per_Session", all.x=T)
  hist(clicksSSF$num_of_Clicks_per_Session, xlab = "Number of df per Session", ylab="Frequency", col="Green", breaks=20, main="The Frequency of Number of Clicks per Distinct Session", labels = T)
  rm("clicksTempS")
  rm("clicksSF")
  rm("clicksTemp2")
  
  return(clicksAF)
  #cor(clicksSSF$num_of_Clicks_per_Session, clicksSSF$session_start)
}

#############################################################################
###                                                                       ###
###                             3. Clustering                             ###
###                                                                       ###
#############################################################################

cluster_arts <- function(clicks_ArtNums){

  ### Reviewing simple K-Means on my Dataset and finding the best "K" >>> fining 2!
  fviz_nbclust(clicks_SID_ARTID,FUNcluster = kmeans, method=c("silhouette", "wss", "gap_stat"), k.max = 12)
  
  ### Reviewing simple K-Means on my Dataset and finding the best "K" >>> fining 2!  
  fviz_nbclust(clicks_SID_ARTID,FUNcluster = pam, method=c("silhouette", "wss", "gap_stat"), k.max = 12)
  
  ###Counting Articles' Frequency and reviewing the best "K" through simple K-Means >>> finding 6!
  fviz_nbclust(clicks_ArtNums[,1:2],FUNcluster = kmeans, method=c("silhouette", "wss", "gap_stat"), k.max = 12)
  kmeans6 <- kmeans(clicks_ArtNums[,1:2], centers = 6, nstart = 25)
  fviz_cluster(kmeans6, data = clicks_ArtNums[,1:2])
  
  ###Counting Articles' Frequency and reviewing the best "K" through PAM >>> fining 25 which is not feasible anymore!
  fviz_nbclust(clicks_ArtNums[,1:2],FUNcluster = pam, method=c("silhouette", "wss", "gap_stat"), k.max = 25)
  
  ###So the PAMK function confirms and clusters data based on 25 clusters:
  PAMK = pamk(clicks_ArtNums[,1:2], krange = 2:25, metric=c("euclidean", "manhattan"))
  PAMK$nc
  
  
  
}
  
#############################################################################
###                                                                       ###
###         4. EDA (Cleaning 1 Articles+ 1 click/session)                 ###
###                                                                       ###
#############################################################################
eda <- function(clicksAF){

  #ClicksMinus1Art <- clicksAF[!(clicksAF$num_of_Art=="1"),]
  #ClicksMinus1Art <- clicksAF
  clicksAF$session_id <- as.character(clicksAF$session_id)
  clicks_1ArtRemoved <- sqldf("select user_id, session_id, click_article_id from clicksAF", verbose=T)
  
  by_1art <- reshape2::dcast(clicks_1ArtRemoved, user_id ~ session_id , fun.aggregate = toString ,na.rm=TRUE, value.var = "click_article_id")
  toStr_1art <- do.call(paste, c(by_1art,sep=","))
  myCLS_1art <- gsub(",+", ",", toStr_1art)
  myCLS_1art <- gsub(",$", "", myCLS_1art)
  myCLS_1art <- str_remove_all(myCLS_1art,"\\s")
  
  cstream_1art <- as.clickstreams(myCLS_1art, header = TRUE) 
  mc_1art <- fitMarkovChain(cstream_1art, order=1, verbose = T ) 
  #Warning message:
  #In fitMarkovChain(cstream_1art, order = 1, verbose = T) :
  # Some click streams are shorter than 1.
  
  ####To Remove Sessions with 1-Art out of Character file!
  df_clicks <- data.frame(myCLS_1art)
  df_clicks_2plus <- df_clicks[!grepl("(\\d+,\\d+),", df_clicks),]
  df_clicks_2plus <- as.character(df_clicks_2plus)
  df_clicks_2plus <- df_clicks[grepl("(\\d+,\\d+),", df_clicks$myCLS_1art),]
  df_clicks_2plus <- as.character(df_clicks_2plus)
  cstream_1art <- as.clickstreams(df_clicks_2plus, header = TRUE)
  mc_1art <- fitMarkovChain(cstream_1art, order=1, verbose = T )
  ready_to_clickstream <- list(char1 = df_clicks_2plus, markov1 = mc_1art, cstream1 = cstream_1art)
  show(mc_1art)
  
  return(ready_to_clickstream)
}


#############################################################################
###                                                                       ###
###          5. Model: Predicting next click after a Pattern              ###
###                                                                       ###
#############################################################################
prediction <- function(patrn){

  startPattern <- new("Pattern", sequence = patrn) 
  predict(mc_1art, startPattern, dist=2) 
  
}


#############################################################################
###                                                                       ###
###               6. Model: Double Articles Probabilities                 ###
###                                                                       ###
#############################################################################
evaluateProbabilities <- function(TrainingMarkov, CstreamTraining, CstreamTest){

  res1 <- mcEvaluateAll(TrainingMarkov, CstreamTraining, CstreamTest) 
  show(res1)
  return(res1)
}


#############################################################################
###                                                                       ###
###         7. Model Evaluation: N-Fold Cross Validation                  ###
###                                                                       ###
#############################################################################
NfoldCross <- function(foldNum){
  
  tot_clicks <- data.frame(df_clicks_2plus)
  
  #Randomly shuffle the entire dataset
  tot_clicks<- as.data.frame(tot_clicks[sample(nrow(tot_clicks)),])
  
  #Create N (foldNum) equally size folds
  folds <- cut(seq(1,nrow(tot_clicks)),breaks=foldNum,labels=FALSE)
  res <- vector("list", foldNum)
  
  #Performing N Fold Cross Validation now:
  
  for(i in 1:foldNum){
    
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- as.character(tot_clicks[testIndexes, ])
    trainData <- as.character(tot_clicks[-testIndexes, ])
    
    #Use the test and train data partitions to buildup MarkovChains and run Clickstream Package on:
    trainCLS <- as.clickstreams(trainData, header = TRUE)
    mc_train <- fitMarkovChain(trainCLS, order=1, verbose = T )
    testCLS <- as.clickstreams(testData, header = TRUE)
    mc_test <- fitMarkovChain(testCLS, order=1, verbose = T )
    
    #Clustering Dataset based on Clickstream package
    clusters <- getConsensusClusters(trainCLS, testCLS, maxIterations=10, optimalProbMean=0.15, range = 0.50, centresMin = 6, clusterCentresRange = 0, order = 1, takeHighest = T, verbose = T)
    
    #summary(clusters)   #Shows the Details of Generated Clusters
    
    mc_clusters <- fitMarkovChains(clusters)
    startPattern <- new("Pattern", sequence = PATTERN) 
    
    #Generating Optimum Markov Chain for our Clickstream PATTERN
    mc_opt <- getOptimalMarkovChain(startPattern, mc_clusters, clusters) 
    
    #Prediction based on Optimum Markov Chain, if needed
    #predict(mc_opt, startPattern)
    
    #Evaluates all next page clicks in a clickstream training data set against a test data.
    res[[i]] <- mcEvaluateAll(mc_train, trainCLS, testCLS)
    
  }
  
  result.full <- bind_rows(res, .id = "FoldNo")
  res_probs <- sqldf("select FoldNo, predictedNextClick, patternSequence, probability from 'result.full' group by FoldNo, predictedNextClick, patternSequence, probability")
  res_probs <- res_probs[!(res_probs$predictedNextClick=="0"),]
  res_probs$probability <- as.numeric(res_probs$probability)
  
  #prob_comp <- data.frame(From=character(), To=character(), Prob_1=numeric(), Prob_2=numeric(), Prob_3=numeric(), Prob_4=numeric(), row.names=NULL)
  #prob_comp <- data.frame()
  #names <- c("From", "To", "Prob_1", "Prob_2", "Prob_3", "Prob_4")
  #for (n in names) prob_comp[[n]] <- as.character()
  prob_comp <- reshape2::dcast(res_probs, patternSequence+predictedNextClick ~ FoldNo , fun.aggregate = NULL ,na.rm=TRUE, value.var = "probability")
  prob_comp$no <- seq(1:nrow(prob_comp))
  prob_comp_na <- prob_comp[rowSums(is.na(prob_comp[, 3:(foldNum+2)]))<=1,]
  prob_comp_na$Major_Clicks <- paste(prob_comp_na$patternSequence, ">",prob_comp_na$predictedNextClick)
  prob_comp_clean <- prob_comp_na[c(-1,-2,-(foldNum+3))]
  
  #ggplot(prob_comp_clean, mapping= aes(x = as.factor(Major_Clicks), y=prob_comp_clean[ ,-5], ymin=0, ymax=1, group='genus') ) + geom_line()
  plot(prob_comp_clean[,(-foldNum-1)])
  show(prob_comp_clean)
  return(prob_comp_clean)

}

#############################################################################
###                                                                       ###
###                       ****** MAIN ******                              ###
###                                                                       ###
#############################################################################

###Please copy your .csv file in this location:
getwd()
#---OR---
###Add your .csv file location in command below, and replace it with '.', i.e. 'C:/Date_Science/Capstone/'
setwd('.')

clicks <- rfile('clicks_sample.csv')
clicks_ArtNums <- descriptiveStats(clicks)
cluster_arts(clicks_ArtNums)

ready_to_clickstream <- eda(clicks_ArtNums)
df_clicks_2plus <- ready_to_clickstream$char1
mc_1art  <- ready_to_clickstream$markov1
cstream_1art <- ready_to_clickstream$cstream1

clusters <- clusterClickstreams(clickstreamList = cstream_1art, order = 1, centers=6)
PAM = pam(clicks_ArtNums[,1:2], k = 6, metric=c("euclidean", "manhattan"))
#Adding Clusters to Data Frame (art_range):
PAMClust = rep("NA", length(clicks_ArtNums$num_of_Art))
PAMClust[PAM$clustering == 1] = "Art1"
PAMClust[PAM$clustering == 2] = "Art2"
PAMClust[PAM$clustering == 3] = "Art3"
PAMClust[PAM$clustering == 4] = "Art4"
PAMClust[PAM$clustering == 5] = "Art5"
PAMClust[PAM$clustering == 6] = "Art6"
clicks_ArtNums$ART_Clusters = PAMClust

art_clusts <- reshape2::dcast(clicks_ArtNums, user_id ~ session_id , fun.aggregate = toString ,na.rm=TRUE, value.var = "ART_Clusters")
art_toStr <- do.call(paste, c(art_clusts,sep=","))
art_clusters <- gsub(",+", ",", art_toStr)
art_clusters <- gsub(",$", "", art_clusters)
art_clusters <- str_remove_all(art_clusters,"\\s")
art_clust_cstream <- as.clickstreams(art_clusters, header = TRUE) 
art_clust_mc <- fitMarkovChain(art_clust_cstream, order=1, verbose = T ) 
#show(art_clust_mc)
plot(art_clust_mc)


#We could supply any patterns we need a prediction for next click on, to put as "PATTERN" here:
#PATTERN <- c("284847","96663")
PATTERN <- c("284847")
prediction(PATTERN)

evaluateProbabilities(mc_1art, cstream_1art, cstream_1art)

#Number of Folds to split Dataset to Training and Test sets for N-Fold Cross Validation and running Model on Each fold separately
#then comparing the results

#options(error=recover)
#options(error = NULL)

FOLDS=4
finalTable <- NfoldCross(FOLDS)   #Takes a while based on number of folds!

finalTable <- finalTable[with(finalTable, order(-finalTable$`1`)), ]
finalTableT <- reshape2::dcast(reshape2::melt(finalTable, id.vars = "Major_Clicks", na.rm=T), variable ~ Major_Clicks, value.var = "value")
finalPlot <- finalTableT[,-1]
rm("finalTableT")


par(las=3, oma=c(5,0,1,0))
barplot(as.matrix(finalPlot), main="The First-Level Clicks Probabilities", ylab = "Percenages", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=rainbow(FOLDS), bty="L")
par(xpd=TRUE)
legend("topleft", c("Test1","Test2","Test3","Test4"), cex=0.6, fill=rainbow(FOLDS), horiz = F, inset = c(0.03,-0.1))

#Validation Tests: If our dataset is considered Time-dependent, then folds are NOT-PAIRED, and Kruskal Wallis Test is advised:
### Kruskal Wallis Test Model, having p-value>0.05 result, confirms the H0 assumption that results are identical among N-Folds:
kruskal.test(finalTable[,-ncol(finalTable)])

#Validation Tests: If our dataset is considered Time-independent, then folds are PAIRED, and Friedman Test is advised:
### Friedman Test Model, having p-value>0.05 result, confirms the H0 assumption that results are identical among N-Folds:
finalTableNoNA <- na.omit(finalTable)
finalTableNoNA <- as.matrix(finalTableNoNA[,-ncol(finalTableNoNA)])
friedman.test(finalTableNoNA)


##### Exporting the DFs and Characters, just in case! #####
#write.csv(finalTable, file="C:/Date_Science/Capstone/Original_myCLS.csv", row.names = F)
#write.table(cstream, file = "C:/Date_Science/Capstone/myCstream.txt", sep = "\t", row.names = T, col.names = T)
