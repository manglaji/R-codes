###########################################################################
# Set working directory
###########################################################################

setwd("C:\\2015 Model Development\\Model_2015\\01_AX\\Data")
rm(list= ls())

##########################################################################
#Input Data
##########################################################################

datafile = 'AX_03092015.txt'

###########################################################################
# library load
# fscaret,caret, plyr, psych, Fselector, corrplot, corrgram library
###########################################################################

library(fscaret)
library(plyr)
library(caret)
library(psych)
library(corrgram)
library(FSelector)
library(corrplot)

#------------------------------------------------------------------------#


##########################################################################
# Function definition 
##########################################################################

#---------  Function to get only variable which are factor or charater or numeric ---------#

varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
  vars <- character(0)
  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }  
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }  
  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}

#--------- Function to get the correlation  ---------#
## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
## Note: spearman have been selected
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs",method = "spearman" )
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

#--------- Converting into flat square matrix ---------#
## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

##########################################################################
# Data preparation
##########################################################################

Data <- read.table(datafile, header = TRUE, sep = '|')

#--------------------------- initial data checks-------------------------#


#Check for NA/Null or missing values
nacase <- sapply(Data, function(x)(sum(is.na(x))))

# Checking the fields where all or Some of the fields are NA or Null or missing
nacase[nacase>0]
dput(names(nacase[nacase>0]))

# Checking the fields where all the fields are NA or Null or missing
nacase[nacase == nrow(Data)]
dput(names(nacase[nacase == nrow(Data)]))

# Checking for Blanks value in categorical variables
blankCase <- sapply(Data, function(x)(ifelse(is.factor(x),sum(ifelse(x=='',1,0)),sum(0))))

blankCase[blankCase>0]
dput(names(blankCase[blankCase>0]))

###################----- Filtering the data set ------##########################
## Removing all the NULL or missing values
#Getting list of variables which are NULL
varlistRmNull <- names(Data) %in% c(names(nacase[nacase == nrow(Data)]))
#Removing the column which are null
Data <- Data[!varlistRmNull]

#dput((varlist(df = DataCor, type = c("factor","character"))))
## Creating Categorical and Continuous data frame for correlation analysis

#Getting list of variables which are factor or character
CatCol <- varlist(df = Data, type = c("factor","character"))
varlistCont <- names(Data) %in% CatCol

# Creating Categorical data frame for chi-square analysis
DataCorCat <- Data[c(dput((varlist(df = Data, type = c("factor","character")))),"WIN")]
DataCorCat$WIN <- as.factor(DataCorCat$WIN)
#str(DataCorCat)

#Creating Continuous data frame for correlation analysis
DataCorCont <- Data[!varlistCont]

#--------- Reordering the data to MISO format -----------------#### 
varRmWIN <- names(Data) %in% c('WIN')
varReordered <- c(names(Data[!varRmWIN]),"WIN")
Data <- Data[varReordered]
#Testing : write.table(Data, "Datatest.txt", sep = '|')

#---------- Dumyfying the data -----------#

DataDummy <- dummyVars("~.", data = Data, fullRank = F)
DataDF<-as.data.frame(predict(DataDummy,Data))


#dmy <- dummyVars(" ~ .", data = DataCor)
#DataCorDF <- data.frame(predict(dmy, newdata = DataCor))


##########################################################################
# Correlation Analysis
##########################################################################

corMasterList <- flattenSquareMatrix (cor.prob(DataDF))
corList <- corMasterList[order(-abs(corMasterList$cor)),]
print(head(corList,10))

corList <- corMasterList[order(corMasterList$cor),]
selectedSub <- subset(corList, (abs(cor) > 0.062 & j == 'WIN'))

Related <- subset(corList, abs(cor)>0.15)

# to get the best variables from original list:
bestSub <-  sapply(strsplit(as.character(selectedSub$i),'[.]'), "[", 1)
bestSub <- unique(bestSub)

# or use the variables from top selectedSub:
bestSub <- as.character(selectedSub$i)

#Write the top correlated variables and other related variables to txt file.
write.table(selectedSub, "SelectedSub.txt", col.names = TRUE, sep = "|")
write.table(Related, "Related.txt", col.names = TRUE, sep = "|")

#-------------Create Charts ------------#
selectedSubOrdered <- selectedSub[order(-abs(selectedSub$cor)),]

selectedSubOrdered<-droplevels(selectedSubOrdered)
top20 <- as.character(droplevels(selectedSubOrdered[1:20,]$i))
Next20 <- as.character(droplevels(selectedSubOrdered[21:40,]$i))
Next20_1 <- as.character(droplevels(selectedSubOrdered[21:nrow(selectedSubOrdered),]$i))
#Next10_3 <- as.character(droplevels(selectedSubOrdered[31:40,]$i))
#Last10 <- as.character(droplevels(selectedSubOrdered[41:46,]$i))

#----------- Viewing Charts ------------#
win.metafile("plotTop20.wmf", res = 300)
corrplot.mixed(cor(DataDF[c(top20, 'WIN')], method = "spearman"), tl.pos = 'lt',tl.cex=0.75, addCoefasPercent = TRUE)
dev.off()

win.metafile("plotNext20.wmf", res = 300)
corrplot.mixed(cor(DataDF[c(Next20, 'WIN')], method = "spearman"), tl.pos = 'lt',tl.cex=0.75, addCoefasPercent = TRUE)
dev.off()

win.metafile("plotNext20_1.wmf", res = 300)
corrplot.mixed(cor(DataDF[c(Next20_1, 'WIN')], method = "spearman"), tl.pos = 'lt',tl.cex=0.75, addCoefasPercent = TRUE)
dev.off()


#pairs.panels(DataDF[c(top10, 'WIN')], method = "spearman")
#pairs.panels(DataDF[c(Next10, 'WIN')], method = "spearman")
#pairs.panels(DataDF[c(Next10_2, 'WIN')], method = "spearman")
#pairs.panels(DataDF[c(Next10_3, 'WIN')], method = "spearman")
#pairs.panels(DataDF[c(Last10, 'WIN')], method = "spearman")


#-------------------Chi-Square analysis -----------------------#

combos <- combn(ncol(DataCorCat),2)
ChiSqrAnalysis <- adply(combos, 2, function(x) {
  test <- chisq.test(DataCorCat[, x[1]], DataCorCat[, x[2]])
  
  out <- data.frame("Row" = colnames(DataCorCat)[x[1]]
                    , "Column" = colnames(DataCorCat[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    ,  "df"= test$parameter
                    ,  "p.value" = round(test$p.value, 3)
  )
  return(out)
  
}) 

write.table(ChiSqrAnalysis, "ChiSqrAnalysisWin.txt", sep = '|')

#------------------ Chi Square (FSelector) ------------------#
weights <-chi.squared(WIN~., Data)
write.table(weights,"ChiSquareWeights.txt", sep ='|')


#------------------ Information Gain (FSelector) ------------------#
weights <- information.gain(WIN~., Data)
write.table(weights,"InformationGainWeights.txt", sep ='|')

#------------------ Random Forrest (FSelector) ------------------#
##### Random forrest importance ######
#importance.type = 1 for mean decrease in accuracy
#                = 2 for mean decrease in node impurity
weights <- random.forest.importance(WIN~., Data, importance.type = 2)
write.table(weights,"RandomForrestWeights.txt", sep ='|')


# ---------------- Ensemble Methods for feature selction ----------------#

#Setting seed
set.seed(1986)

#splitting the data set into training and testing set
splitIndex <- createDataPartition(DataDF$WIN, p = 0.85, list = FALSE, times = 1)
trainDataDF <- DataDF[splitIndex,]
testDataDF <- DataDF[-splitIndex,]

#fsModels list for regression
fsModels <- c("glm","gbm","lasso","ridge","svmRadial","neuralnet") 

#fsModels list for classification
fsModelsClass <- c("glm","gbm","svmRadial","nnet","treebag")  

myFS<-fscaret(trainDataDF, testDataDF, myTimeLimit = 40, preprocessData=TRUE,
              Used.funcRegPred = fsModels, with.labels=TRUE,
              supress.output=FALSE, no.cores=4)

#trainDataDF$WIN <- as.factor(trainDataDF$WIN)
#testDataDF$WIN <- as.factor(testDataDF$WIN)

myFS<-fscaret(trainDataDF,testDataDF , myTimeLimit = 100, preprocessData=TRUE, classPred = TRUE
              ,regPred = FALSE,                    
              Used.funcClassPred = fsModelsClass, with.labels=TRUE,
              supress.output=FALSE, no.cores=4)

#Writing the outcome to folder
write.table(myFS$VarImp, "VarImp.txt", sep ="|")

results <- myFS$VarImp$matrixVarImp.MeasureError
results$Input_no <- as.numeric(results$Input_no)
results <- results[c("SUM","SUM%","ImpGrad","Input_no")]
myFS$PPlabels$Input_no <-  as.numeric(rownames(myFS$PPlabels))
results <- merge(x=results, y=myFS$PPlabels, by="Input_no", all.x=T)
results <- results[c('Labels', 'SUM')]
results <- subset(results,results$SUM !=0)
results <- results[order(-results$SUM),]
print(results)


#Writing the consolidated results of ensemble methods to the folder
write.table(results, "ResultsEnsemble.txt", sep ="|")

#Copy ensemble files from temp file to permanent file
flist<-list.files(tempdir(), "^.+[.]txt$", full.names = TRUE)
CopyTo <- paste(getwd(),"/Ensemble", sep ="")
dir.create(CopyTo)
file.copy(flist, CopyTo, copy.mode = TRUE,overwrite = TRUE)

##################### Correlation Analysis on selected Variables  ################################
selVar = c('WIN','MCS_rev_CRMPast1Year','MCS_rev_CRMPast3Year','MCS_rev_CRM','MCS_RevPast1Year','MCS_RevPast3Year','MCS_RevPast5Year','MCS_rev_BMCPast1Year','MCS_rev_BMCPast3Year','GSX_ServiceRevenue','MCS_rev_BMC','Model_Past3yr_Dynamics_CRM','SAMI_SEShare','Model_Past1yr_Dynamics_CRM','MCS_rev_SharepointPast3Year','MCS_rev_Sharepoint','MCS_ESSContractsPast5year','MCS_EarilestContractYear','Model_Past1yr_Dynamics_CRM_Online','Model_Past3yr_Dynamics_CRM_Online','CSwins_Past3yrs','MCS_rev_DCPast3Year','Model_Past3yr_SP_VisualStudioMSDN','Model_Past1yr_System_Center_Server','Model_Past3yr_Biztalk_Server','Model_RevEAPast5Years','Model_NumProductDivisions','PCIB','Model_Past1yr_Biztalk_Server','Premier_Past1yr_Premier_Plus','BMX_MCS_NSAT','Model_Past1yr_Windows_Remote_Desktop_Services')

DataSel <- Data[selVar]

DataSeldmy <- dummyVars("~.", data = DataSel, fullRank = F)
DataSelDF <- as.data.frame(predict(DataSeldmy,DataSel))

## Correlation between selected variables
CorrelationsSelected <-cor(DataSelDF, method = "spearman")

corMasterListSel <- flattenSquareMatrix (cor.prob(DataSelDF))
corListSel <- corMasterListSel[order(-abs(corMasterListSel$cor)),]
head(corListSel)

write.table(corListSel,'corListSel.txt', sep = '|' )

#Writing the result to file
write.table(CorrelationsSelected,"CorrelationsSelected.txt", sep = '|')

#Creating correlation graphs
corrplot(CorrelationsSelected, type = "upper", order = "hclust", method = "number")
pairs.panels(DataSel, method = "spearman")
pairs.panels(DataSel[1:20], method = "spearman", smooth = FALSE, density = FALSE )
image(CorrelationsSelected)














