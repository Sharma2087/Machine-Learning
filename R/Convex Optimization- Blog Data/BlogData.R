install.packages("Rmosek", type="source", INSTALL_opts="--no-multiarch",repos="http://download.mosek.com/R/8", configure.vars="PKG_MOSEKHOME=C:/Program Files/Mosek/8/tools/platform/win64x86 PKG_MOSEKLIB=mosek64")
library(Rmosek)

#Reading CSV from local system
blogData <- read.csv('C:\\Users\\Rahul\\Documents\\Spring 2018 Courses\\ML\\BlogFeedback\\blogData_train.csv',header=FALSE, sep=",")
dim(blogData) #Checking dimensions

#Function for Getting split Index for a defined %
GetSplitIndex<-function(data, splitPercentage){
  smp_size <- floor(splitPercentage * nrow(data))
  return(smp_size)
}
#Function for mean squared error
mse <- function(error)
{
  mean(error^2)
}


#Experiment
set.seed(7865)
#Taking 6500 rows of 52397
blogData=blogData[c(0:6500), c(51:60,281)]
dropCol=c('V55', 'V60') # dropping columns as these are not sigificant; as inferred from last regression
blogData=blogData[,!(names(blogData) %in% dropCol)]

#Creating train and test sets
train_ind <- sample(seq_len(nrow(blogData)), size = GetSplitIndex(blogData,.75))
blog_Train <- blogData[train_ind, ]
blog_Test <- blogData[-train_ind, ]
dim(blog_Test)
View(blog_Train)
#Taking Feature matrix X
blog_Train_X=blog_Train[,1:ncol(blog_Train)-1]
blog_Train_X
blog_Train_Y=blog_Train[,ncol(blog_Train)]
View(blog_Train_Y)
blog_Train_Y=as.matrix(blog_Train_Y)
blog_Train_X=as.matrix(blog_Train_X)
#Scaling
blog_Train_X=scale(blog_Train_X, center = TRUE)
blog_Train_X
#For test sets
blog_Test_X=blog_Test[,1:ncol(blog_Test)-1]
blog_Test_X=scale(blog_Test_X, center = TRUE)
blog_Test_Y=blog_Test[,ncol(blog_Test)]
View(blog_Test_Y)




#Mosek initialization

XTranspose_X=t(blog_Train_X)%*%blog_Train_X
XTranspose_X
XTranspose_Y<--t(blog_Train_X)%*%blog_Train_Y # X'y=c variable in the Quadratic program
#For creating a triangular matrix from XTranspose_X
TriangularMatrix=XTranspose_X

TriangularMatrix[upper.tri(XTranspose_X)]=0 #Setting all elements above diagonal as 0;creating triangular matrix
NonZeroIndexes = which(TriangularMatrix != 0, arr.ind=TRUE)

#problem definition in Mosek
QuadraticBlogModel<-list() 
QuadraticBlogModel$sense<-"min"
QuadraticBlogModel$c<-as.vector(XTranspose_Y) #Cofficients list
#For objective (quadtratic); imputing matrix Q with rows and cols of non zero entries 
QuadraticBlogModel$qobj<-list(i = NonZeroIndexes[,1], j = NonZeroIndexes[,2], v = TriangularMatrix[NonZeroIndexes] ) 
NumberOfParameters<-dim(blog_Train_X)[2] # 2nd index gives dimension implying number of variables
QuadraticBlogModel$A<-Matrix(rep(0,NumberOfParameters), nrow=1,byrow=TRUE,sparse=TRUE) #A Null matrix

#Constraints; defining feature constrains as max and min of particular columns
XF=blog_Train_X #Just for short hand so that it fits on screen
blx<-c(min(XF[,1]),min(XF[,2]),min(XF[,3]),min(XF[,4]),min(XF[,5]),min(XF[,6]),min(XF[,7]),min(XF[,8]))
bux<-c(max(XF[,1]),max(XF[,2]),max(XF[,3]),max(XF[,4]),max(XF[,5]),max(XF[,6]),max(XF[,7]),max(XF[,8]))
QuadraticBlogModel$bx<-rbind(blx,bux)

#(Target variable) contraints: Comments at minimum can be zero or can be an infinite at maximum
QuadraticBlogModel$bc<-rbind(blc=0, buc= Inf)
#Finally, Mosek Solver!
RMoskeModel<-mosek(QuadraticBlogModel, opts = list(verbose = 1)) 
RMoskeModel
#For calculaing errors; (y - yHat); coefficients/weights obtained from Mosek sover
yHat=blog_Test_X[,1]*(-0.8274129) + blog_Test_X[,2]*11.8081904 + blog_Test_X[,3]*(0.3141730)+ blog_Test_X[,4]*(-0.8039801) + blog_Test_X[,5]*(-0.2668045) +blog_Test_X[,6]*(-0.1847590) +blog_Test_X[,7]*(-0.1657888)+  blog_Test_X[,8]*(-0.2584301)
print(yHat)
YMinusYHat = blog_Test_Y-yHat
print(YMinusYHat)
#Calling mse; Mean Squared Error
MSE_Mosek=mse(YMinusYHat)
print(MSE_Mosek)
#Calculating RSS
RSS_Mosek=sum(YMinusYHat^2)
print(RSS_Mosek)



#Linear Regression; defining column name for target variable as Target
colnames(blog_Train) <- c('V51','V52','V53','V54','V56','V57','V58','V59',"Target")
head(blog_Train)
head(blog_Test_X)
Model_LM_1 <- lm(formula = Target ~ .,data = data.frame(blog_Train))
Prediction_LM <- predict(Model_LM_1,data.frame(blog_Test_X),se.fit = TRUE)
summary(Model_LM_1)
Prediction_LM$fit
#mean squared error: -
Error_LM <- (blog_Test_Y - Prediction_LM$fit)
MSE_LM=mse(Error_LM)
MSE_LM
RSS_LM=sum(Error_LM**2)
RSS_LM

#Summary
summary(Model_LM_1)