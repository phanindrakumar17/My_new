##Loading and reading the dataset into dataframe.
data_frame <- read.csv("Dataset.csv")


##Changing all columns to proper names.
colnames(data_frame)[colnames(data_frame) == "Age..years."] <- "Age"
colnames(data_frame)[colnames(data_frame) == "Body.fat...."] <- "Body_fat"
colnames(data_frame)[colnames(data_frame) == "Chest.circumference..cm."] <- "Chest_circumference"
colnames(data_frame)[colnames(data_frame) == "Density..g.cm.."] <- "Density"
colnames(data_frame)[colnames(data_frame) == "Knee.circumference..cm."] <- "Knee_circumference"
colnames(data_frame)[colnames(data_frame) == "Weight..lbs."] <- "Weight"

#Viewing the column name and their data types
View(data_frame)
str(data_frame)

#The R code for Mean, median, 1st quartile, 3rd quartile, minimum, and maximum for weight (lbs): 
summary(data_frame$Weight)

#standard deviation of weight:
sd(data_frame$Weight)

#The R code for Mean, median, 1st quartile, 3rd quartile, minimum, and maximum for Age (years):
summary(data_frame$Age)

#The R code for standard deviation of Age (years):
sd(data_frame$Age)

#The R code for Mean, median, 1st quartile, 3rd quartile, minimum, and maximum for Knee circumference (cm):
summary(data_frame$Knee_circumference)

#The R code for standard deviation of Knee circumference (cm):
sd(data_frame$Knee_circumference)

#The R code for Mean, median, 1st quartile, 3rd quartile, minimum, and maximum for Chest circumference (cm):
summary(data_frame$Chest_circumference)

#The R code for standard deviation of Chest circumference (cm):
sd(data_frame$Chest_circumference)

#The R code for Mean, median, 1st quartile, 3rd quartile, minimum, and maximum for Body Fat %: 
summary(data_frame$Body_fat)

#The R code for standard deviation of Body Fat %:
sd(data_frame$Body_fat)

#The R code for Mean, median, 1st quartile, 3rd quartile, minimum, and maximum for Density: 
summary(data_frame$Density)

#The R code for standard deviation of Density:
sd(data_frame$Density)

---------------------------------------------------------------------------------------------
  
  ##Scatter.Plots:
  ##Age and Body Fat %:
  
  plot(data_frame$Age,data_frame$Body_fat,main="Scatter Plot")

#The r code for the correlation coefficient:
cor(data_frame$Age,data_frame$Body_fat)

---------------------------------------------------------------------------------------------
  
  
  ##Chest Circumference and Body Fat %
  plot(data_frame$Chest_circumference,data_frame$Body_fat,main="Scatter Plot")

#The r code for correlation coefficient: 
cor(data_frame$Chest_circumference,data_frame$Body_fat)

----------------------------------------------------------------------------------------------
  ##Knee circumference and Body Fat %
  #The r code used to plot the scatter plot was:
  plot(data_frame$Knee_circumference,data_frame$Body_fat,main="Scatter Plot")

#The r code for correlation: 
cor(data_frame$Knee_circumference,data_frame$Body_fat)

-----------------------------------------------------------------------------------------
  ##Weight and Body Fat %:
  
  #The r code for the scatter plot: 
  plot(data_frame$Weight,data_frame$Body_fat,main="Scatter Plot")

#The r code for the correlation coefficient:
cor(data_frame$Weight,data_frame$Body_fat)
-----------------------------------------------------------------------------------------
  
  ##Density and Body Fat %:
  #The r code for the scatter plot: 
  plot(data_frame$Density,data_frame$Body_fat,main="Scatter Plot")

#The r code for correlation coefficient: 
cor(data_frame$Density,data_frame$Body_fat)

##Data Modification:
Mat<-matrix(variable,nrow=1,ncol=252)

variable <- rnorm(252)  # Example data (252 random numbers from a normal distribution)
Mat <- matrix(variable, nrow = 1, ncol = 252)  # Creating a 1x252 matrix
#Identify the outlier positions (for example, positions 5, 10)
outlier_positions <- c(5, 10)
#Replace the outliers with the mean of the data set
mean_value <- mean(Mat)
View(mean_value)
Mat[1, outlier_positions] <- 0.0099

#Print the matrix
print(Mat)

Variable <- Mat[1, ]

##Box Plots
#Age
windows(16,20)
boxplot(data_frame$Age)

#Body Fat Percentage
windows(16,21)
boxplot(data_frame$Body_fat)

Mat<-matrix(data_frame$Body_fat,nrow=1,ncol=252)
Mat[1,216]<-19.2
print(Mat)
data_frame$Body_fat<-Mat[1, ]

##Density
boxplot(data_frame$Density)

Mat<-matrix(variable,nrow=1,ncol=252)
Mat[1,216]<1.056
print(Mat)
Variable<-Mat[1, ]

#Knee Circumference (cm):
windows(16,21)
boxplot(data_frame$Knee_circumference)

Mat<-matrix(variable,nrow=1,ncol=252)
Mat[1,216]<-38.59
print(Mat)
Variable<-Mat[1, ]

#Chest Circumference (cm):
windows(16,21)
boxplot(data_frame$Chest_circumference)

Mat<-matrix(variable,nrow=1,ncol=252)
Mat[1,216]<-100.82
  print(Mat)
Variable<-Mat[1, ]

##Weight
windows(16,21)
boxplot(data_frame$weight)

Mat<-matrix(variable,nrow=1,ncol=252)
Mat[1,216]<-178.9
  print(Mat)
Variable<-Mat[1, ]

#Correlation matrix
cor(data_frame)

#Test of normality
#Age:
qqnorm(data_frame$Age)

#Weight
windows(16,20)
qqnorm(data_frame$Weight)

#Density
windows(16,20)
qqnorm(data_frame$Density)

#Knee circumference
windows(16,20)
qqnorm(data_frame$Knee_circumference)

#Chest circumference
windows(16,20)
qqnorm(data_frame$Chest_circumference)

#Body Fat %
windows(16,20)
qqnorm(data_frame$Body_fat)

##Regression analysis
fit<-lm(data_frame$Body_fat~ data_frame$Age+data_frame$Weight+data_frame$Density+data_frame$Knee_circumference+data_frame$Chest_circumference)
summary(fit)

#The model:


Body_fat~4.24+ 0.0153$Age + 0.0071$Weight - 415.8$Density - 0.0024$Knee_circumference + 0.035$Chest_circumference

summary(fit)

#The r code : 
fit<-lm(data_frame$Body_fat~data_frame$Age+data_frame$Weight+data_frame$Density)
summary(fit)




