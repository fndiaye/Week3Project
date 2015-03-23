## This function allows to go in the specified directory 
## in order to create the entire data set with all variables from the text files

createGlobalDS <- function(dir) {
  baseDir <- "./UCI\ HAR\ Dataset"
  fileDir <- paste(baseDir,"/",dir, sep = "")
  
  subjectFile <- paste(fileDir,"/","subject_",dir,".txt", sep = "")
  activityFile <- paste(baseDir,"/activity_labels.txt", sep = "")
  
  total_acc_xFile <- paste(fileDir,"/","Inertial\ Signals/total_acc_x_",dir,".txt", sep = "")
  total_acc_yFile <- paste(fileDir,"/","Inertial\ Signals/total_acc_y_",dir,".txt", sep = "")
  total_acc_zFile <- paste(fileDir,"/","Inertial\ Signals/total_acc_z_",dir,".txt", sep = "")
  
  body_acc_xFile <- paste(fileDir,"/","Inertial\ Signals/body_acc_x_",dir,".txt", sep = "")
  body_acc_yFile <- paste(fileDir,"/","Inertial\ Signals/body_acc_y_",dir,".txt", sep = "")
  body_acc_zFile <- paste(fileDir,"/","Inertial\ Signals/body_acc_z_",dir,".txt", sep = "")
  
  body_gyro_xFile <- paste(fileDir,"/","Inertial\ Signals/body_gyro_x_",dir,".txt", sep = "")
  body_gyro_yFile <- paste(fileDir,"/","Inertial\ Signals/body_gyro_y_",dir,".txt", sep = "")
  body_gyro_zFile <- paste(fileDir,"/","Inertial\ Signals/body_gyro_z_",dir,".txt", sep = "")
  
  profilSetFile <- paste(fileDir,"/X_",dir,".txt", sep = "")
  profilLabelsFile <- paste(fileDir,"/y_",dir,".txt", sep = "")
  featuresFile <- paste(baseDir, "/", "features.txt", sep = "")
  
  subjectVar <- read.table(subjectFile)
  activityVar <- read.table(activityFile)
  total_acc_x <- read.table(total_acc_xFile)
  total_acc_y <- read.table(total_acc_yFile)
  total_acc_z <- read.table(total_acc_zFile)
  body_acc_x <- read.table(body_acc_xFile)
  body_acc_y <- read.table(body_acc_yFile)
  body_acc_z <- read.table(body_acc_zFile)
  body_gyro_x <- read.table(body_gyro_xFile)
  body_gyro_y <- read.table(body_gyro_yFile)
  body_gyro_z <- read.table(body_gyro_zFile)
  
  profilSet <- read.table(profilSetFile)
  profilLabels <- read.table(profilLabelsFile)
  features <- read.table(featuresFile)
  
  
  r <- data.frame(profilSet)
  cn <- features$V2
  colnames(r) <- cn
  
  globalDS <- data.frame(subjectVar)  
  globalDS <- cbind(globalDS, setNames(data.frame(profilLabels),"activity"))
  globalDS <- cbind(globalDS, data.frame(total_acc_x))
  globalDS <- cbind(globalDS, data.frame(total_acc_y))
  globalDS <- cbind(globalDS, data.frame(total_acc_z))
  globalDS <- cbind(globalDS, data.frame(body_acc_x))
  globalDS <- cbind(globalDS, data.frame(body_acc_y))
  globalDS <- cbind(globalDS, data.frame(body_acc_z))
  globalDS <- cbind(globalDS, data.frame(body_gyro_x))
  globalDS <- cbind(globalDS, data.frame(body_gyro_y))
  globalDS <- cbind(globalDS, data.frame(body_gyro_z))
  
  globalDS <- cbind(globalDS, r)
  
  ##Replace the activity number by the activity name
  #globalDS <- mutate(globalDS, activity = factor(c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING","STANDING","LAYING")))
  
  globalDS
}

##This function merges 2 data sets located in directories
##dir1 The location of the 1st data set
##dir2 The location of the 2nd data set

mergeSets <- function(dir1,dir2){
  set1 <- createGlobalDS(dir1)
  set2 <- createGlobalDS(dir2)
  totalSet <- rbind(set1,set2)
  write.table(totalSet, "./output2.txt", row.name = FALSE)
}


##This function searches the colums that represent a mean or a standard deviation calculation
##str1 is a vector of the strings to search
##df a data frame containing the columns in which str1 will be searched
##The function will return a subset of the data frame with only the columns found

searchColumns <- function(str1, df) {
  cNames <- names(df)
  nr <- NROW(str1)
  colTab <- vector()
  for(j in 1:nr) {
    columns <- grep(str1[j], cNames, value=FALSE)
    colTab <- rbind(colTab,columns)
  }
  df[c(colTab)]
  
}