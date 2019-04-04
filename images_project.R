#1a
load_training_images = function(in_dir = "~/Documents/STA141A/", out_file = "~/Documents/STA141A/train.rds"){ 
  #Read 5 bin files
  batch1 = file("/Users/dannie/Documents/STA141A/data_batch_1.bin", "rb")
  batch2 = file("/Users/dannie/Documents/STA141A/data_batch_2.bin", "rb")
  batch3 = file("/Users/dannie/Documents/STA141A/data_batch_3.bin", "rb")
  batch4 = file("/Users/dannie/Documents/STA141A/data_batch_4.bin", "rb")
  batch5 = file("/Users/dannie/Documents/STA141A/data_batch_5.bin", "rb")
  
  #Define number of images per batch
  num_images = 10000
  
  #Create a list that contains all the batches
  training_files <- list(batch1, batch2, batch3, batch4, batch5)
  
  #Define labels and images
  labels =  matrix(0, nrow = 5*num_images, ncol = 1)
  images = matrix(0, 5*num_images, 1024*3)
  
  #Create nested for loops to save labels and images of all training data
  for (j in 1:5){
    for(i in 1:num_images){
      labels[i] <- readBin(training_files[[j]], integer(), size = 1, n = 1, endian = "big")
      images[i, ] <- as.integer(readBin(training_files[[j]], raw(), size = 1, n = 1024*3, endian = "big"))
    }
  }
  #Bind labels and images as columns into one data type
  training <- cbind(labels, images)
  
  #Save training as RDS file
  saveRDS(training, out_file)
}

#1b
load_testing_images = function(in_dir = "~/Documents/STA141A/", out_file = "~/Documents/STA141A/test.rds"){ 
  #Read test bin files
  batch.t = file("/Users/dannie/Documents/STA141A/test_batch.bin","rb")
  
  #Define number of images per batch
  num_images = 10000
  
  #Define labels and images
  labels = numeric(num_images)
  images = matrix(0, num_images, 1024*3)
  
  #Create nested for loops to save labels and images of all testing data
  for(i in 1:num_images){
    labels[i] <- readBin(batch.t, integer(), size = 1, n = 1, endian = "big")
    images[i, ] <- as.integer(readBin(batch.t, raw(), size = 1, n = 1024*3, endian = "big"))
  }
  
  #Bind labels and images as columns into one data type
  testing <- cbind(labels, images)
  
  #Save training as RDS file
  saveRDS(testing, out_file)
}
#Call two functions to load the training and testing images to get the train.rds and test.rds file
load_training_images(in_dir = "~/Documents/STA141A/", out_file = "~/Documents/STA141A/train.rds")
load_testing_images(in_dir = "~/Documents/STA141A/", out_file = "~/Documents/STA141A/test.rds")

#2
library(grid)
#Load the training data set 
training <- readRDS("~/Documents/STA141A/train.rds")
#Making a lookup table, matching each number to a label
look<- as.data.frame(matrix(cbind(c(0:9),c("airplane","automobile",
                                           "bird","cat","deer","dog", "frog",
                                           "horse", "ship", "truck")),10, 2))
view_images = function(i) {
  #Reading the image as an 1-d array.
  img <- training[i,-1] 
  # construct a matrix for each color channel
  r <- matrix(img[1:1024], ncol=32, byrow = TRUE)
  g <- matrix(img[1025:2048], ncol=32, byrow = TRUE)
  b <- matrix(img[2049:3072], ncol=32, byrow = TRUE)
  
  # combine together and normalize the pixel intensities
  img_col <- rgb(r,g,b, maxColorValue = 255)
  dim(img_col) <- dim(r)
  
  # Now display the image 
  grid.raster(img_col, interpolate=FALSE)
  
  #lookup the label
  sprintf(as.character(look$V2[training[i,1]+1]))
}

#3
#Load the testing data set
testing <- readRDS("~/Documents/STA141A/test.rds")
#Rescaling the data to be 10% of the original data
data_rescale<-function(labels,k=500)sort(as.vector(sapply(unique(labels),function(i)which(labels==i)[1:k])))
training2<-training[data_rescale(training[,1],k=500),]
testing2<-testing[data_rescale(testing[,1],k=100),]
#Remove the original training and testing data set and assign it back to training and testing variable for future function call
remove(training)
remove(testing)
training = training2
testing = testing2
#dog
view_images(15)
#deer
view_images(500)
#automobile
view_images(35)
#truck
view_images(60)
#horse
view_images(200)
#airplane
view_images(13)
#frog
view_images(75)
#cat
view_images(79)
#bird
view_images(85)
#Split the training data based on color channels (Red, green, and blue)
red_channel = training[,2:1025]
green_channel = training[,1026:2049]
blue_channel = training[,2050:3073]
#Getting the standard deviation for each channel
sd_red = apply(red_channel, MARGIN = 2, sd)
sd_green = apply(green_channel, MARGIN = 2, sd)
sd_blue = apply(blue_channel, MARGIN = 2, sd)
#Finding the maximum and minimum standard deviation of each channel
which.max(sd_red)
which.min(sd_red)
which.max(sd_green)
which.min(sd_green)
which.max(sd_blue)
which.min(sd_blue)

#4
x_matrix = rbind(testing,training)
#saveRDS(as.matrix(dist(x_matrix)),"~/Documents/STA141A/distance.rds")
matrix_x = readRDS("~/Documents/STA141A/distance.rds")
distance_matrix = matrix_x[1001:6000,1001:6000]
# Actual predict_knn function 
predict_knn = function(predict_point , training_point, distance_metric, k ) {
  #Finding distances between training and predict point
  dis = as.matrix(dist(rbind(training_point[,2:ncol(training_point)],predict_point[,2:ncol(predict_point)]),method  = distance_metric))
  numPoints = nrow(predict_point)
  if (is.null(numPoints)) { numPoints = 1 }
  offset = nrow(training_point)
  labels = numeric()
  dis = dis[,-(offset+1:ncol(dis))]
  #For each predict points, find the k lowest distances and extract the labels
  for (j in 1:numPoints) {
    distPoint = dis[offset+j,]
    lst <- sort(distPoint, index.return=TRUE, decreasing=FALSE)
    valuesIdx = lapply(lst, `[`, lst$x %in% head(unique(lst$x),k))
    indexes = valuesIdx$ix
    addingLabels = numeric()
    for (idx in 1:length(indexes)) {
      addingLabels = c(addingLabels,  training_point[indexes[idx],1])
    }
    label = as.integer(names(which.max(table(addingLabels))))
    
    labels = c(labels, label)
  }
  return(labels)
}

#5
cv_error_knn = function(distance_metric, k) {
  #Shuffle the training data randomly
  random_training = training[sample(nrow(training)),]
  breaks = 10
  num_predictions = nrow(random_training)/breaks
  
  #Divide the random_training into 10 equally sized folds
  folds <- cut(seq(1, nrow(random_training)), breaks = breaks, labels = F)
  #10 fold CV
  
  #error = matrix(, nrow=0, ncol=num_predictions)
  error = numeric()
  for(i in 1:breaks) {
    test_indexes <- which(folds == i, arr.ind = TRUE)
    test_data <- random_training[test_indexes,]
    train_data <- random_training[-test_indexes,]
    predictions = predict_knn(test_data,train_data,distance_metric, k)
    numCorrectClassify = 0
    #Get the error rate for each fold - return the percentage
    for (j in 1:nrow(test_data)) {
      if(identical(test_data[j,1], predictions[j])) {numCorrectClassify=numCorrectClassify+1}
    }
    error = c(error, numCorrectClassify/nrow(test_data))
  }
  return(error*100)
}

#6
#Using “euclidean” as distance metric at k from 1:15
err_eu_1 = cv_error_knn("euclidean",1)
err_eu_2 = cv_error_knn("euclidean",2)
err_eu_3 = cv_error_knn("euclidean",3)
err_eu_4 = cv_error_knn("euclidean",4)
err_eu_5 = cv_error_knn("euclidean",5)
err_eu_6 = cv_error_knn("euclidean",6)
err_eu_7 = cv_error_knn("euclidean",7)
err_eu_8 = cv_error_knn("euclidean",8)
err_eu_9 = cv_error_knn("euclidean",9)
err_eu_10 = cv_error_knn("euclidean",10)
err_eu_11 = cv_error_knn("euclidean",11)
err_eu_12 = cv_error_knn("euclidean",12)
err_eu_13 = cv_error_knn("euclidean",13)
err_eu_14 = cv_error_knn("euclidean",14)
err_eu_15 = cv_error_knn("euclidean",15)

#Mean of the error rate using "euclidean" as distance metric for all k
mean_err_eu = c(mean(err_eu_1),mean(err_eu_2),mean(err_eu_3),mean(err_eu_4),mean(err_eu_5),mean(err_eu_6),mean(err_eu_7),mean(err_eu_8),mean(err_eu_9),mean(err_eu_10),mean(err_eu_11),mean(err_eu_12),mean(err_eu_13),mean(err_eu_14),mean(err_eu_15))

#Using “maximum” as distance metric at k from 1:15
err_max_1 = cv_error_knn("maximum",1)
err_max_2 = cv_error_knn("maximum",2)
err_max_3 = cv_error_knn("maximum",3)
err_max_4 = cv_error_knn("maximum",4)
err_max_5 = cv_error_knn("maximum",5)
err_max_6 = cv_error_knn("maximum",6)
err_max_7 = cv_error_knn("maximum",7)
err_max_8 = cv_error_knn("maximum",8)
err_max_9 = cv_error_knn("maximum",9)
err_max_10 = cv_error_knn("maximum",10)
err_max_11 = cv_error_knn("maximum",11)
err_max_12 = cv_error_knn("maximum",12)
err_max_13 = cv_error_knn("maximum",13)
err_max_14 = cv_error_knn("maximum",14)
err_max_15 = cv_error_knn("maximum",15)

#Mean of the error rate using "euclidean" as distance metric for all k
mean_err_max = c(mean(err_max_1),mean(err_max_2),mean(err_max_3),mean(err_max_4),mean(err_max_5),mean(err_max_6),mean(err_max_7),mean(err_max_8),mean(err_max_9),mean(err_max_10),mean(err_max_11),mean(err_max_12),mean(err_max_13),mean(err_max_14),mean(err_max_15))

#Plot Euclidean distances
plot(mean_err_eu, col = "red", lty = 2, type = "b", ylab = "Average Error Rates", xlab = "K", ylim = c(0,30), xlim = c(1, 15),
     main = "10-Fold CV Average Error Rates for k = 1, ..., 15\nusing Euclidean and Maximum", bty = 'L')

#Plot Maximum distance
lines(mean_err_max, col = "green", lty = 1, type = "b")
legend(2, 10, title = "Legend", legend = c("Euclidean Average", "Maximum Average"), lty = rep(2, 1), col = c("red", "green"))


#9
cv_error_knn_test = function(distance_metric, k) {
  #Shuffle the testing data randomly
  random_testing = testing[sample(nrow(testing)),]
  breaks = 10
  num_predictions = nrow(random_testing)/breaks
  
  #Divide the random_testing into 10 equally sized folds
  folds <- cut(seq(1, nrow(random_testing)), breaks = breaks, labels = F)
  #10 fold CV
  
  #error = matrix(, nrow=0, ncol=num_predictions)
  error = numeric()
  for(i in 1:breaks) {
    test_indexes <- which(folds == i, arr.ind = TRUE)
    test_data <- random_testing[test_indexes,]
    train_data <- random_testing[-test_indexes,]
    predictions = predict_knn(test_data,train_data,distance_metric, k)
    numCorrectClassify = 0
    
    for (j in 1:nrow(test_data)) {
      if(identical(test_data[j,1], predictions[j])) {numCorrectClassify=numCorrectClassify+1}
    }
    error = c(error, numCorrectClassify/nrow(test_data))
  }
  return(error*100)
}

#Getting all error rates of K = 1...15 for euclidiean
err_eu_test =  sapply(1:15, function(x) cv_error_knn_test("euclidean", x)) err_test_mat<-matrix(err_eu_test,15,10)
mean_eu_test <- sapply(1:15, function(x)mean(err_test_mat[x,]))

#Getting all error rates of K = 1...15 for maximum
err_max_test =  sapply(1:15, function(x) cv_error_knn_test("maximum", x)) err_max_test_mat<-matrix(err_max_test,15,10)
mean_max_test <- sapply(1:15, function(x)mean(err_max_test_mat[x,]))

#Plot means of Euclidean and Maximum Methods for Testing data
plot(mean_eu_test, col = "red", lty = 2, type = "b", ylab = "Error Rates", xlab = "Number of K", ylim = c(0,47), main = "Test Set 10-Fold CV Error Rates for k = 1, ..., 15\nusing Euclidean and Maximum", bty = 'L')
lines(mean_max_test, col = "green", lty = 2, type = "b")
legend(2, 10, title = "Legend", legend = c("Euclidean Average", "Maximum Average"), lty = rep(2, 1), col = c("red", "green"))


