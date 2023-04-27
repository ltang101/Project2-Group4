image_matrix <- matrix(0,ncol=10,nrow=10)
image_matrix[2,]<-255
image_matrix[,2]<-255
image_matrix[9,]<-255
image_matrix[,9]<-255

library(magick)
current_image <- image_read("dandelion-1307475185dYm-3246239646.jpg")
print(current_image)
current_image_data[1:20]

dandelion_data_matrix <- NULL
for(i in list.files(path = '~/Documents/IE 332/Project 2/data-for-332/data-for-332/dandelions', full.names = TRUE)) {
  current_image <- image_read(i)
  current_image_data <- as.numeric(current_image[[1]][1, , ])
  dandelion_data_matrix <- rbind(dandelion_data_matrix, current_image_data)
}

rownames(dandelion_data_matrix) <- list.files(path = "~/Documents/IE 332/Project 2/data-for-332/data-for-332/dandelions")

dandelion_data_matrix[1:4, 1:4] #matrix identity of the pictures
dim(dandelion_data_matrix) # First value is the number of pictures (38) and second value is the pixels for each image (5785600)

dandelion_pca <- prcomp(dandelion_data_matrix) #prcomp dimension reduction method that projects each data point into a lower dimensional space while also preserving as much data as possible
plot(dandelion_pca$x[, 1:2]) #each dot is one image

rightlocate_indexes <- grep(rownames(dandelion_pca$x), pattern = "AdobeStock_206345546-426480574.jpg")
centerlocate_indexes <- grep(rownames(dandelion_pca$x), pattern = "common-dandelion-43x_pam3-3575848235.jpg")
leftlocate_indexes <- grep(rownames(dandelion_pca$x), pattern = "636597665741397587-dandelion-1097518082.jpg")
points(dandelion_pca$x[rightlocate_indexes, 1:2], col = "green")
points(dandelion_pca$x[centerlocate_indexes, 1:2], col = "blue")
points(dandelion_pca$x[leftlocate_indexes, 1:2], col = "red")

locate <- rep(0, times = nrow(dandelion_pca$x))
locate[leftlocate_indexes] <- 1
locate[rightlocate_indexes] <- 2
dandelion_data_matrix <- cbind(locate, dandelion_pca$x)

library(caret)
model <- train(as.factor(locate)~., data = dandelion_data_matrix, trControl = trainControl(method = "LOOCV"), method = "rf")
model$finalModel