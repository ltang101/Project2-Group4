library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(imager)
library(magick)
install_tensorflow(extra_packages="pillow")
install_keras()
model=load_model_tf("/home/jupyter/332_data/dandelion_model")
width <- 224
height <- 224
target_size <- c(width,height)
rgb <- 3

# Set the path to the folder containing the images
img_path <- "/home/jupyter/332_data/data-for-332/grass/"

# Load all images in the folder into a list and resize them
img_list <- lapply(list.files(path = img_path, pattern = ".jpg", full.names = TRUE), function(x) {
  img <- image_read(x)
  img_noise <- image_noise(img, noisetype = "gaussian")
  img_background <- image_transparent(img_noise, "navyblue", fuzz = 50)
  img_resize <- image_resize(img_background, "500x500") # resize the image
  gc()
  img_resize
})

# Rotate each image
for (i in seq_along(img_list)) {
  img_list[[i]] <- image_rotate(img_list[[i]], 32)
}

# Save the transformed images to a new folder
for (i in seq_along(img_list)) {
  image_write(img_list[[i]], path = paste("/home/jupyter/332_data/data-for-332/grass_transformed/", i, ".jpg"))
}

# Set the path to the folder containing the images
img_path <- "/home/jupyter/332_data/data-for-332/dandelions/"

# Load all images in the folder into a list and resize them
img_list <- lapply(list.files(path = img_path, pattern = ".jpg", full.names = TRUE), function(x) {
  img <- image_read(x)
  img_noise <- image_noise(img, noisetype = "gaussian")
  img_background <- image_transparent(img_noise, "navyblue", fuzz = 50)
  img_resize <- image_resize(img_background, "500x500") # resize the image
  gc()
  img_resize
})

# Rotate each image
for (i in seq_along(img_list)) {
  img_list[[i]] <- image_rotate(img_list[[i]], 32)
}

# Save the transformed images to a new folder
for (i in seq_along(img_list)) {
  image_write(img_list[[i]], path = paste("/home/jupyter/332_data/data-for-332/dandelions_transformed/", basename(list.files(path = img_path, pattern = ".jpg", full.names = TRUE))[i]))
}

# Sanity check for modified grass
gm = list.files("/home/jupyter/332_data/data-for-332/grass_transformed/")
print ("These images are not grass: ")
count = 0
for (i in gm) {
  test_image <- image_load(paste("/home/jupyter/332_data/data-for-332/grass_transformed/",i,sep=""),target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1,dim(x)))
  x <- x/255
  pred <- model %>% predict(x)
  if(pred[1,2]<0.50){
    print(i)
    count = count + 1
  }
}
print(count)

# Sanity check for modified dandelions
dm = list.files("/home/jupyter/332_data/data-for-332/dandelions_transformed/")
print ("These images are not dandelions: ")
count = 0
for (i in dm) {
  test_image <- image_load(paste("/home/jupyter/332_data/data-for-332/dandelions_transformed/",i,sep=""),target_size = target_size)
  x <- image_to_array(test_image)
  x <- array_reshape(x, c(1,dim(x)))
  x <- x/255
  pred <- model %>% predict(x)
  if(pred[1,1]<0.50){
    print(i)
    count = count + 1
  }
}
print(count)



