## -----------------------------------------------------------------------------
# One pixel attack concept

# Load libraries
library(imager)

# Load the images from the dandelions folder
path <- "./332_data/data-for-332/dandelions/"
files <- list.files(path, full.names = TRUE)
images <- lapply(files, load.image)

# Define the target class for the attack
target_class <- 1 # For example, let's say we want to turn all dandelions into roses

# Define the pixel budget for the attack
pixel_budget <- 0.01 # Let's say we want to modify only 1% of the pixels

# Define the number of iterations for the attack
num_iterations <- 50

# Define the optimizer function for the attack
optimizer <- function(img, pixels_to_modify) {
  # For the One Pixel Attack, we modify only one pixel
  # Choose the pixel that maximizes the attack success rate
  best_pixel <- NULL
  best_score <- -Inf
  for (pixel in pixels_to_modify) {
    for (c in 1:3) {
      # Try changing the color of the pixel to each possible color value
      for (i in 0:255) {
        img_mod <- img
        img_mod[pixel[1], pixel[2], c] <- i
        score <- predict(model, img_mod)
        if (score > best_score) {
          best_score <- score
          best_pixel <- c(pixel[1], pixel[2], c, i)
        }
      }
    }
  }
  # Return the modified image and the pixel that was changed
  img_mod <- img
  img_mod[best_pixel[1], best_pixel[2], best_pixel[3]] <- best_pixel[4]
  list(img_mod = img_mod, pixel_changed = best_pixel[1:3])
}

# Define the prediction function for the attack
predict <- function(model, img_mod) {
  # Preprocess the image and make a prediction using the model
  img_mod <- resize(img_mod, c(224, 224))
  img_mod <- img_mod / 255
  pred <- predict(model, img_mod)
  # Return the probability of the target class
  pred[target_class]
}

# Load the pre-trained model for the One Pixel Attack
model <- load_model_hdf5("one_pixel_model.h5")

# Apply the One Pixel Attack to each image
for (i in 1:length(images)) {
  img <- images[[i]]
  # Get the indices of the pixels to modify based on the pixel budget
  n_pixels <- prod(dim(img)[1:2])
  pixels_to_modify <- sample(n_pixels, round(n_pixels * pixel_budget))
  pixels_to_modify <- cbind(pixels_to_modify %% dim(img)[1] + 1, floor(pixels_to_modify / dim(img)[1]) + 1)
  # Perform the attack for the specified number of iterations
  for (j in 1:num_iterations) {
    # Use the optimizer function to modify the image
    attack_result <- optimizer(img, pixels_to_modify)
    img_mod <- attack_result$img_mod
    # Check if the attack was successful
    if (predict(model, img_mod) > 0.5) {
      # Save the modified image to the mod_dandelions folder
      filename <- paste0("./332_data/data-for-332/mod_dandelions/", i, "_mod.jpg")
      save.image(img_mod, filename)
      break
   


## -----------------------------------------------------------------------------
# Basic image modification code concept

# Load the imager package
library(imager)

# Set the input and output directories
input_dir <- "./332_data/data-for-332/dandelions/"
output_dir <- "./332_data/data-for-332/mod_dandelions/"

# Get a list of all image file names in the input directory
files <- list.files(input_dir, full.names = TRUE)

# Loop through each file and modify the image
for (i in 1:length(files)) {
  # Load the image
  img <- load.image(files[i])
  
  # Apply a Gaussian blur filter to the image to smooth out edges
  img_blur <- blur(img, sigma = 10, boundary = "wrap")
  
  # Add random noise to the image to create a more complex pattern
  img_noise <- img_blur + runif(img_blur)
  
  # Scale the image down to a smaller size to make it harder for the image detection model to detect details
  img_scale <- resize(img_noise, scale = 0.5)
  
  # Get the file name without the directory path
  file_name <- tools::file_path_sans_ext(basename(files[i]))
  
  # Save the modified image to the output directory with the same file name
  save.image(img_scale, file.path(output_dir, paste0(file_name, ".jpg")))
}


## -----------------------------------------------------------------------------
# C&W Attack concept

# Load libraries
library(keras)
library(tidyverse)

# Load the pre-trained image classification model
model <- load_model_hdf5('path/to/model.h5')

# Set the attack parameters
max_iterations <- 1000
learning_rate <- 0.1
confidence <- 0.0

# Define the C&W attack function
cw_attack <- function(image, label, model, max_iterations, learning_rate, confidence) {
  # Convert the image to a tensor
  image_tensor <- array_reshape(image, c(1, dim(image)))
  
  # Define the loss function
  loss_fn <- function(x) {
    predictions <- predict(model, x)
    max_other <- apply(predictions - diag(diag(predictions)), 1, max)
    max_target <- predictions[label + 1]
    margin_loss <- pmax(0, max_other - max_target + confidence)
    l2_loss <- sum((x - image_tensor) ^ 2)
    return(mean(margin_loss) + l2_loss)
  }
  
  # Define the gradient function
  grad_fn <- function(x) {
    grad <- keras::gradients(loss_fn, x)[[1]]
    return(grad)
  }
  
  # Initialize the perturbation
  perturbation <- array(0, dim(image))
  
  # Run the attack
  for (i in 1:max_iterations) {
    # Compute the adversarial image
    adv_image <- pmax(0, pmin(255, image + perturbation))
    
    # Compute the gradient of the loss function with respect to the image
    gradient <- grad_fn(array_reshape(adv_image, c(1, dim(adv_image))))
    
    # Update the perturbation using the gradient ascent rule
    perturbation <- perturbation + learning_rate * sign(gradient)
    perturbation <- pmax(pmin(perturbation, 255 - image), -image)
  }
  
  # Compute the adversarial image
  adv_image <- pmax(0, pmin(255, image + perturbation))
  
  return(adv_image)
}

# Define the image directory and output directory
image_dir <- './332_data/data-for-332/dandelions/'
output_dir <- './332_data/data-for-332/mod_dandelions/'

# Get a list of all image files in the directory
image_files <- list.files(image_dir)

# Loop over all image files
for (file in image_files) {
  # Load the image
  image <- image_load(file.path(image_dir, file))
  
  # Preprocess the image
  image <- image_preprocess(image, target_size = c(224, 224))
  
  # Get the true label of the image
  true_label <- which.max(predict(model, image)[1, ])
  
  # Generate the adversarial image using the C&W attack
  adv_image <- cw_attack(image, true_label, model, max_iterations, learning_rate, confidence)
  
  # Save the adversarial image to the output directory
  basename <- tools::file_path_sans_ext(file)
  extension <- tools::file_ext(file)
  output_file <- paste0(basename, '_adv', '.', extension)
  output_path <- file.path(output_dir, output_file)
  image_save(adv_image, output_path)
}


## -----------------------------------------------------------------------------
# FGSM concept

# Load libraries
library(keras)
library(tidyverse)

# Define FGSM attack function
fgsm_attack <- function(image, epsilon) {
  # Compute gradients of loss w.r.t. input image
  image_gradient <- k_gradient(model, image)
  # Compute sign of gradients
  sign_gradient <- sign(image_gradient)
  # Add small perturbation to image
  perturbed_image <- image + epsilon * sign_gradient
  # Clip values to [0, 1] range
  perturbed_image <- pmax(pmin(perturbed_image, 1), 0)
  return(perturbed_image)
}

# Define path to input directory
input_dir <- "./332_data/data-for-332/dandelions/"

# Define path to output directory
output_dir <- "./332_data/data-for-332/mod_dandelions/"

# Define epsilon value for FGSM attack
epsilon <- 0.1

# Iterate over input images and generate adversarial images
list.files(input_dir) %>%
  walk(function(file_name) {
    # Load input image
    image <- image_load(file.path(input_dir, file_name))
    # Convert image to array
    image_array <- image_to_array(image)
    # Apply FGSM attack to image
    perturbed_image <- fgsm_attack(image_array, epsilon)
    # Convert perturbed image back to Keras image format
    perturbed_image <- array_to_image(perturbed_image, data_format = "channels_last", scale = TRUE)
    # Save perturbed image to output directory
    output_file_name <- paste0("mod_", file_name)
    image_save(perturbed_image, file.path(output_dir, output_file_name))
  })

