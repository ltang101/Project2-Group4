## -----------------------------------------------------------------------------
# Load the packages
library(imager)
library(magick)

# Set the input and output directories for dandelions
input_dir <- "./332_data/data-for-332/dandelions/"
output_dir <- "./332_data/data-for-332/mod_dandelions/"

# Get a list of all image file names in the input directory
files <- list.files(input_dir, full.names = TRUE)
for (i in 1:length(files)) {

# Load the image
img <- image_read(files[i])

# Modify the image
img <- image_modulate(img, brightness = 40, saturation = 500, hue = 25)
img <- image_charcoal(img)
    
# Get the file name without the directory path
file_name <- tools::file_path_sans_ext(basename(files[i]))

# Save the modified image to the output directory with the same file name
image_write(img, path = paste0(output_dir, file_name, ".jpg"))
}

# Set the input and output directories for grass
input_dir <- "./332_data/data-for-332/grass/"
output_dir <- "./332_data/data-for-332/mod_grass/"

# Get a list of all image file names in the input directory
files <- list.files(input_dir, full.names = TRUE)
for (i in 1:length(files)) {

# Load the image
img <- image_read(files[i])

# Modify the image
img <- image_modulate(img, brightness = 40, saturation = 500, hue = 25)
img <- image_charcoal(img)
    
# Get the file name without the directory path
file_name <- tools::file_path_sans_ext(basename(files[i]))

# Save the modified image to the output directory with the same file name
image_write(img, path = paste0(output_dir, file_name, ".jpg"))
}

