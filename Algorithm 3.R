## -----------------------------------------------------------------------------
# Load libraries
library(imager)
library(magick)

# Set input/output directories
input_dir <- "./332_data/data-for-332/dandelions/"
output_dir <- "./332_data/data-for-332/mod_dandelions/"

files <- list.files(input_dir, full.names = TRUE)
out_files <- list.files(output_dir, full.names = TRUE)
for (i in 1:length(files)) {

# Load image
img <- load.image(files[i])

# Convert image to pixset and edit
px <- (isoblur(img,1)  > .4)
img <- colorise(img,px,"green",alpha=.5)

# Convert pixelset back to image
img <- cimg2magick(img)

# Retrieve filename
file_name <- tools::file_path_sans_ext(basename(files[i]))    
    
# Save image
image_write(img, path = paste0(output_dir, file_name, ".jpg"))
}

# Set input/output directories
input_dir <- "./332_data/data-for-332/grass/"
output_dir <- "./332_data/data-for-332/mod_grass/"

files <- list.files(input_dir, full.names = TRUE)
out_files <- list.files(output_dir, full.names = TRUE)
for (i in 1:length(files)) {

# Load image
img <- load.image(files[i])

# Covert image to pixeset and edit
px <- (isoblur(img,1)  > .4)
img <- colorise(img,px,"yellow",alpha=.5)

# Convert pixelset back to image
img <- cimg2magick(img)

# Retrieve filename
file_name <- tools::file_path_sans_ext(basename(files[i]))    
    
# Save image
image_write(img, path = paste0(output_dir, file_name, ".jpg"))
}

