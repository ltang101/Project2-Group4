## -----------------------------------------------------------------------------
# Load libraries
library(imager)
library(magrittr)
library(nabor)
library(seriation)
library(magick)

# Set input/output directories
input_dir <- "./332_data/data-for-332/dandelions/"
output_dir <- "./332_data/data-for-332/mod_dandelions/"

scramble <- function(im,axis="x")
{
    imsplit(im,axis) %>% { .[sample(length(.))] } %>% imappend(axis) 
}

unscramble <- function(im.s,axis="x",method="TSP",...)
{
    cols <- imsplit(im.s,axis)
    #Compute a distance matrix (using L1 - Manhattan - distance)
    #Each entry D_ij compares column i to column j  
    D <- map(cols,as.vector) %>% do.call(rbind,.) %>% dist(method="manhattan")
    out <- seriate(D,method=method,...)
    cols[get_order(out)] %>% imappend(axis) 
}

files <- list.files(input_dir, full.names = TRUE)
out_files <- list.files(output_dir, full.names = TRUE)
for (i in 1:length(files)) {

# Load image
img <- load.image(files[i])

# Scramble and unscrable images
im.s <- scramble(img,"x")
    
out <- unscramble(im.s)

# Retrieve filename
file_name <- tools::file_path_sans_ext(basename(files[i]))    
    
# Save image
image_write(im.s, path = paste0(output_dir, file_name, ".jpg"))
}

# Switch input/output directories for grass
input_dir <- "./332_data/data-for-332/grass/"
output_dir <- "./332_data/data-for-332/mod_grass/"

files <- list.files(input_dir, full.names = TRUE)
out_files <- list.files(output_dir, full.names = TRUE)
for (i in 1:length(files)) {

# Load image
img <- load.image(files[i])

# Scramble and unscrable images
im.s <- scramble(img,"x")
    
out <- unscramble(im.s)

# Retrieve filename
file_name <- tools::file_path_sans_ext(basename(files[i]))    
    
# Save image
image_write(im.s, path = paste0(output_dir, file_name, ".jpg"))
}

