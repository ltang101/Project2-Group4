library(pso)
library(reticulate)
library(magick)

# Set the input and output directories for dandelions
input_dir <- "~/Desktop/332 R/data-for-332/data-for-332/dandelions/"
output_dir <- "~/Desktop/332 R/data-for-332/data-for-332/dandelions/"

# Get a list of all image file names in the input directory
files <- list.files(input_dir, full.names = TRUE)


sigma = 1
Max_gen = 1000
num_var = 2
xx1 = runif(10,-5,5)
xx2 = runif(10,-5,5)
x0 = cbind(xx1,xx2) #initial solution
points(x0[,1],x0[,2],pch = 16,col = "blue")

gen = 1
while (gen <= Max_gen) {
  f0 = f(x0[,1],x0[,2]) #calculate fitness of initial solution
  
  randomness = rnorm(20,0,sigma)
  randomness = array(randomness,dim = c(10,2))
  y = x0 + randomness #mutation
  
  f1 = f(y[,1],y[,2]) #fitness of mutated generation
  
  x = rbind(x0,y)
  fval1 = cbind(f0,f1)
  fval1 = array(fval1, dim = c(20,1))
  
  Index = order(fval1) #get the index in increasing order
  
  x0 = x[Index[1:10],] #select the smallest 10 values
  gen = gen + 1
}
plot(x0[,1],x0[,2],pch=16,col="green")


