healthy = read.csv("healthy.csv",header = FALSE)
# Build data set as matrix.
healthy_matrix = as.matrix(healthy)
str(healthy_matrix)
# See MRI by gray.
image(healthy_matrix,axes = FALSE ,col = gray(seq(0,1,length = 256)))
# Before compute we build to vector.
healthy_vector = as.vector(healthy_matrix)
str(healthy_vector)
# !!!!Beware!!!!!
# !!!Don't compute dist function on this data set in local computer!!!.
# Because image have high resolution.
# Compute distance matrix.
# distance = dist(healthy_vector,method = "euclidean")
# Because this picture make vector equal 365,636 as n some time it make hour to compute by dist.
# Now we calculate pairwise distances by n(n-1)/2
n = 365636
n*(n-1)/2
# We can see 67 billion values that were asking R to store in a matrix.
# It bad new to use hierachical clustering.
# Another choice we need to build clustering by k-mean nearest.
# Our clusters would ideally assign each point in the image to a tissue class.
k = 5
set.seed(1)
# We need to set maximize iteration.
KMC = kmeans(healthy_vector,centers = k,iter.max = 1000)
str(KMC)
# We can see on cluster is 1-5.
# We need to extract this vector.
healtht_clusters = KMC$cluster 
# Can we obtain the mean intensity value within each 5 cluster.
# In this case already in centers.
KMC$centers[2]
# We can see size 133162 and why intensity is 0.0196?
# The smallest mean intensity value which means that it corresponds to the darkest shade in out image.
# We can see that they are all less than 0.5 it pretty close to 0 and this means that our images is pretty dark.
# Now convert vector to matrix.
dim(healtht_clusters) = c(nrow(healthy_matrix),ncol(healthy_matrix))
# Now visualize our cluster by function image.
image(healtht_clusters,axes = FALSE,col = rainbow(k))
#--------------------------------------------------
tumor = read.csv("tumor.csv",header = FALSE)
tumor_matrix = as.matrix(tumor)
str(tumor_matrix)
image(tumor_matrix,axes = FALSE,col = gray(seq(0,1,length = 256)))
tumor_vector = as.vector(tumor_matrix)
# We treat healthy vector as training set and tumor vector as test set.
library(flexclust)
# Flexclust packages contains object class KCCA Stand for k-crntroids cluster analysis.
# We need to convert the information from the clustering algorithm ta an object of the class KCCA.
# And this conversion is needed before we can use the predict function on the test set tumor_vector.
KMC.kcca = as.kcca(KMC,healthy_vector)
# The first inpus as original KMC variable that stored all the information from k-means clustering function.
# Second input is the data that we clustered.
# We can cluster the picxels in the tumor_vector using predict function.
tumor_clusters = predict(KMC.kcca,newdata = tumor_vector)
# Convert tumor_vector to matrix.
dim(tumor_clusters) = c(nrow(tumor_matrix),ncol(tumor_matrix))
# 
image(tumor_clusters,axes = FALSE,col = rainbow(k))
