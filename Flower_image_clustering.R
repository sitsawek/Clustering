flower = read.csv("flower.csv",header = FALSE)
str(flower)
# Change data type to matrix.
flower_matrix = as.matrix(flower)
str(flower_matrix)
# Before compute we need to change matrix to vector.
flower_vector = as.vector(flower_matrix)
str(flower_vector)
# Now ready to start our hierarchical clustering.
# First create distance this case computes the difference between every two intensity values in our flower vector.
distance = dist(flower_vector,method = "euclidean")
# Create hierachical clustering.
cluster_intensity = hclust(distance,method = "ward")
# The ward method is minimum variance which try to find compact and spherical clusters.
# We can think about it as trying to minimize the variance within each cluster and distance among clusters.
# Now we can plot the cluster dedrogram.
plot(cluster_intensity)
# In this case we can cut between levels 2 and 3 ,3 and 4, 4 and 5.
# Then 3 cluster is reasonable in this case.
# Now visualize the cuts by plotting rectangles around the clusters on this tree.
rect.hclust(cluster_intensity, k = 3,border = "red")
# Now split data into these 3 clusters.
flower_clusters = cutree(cluster_intensity, k = 3)
# This function cuts the dendrogram into however many cluster we want.
# Now let's see flower_clusters look like.
flower_clusters
# To find mean intensity value of each of our clusters.
tapply(flower_vector,flower_clusters,mean)
# Let see how image was segmented to output an image by convert to matrix.
dim(flower_clusters) = c(50,50)
image(flower_clusters,axes = FALSE)
# Let check how original image look like.
image(flower_matrix,axes = FALSE,col = gray(seq(0,1,length=256)))
