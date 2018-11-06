movies = read.table("MovieLens.txt",header = FALSE,sep ="|",quote = "\"")
str(movies)
# Labeled name to data set by add the column names.
colnames(movies) = c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknow","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")
str(movies)
# We won't be using the ID,release date,video release date, or IMDB variables,
# Now remove them.
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
# Remove them by unique function.
movies = unique(movies)
str(movies)
# How many movies are classified as comedies?
table(movies$Comedy)
# How many movies are classified as westerns?
table(movies$Western)
# How many movies are classified as romance and drama?
table(movies$Romance,movies$Drama)
# Use hierarchical clustering to cluster the movies in the movies lens data set by genre.
# There are two step to hierarchical clustering.
# First we have to compute distance between all data.
# And then cluster the point.
# Cluster by only genre variable not title vairable.
# We'll cluster on 2 to 20.
distances = dist(movies[2:20],method = "euclidean")
# Use method of euclidean distance.
# Now hierarchical cluster by hclust function.
cluster_movies = hclust(distances,method = "ward.D")
# Plot dendrogram of out clustering algorithm.
plot(cluster_movies)
# How many cluster would you pick.
# It look like three or four.
# To pick the number of clusters let's stick with 10 cluster for now.
cluster_groups = cutree(cluster_movies, k = 10)
# Use tapply to compute percentage of movies in each genre and cluster.
tapply(movies$Action,cluster_groups,mean)
# It divides our data points into 10 cluster and then compute average value of the action variable for each cluster.
tapply(movies$Romance,cluster_groups,mean)
# Let's figure out what cluster Men in Black is in?
subset(movies, Title == "Men in Black (1997)")
# It look like "Men in Black (1997) is 257th row.
cluster_groups[257]
# It look like "Men in Black (1997) went into cluster 2.
# Now create a new data set with just the movies from cluster 2.
cluster2 = subset(movies,cluster_groups == 2)
cluster2$Title[1:10]
# Now create the cluster groups but this time pick k = 2 clusters.
cluster_groups2 = cutree(cluster_movies, k = 2)
tapply(movies$Drama,cluster_groups2,mean)
colMeans(subset(movies[2:20], cluster_groups2 == 2))
