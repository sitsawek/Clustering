dk = read.csv("dailykos.csv")
str(dk)
distance = dist(dk,method = "euclidean")
cluster = hclust(distance,method = "ward.D")
plot(cluster)
rect.hclust(cluster, k = 5,border = "red")
summary(dk_vector)
n = 5300895
n*(n-1)/2
