
############
## KMEANS ##
############

#View(data_binary[,15:16])

library('scales')

clusters <- kmeans(data_binary[,15:16], 5)
print(clusters)

centers <- data.frame(clusters$centers,percent((clusters$size)/sum(clusters$size)))
colnames(centers) <- c("mean_basket_value", "mean_recency_days", "percentage_to_the_whole")
rownames(centers) <- c("Cluster0", "Cluster1", "Cluster2", "Cluster3", "Cluster4")
print(centers)

cluster_columns <- unlist(clusters$cluster)
data_binary <- data.frame(data_binary, cluster_columns)
data_discrete <- data_binary
data_discrete$cluster_columns_bin <- cut(data_discrete$cluster_columns, breaks=c(0, 1, 2, 3, 4, 5), labels=c("Cluster0", "Cluster1", "Cluster2", "Cluster3", "Cluster4"), include.lowest=TRUE) 
data_discrete <- binarize(as.data.frame(data_discrete$cluster_columns_bin), data_discrete)
#binarize the variable of clusters

#View(data_discrete)

data_binary <- data.frame(data_binary[,1:19], data_discrete[,22:26])
#reform the used data frame

