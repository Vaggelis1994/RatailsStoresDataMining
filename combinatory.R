
#################
## COMBINATORY ##
#################

#variables for indexing
columns <- ncol(data_binary)
productEnd <- columns-10
clusterStart <- columns-4
clusterEnd <- columns
basketStart <- columns-7
basketEnd <- columns-5

#View(data_binary[,2:productEnd])
#View(data_binary[,clusterStart:clusterEnd])
#View(data_binary[,basketStart:basketEnd])

#top-20 confidence for products and clusters
rules <- apriori(data.frame(data_binary[,2:products],data_binary[,clusterStart:clusterEnd]), parameter = list(minlen=2, supp=0.005, conf=1))
rules_sorted <- sort(rules, by="confidence")
if(length(rules_sorted)>=20) {
  inspect(rules_sorted[1:20])
}
  
#top-20 confidence for products and clusters
rules <- apriori(data.frame(data_binary[,2:products],data_binary[,clusterStart:clusterEnd],data_binary[,basketStart:basketEnd]), parameter = list(minlen=2, supp=0.01, conf=1))
rules_sorted <- sort(rules, by="confidence")
if(length(rules_sorted)>=20) {
  inspect(rules_sorted[1:20])
}