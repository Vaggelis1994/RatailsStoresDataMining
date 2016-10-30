
#############
## APRIORI ##
#############

require (arules)

#variables for indexing
columns <- ncol(data_binary)
products <- columns-5
basketStart <- columns-2
basketEnd <- columns

#View(data_binary[,2:products])
#View(data_binary[,basketStart:basketEnd])

#test apriori rule with different input values for minimum support
min <- c(0.001, 0.002, 0.005, 0.01, 0.02)#vector with minimum values for support 

#execute apriori with different values
for(m in min) {
  rules <- apriori(data.frame(data_binary[,2:products],data_binary[,basketStart:basketEnd]), parameter = list(minlen=2, supp=m, conf=1))
  inspect(rules)
}


#top-20 confidence for products only  
rules <- apriori(data_binary[,2:products], parameter = list(minlen=2, supp=0.001)) 
rules_sorted <- sort(rules, by="confidence")
inspect(rules_sorted[1:20])


#top-20 confidence for products AND basket value
rules <- apriori(data.frame(data_binary[,2:products],data_binary[,basketStart:basketEnd]), parameter = list(minlen=2, supp=0.001, conf=1))
rules_sorted <- sort(rules, by="confidence")
inspect(rules_sorted[1:20])

#check apriori parameter list documentation maxlen
