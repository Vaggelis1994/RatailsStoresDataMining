
#################
## PREPARATION ##
#################


binarize <-function(data_columns,extra_columns=NULL){
  
  column_names <- levels(unlist(data_columns))
  blank <- which(column_names == "")
  if (length(blank) !=0)
    column_names <- column_names[-c(blank)]
  
  binary_result <- as.data.frame(t(apply(data_columns,1,  function(x) column_names %in% as.character(unlist(x)))))
  names(binary_result) <- column_names
  if (is.null(extra_columns)==FALSE)
    binary_result<- cbind(extra_columns,binary_result)
  return(binary_result)
}


data_raw <- read.csv(file="GroceriesInitial.csv", header=TRUE, sep=",")#read raw data from file

important <- c("citrus fruit", "tropical fruit", "whole milk", "other vegetables", "rolls/buns", "chocolate", "bottled water", "yogurt", "sausage", "root vegetables", "pastry", "soda", "cream cheese ")

product_names <- levels(unlist(data_raw[,4:35]))
blank <- which(product_names == "") 
product_names <- product_names[-c(blank)]

products <- as.data.frame(t(apply(data_raw[,4:35], 1, function(x) 
  (important) %in% as.character(unlist(x)))))
#keep only the useful products

data_discrete <- data_raw
cut_points <- quantile(data_discrete$basket_value, probs=c(0, 0.33, 0.66, 1), na.rm=TRUE, names=FALSE)
data_discrete$basket_value_bin <- cut(data_discrete$basket_value, breaks=cut_points, labels=c("Low", "Medium", "High"), include.lowest=TRUE) 
data_discrete <- binarize(as.data.frame(data_discrete$basket_value_bin), data_discrete)
#discreted data on basket values

data_binary <- data.frame(as.integer(data_raw$id), products, data_raw$basket_value, as.integer(data_raw$recency_days), data_discrete$Low, data_discrete$Medium, data_discrete$High)
names <- c("id", "citrus.fruit", "tropical.fruit", "whole.milk", "other.vegetables", "rolls.buns", "chocolate", "bottled.water", "yogurt", "sausage", "root.vegetables", "pastry", "soda", "cream.cheese", "basket_value", "recency_days", "low_value_basket", "medium_value_basket", "high_value_basket")
colnames(data_binary) <- names
#reform the used data frame

##FOR CHECK

#new data frame for output only!
#View(data_binary)
#data_final[data_final == TRUE] <- "Yes" 
#data_final[data_final == FALSE] <- ""
#write.csv(data_final, "GroceriesUpdated.csv", row.names=FALSE) #"GroceriesProcessed.csv" 
#data_updated <- read.csv(file="GroceriesUpdated.csv", header=TRUE, sep=",")
#View(data_updated)
#data_processed <- read.csv(file="GroceriesProcessed.csv", header=TRUE, sep=",")
#View(data_processed)
#data_binary <- read.csv("GroceriesProcessed.csv", header=TRUE, sep=",")



