wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd <- wbcd[-1]
wbcd$diagnosis <- factor(wbcd$diagnosis, levels =  c("B","M"),
                         labels = c("Benign", "Malign"))

normalize <- function(x) {
  (x-min(x)) / (max(x)-min(x))
}

wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize))
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_lables <- wbcd[470:569, 1]
test <- wbcd_test[9,]

minkowski_distance = function(a,b,p){
  #Manhattan if p =1, Euclidian if p=2
  if(p<=0){
    stop('p must be higher than 0') 
  }
  
  if(length(a)== length(b)){
    sum(abs(a-b)^p)^(1/p)
  }else{
    stop('Vectors must be of the same length')
    
  }
}

k = 5

dists = apply(wbcd_train,1, minkowski_distance,test,2)
distances = sort(dists)[1:k]
neighbor_ind = which(dists %in% distances)

pred_k <- wbcd_train_labels[neighbor_ind]

#simplification en oublions le cas 50:50 entre B:M
if (sum(as.numeric(pred_k)) >= (1+k) ) {
  print("Malign")
} else {
  print("Begign")
}