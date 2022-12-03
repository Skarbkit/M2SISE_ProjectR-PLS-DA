# Stratified Split on train/test datasets based on caret package

library(caret)

split_train_test = function(data, class, p){
  train.index = createDataPartition(data[,class], p, list = FALSE, times = 1)
  
  train = data[train.index,]
  test = data[-train.index,]
  
  # Return
  
  res.split = list("Train" = train,
                   "Test" = test,
                   "Split" = p,
                   "Class.names"= levels(data[,class]),
                   "Class.prop"=prop.table(table(data[,class]))
                   
                   
  )
}

#print(split_train_test(iris,class="Species",p=0.7))
