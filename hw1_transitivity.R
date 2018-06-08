#Link prediction 
#Social Network Media Analytics 
#HW1 

library(igraph)
library(csv)

#read csv
node <- read.csv("node_information.csv", header = F)          
testDatalink <- read.csv("TestData.csv", header = F)          
link <- read.csv("Period1+2.csv", header = F)    

#draw the graph
Allpic <- graph_from_data_frame(d = link, vertices = node, directed = T) 

#transitivity function
t1 <- transitivity(Allpic,type = "weighted")                  

temp<-1    
while(temp<=10000){ #find the node which is in testdata and map to node_information's index
  index1 = findInterval(testDatalink[temp,1], node[,1])  #source vertex
  index2 = findInterval(testDatalink[temp,2], node[,1])  #target vertex
  
  #source node's weight +  target node's weight
  temp_v1 <- t1[index1] =="NaN"        
  temp_v2 <- t1[index2] =="NaN"
  if(temp_v1 != TRUE){
    if(temp_v2 != TRUE){
      testDatalink[temp,4] <- (t1[index1] + t1[index2])
    }
    else if (temp_v2 == TRUE){
      testDatalink[temp,4] <- t1[index1]
    }
  }
  if(temp_v1 == TRUE){
    if(temp_v2 !=TRUE){
      testDatalink[temp,4] <- t1[index2]
    }
  }
  if(temp_v1 == TRUE && temp_v2 ==TRUE){
    testDatalink[temp,4] <- 0
  }
  temp <- temp+1
}

#output csv
write.table(testDatalink, file = "myResult.csv", sep = ",")