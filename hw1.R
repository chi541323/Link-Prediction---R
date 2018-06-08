library(igraph)
library(csv)
library(RcppZiggurat)
library(dplyr)

node <- read.csv("node_information.csv", header = F)
p1link <- read.csv("Period1.csv", header = F)
p2link <- read.csv("Period2.csv", header = F)
testDatalink <- read.csv("TestData.csv", header = F)
node[,2] <-0


######################## P1有的點 ########################
i<-1
while(i<=154836){
p1_v1 <- p1link[i,1]
p1_v2 <- p1link[i,2]


sv_InP1 <- binary_search(node[,1],p1_v1)  # period1的source vertex若在node內，則b=true
tv_InP1 <- binary_search(node[,1],p1_v2) # period1的target vertex若在node內，則b=true


if(sv_InP1 == TRUE){
   index = findInterval(p1link[i,1], node[,1]) #找source vertex在node內是第幾個node，丟入index
   node[index,2] <- 1
}
if(tv_InP1 == TRUE){
  index = findInterval(p1link[i,2], node[,1])  #找target vertex在node內是第幾個node，丟入index
  node[index,2] <- 1
}


i <- i+1
}

######################## P2有的點 ########################
i<-1
while(i<=98353){
  p2_v1 <- p2link[i,1]
  p2_v2 <- p2link[i,2]
  sv_InP2 <- binary_search(node[,1],p2_v1)  # period2的source vertex若在node內，則b=true
  tv_InP2 <- binary_search(node[,1],p2_v2) # period2的target vertex若在node內，則b=true
  if(sv_InP2 == TRUE){
    index = findInterval(p2link[i,1], node[,1]) #找source vertex在node內是第幾個node，丟入index
    node[index,2] <- 1
  }
  if(tv_InP2 == TRUE){
    index = findInterval(p2link[i,2], node[,1])  #找target vertex在node內是第幾個node，丟入index
    node[index,2] <- 1
  }
  i <- i+1
}

######################## testdata有的點 ########################
i<-1
while(i<=10000){
  t_v1 <- testDatalink[i,1]
  t_v2 <- testDatalink[i,2]
  sv_Intest <- binary_search(node[,1],t_v1)  # testdata的source vertex若在node內，則b=true
  tv_Intest <- binary_search(node[,1],t_v2)  # testdata的target vertex若在node內，則b=true
  if(sv_Intest == TRUE){
    index = findInterval(testDatalink[i,1], node[,1]) #找source vertex在node內是第幾個node，丟入index
    node[index,2] <- 1
  }
  if(tv_Intest == TRUE){
    index = findInterval(testDatalink[i,2], node[,1])  #找target vertex在node內是第幾個node，丟入index
    node[index,2] <- 1
  }
  i <- i+1
}

node <- node %>% filter(!V2==0)

write.table(node, file = "node.csv", sep = ",")

pic <- graph_from_data_frame(d = p1link, vertices = node, directed = T)

temp <- 1
while(temp<=98353){
source_ver = findInterval(p2link[temp,1], node[,1]) #找source vertex在node內是第幾個node，丟入index
target_ver = findInterval(p2link[temp,2], node[,1])  #找target vertex在node內是第幾個node，丟入index
p2link[temp,3] <- distances(pic,source_ver,target_ver)
temp = temp+1
}

link <- read.csv("Period1+2.csv", header = F)
Allpic <- graph_from_data_frame(d = link, vertices = node, directed = T)

temp <- 1
while(temp<=10000){
  source_ver = findInterval(testDatalink[temp,1], node[,1]) #找source vertex在node內是第幾個node，丟入index
  target_ver = findInterval(testDatalink[temp,2], node[,1])  #找target vertex在node內是第幾個node，丟入index
  testDatalink[temp,3] <- distances(Allpic,source_ver,target_ver)
  temp = temp+1
}


write.table(testDatalink, file = "myResult.csv", sep = ",")

result <- read.csv("myResult.csv", header = F)
result[,2] <- result[,3]
write.table(result, file = "myResult.csv", sep = ",")
t1 <- transitivity(Allpic,type = "weighted")
Allpic

i<-1
while(i<=10000){
  index1 = findInterval(testDatalink[i,1], node[,1]) #找source vertex在node內是第幾個node，丟入index
  index2 = findInterval(testDatalink[i,2], node[,1])  #找target vertex在node內是第幾個node，丟入index
  temp_v1 <- t1[index1] =="NaN"
  temp_v2 <- t1[index2] =="NaN"
  if(temp_v1 != TRUE){
   if(temp_v2 != TRUE){
      testDatalink[i,4] <- (t1[index1] + t1[index2])
   }
    else if (temp_v2 == TRUE){
      testDatalink[i,4] <- t1[index1]
    }
  }
  if(temp_v1 == TRUE){
    if(temp_v2 !=TRUE){
      testDatalink[i,4] <- t1[index2]
    }
  }
  if(temp_v1 == TRUE && temp_v2 ==TRUE){
    testDatalink[i,4] <- 0
  }
  
  i <- i+1
}

write.table(testDatalink, file = "myResult.csv", sep = ",")
result <- read.csv("myResult.csv", header = F)
result[,2] <- result[,3]
write.table(result, file = "myResult.csv", sep = ",")
