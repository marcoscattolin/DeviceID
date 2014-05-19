setHeap <- function(data){
        device_col <- 1
        user_col <- 2
        groupList <- list()
        for (i in 1:length(data[,1])){
                group <- list(device=data[i,device_col],user=data[i,user_col])
                groupList[i] <- list(group=group)
        }
        groupList
}
intersection <- function(group1,group2){
        x <- is.element(group1$device, group2$device)
        y <- is.element(group1$user, group2$user)
        x|y
}
mergeGroup <- function(groupList,i,j){
        x <- union(groupList[[i]]$device, groupList[[j]]$device)
        y <- union(groupList[[i]]$user, groupList[[j]]$user)
        groupList[[i]]$device <- x
        groupList[[i]]$user <- y
        groupList[[i]]
}



#generate data
a <- 1:20
dev <- paste0("Dev",a)
user <- paste0("Usr",a)
set.seed(123)
dev <- sample(dev,50, replace=T)
user <- sample(user,50, replace=T)
data <- data.frame(dev,user)

groupList <- setHeap(data)
newGroupList <- groupList

mergers <- 1
while(mergers > 0){
        mergers = 0
        for (i in 1:(length(groupList)-1)){
                for (j in (i+1):length(groupList)){
                        inters <- intersection(groupList[[i]],groupList[[j]])
                        if(inters){
                                newGroupList[[i]] <- mergeGroup(groupList,i,j)
                                newGroupList[[j]] <- NULL
                                mergers = 1
                        }
                        else{
                                newGroupList[[i]] <- groupList[[i]]
                                newGroupList[[j]] <- groupList[[j]]
                        }
                }
        }
        groupList <- newGroupList
}