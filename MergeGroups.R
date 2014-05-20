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
        sum(x)|sum(y)
}
mergeGroup <- function(groupList,i,j){
        x <- union(groupList[[i]]$device, groupList[[j]]$device)
        y <- union(groupList[[i]]$user, groupList[[j]]$user)
        groupList[[i]]$device <- x
        groupList[[i]]$user <- y
        groupList[[i]]
}
generateData <- function(pop,density){
        #generate data
        a <- 1:pop
        dev <- paste0("Dev",a)
        user <- paste0("Usr",a)
        set.seed(123)
        dev <- sample(dev,density, replace=T)
        user <- sample(user,density, replace=T)
        data <<- data.frame(dev,user)
        
        groupList <- setHeap(data)
}
makeGroups <- function(groupList){
        mergers <- 1
        while (mergers > 0){
                mergers = 0
                for (i in 1:(length(groupList)-1)){
                        if((i+1) <= length(groupList)){
                                for (j in (i+1):length(groupList)){
                                        first <- groupList[[i]]
                                        second <- groupList[[j]]
                                        inters <- intersection(first,second)
                                        if(sum(inters) == 1){
                                                groupList[[i]] <- mergeGroup(groupList,i,j)
                                                groupList[[j]] <- NULL
                                                mergers = 1
                                                break
                                        }
                                }
                        } else{break}
                        if(mergers == 1){
                                break
                        }
                }
                
        } 
        groupList
}



data <- 0
pop <- 100
density <- 20
groupList <- generateData(pop,density)
newGroupList <- groupList
a <- makeGroups(groupList)
