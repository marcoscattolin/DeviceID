makeGroups <-function(dataset,verbose = F){
        
        L <- length(dataset[,1])
        
        groupList <- data.frame()
        groupID <- 1
        while(length(dataset[,1]) > 0){      #cicla fino a che ci sono elementi da estrarre
                
                if(verbose){
                        print((1-(length(dataset[,1])/L))*100)        
                }
                
                
                #estra la prima coppia dalla lista
                tmp <- dataset[1,]           
                dataset <- dataset[-1,]
                
                index <- 1
                while (sum(index) > 0){
                        #cerca corrispondenze
                        index1 <- which(dataset$dev %in% tmp$dev)
                        index2 <- which(dataset$usr %in% tmp$usr)
                        index <- union(index1,index2)
                        if(sum(index) > 0){
                                #estrai corrispondenze
                                tmp <- rbind(tmp,dataset[index,])
                                dataset <- dataset[-index,]
                        }
                }
                tmp$groupID <- groupID
                groupID <- groupID+1
                groupList <- rbind(groupList,tmp)
                
                
        }
        
        #rimuovi i duplicati e ritorna
        groupList <- groupList[!(duplicated(groupList)),]
        groupList
}


#read
data <- read.csv("./DeviceID.csv")

#extract only devices and users
subdata <- data[,c(1,2)]
colnames(subdata) <- c("dev","usr")

#run makeGroups function
subdata <- makeGroups(subdata,verbose=T)



#merge defining groupID
colnames(data) <- c("dev","usr","email","Cluster","ClusterLong","Canale","Subcanale")
grouped <- merge(data,subdata)


#identify VIP Groups
groupType <- dcast(grouped,groupID~ClusterLong)
groupType$groupType <- ifelse(groupType$SuperVIP > 0 | groupType$VIP > 0, "VIP Group","Client Group")
groupType <- groupType[,c("groupID","groupType")]
grouped <- merge(grouped,groupType)
