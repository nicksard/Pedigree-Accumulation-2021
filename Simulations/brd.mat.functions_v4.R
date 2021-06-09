
#moms = 7;dads = 3;lambda.low = 1;lambda.high = 1

brd.mat <- function(moms = 10,dads = 10,lambda.low = 1,lambda.high = 1){
  
  #making a blank matrix to be filled in
  mat <- matrix(data = 0,nrow = dads,ncol = moms)
  mat
  
  #picking a lambda mates
  #I have a range here because maybe the mean number of mates is unknown, thus you can randomly draw from
  #some uniform prior distribution
  lambda.mates <- sample(lambda.low:lambda.high,size=1)
  
  #defining mate.probs for dads using a poisson distribution
  possible.dad.mates <- 1:dads
  df <- data.frame(mates = possible.dad.mates)
  suppressWarnings(df$prob <- (lambda.mates^df$mates)*(exp(-lambda.mates))/factorial(df$mates))
  #when the number of parents gets really large and lambda stays small the probability becomes so small it biologically equivilant
  #to zero, which is why I removed them here.
  dad.mate.probs <- df[df$prob !=0 & !is.na(df$prob),]
  dad.mate.probs
  
  #defining mate.probs for moms using a poisson distribution
  possible.mom.mates <- 1:moms
  df <- data.frame(mates = possible.mom.mates)
  suppressWarnings(df$prob <- (lambda.mates^df$mates)*(exp(-lambda.mates))/factorial(df$mates))
  #when the number of parents gets really large and lambda stays small the probability becomes so small it biologically equivilant
  #to zero, which is why I removed them here.
  mom.mate.probs <- df[df$prob !=0 & !is.na(df$prob),]
  mom.mate.probs
  
  #creating a vector to work through systematically to:
  #1) Identify sex and then within a series of for loops
  #2) Determine the number of mates for that individual based on  a poisson distribution 
  #3) Determine if that individual has mated already with someone and account for those mates in step 4
  #4) Randomly identify individuals of the opposite sex to mate with, accounting for individuals identified in step 3
  
  #making the vector first
  mom.order <- paste(1:moms,"Female",sep="_")
  dad.order <- paste(1:dads,"Male",sep="_")
  #doing it systematically now
  #parent.order <- sort(c(mom.order,dad.order))
  #parent.order
  #doing it ranomdly later
  parent.order <- sample(x = c(mom.order,dad.order),size = moms+dads,replace = F)
  parent.order
  
  #making it into a df, so that I can get some stats back
  tmp <- data.frame(parent = parent.order, stringsAsFactors = F)
  tmp$sex <- ifelse(grepl(pattern = "Female",x = tmp$parent),"Female","Male")
  tmp$number <- as.numeric(gsub(pattern = "_.*",replacement = "",x = tmp$parent))
  tmp$mates_before <- NA
  tmp

  #now the for loops
  
  i <- 9
  tmp$parent[i]
  i <- NULL
  for(i in 1:nrow(tmp)){
#  for(i in 1:6){    
    #print statement to find errors
    #print(tmp$parent[i])
    
    #using logic to determine the sex of the parent
    if(grepl(pattern = "Female",x = tmp$parent[i])){
      
      #clearing things out for moms
      my.col <- NULL
      mates <- NULL
      my.rows <- NULL
      pdm2 <- NULL
      mates2 <- NULL
      cml2 <- NULL
      rem.mates <- NULL
      
      #for moms identifying the column
      my.col <- tmp$number[i]
      my.col
      
      #identifying the number of mates for a given parent
      mates <- sample(dad.mate.probs$mates,size = 1,prob = dad.mate.probs$prob)
      mates
      
      #filling in some summary information
      tmp$mates_before[i] <- mates
      tmp
      
      #identifying mates already acquired
      current.mates.list <- which(mat[,my.col]==1)
      current.mates.list
      
      possible.male.males <- tmp[tmp$sex == "Male",]
      possible.male.males$mate_prob <- 0
      possible.male.males  
      if(length(current.mates.list) == 0){
        
        #now identifying new mates and filled with a 1
        my.rows <- sample(x = possible.dad.mates,size = mates)
        my.rows
        mat[my.rows,my.col] <- 1
        mat
        
      } else {
      
        if(length(current.mates.list) < mates){
          
          #getting a subsetted list to sample from 
          pdm2 <- possible.dad.mates[!(possible.dad.mates %in% current.mates.list)]
          pdm2
          
          #modifying the number of mates to select
          mates2 <- mates - length(current.mates.list)
          mates2
          
          #now identifying new mates and filled with a 1
          my.rows <- sample(x = pdm2,size = mates2)
          mat[my.rows,my.col] <- 1
          mat
          
        } #end of if statement when current.mates < mates
        
        if(length(current.mates.list) > mates){
          
          #picking some number of mates that matches the mates drawn
          cml2 <- sample(x = current.mates.list,size = mates)
          cml2
          
          #identifying those to be removed
          rem.mates <- current.mates.list[!(current.mates.list %in% cml2)]
          
          #removing them
          mat[rem.mates,my.col] <-  0

          #at this point I am just going to do nothing, but I could 
          #randomly down-sample mates if need be
          
        } #end of if statement when the current mates > number of mates
 
      
      } #end of mate selection if statement

      
    } else {
      
      #clearing things out for dads
      my.row <- NULL
      mates <- NULL
      my.cols <- NULL
      pmm2 <- NULL
      mates2 <- NULL
      cml2 <- NULL
      rem.mates <- NULL
      
      #for dads identifying the row
      my.row <- as.numeric(gsub(pattern = "_.*",replacement = "",x = parent.order[i]))
      my.row
      
      #identifying the number of mates for a given parent
      mates <- sample(mom.mate.probs$mates,size = 1,prob = mom.mate.probs$prob)
      mates
      
      #filling in some summary information
      tmp$mates_before[i] <- mates
      tmp
      mat
      #identifying mates already acquired
      current.mates.list <- which(mat[my.row,]==1)
      current.mates.list
      
      if(length(current.mates.list) == 0){
        
        #now identifying new mates and filled with a 1
        my.cols <- sample(x = mom.mate.probs$mates,size = mates)
        mat[my.row,my.cols] <- 1
        mat
        
      } else {
      
         if(length(current.mates.list) < mates){
           
           #getting a subsetted list to sample from 
           pmm2 <- possible.mom.mates[!(possible.mom.mates %in% current.mates.list)]
           pmm2
           
           #modifying the number of mates to select
           mates2 <- mates - length(current.mates.list)
           mates2
           
           #now identifying new mates and filled with a 1
           my.cols <- sample(x = pmm2,size = mates2)
           my.cols
           mat[my.row,my.cols] <- 1
           mat
      
          
        } #end of if statement when current.mates < mates
        
        if(length(current.mates.list) > mates){
          
          #picking some number of mates that matches the mates drawn
          cml2 <- sample(x = current.mates.list,size = mates)
          cml2
          
          #identifying those to be removed
          rem.mates <- current.mates.list[!(current.mates.list %in% cml2)]
          rem.mates
          
          #removing them
          mat[my.row,rem.mates] <-  0
          
        } #end of if statement when the current mates > number of mates
 

      } #end of mate selection if statement

    } 
  }

  #working on some stats about the breeding matrix
  tmp <- tmp[order(tmp$number),]
  tmp <- tmp[order(tmp$sex),]
  tmp$order <- 1:nrow(tmp)
  tmp$mates_after <- c(colSums(mat),rowSums(mat))
  tmp$diff <- tmp$mates_after - tmp$mates_before
  tmp
  #mean(tmp$diff)
  #mean(colSums(mat))
  #mean(rowSums(mat))
  #what I have noticed is that there is systematic inflation of the number
  #of mates by filling in the matrix this way; each individual
  #on average have roughtly 1 extra mate, so I am going to go through
  #the females and try to create a for loop to down sample the names in a 
  #that minimizes this inflation in a random away
  tmp <- tmp[order(tmp$diff,decreasing = T),]
  tmp
  
  #saving someting things just in case
  mat1 <- mat
  mat1
  #mat <- mat1
  tmp.x <- tmp
  #tmp <- tmp.x
  mat
  tmp
  3

  i <- 0
  black.list <- NULL
  blacklist.carryover <- NULL
  while(sum(tmp$diff)>0){
    
    #keeping a counter to break the loop, if need be
    i <- i + 1
  #  print(i)
    #this while loop works through the first row tmp, which
    #deviates from the expectation the most and attemps to reduce that deviation
    #as much as possible, after it can't any more
    #the name gets moved to a blacklist
    
    #first removing black listed individuals and then 
    #picking the highest deviator that remains
    tmp1 <- tmp
    tmp1 <- tmp1[!(tmp1$parent %in% black.list),]
    tmp1
    if(length(blacklist.carryover)>0){
      tmp1 <- tmp1[!(tmp1$parent %in% blacklist.carryover),]
    }
    
    #if there is anything left getting the first row and working with that 
    if(nrow(tmp1)>0){
      tmp1 <-  tmp1[1,]
      tmp1
    }
    
    
    #if there is nothing left in in tmp to work with then, the while loop can end
    if(nrow(tmp1) == 0){
  #    print("Lambda deviations have been minimized")
      break
    }
    
    #if there are some individuals left, but they only represent underestimated individuals then the while loop can end
    if(tmp1$diff < 0){
   #   print("Lambda deviations have been minimized")
      break
    }
    
    #identifying black listed individuals

    tmp1
    if(tmp1$sex == "Female"){
      
      #first identifying if the individual has a diff > 0
      if(tmp1$diff > 0){
        
        #identifying the males that she mated with
        my.males <- which(mat[,tmp1$number]>0)
        my.males <- tmp[tmp$sex == "Male" & (tmp$number %in% my.males),]
        my.males
        
        #determining if any are inflated
        inflated <- my.males[my.males$diff > 0,]
        inflated
        
        #if its a perfect match, its a no brainer - just remove them
        if(nrow(inflated) == tmp1$diff){
          mat[inflated$number,tmp1$number[1]] <- 0
          mat
        } #diffs = inflated
        
        #if theres more in inflated than in the diff than, just randomly down sample
        if(nrow(inflated) > tmp1$diff){
          
          #determine how many to remove
          my.keeps <- NULL
          my.keeps <- nrow(inflated) - tmp1$diff
          my.keeps <- sample(x = inflated$number,size = my.keeps)
          my.removals <- inflated$number[!(inflated$number %in% my.keeps)]
          my.removals
          
          #removing that inflated number
          mat[my.removals,tmp1$number[1]] <- 0
          mat
        } #end of if inflated > diffs
        
        #if there are fewer inflated than there are diffs, just reducing those for now
        if(nrow(inflated) != 0){
          
          if(nrow(inflated) < tmp1$diff){
            mat[inflated$number,tmp1$number[1]] <- 0
            mat
            } #end of if inflated < diffs
          } # end of if inflated != 0 
        
        if(nrow(inflated) == 0){
          blacklist.carryover <- c(blacklist.carryover,tmp1$parent)
          } #end of if inflated == 0
        
      } # end of if > 0
      
    } else {
      
      #start of male side of thigns
      tmp1
      
      #first identifying if the individual has a diff > 0
      if(tmp1$diff > 0){
        
        #identifying the males that she mated with
        my.females <- which(mat[tmp1$number,]>0)
        my.females <- tmp[tmp$sex == "Female" & (tmp$number %in% my.females),]
        my.females
        
        #determining if any are inflated
        inflated <- my.females[my.females$diff > 0,]
        inflated

        #if its a perfect match, its a no brainer - just remove them
        if(nrow(inflated) == tmp1$diff){
          mat[tmp1$number[1],inflated$number] <- 0
          mat
        } #diffs = inflated
        
        #if theres more in inflated than in the diff than, just randomly down sample
        if(nrow(inflated) > tmp1$diff){
          
          #determine how many to remove
          my.keeps <- NULL
          my.keeps <- nrow(inflated) - tmp1$diff
          my.keeps <- sample(x = inflated$number,size = my.keeps)
          my.removals <- inflated$number[!(inflated$number %in% my.keeps)]
          my.removals
          
          #removing that inflated number
          mat[tmp1$number[1],my.removals] <- 0
          mat
        } #end of if inflated > diffs
        
        #if there are fewer inflated than there are diffs, just reducing those for now
        if(nrow(inflated) != 0){
          
          if(nrow(inflated) < tmp1$diff){
            mat[tmp1$number[1],inflated$number] <- 0
            mat
            } #end of if inflated < diffs
          } # end of if inflated != 0 
        
        if(nrow(inflated) == 0){
          blacklist.carryover <- c(blacklist.carryover,tmp1$parent)
          } #end of if inflated == 0
        
      } # end of if > 0
      
    } #end of if states related to males
    
    #recalculating this everytime to update male states
    tmp <- tmp[order(tmp$order),]
    tmp$mates_after <- c(colSums(mat),rowSums(mat))
    tmp$diff <- tmp$mates_after-tmp$mates_before
    tmp <- tmp[order(tmp$diff,decreasing = T),]
    tmp
    
    #my blacklist
    black.list <- tmp$parent[tmp$diff == 0]
    black.list
  
  if(i == moms+dads){
  #  print("Maximum attempts to limit Lambda deviations was reached")
    break
    }
  } #end of breding matrix fix while loop
  
  #recalculating this everytime to update male states
  #tmp <- tmp[order(tmp$order),]
  #tmp$mates_after <- c(colSums(mat),rowSums(mat))
  #tmp$diff <- tmp$mates_after-tmp$mates_before
  #tmp <- tmp[order(tmp$diff,decreasing = T),]
  #tmp[tmp$diff!=0,]
  #mean(tmp$diff)
  #mean(colSums(mat))
  #mean(rowSums(mat))
  
  #fixing any zeros left behind
  if(length(which(colSums(mat) == 0)) > 0){
    my.cols <- which(colSums(mat) == 0)
    i <- 1
    i <- NULL
    for(i in 1:length(my.rows)){
      mat[sample(1:dads,x = 1),my.cols[i]] <- 1
    }
  }
  if(length(which(rowSums(mat) == 0)) > 0){
    my.rows <- which(rowSums(mat) == 0)
    i <- 1
    i <- NULL
    for(i in 1:length(my.rows)){
      mat[my.rows[i],sample(1:moms,x = 1)] <- 1
    }
  }
  #tmp[tmp$diff!=0,]
  #mean(tmp$diff)
  #mean(colSums(mat))
  #mean(rowSums(mat))
  
  #returning the filled matrix
  return(mat)
}

brd.mat.fitness <- function(mat = mat, min.fert = 2500, max.fert = 6500, type = "uniform"){
  
  if(min.fert == max.fert){stop("min.fert and max.fert can not be the same value")}
  if(type == "uniform"){
    #equal success
    #now need to fill in those successful mate pairs with 
    #some number of viable offpsring
    i <- 3
    i <- NULL
    for(i in 1:ncol(mat)){
    
      #clearning things out
      my.mates <- NULL
      my.off <- NULL
      mate.props <- NULL
      my.off1 <- NULL
      
      #identifying mates and offspring
      my.mates <- which(mat[,i]>0)
      my.off <- sample(x = min.fert:max.fert,size = 1)
      
      if(length(my.mates)>1){
        mate.props <- replicate(1/length(my.mates),n = length(my.mates))
        my.off1 <- round(mate.props*my.off)
        my.off1 <- sample(x = my.off1,size = length(my.off1),replace = F)
        mat[my.mates,i] <- my.off1
      } else {
        mat[my.mates,i] <- my.off
      }

    }
    return(mat)  
  }
  
  if(type == "decline"){
  #decline in success
  #om = m/(tm(tm+1)/2)*Noff
  #now need to fill in those successful mate pairs with 
  #some number of viable offpsring
  i <- 1
  i <- NULL
  for(i in 1:ncol(mat)){
    
    #clearing things out
    my.mates <- NULL
    my.off <- NULL
    mate.props <- NULL
    my.off1 <- NULL
    
    #identifying mates and offspring
    my.mates <- which(mat[,i]>0)
    my.off <- sample(x = min.fert:max.fert,size = 1)
    
    if(length(my.mates)>1){
      #mate.props <- (rev(1:length(my.mates))/(length(my.mates)+1)) /  sum(rev(1:length(my.mates))/(length(my.mates)+1))
      mate.props <-1:length(my.mates) /(length(my.mates)*(length(my.mates)+1)/2)
      my.off1 <- round(mate.props*my.off)
      my.off1 <- sample(x = my.off1,size = length(my.off1),replace = F)
      mat[my.mates,i] <- my.off1
    } else {
      mat[my.mates,i] <- my.off
    }

  }
  return(mat)  
  }
  
}

mat.sub.sample <- function(mat,noff = 250){
  

 #making some generic dad and mom names and converting mat to DF
 dads <- paste0("dads",1:nrow(mat)) 
 moms <- paste0("moms",1:ncol(mat))
 mat <- as.data.frame(mat)
 colnames(mat) <- moms
 mat <- data.frame(cbind(dads,mat),stringsAsFactors=F)
 mat$dads <- as.character(mat$dads)
 mat
 
 #making the breeding matrix into mate pair RS data frame (long form)
 ped1 <- gather(data = mat,key = "moms",value = "off",-dads) %>% filter(off != 0)
 ped1$mp <- paste(ped1$dads,ped1$moms,sep = "_")
 head(ped1)
 
 #making the probs vector for each mate pair. First I total the number of fertilized eggs
 #then I take each mate pairs fitness and divide it by the total (I consider this the probability of sampling that mate pair randomly)
 off_total <- sum(ped1$off)
 ped1$probs <- ped1$off/off_total
 ped1$probs
 
 #randomly sub-sampling the mate pairs the size of noff with the mate pairs being drawn by various probabilities
 #calculated above
 #noff = 100
 my.picks <- sample(x = ped1$mp,size = noff,replace = T,prob = ped1$probs)
 my.picks
 
 #getting counts of each mp left
 my.picks <- as.data.frame(table(my.picks),stringsAsFactors = F)
 names(my.picks) <- c("mp","off1")
 head(my.picks)
 
 #merging with ped1 and making sure to fill in all mp lost with zero
 ped1 <- merge(x = ped1,y = my.picks,by = "mp",all.x =T)
 ped1$off1[is.na(ped1$off1)] <- 0
 ped1 <- ped1 %>% arrange(mp)
 ped1 <- ped1[,c(1,4,5,2,3,6)]
 head(ped1)
 
 return(ped1)
}

mat.stats <- function(mat,id.col = F){
  
  if(id.col){
    #calculating the stats
  if(ncol(mat)>2){
    female.rs <- round(mean(colSums(mat[,-1])),2)
    female.sd <- round(sd(colSums(mat[,-1])),2)
  } else{
    female.rs <- sum(mat[,-1])
    female.sd <- sd(mat[,-1])
  }
 
  if(nrow(mat)>1){
    male.rs <- round(mean(rowSums(mat[,-1])),2) 
    male.sd <- round(sd(rowSums(mat[,-1])),2) 
  } else {
    male.rs <- sum(mat[1,])
    male.sd <- sd(mat[1,])
  }

  mp.rs <- round(mean(as.matrix(mat[,-1])),3)
  mat.str2 <- mat[,-1]
  mat.str2[mat.str2 > 0] <- 1
  mp.count <- sum(mat.str2)
  females.mates <- round(mean(colSums(mat.str2)),2)
  male.mates <- round(mean(rowSums(mat.str2)),2)
  max.female.mates <- max(colSums(mat.str2))
  max.male.mates <- max(rowSums(mat.str2))
  n.mom <- ncol(mat)
  n.dad <- nrow(mat)
  n.par <- n.mom+n.dad
  sex.ratio <- round(n.dad/n.mom,2)
  npar <- n.mom + n.dad
  noff <- sum(mat)
  
  } else {

    #calculating the stats
  if(ncol(mat)>1){
    female.rs <- round(mean(colSums(mat)),2)
    female.sd <- round(sd(colSums(mat)),2)
  } else{
    female.rs <- round(sum(mat),2)
    female.sd <- round(sd(mat),2)
  }
 
  if(nrow(mat)>1){
    male.rs <- round(mean(rowSums(mat)),2)   
    male.sd <- round(sd(rowSums(mat)),2)   
  } else {
    male.rs <- sum(mat[1,])
    male.sd <- sd(mat[1,])
  }

  mp.rs <- round(mean(as.matrix(mat)),3)
  mat.str2 <- mat
  mat.str2[mat.str2 > 0] <- 1
  mp.count <- sum(mat.str2)
  females.mates <- round(mean(colSums(mat.str2)),2)
  male.mates <- round(mean(rowSums(mat.str2)),2)
  max.female.mates <- max(colSums(mat.str2))
  max.male.mates <- max(rowSums(mat.str2))
  n.mom <- ncol(mat)
  n.dad <- nrow(mat)
  n.par <- n.mom+n.dad
  sex.ratio <- round(n.dad/n.mom,2)
  npar <- n.mom + n.dad
  noff <- sum(mat)
  }

    #making a DF to return back
  mat.info <- data.frame(n.par,n.mom,n.dad,sex.ratio, noff,
                          female.rs,female.sd,females.mates,max.female.mates,
                          male.rs,male.sd,male.mates,max.male.mates,mp.count,mp.rs)
  return(mat.info)
}

convert2ped <- function(df){
  
  #makin sure there are not zeros
  df <- df[df$off1 != 0,]
  
  #making a generic dfigree with the remaining off
  i <- 1
  i <- NULL
  df.out <- NULL
  for(i in 1:nrow(df)){
    #print(paste(i,"of",nrow(df),"mate pairs"))
    off1 <- paste(paste0("o",1:df$off1[i]),df$moms[i],df$dads[i],sep="_")  
    off1 <- gsub(pattern = "moms",replacement = "m",x = off1)
    off1 <- gsub(pattern = "dads",replacement = "d",x = off1)
    df2 <- data.frame(off = off1, mom = df$moms[i], dad = df$dads[i])
    df.out <- rbind(df.out,df2)
  }
  
  return(df.out)
}


ped2mat <- function(ped){
  
  #removing some columns
  ped <- ped[,c(-1:-3)]
  ped <- ped[ped$off1 > 0,]
  ped
  sum(ped$off1)

  #making the matrix
  mat <- ped %>% spread(key = moms,value = off1,fill = 0)
  mat

  #making it a matrix
  row.names(mat) <- mat$dads
  mat$dads <- NULL
  return(mat)
}

mat2ped <- function(mat = mat.str){
  
  mat1 <- data.frame(mat)
  colnames(mat1) <- paste("mom",1:ncol(mat1))
  mat1$dads <- paste("dad",1:nrow(mat1))
  mat1 <- mat1 %>% gather(key = "moms",value = "off1",-dads)
  ped <- convert2ped(df = mat1)
  return(ped)
}
