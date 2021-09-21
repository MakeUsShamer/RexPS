Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")

#Question 1

v1 <- 51:90

v1_length <- length(v1) #Length of v1



#(1-a)
print('가나')

for(i in 1:v1_length){
  if(v1[i]<60)print(v1[i])
}



#(1-b)
under70_cnt <- 0 #Counted number under 70


for(i in 1:v1_length){
  if(v1[i]<70)under70_cnt<-under70_cnt+1
}

print(under70_cnt)



#(1-c)

sum_over65 <- 0  #Sum of numbers over 65

for(i in 1:v1_length){
  if(v1[i]>65)sum_over65<-sum_over65+v1[i]
}

print(sum_over65)



#(1-d)

for(i in 1:v1_length){
  if((v1[i]<73)&(v1[i]>60))print(v1[i])
}



#(1-e)

for(i in 1:v1_length){
  if((v1[i]<6)|(v1[i]>80))print(v1[i])
}



#(1-f)

remainder <- 0   #remainder slot

for(i in 1:v1_length){
  remainder <- v1[i]%%7
  if(remainder==3)print(v1[i])
}



#(1-g)

for(i in 1:v1_length){
  remainder <- v1[i]%%7
  if(remainder==0)v1[i]<-0
}

print(v1)





#(1-h)

even_cnt <- 0

for(i in 1:v1_length){
  remainder <- v1[i]%%2
  if(remainder==0)even_cnt<-even_cnt+v1[i]
}

print(even_cnt)





#(1-i)

for(i in 1:v1_length){
  remainder <- v1[i]%%2
  if((remainder==1)|(v1[i]>80))print(v1[i])
}



#(1-j)

rem1 <- 0
rem2 <- 0

for(i in 1:v1_length){
  rem1 <- v1[i]%%3
  rem2 <- v1[i]%%5
  if((rem1==0)&(rem2==0)&(v1[i]>0))print(v1[i])
}

#1-k

for(i in 1:v1_length){
  remainder <- v1[i]%%2
  if(remainder==0)v1[i] <- v1[i]*2
}

print(v1)

#1-l

cnt7 <- 0

for(i in 1:v1_length){
  remainder <- v1[i]%%7
  if(remainder==0)cnt7 <- cnt7 + 1
}
cnt7
v2 <- 1:(v1_length-cnt7)

j <- 0

for(i in 1:v1_length){
  remainder <- v1[i]%%7
  if(remainder!=0){
    j <- j + 1
    v2[j] <- v1[i]
  }
}
v1 <- v2

print(v1)


#Question 2

data2 <- swiss


#2-a

summary(data2)




#2-b

rownames(data2[which.max(data2$Agriculture),])



#2-c

data2[order(data2$Agriculture, decreasing = TRUE),]



#2-d

for(i in 1:length(data2[,1])){
  if(data2[i,5]>=80)cat(rownames(data2[i,]),":",data2[i,2],'\n')
}



#2-e

for(i in 1:length(data2[,1])){
  if((data2[i,3]<20)&(data2[i,2]<=50))
    cat(rownames(data2[i,]),"->   Examination :",data2[i,3],"         Agriculture :",data2[i,2],'\n')
}



#Question 3

data3 <- state.x77

data3 <- data3[,-c(3,4,5,6,7)]

data3 <- data3[-c(which(data3[,2]<5000)),]

write.csv(data3, "rich state.csv")

ds <- read.csv("rich state.csv")

print(ds)


#Question 4

prim_check <- FALSE

k <- 0

for(i in 2:1000){
  if(i==2)print(i)
  else
    k <- i-1
  for(j in 2:k){
    rem3 <- i%%j
    if(rem3==0){
      prim_check <- TRUE
      break
    }
  }
  if(prim_check==FALSE){
    print(i)
    
  } else if(prim_check==TRUE) prim_check <- FALSE
}


#Question 5

first <- 0
second <- 0
sum <- 0


for(i in 1:50){
  if(sum==0){
    cat('0 row :', sum, '\n')
    second <- 1
    sum <- first + second
    cat(i,'row :', sum, '\n')
  }else{
    sum <- first + second
    first <- second
    second <- sum
    cat(i,'row :', sum, '\n')
  }
}


#Question 6


#6-a

find_gcf <- function(x, y) {
  
  remx <- 0
  remy <- 0
  gcf <- 0
  
  if(y<x){
    slot <- x
    x <- y
    y <- slot
  }
  
  for(i in 1:x){
    remx <- x%%i
    remy <- y%%i
    if((remx==0)&(remy==0))  gcf<- i
  }
  
  return(gcf)
  
}

print('First number? : ')
n1 <- scan()
print('Second number? : ')
n2 <- scan()

print(find_gcf(n1,n2))



#6-b

find_mxmn <- function(v) {
  
  mm <- c(min(v),max(v))
  
  return(mm)
}


vct <- sample(x=1:100,size=10)

cat('min:',find_mxmn(vct)[1],' max:',find_mxmn(vct)[2], '\n')



#Question 7

weight <- c(69,50,55,71,89,64,59,70,71,80)

#7-a

mx <- max(weight)

for(i in 1:length(weight)){
  if(weight[i]==mx)mx_point<-i
}

print(mx_point)


#7-b

mn <- min(weight)

for(i in 1:length(weight)){
  if(weight[i]==mn)mn_point<-i
}

print(mn_point)


#7-c

betlist <- c()

for(i in 1:length(weight)){
  if((weight[i]>61)&(weight[i]<69))betlist <- c(betlist, i)
}

print(betlist)


#7-d

underlist <- c()

for(i in 1:length(weight)){
  if(weight[i]<=60)underlist <- c(underlist, i)
}


write.csv(underlist, "weight.s", row.names = FALSE)



print(read.csv("weight.s"))