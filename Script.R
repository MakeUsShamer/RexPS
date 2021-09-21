#Question 1

v1 <- 51:90

v1_length <- length(v1) #Length of v1



#(1-a)

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





print("(i) v1에서 홀수이거나 80보다 큰 수를 모두 출력하시오.")

for(i in 1:v1_length){
  remainder <- v1[i]%%2
  if((remainder==1)|(v1[i]>80))print(v1[i])
}




print("(j) v1에서 3과 5의 공배수를 출력하시오.")

rem1 <- 0
rem2 <- 0

for(i in 1:v1_length){
  rem1 <- v1[i]%%3
  rem2 <- v1[i]%%5
  if((rem1==0)&(rem2==0)&(v1[i]>0))print(v1[i])
}


print("(k) v1에서 짝수에 대해서만 2를 곱하여 저장하시오.")

for(i in 1:v1_length){
  remainder <- v1[i]%%2
  if(remainder==0)v1[i] <- v1[i]*2
}

print(v1)


print("(l) v1에서 7의 배수들을 제거한 후 v1의 내용을 출력하시오.")

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



print("(a) 자료의 요약정보를 나타내시오.")

summary(data2)





print("(b) 남성의 농업인 비율이 가장 높은 주는?")

rownames(data2[which.max(data2$Agriculture),])




print("(c) 남성의 농업인 비율을 내림차순으로 정렬하여 주의 이름과 함께 나타내시오.")

data2[order(data2$Agriculture, decreasing = TRUE),]




print("(d) 카톨릭 신자의 비율이 80%이상인 주들의 남성의 농업인 비율을 보이시오.")

for(i in 1:length(data2[,1])){
  if(data2[i,5]>=80)cat(rownames(data2[i,]),":",data2[i,2],'\n')
}




print("(e) 징집대상자 둥 입대시험에서 높은 평가를 받은 사람들의 비율이 20% 미만이고 남성의
농업인 비율이 50% 미만인 주의 이름과 Examination, Agriculture 열의 값을 나타내
시오.")

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
    cat('0 열 :', sum, '\n')
    second <- 1
    sum <- first + second
    cat(i,'열 :', sum, '\n')
  }else{
    sum <- first + second
    first <- second
    second <- sum
    cat(i,'열 :', sum, '\n')
  }
  
}
