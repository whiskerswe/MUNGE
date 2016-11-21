#native country används ej

performMungeForAdult<-function(){
  library(tree)
  library(ggplot2)
  #av någon anledning är inte data som behandlas som fakturor i munge faktor efter metoden. 
  updateFactor<-function(data){
    data[,4]<-as.numeric(data[,4])
    data[,4]<-as.factor(data[,4])
    data[,5]<-as.factor(data[,5])
    data[,6]<-as.factor(data[,6])
    data[,10]<-as.factor(data[,10])

    return(data)
  }
  addColNames<-function(nadultData){

    colnames(nadultData) <- c("age","fnlwgt","educationnum",
    "relationship","race", "sex","capitalgain", "capitalloss", "hoursperweek","income")
    return(nadultData)
  }
  
  preparedAdult<-function(adultData){
    adultData<-mixData(adultData)
    adultData[,1:10]<-adultData[,c(1,3,5,8,9,10,11,12,13,15)]
    nadultData<-adultData[,1:10]   
    return(nadultData)
  }
  
  adultSingleTree<-function(trainData, testData){
    set.seed(9895)
    test = testData[,1:9]
    mytree.train = tree(data=trainData, income ~  age +fnlwgt +educationnum+ relationship + race+ sex+ capitalgain +capitalloss+ hoursperweek)
    mytree.pred = predict(mytree.train, test, type="class")
    # type class for classification
    table(mytree.pred, testData[, 10])
    result<-(sum(diag(table(mytree.pred, testData[, 10]))))/nrow(testData)
    return(result)
  }  
  
  plotSelectedData<-function(adultA,adult1, adult2, adult3){
    adultA<-subset(adultA[adultA$income == 1, ])
    adult1<-subset(adult1[adult1[,10] == 1, ])
    adult2<-subset(adult2[adult2[,10] == 1, ])
    adult3<-subset(adult3[adult3[,10] == 1, ])
    ourdata=list(adultA$age, adult1[,1], adult2[,1],adult3[,1])
    percentDiagram(ourdata,"Adult age income cat 1")    
  }
 #Returnerar ny dataframe efter bagging 
  prepBagging<-function(trainData, testData){
    #Småfix av egenskaper som försvinner när ett dataframe tas emot
    trainData<-as.data.frame(trainData)
    trainData<-addColNames(trainData)
    trainData<-updateFactor(trainData)
    set.seed(123)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = income ~ . , data = trainData, mtr=2)
    test = testData[,1:9]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    test<-cbind(test, predicted.bagging)
    return(test)
  }  
 #Returnerar resultat av bagging 
  bagging<-function(trainData,testData){
    set.seed(123)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = income ~ . , data =trainData, mtr=2)
    test = testData[,1:9]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    x<-(sum(diag(table(predicted.bagging, testData[,10])))/(nrow(testData)))
    return(x)
  } 

  munge3<-function(nrRows,adultData){
    #Skapa ny dataframe för de olika varianserna
    detailedVariance<-data.frame()
    #Räkna ut varians enligt grupp
    library(plyr)
    detailedVariance<-ddply(adultData, .(income), summarize,  age=(var(age)), fnlwgt=(var(fnlwgt)),educationnum=(var(educationnum)),capitalgain=(var(capitalgain)), capitalloss=(var(capitalloss)), hoursperweek=(var(hoursperweek)))
    detailedVariance
    head(adultData)
    adultData2<- adultData[order(adultData$income),] 
    x=sum(adultData$income == 1)
    y=sum(adultData$income == 2)
    freq<-c(x,y)
    detailedVariance<-cbind(detailedVariance,freq)
    library(splitstackshape)
    temp<-expandRows(detailedVariance, "freq")
    dV<-as.data.frame(temp[,2:7])
    syntheticData<-data.frame()  
    newData<-data.frame()
    i=0
    nrSeed=321
    loops=(nrRows/nrow(adultData))+1
    while (i < nrRows) {
      if(i==0){
        newData=rbind(mungeWithDetailedColumnVariance(adultData, syntheticData, dV, 4,(nrSeed+i)))     
      }else{
        temp<-mungeWithDetailedColumnVariance(adultData, syntheticData, dV, 4, (nrSeed+i))
        temp<-addColNames(temp)
        newData=rbind(newData,temp)     
      } 

      i=nrow(newData)
    }
    adultData<-mixData(adultData)
    newData<-mixData(newData)
    newData<-addColNames(newData)
    return(newData[1:nrRows,])
  }

  collectData<-function(testData, trainData){
    singleBaggingResult<-bagging(trainData, testData) 
    singleTreeResult<-adultSingleTree(trainData, testData)
  #Skapar syntetiska exempel  
    adultResult1.1000<-munge1(20000, trainData)
    adultResult1.500<-munge1(5000, trainData)
    adultResult2.1000<-munge2(20000,trainData)
    adultResult2.500<-munge2(5000,trainData)
    adultResult3.1000<-munge3(20000,trainData)
    adultResult3.500<-munge3(5000,trainData)

    print("bagging för att klassificera nya ex. munge 1, 2, 3")
    adultResult1.1000<-prepBagging(adultResult1.1000, trainData)
    adultResult1.500<-prepBagging(adultResult1.500, trainData)
    adultResult2.1000<-prepBagging(adultResult2.1000, trainData)
    adultResult2.500<-prepBagging(adultResult2.500, trainData)
    adultResult3.1000<-prepBagging(adultResult3.1000, trainData)
    adultResult3.500<-prepBagging(adultResult3.500, trainData)
    #Småfix av egenskaper som försvinner när ett dataframe tas emot
    adultResult2.1000<-as.data.frame(adultResult2.1000)
    adultResult3.1000<-as.data.frame(adultResult3.1000)
    adultResult2.500<-as.data.frame(adultResult2.500)
    adultResult3.500<-as.data.frame(adultResult3.500)
    adultResult1.1000<-addColNames(adultResult1.1000)
    adultResult2.1000<-addColNames(adultResult2.1000)
    adultResult3.1000<-addColNames(adultResult3.1000)
    adultResult1.1000<-updateFactor(adultResult1.1000)
    adultResult2.1000<-updateFactor(adultResult2.1000)
    adultResult3.1000<-updateFactor(adultResult3.1000)
    adultResult1.500<-addColNames(adultResult1.500)
    adultResult2.500<-addColNames(adultResult2.500)
    adultResult3.500<-addColNames(adultResult3.500)
    adultResult1.500<-updateFactor(adultResult1.500)
    adultResult2.500<-updateFactor(adultResult2.500)
    adultResult3.500<-updateFactor(adultResult3.500)
    
    print(plotSelectedData(trainData, adultResult1.500, adultResult2.500, adultResult3.500))
    
    adultResult1.1000<-rbind(adultResult1.1000, trainData)
    adultResult2.1000<-rbind(adultResult2.1000, trainData)
    adultResult3.1000<-rbind(adultResult3.1000, trainData)
    adultResult1.500<-rbind(adultResult1.500, trainData)
    adultResult2.500<-rbind(adultResult2.500, trainData)
    adultResult3.500<-rbind(adultResult3.500, trainData)    

    result<-data.frame()
    tempA<-c( adultSingleTree(adultResult1.1000, testData))
    tempB<-c( adultSingleTree(adultResult1.500, testData))
    temp<-c(tempB,tempA)
    result<-rbind(temp)
    tempA<-c( adultSingleTree(adultResult2.1000, testData))
    tempB<-c( adultSingleTree(adultResult2.500, testData))
    temp<-c(tempB,tempA)
    result<-rbind(result,temp)  
    tempA<-c( adultSingleTree(adultResult3.1000, testData))
    tempB<-c( adultSingleTree(adultResult3.500, testData))
    temp<-c(tempB,tempA)
    result<-rbind(result,temp)  
    row.names(result)<-c("Munge1","Munge2","Munge3")
    colnames(result)<-c("5000","20000")
    singleResultVector<-c(singleTreeResult,singleBaggingResult)
    result<-rbind(result, singleResultVector)
    return(result)
  }  


  #Dataframe med normaliserad och ihopblandad data
  getwd()
  data <- read.table("datasets/adult.data.txt",
                          header = FALSE)
  data<-preparedAdult(data)
  data<-addColNames(data)
  data<-normaliseAndFactor(data)

  i = 1
  start=1
  result<-data.frame()
  allResults<-data.frame()
  temp=data.frame()
  nrsIts=10
  tree=0
  bagg=0
  while(i<=nrsIts){
    print("Iteration: ")
    print(i)
    trainData = data.frame()
    testData = data.frame()
    if(i==nrsIts){
      testData<-data[start:nrow(data),]      
    }else{
      testData<-data[start:(start+3256),]  
    }
    #ta den delen som inte är train och gör till testdata
    
    if(i==nrsIts){
      trainData<-data[1:(start-1),]
    }
    else if(i>1){
      trainData<-data[1:(start-1),]
      trainData<-rbind(trainData,data[(start+3256):nrow(data),] )
    }else{
      trainData<-data[(start+3256):nrow(data),]
    }
    print(nrow(trainData))
    print(nrow(testData))
    t<-data.frame()
    t<-collectData(testData, trainData)
    temp<-t[1:(nrow(t)-1),]
    temp.tree<-t[nrow(t),1]
    temp.bagging<-t[nrow(t),ncol(t)]
    print("-------------------------------------------------------------------")
    tVector<-c(temp[1,],temp[2,],temp[3,],temp.tree,temp.bagging)
    if(i==1){
      result=temp
      allResults<-rbind(tVector)
      bagg=temp.bagging
      tree=temp.tree
    }else{
      j=1
      while(j<=ncol(temp)){
        result[,j]=result[,j]+temp[,j]
        j=j+1
      }
      bagg=bagg+temp.bagging
      tree=tree+temp.tree
      allResults<-rbind(allResults,tVector)
    }
    i=i+1
    start=start+3256
  } 
  print("Result")
  result=result/nrsIts
  tree=tree/nrsIts
  bagg=bagg/nrsIts
  print(result)
  print(bagg)
  print(tree)
  
  
  allResultsDiagram(allResults,"Result 10 fold cross validation")
  averageResultDiagram(bagg,tree, result,"Average adult")
  
  colnames(allResults) <- c("M1:5000","M1:20000","M2:5000","M2:20000","M3:5000","M3:20000","SingleTree","Bagging")
  rn<-c(1:nrow(allResults),"Average")
  g<-colMeans(allResults)
  allResults<-rbind(allResults,g)
  rownames(allResults)<-rn
  print(allResults)


}
performMungeForAdult()

