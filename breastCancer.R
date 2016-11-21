
performMungeForBreastCancer<-function(){
  library(tree)
  library(ggplot2)
  #av någon anledning är inte data som behandlas som fakturor i munge faktor efter metoden. 
 
  
  updateFactor<-function(data){

    data[,1]<-as.factor(data[,1])
    #, visar att det är column 1. 
    return(data)
  }
  
  addColNames<-function(nbreastCancer){
    colnames(nbreastCancer) <- c("diagnosis","thickness","size.unif",
                             "shape.unif","adhesion", "epi","bare", "bland", "normal","mitoses")
    return(nbreastCancer)
  }

  # Ser till att all data kommer på rätt plats ta bort kolumner group och ID
  preparedBreastCancer<-function(breastCancer){
    breastCancer<-mixData(breastCancer)
    nbreastCancer<-breastCancer[,3:12]
    return(nbreastCancer)
  }

  
  breastCancerSingleTree<-function(trainData, testData){
    set.seed(6878)
    test = testData[,2:10]
    mytree.train = tree(data=trainData, diagnosis ~  thickness +size.unif +shape.unif +adhesion +epi +bare +bland +normal +mitoses)
    mytree.pred = predict(mytree.train, test, type="class")
    mytree.pred
    table(mytree.pred, testData[, 1])
    result<-(sum(diag(table(mytree.pred, testData[, 1]))))/nrow(testData)
    return(result)
  }  

  plotSelectedData<-function(dataA,data1, data2, data3){
    dataA<-subset(dataA[dataA$diagnosis == 2, ])
    data1<-subset(data1[data1[,1] == 2, ])
    data2<-subset(data2[data2[,1] == 2, ])
    data3<-subset(data3[data3[,1] == 2, ])
    ourdata=list(dataA$thickness, data1[,2], data2[,2],data3[,2])
    print(percentDiagram(ourdata,"Cancer malign thickness")   ) 
  }
  #Returnerar ny dataframe efter bagging 
  prepBagging<-function(trainData, testData){
    testData<-as.data.frame(testData)
    testData<-addColNames(testData)
    testData<-updateFactor(testData)
    set.seed(123)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = diagnosis ~ . , data = trainData, mtr=2)
    test = testData[,2:10]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    test<-cbind(predicted.bagging, test)
    return(test)
  }  


  bagging<-function(trainData,testData){
    set.seed(125)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = diagnosis ~ . , data =trainData, mtr=2)
    test = testData[,2:10]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    x<-(sum(diag(table(predicted.bagging, testData[,1])))/(nrow(testData)))
    return(x)
  } 

  
  munge3<-function(nrRows,breastCancer){
    #Skapa ny dataframe för de olika varianserna
    detailedVariance<-data.frame()
    library(plyr)
    detailedVariance<-ddply(breastCancer, .(diagnosis), summarize,  thickness=(var(thickness)), size.unif=(var(size.unif)),shape.unif=(var(shape.unif)),adhesion=(var(adhesion)),  epi=(var(epi)), bare=(var(bare)), bland=(var(bland)), normal=(var(normal)), mitoses=(var(mitoses)))
    detailedVariance
    breastCancer2<- breastCancer[order(breastCancer$diagnosis),] 
    x=sum(breastCancer$diagnosis == 1)
    y=sum(breastCancer$diagnosis == 2)
    freq<-c(x,y)
    detailedVariance<-cbind(detailedVariance,freq)
    library(splitstackshape)
    temp<-expandRows(detailedVariance, "freq")
   
    dV<-as.data.frame(temp[,2:10])
    head(dV)
    syntheticData<-data.frame()  
    newData<-data.frame()
    i=0
    nrSeed=321
    loops=(nrRows/nrow(breastCancer))+1
    while (i < nrRows) {
      #  (trainingEx, syntheticData, dV,p)
      if(i==0){
        newData=rbind(mungeWithDetailedColumnVariance(breastCancer, syntheticData, dV, 4,(nrSeed+i)))     
      }else{
        temp<-mungeWithDetailedColumnVariance(breastCancer, syntheticData, dV, 4, (nrSeed+i))
        temp<-addColNames(temp)
        newData=rbind(newData,temp)     
      } 
      
      i=nrow(newData)
    }
    #breastCancer<-mixData(breastCancer)
    newData<-mixData(newData)
    newData<-addColNames(newData)
    return(newData[1:nrRows,])
  }

  collectData<-function(testData, trainData){
    trainData = updateFactor(trainData)
    testData = updateFactor(testData)
    singleBaggingResult<-bagging(trainData, testData) 
    singleTreeResult<-breastCancerSingleTree(trainData, testData)
    #Skapar syntetiska exempel  
    breastCancerResult1.1000<-munge1(5000, trainData)
    breastCancerResult1.500<-munge1(20000, trainData)
    breastCancerResult2.1000<-munge2(5000, trainData)
    breastCancerResult2.500<-munge2(20000, trainData)
    breastCancerResult3.1000<-munge3(5000, trainData)
    breastCancerResult3.500<-munge3(20000, trainData)

    print("bagging för att klassificera nya ex. munge 1, 2, 3")
    breastCancerResult1.1000<-prepBagging(trainData, breastCancerResult1.1000 )
    breastCancerResult1.500<-prepBagging(trainData, breastCancerResult1.500)
    #adultResult2.1000<-prepBagging(adultResult2.1000, adultData[1001:1500,])
    
    breastCancerResult2.1000<-prepBagging(trainData, breastCancerResult2.1000)
    breastCancerResult2.500<-prepBagging(trainData, breastCancerResult2.500)
    #  adultResult2.1000<-prepBagging(adultResult2.1000, adultData[1001:1500,])
    
    # adultResult3.1000<-prepBagging(adultResult3.1000, adultData[1001:1500,])
    breastCancerResult3.1000<-prepBagging(trainData, breastCancerResult3.1000)
    breastCancerResult3.500<-prepBagging(trainData, breastCancerResult3.500)
    
    breastCancerResult1.1000<-addColNames(breastCancerResult1.1000)
    breastCancerResult2.1000<-addColNames(breastCancerResult2.1000)
    breastCancerResult3.1000<-addColNames(breastCancerResult3.1000)
    breastCancerResult1.1000<-updateFactor(breastCancerResult1.1000)
    breastCancerResult2.1000<-updateFactor(breastCancerResult2.1000)
    breastCancerResult3.1000<-updateFactor(breastCancerResult3.1000)
    breastCancerResult1.500<-addColNames(breastCancerResult1.500)
    breastCancerResult2.500<-addColNames(breastCancerResult2.500)
    breastCancerResult3.500<-addColNames(breastCancerResult3.500)
    breastCancerResult1.500<-updateFactor(breastCancerResult1.500)
    breastCancerResult2.500<-updateFactor(breastCancerResult2.500)
    breastCancerResult3.500<-updateFactor(breastCancerResult3.500)
    
    print(plotSelectedData(trainData,breastCancerResult1.1000,breastCancerResult2.1000,breastCancerResult3.1000))
   
    breastCancerResult1.1000<-rbind(breastCancerResult1.1000, trainData)
    breastCancerResult2.1000<-rbind(breastCancerResult2.1000, trainData)
    breastCancerResult3.1000<-rbind(breastCancerResult3.1000, trainData)
    breastCancerResult1.500<-rbind(breastCancerResult1.500, trainData)
    breastCancerResult2.500<-rbind(breastCancerResult2.500, trainData)
    breastCancerResult3.500<-rbind(breastCancerResult3.500, trainData)   

    #Skapar en dataframe med resultat från träd 
    result<-data.frame()

    tempA<-c( breastCancerSingleTree(breastCancerResult1.1000, testData))
    tempB<-c( breastCancerSingleTree(breastCancerResult1.500, testData))
    temp<-c(tempA,tempB)

    result<-rbind(temp)
    tempA<-c( breastCancerSingleTree(breastCancerResult2.1000, testData))
    tempB<-c( breastCancerSingleTree(breastCancerResult2.500, testData))
    temp<-c(tempA,tempB)

    result<-rbind(result,temp)  
    tempA<-c( breastCancerSingleTree(breastCancerResult3.1000, testData))
    tempB<-c( breastCancerSingleTree(breastCancerResult3.500, testData))
    temp<-c(tempA,tempB)
 
    result<-rbind(result,temp)  
    row.names(result)<-c("Munge1","Munge2","Munge3")
    colnames(result)<-c("5000","20000")

    
#######################################################################################
    singleResultVector<-c(singleTreeResult,singleBaggingResult)
    result<-rbind(result, singleResultVector)
    print(singleTreeResult)
    print(singleBaggingResult)
    return(result)
#######################################################################################
  }  
  
  
  #Dataframe med normaliserad och ihopblandad data
  getwd()
  breastCancer <- read.table("datasets/breastCancer.data.txt",
                          header = TRUE)
  breastCancer<-preparedBreastCancer(breastCancer)
  breastCancer<-addColNames(breastCancer)
  breastCancer<-updateFactor(breastCancer)
  breastCancer<-normaliseAndFactor(breastCancer)
  head(breastCancer)
  result<-data.frame()
  temp=data.frame()
  #Skapar 10 delar av data som ska tränas och testas separat
  i = 1
  start=1
  ###########################################################################
  result<-data.frame()
  allResults<-data.frame()
  temp=data.frame()
  nrsIts=10
  tree=0
  bagg=0
  print("breast cancer")
  ###########################################################################
  while(i<=nrsIts){
    print("-------------------------------------------------------------------")
    print("Iteration: ")
    print(i)
    trainData = data.frame()
    testData = data.frame()

    if(i==nrsIts){
      testData<-breastCancer[start:nrow(breastCancer),]      
    }else{
      testData<-breastCancer[start:(start+69),]      
    }
    #ta den delen som inte är train och gör till testdata
    if(i==nrsIts){
      trainData<-breastCancer[1:(start-1),]
    }
    else if(i>1){
      trainData<-breastCancer[1:(start-1),]
      trainData<-rbind(trainData,breastCancer[(start+69):nrow(breastCancer),] )
    }else{
      trainData<-breastCancer[(start+70):nrow(breastCancer),]
    }
    print(nrow(trainData))
    print(nrow(testData))

###############################################################################
    t<-data.frame()
    t<-collectData(testData, trainData)
    temp<-t[1:(nrow(t)-1),]
    temp.tree<-t[nrow(t),1]
    temp.bagging<-t[nrow(t),ncol(t)]

    start=start+70
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
###################################################################################
    i=i+1
  } 
  print("Result")
  result=result/nrsIts
  tree=tree/nrsIts
  bagg=bagg/nrsIts
  print(result)
  print(bagg)
  print(tree)

  allResultsDiagram(allResults,"Breastcancer result 10 fold cross validation")
  averageResultDiagram(bagg,tree, result,"Average breast cancer")
 
  colnames(allResults) <- c("M1:5000","M1:20000","M2:5000","M2:20000","M3:5000","M3:20000","SingleTree","Bagging")
  rn<-c(1:nrow(allResults),"Average")
  g<-colMeans(allResults)
  allResults<-rbind(allResults,g)
  rownames(allResults)<-rn
  print(allResults)
}

performMungeForBreastCancer()


