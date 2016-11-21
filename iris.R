performMungeForIris<-function(){
  library(tree)
  library(ggplot2)

  addColNames<-function(irisData){
    colnames(irisData) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
    return(irisData)
  }
  
  updateFactor<-function(irisData){
    irisData[,5]<-as.factor(irisData[,5])
    return(irisData)
  }
  irisSingleTree<-function(trainData, testData){
    set.seed(534)
    test = testData[,1:4]
    mytree.train = tree(data=trainData, Species ~ Sepal.Length +Sepal.Width+Petal.Width+Petal.Length)
    mytree.pred = predict(mytree.train, test, type="class")
    # type class for classification
    mytree.pred
    table(mytree.pred, testData[, 5])
    result<-(sum(diag(table(mytree.pred, testData[, 5]))))/nrow(testData)

    return(result)
  }

  prepBagging<-function(trainData, testData){

    testData<-as.data.frame(testData)
    testData<-addColNames(testData)
    testData<-updateFactor(testData)

    set.seed(123)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = Species ~ . , data = trainData, mtr=2)
    test = testData[,1:4]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    test<-cbind(test, predicted.bagging)
    test<-addColNames(test)
    return(test)
  }
  bagging<-function(trainData,testData){
    set.seed(123)
    library(randomForest)
    library(ggplot2)
    bagging = randomForest(formula = Species ~ . , data =trainData, mtr=2)
    test = testData[,1:4]
    predicted.bagging = predict(newdata=test, bagging, type = "class")
    test<-cbind(test, predicted.bagging)
    return(sum(diag(table(predicted.bagging, testData[,5])))/(nrow(testData)))
  }
  #Förbereder data
  prepareIris<-function(){
    set.seed(9850)
    #Blanda ihop Iris. Detta är nödvändigt innan vi analyserar 
    gp<-runif(nrow(iris))
    gp
    iris3<-data.frame(stringsAsFactors = TRUE)
    iris3<-iris[order(gp),]
    #Normaliserar iris. 
    iris3$Sepal.Width<-normalise(iris3$Sepal.Width)
    iris3$Sepal.Length<-normalise(iris3$Sepal.Length)
    iris3$Petal.Width<-normalise(iris3$Petal.Width)
    iris3$Petal.Length<-normalise(iris3$Petal.Length)
    iris3$Species<-(iris3$Species)
    v<-as.numeric(iris3[,5])
    iris3$Species<-as.factor(v)
    #Returnerar behandlad dataframe
    return (iris3)
  }

  plotSelectedData<-function(iris1, iris2, iris3){
    irisA<-prepareIris()
    irisA<-subset(irisA[irisA$Species == 1, ])
    iris1<-subset(iris1[iris1[,5] == 1, ])
    iris2<-subset(iris2[iris2[,5] == 1, ])
    iris3<-subset(iris3[iris3[,5] == 1, ])
    ourdata=list(irisA$Sepal.Width, iris1[,2], iris2[,2],iris3[,2])
   percentDiagram(ourdata,"Iris Setosa sepal.width")    
  }


  performDetailedMUNGE<-function(nrRows,irisData){
 # irisData<-prepareIris()
    #Skapa ny dataframe för de olika varianserna
    detailedVariance<-data.frame()
    #Räkna ut varians enligt grupp
    library(plyr)
  #  detailedVariance<-ddply(preparedIris, .(Species), summarize,  Sepal.Length=sqrt(var(Sepal.Length)), Sepal.Width=sqrt(var(Sepal.Width)),Petal.Length=sqrt(var(Petal.Length)), Petal.Width=sqrt(var(Petal.Width)))
    detailedVariance<-ddply(irisData, .(Species), summarize,  Sepal.Length=(var(Sepal.Length)), Sepal.Width=(var(Sepal.Width)),Petal.Length=(var(Petal.Length)), Petal.Width=(var(Petal.Width)))
       #Förbereder iris för detailedMUNGE
    iris2<-merge(irisData, detailedVariance, by="Species")
    #Dataframe med varians i samma ordning som iris
    dV<-data.frame()
    dV<-iris2[6:9]
    head(dV)
    postPrepIris<-iris2[2:5]
    postPrepIris<-cbind(postPrepIris, iris2[,1])
    syntheticData<-data.frame()  
    iris6<-data.frame()
    i=0
    nrSeed=321
    loops=(nrRows/nrow(irisData))+1
    while (i < nrRows) {
      #  (trainingEx, syntheticData, dV,p)
      if(i==0){
        iris6=rbind(mungeWithDetailedColumnVariance(postPrepIris, syntheticData, dV, 4,(nrSeed+i)))     
      }else{
        temp<-mungeWithDetailedColumnVariance(postPrepIris, syntheticData, dV, 4, (nrSeed+i))
      #  colnames(temp) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
        iris6=rbind(iris6,temp)     
      } 
   #   colnames(iris6) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
      i=nrow(iris6)
    }
    iris6<-mixData(iris6)
    return(iris6)
  }
  sendNewDataIntoTree<-function(newData, testData){
    colnames(newData) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
    result<-irisSingleTree(newData, testData)
    return(result)
  }


  collectData<-function(testData, trainData){
    singleBaggingResult<-bagging(trainData, testData) 
    singleTreeResult<-irisSingleTree(trainData, testData)
    irisResult1.50<-munge1(5000, trainData)
    irisResult1.200<-munge1(20000,trainData)
    irisResult2.50<-munge2(5000,trainData)
    irisResult2.200<-munge2(20000, trainData)
    irisResult3.50<-performDetailedMUNGE(5000, trainData)
    irisResult3.200<-performDetailedMUNGE(20000,trainData)
    print("Har skapat exempel")

    irisResult1.50<-prepBagging(trainData, irisResult1.50)
    irisResult1.200<-prepBagging(trainData, irisResult1.200)
    irisResult2.50<-prepBagging(trainData, irisResult2.50)
    irisResult2.200<-prepBagging(trainData, irisResult2.200)
    irisResult3.50<-prepBagging(trainData, irisResult3.50)
    irisResult3.200<-prepBagging(trainData, irisResult3.200)
   
    #Småfix av egenskaper som försvinner när ett dataframe tas emot
    irisResult1.50<-as.data.frame(irisResult1.50)
    irisResult2.50<-as.data.frame(irisResult2.50)
    irisResult3.50<-as.data.frame(irisResult3.50)
    irisResult1.200<-as.data.frame(irisResult1.200)
    irisResult2.200<-as.data.frame(irisResult2.200)
    irisResult3.200<-as.data.frame(irisResult3.200)
    irisResult1.50<-addColNames(irisResult1.50)
    irisResult2.50<-addColNames(irisResult2.50)
    irisResult3.50<-addColNames(irisResult3.50)
    irisResult1.50<-updateFactor(irisResult1.50)
    irisResult2.50<-updateFactor(irisResult2.50)
    irisResult3.50<-updateFactor(irisResult3.50)
    irisResult1.200<-addColNames(irisResult1.200)
    irisResult2.200<-addColNames(irisResult2.200)
    irisResult3.200<-addColNames(irisResult3.200)
    irisResult1.200<-updateFactor(irisResult1.200)
    irisResult2.200<-updateFactor(irisResult2.200)
    irisResult3.200<-updateFactor(irisResult3.200)
    
   # print(plotSelectedData(irisResult1.200,irisResult2.200,irisResult3.200)) 
    
    irisResult1.50<-rbind(irisResult1.50,trainData)
    irisResult2.50<-rbind(irisResult2.50,trainData)
    irisResult3.50<-rbind(irisResult3.50,trainData)
    irisResult1.200<-rbind(irisResult1.200,trainData)
    irisResult2.200<-rbind(irisResult2.200,trainData)
    irisResult3.200<-rbind(irisResult3.200,trainData)    
    
    #Skapar en dataframe med resultat från träd 
    result<-data.frame()
    temp<-c(sendNewDataIntoTree(irisResult1.50, testData),  sendNewDataIntoTree(irisResult1.200, testData))
    result<-rbind(temp)
    temp<-c(sendNewDataIntoTree(irisResult2.50, testData),  sendNewDataIntoTree(irisResult2.200, testData))
    result<-rbind(result,temp)  
    temp<-c(sendNewDataIntoTree(irisResult3.50, testData),  sendNewDataIntoTree(irisResult3.200, testData))
    result<-rbind(result,temp)  
    row.names(result)<-c("Munge1","Munge2","Munge3")
    colnames(result)<-c("5000","20000")

    singleResultVector<-c(singleTreeResult,singleBaggingResult)
    result<-rbind(result, singleResultVector)
    print(result)
    print(singleTreeResult)
    print(singleBaggingResult)
    return(result)
    
  }
  #Dataframe med normaliserad och ihopblandad iris
  preparedIris<-prepareIris()
  preparedIris<-mixData(preparedIris)
  #Skapar 10 delar av iris som ska tränas och testas separat
  i = 1
  start=1
  result<-data.frame()
  allResults<-data.frame()
  temp=data.frame()
  nrsIts=10
  tree=0
  bagg=0
  result<-data.frame()
  temp=data.frame()
  print("iris")
  while(i<=nrsIts){
    print("iteration:")
    print(i)
    #ta den delen som inte är train och gör till testdata
    if(i==nrsIts){
      testData<-preparedIris[start:nrow(preparedIris),]      
    }else{
      testData<-preparedIris[start:(start+14),]      
    }
    #ta den delen som inte är train och gör till testdata
    if(i==nrsIts){
      trainData<-preparedIris[1:(start-1),]
    }
    else if(i>1){
      trainData<-preparedIris[1:(start-1),]
      trainData<-rbind(trainData,preparedIris[(start+15):nrow(preparedIris),] )
    }else{
      trainData<-preparedIris[(start+15):nrow(preparedIris),]
    }
    print(nrow(trainData))
    print(nrow(testData))
    t<-data.frame()
    t<-collectData(testData, trainData)
    temp<-t[1:(nrow(t)-1),]
    temp.tree<-t[nrow(t),1]
    temp.bagging<-t[nrow(t),ncol(t)]
    tVector<-c(temp[1,],temp[2,],temp[3,],temp.tree,temp.bagging)
    if(i==1){
      result=temp
      tree=temp.tree
      bagg=temp.bagging
      allResults<-rbind(tVector)
    }else{
      j=1
      while(j<=ncol(temp)){
        result[,j]=result[,j]+temp[,j]
        j=j+1
      }
      tree=tree+temp.tree
      bagg=bagg+temp.bagging
      allResults<-rbind(allResults,tVector)
    }
    i=i+1
    start=start+15

  }

  print("-------------------------------------------------------------------")
  print("Result")
  result=result/nrsIts
  tree=tree/nrsIts
  bagg=bagg/nrsIts
  print(result)
  print(bagg)
  print(tree)

  allResultsDiagram(allResults,"Iris 10 fold cross validation")
  averageResultDiagram(bagg,tree, result,"Iris")
  colnames(allResults) <- c("M1:5000","M1:20000","M2:5000","M2:20000","M3:5000","M3:20000","SingleTree","Bagging")
  rn<-c(1:nrow(allResults),"Average")
  g<-colMeans(allResults)
  allResults<-rbind(allResults,g)
  rownames(allResults)<-rn
  print(allResults)

}

performMungeForIris()



