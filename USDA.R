usda <-read.csv("/dsa/data/all_datasets/USDA.csv")

usda$Ratio <- usda$Sugar/usda$TotalFat

usda<-usda[usda$Ratio!=0]

usda<-usda[order(usda$Ratio)]

head(usda,8)