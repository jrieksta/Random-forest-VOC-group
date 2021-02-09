###########09022021#############
#Created by : Jolanta Rieksta
######## RANDOM FOREST (RF) CODE FOR VOC GROUP #########
#Code includes: 
             # Supervised and unsupervised RF 
             # MDS plots
             # Variable importance plots
             # nicer plots using ggplot

######load libraries and data ########
.libPaths("C:/R Packages")
suppressPackageStartupMessages({
library(randomForest)
library(tidyverse)
library(ggplot)
library(ggpubr)
})

#load data
setwd("C:/Users/lcm767/OneDrive - Københavns Universitet/Desktop/Projects/Repositories/Random-forest-VOC-group")
voc<-read.table("09022021_Paddus_RF.csv",head = TRUE, sep=",")
voc[is.na(voc)] <- 0
#transform data into relative proportions
names(voc)
e=voc[,-c(1, 1:42)] #remove non compound rows
prop<-cbind(id = e[, 1], e[, -1]/rowSums(e[, -1]))
propdatanstd<-cbind(voc[,c(1, 1:42)],prop) #bind back id and info data

total<-propdatanstd
#subset data, in this example, in three measurement campaigns and call it t1, t2, t3

#all three campaigns together
total$Date[total$Date=="30.06.2018"] <- "June 30"
total$Date[total$Date=="05.07.2018"] <- "July 5"
total$Date[total$Date=="11.07.2018"] <- "July 11"

#each campaign separately
t1<- subset(total, Date==c("June 30"))
t2<- subset(total, Date==c("July 5"))
t3<- subset(total, Date==c("July 11"))


#SUPERVISED FR 
#mtry=sqrt( or # of VOC) how many compound each time will be excluded 
#sqrt(221) 14.86607 = 15
#mtree=how many trees RF will build
#do trace= visual, see the progress after every 1000 trees

##########SUPERVISED FR #################
t2_total<- randomForest(t2[, 45:263], y=as.factor(t2$herbivory), proximity=TRUE, mtry=15, importance=T, do.trace=1000, ntree=10000)


plot(t2_total) #we see high error, model has not learned enough with 10 000 trees, let´s increase the # of trees
print(t2_total) # returns the model output, that shows the model, out-of-bag-error, and error for each class (control, herbivory)

classification <- cbind(t2[21], t2_total$predicted) #we can make a table to see RF´s predicted and our assigned samples 
classification

varimp <- importance(t2_total, scale=TRUE)[,3] #variable importance 
sort(varimp)

varImpPlot(t2_total) #variable importance table

########### UNSUPERVISED RF ###############

t2_total_ns<- randomForest(t2[, 45:263], proximity=TRUE, mtry=15, importance=T, do.trace=1000, ntree=10000)


print(t2_total_ns) # returns the model output, that shows the model, and that it has been performed as unsupervised

#obtain proximity matrix
t2.prox_ns<-cmdscale(1-t2_total_ns$proximity)
# as data frame
t2_df_ns<-as.data.frame(t2.prox_ns, row.names = NULL)


prox <- t2_total_ns$proximity

#################### VISUALISATION ##########################
#obtain proximity matrix
#RF proximity: samples are “close” if they end up in the same leaf frequently (in many trees)
#the proximity of samples can be visualized using a Multi-Dimensional Scaling (MDS) plot

#obtain the proximity
t2.prox<-cmdscale(1-t2_total$proximity) #cmdscale must be a distance matrix, so 1 - proximity can be used
t2.prox_ns<-cmdscale(1-t2_total_ns$proximity) #cmdscale must be a distance matrix, so 1 - proximity can be used


#defauld MDS plot based on RF proximity  = Plot the scaling coordinates of the proximity matrix from randomForest.

MDSplot(t2_total, t2$herbivory)
MDSplot(t2_total_ns, t2$herbivory)


#litttle nicer version
t2$herbivory=as.factor(t2$herbivory)
#supervised
plot(t2.prox, col=c("orange", "#3366FF")[t2$herbivory], pch = c( 17,16)[as.numeric(t2$herbivory)],cex=2)
legend("topleft", pch = c(17,16), col=c("orange", "#3366FF"), c("control", "herbivory"), cex =1.2)

#unsupervised
plot(t2.prox_ns, col=c("orange", "#3366FF")[t2$herbivory], pch = c( 17,16)[as.numeric(t2$herbivory)],cex=2)
legend("topleft", pch = c(17,16), col=c("orange", "#3366FF"), c("control", "herbivory"), cex =1.2)


#nicer ggplot option

#theme
my_theme=theme(text = element_text(size=15),
               panel.border = element_rect(colour = "black", fill=NA, size=0.5),
               axis.text.x = element_text(size = 13),
               axis.text.y = element_text(size = 13),
               axis.title.x=element_text(size = 13),
               axis.title.y=element_text(size = 13)
               
) 



#make proximity matrix as dataframe
t2_df<-as.data.frame(t2.prox, row.names = NULL)
t2_df_ns<-as.data.frame(t2.prox_ns, row.names = NULL)

#without ellipse

#supervised
t2_non_ell=ggplot(t2_df, aes(x=V1, y=V2, color=t2$herbivory))+
  geom_point(aes(shape=t2$herbivory, color=t2$herbivory),size=4.5)+theme_classic()+
  labs(fill = "Herbivory")+
  ggtitle("July 5 supervised RF")+
  scale_color_manual(values=c("#440154FF", "#DCE318FF"))+my_theme

#unsupervised
t2_non_ell_ns=ggplot(t2_df_ns, aes(x=V1, y=V2, color=t2$herbivory))+
  geom_point(aes(shape=t2$herbivory, color=t2$herbivory),size=4.5)+theme_classic()+
  labs(fill = "Herbivory")+
  ggtitle("July 5 unsupervised RF")+
  scale_color_manual(values=c("#440154FF", "#DCE318FF"))+my_theme

#with ellipse

#supervised 
t2_ell=ggplot(t2_df, aes(x=V1, y=V2, color=t2$herbivory))+
  geom_point(aes(shape=t2$herbivory, color=t2$herbivory),size=4.5)+theme_classic()+
  labs(fill = "Herbivory")+
  ggtitle("July 5 supervised RF")+
  scale_color_manual(values=c("#440154FF", "#DCE318FF"))+my_theme+
  stat_ellipse(aes(x = V1,y=V2,fill=factor(t2$herbivory)),
               geom="polygon",level=0.95,alpha=0.2)+guides(fill = FALSE)   +
  scale_fill_manual(values=c("#440154FF", "#DCE318FF"))

#unsupervised 
t2_ell_ns=ggplot(t2_df_ns, aes(x=V1, y=V2, color=t2$herbivory))+
  geom_point(aes(shape=t2$herbivory, color=t2$herbivory),size=4.5)+theme_classic()+
  labs(fill = "Herbivory")+
  ggtitle("July 5 unsupervised RF")+
  scale_color_manual(values=c("#440154FF", "#DCE318FF"))+my_theme+
  stat_ellipse(aes(x = V1,y=V2,fill=factor(t2$herbivory)),
               geom="polygon",level=0.95,alpha=0.2)+guides(fill = FALSE)   +
  scale_fill_manual(values=c("#440154FF", "#DCE318FF"))

merge<-ggarrange(t2_non_ell,t2_non_ell_ns,t2_ell,t2_ell_ns,nrow=2, ncol=2, common.legend = TRUE,labels = c("A", "B", "C", "D"))




########## nicer visualisation of Variable importance ############
#VARIABLE IMPORTANCE GRAPHS 
varimp <- importance(t2_total, scale=TRUE)[,3] 
varimp.df <-as.data.frame(varimp)
varimp.df <- tibble::rownames_to_column(varimp.df, "VOC")
varimp.df<-varimp.df %>% arrange(desc(varimp), VOC) # descending order

t2_plot<-varimp.df[1:20,] #take top 20 VOCS
names(t2_plot)[2] <- "Variable_importance" #rename column 


#plot variable importance
t2_graph=ggplot(data=t2_plot, mapping = aes(x = reorder(VOC,Variable_importance), Variable_importance), col=Variable_importance)+
  theme_classic()+geom_bar(stat="identity")+
  coord_flip()+scale_fill_brewer(palette="Set2")+ggtitle("July 5")+
  labs(x=expression(VOC),
       y=expression(Variable~importance))+theme(legend.position = "none")+
  theme(panel.background = element_rect(colour = "black", size=1, fill=NA))

#END#