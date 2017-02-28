#Code to try out megaphone plot
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)


RandomData <- function(n=100, raw_mean = -1, noise_sd=1) {
	#Function to generate random data with f,y, and labels
	#inputs:
	#   n        : number of values desired
	#   raw_mean : mean of input to forecasts (untransformed)
	#   noise_sd : noise of how forecast is turned into actual (higher is worse forecast)
	#   p        : proportion of actual outcomes you want to be 1
	#output:
	#   n by 3 data.frame with columns
	#		f is forecast (numeric)
	#       y is actual (numeric)
	#       labels is labels (string)

	#generate n realizations of ystar and transform to probability scale
	#using inverse logistic
	ystar <- rnorm(mean=raw_mean ,n=n)
	f <- plogis(ystar)
	y <- rbinom(n,1,p=plogis(ystar+rnorm(n=n,sd=noise_sd)))
	raw_labels <- c(letters,LETTERS,month.abb)
	labels <- raw_labels[1]
	for (i in 2:n) {
		#Use modulus to recycle letters/labels
		j <- i %% length(raw_labels) 
		if (j==0) {
			labels<-c(labels,raw_labels[length(raw_labels)])
		} else {
			labels<-c(labels,raw_labels[j+1])
		}
	}
	return(data.frame(f=f,y=y,labels=labels))
}



#################
#Gives f1 and f2, systematically favoring f1. 
################
RandomData2 <- function(n=100, raw_mean = -1, noise_sd=1,err_m=5) {
	ystar <- rnorm(mean=raw_mean ,n=n,sd=noise_sd)
	f1 <- plogis(ystar)
	y<-rbinom(n,1,p=plogis(ystar+rnorm(n=n,sd=noise_sd)))
	raw_labels <- c(letters,LETTERS,month.abb)
	labels <- raw_labels[1]
	for (i in 2:n) {
		#Use modulus to recycle letters/labels
		j <- i %% length(raw_labels) 
		if (j==0) {
			labels<-c(labels,raw_labels[length(raw_labels)])
		} else {
			labels<-c(labels,raw_labels[j+1])
		}
	}
	df<-data.frame(f1=f1,y=y,ystar=ystar,labels=labels)
	df0<-subset(df,y==0)
	n0<-nrow(df0)
	df1<-subset(df,y==1)
	n1<-nrow(df1)
	df0$f2<-plogis(df0$ystar+rgamma(n=n0,shape=.5,rate=err_m)-rgamma(n=n0,shape=.5,rate=err_m+(.5*err_m)))
	df1$f2<-plogis(df1$ystar-rgamma(n=n1,shape=.5,rate=err_m)+rgamma(n=n1,shape=.5,rate=err_m+(.5*err_m)))
	df_fin<-rbind(df0,df1)
	return(df_fin)
}



MegaPlot9 <- function(f, y, labels, worstN=10, size_adjust=0,right_margin=7,top_margin=1.5,label_spacing=10,lab_adjust=.5,text_size=10,title="Model Diagnostic Plot") {
	################################################################
	#Function to create diagnostic plot for a single model
	#
	#input:
	#  f      : N by 1 vector of forecasts f_{i} in (0,1)
	#  y      : N by 1 vector of forecasts y_{i} is 0 or 1
	#  labels : N by 1 vector of textual labels (strings)
	#  worstN : How many values to label, these will be worst for each class
	#  
	#
	#  size_adjust	: adjusts distribution of space between density plot and
	#						the main plot. Usually values from 
	#						[0,.7], but you can also go into negative 
	#						numbers where you need the margx plot
	#						(density plot on x axis) to get larger relative 
	#						to main plot. >0.6 starts getting ugly. 
	#						Default is 0.5.
	#  right_margin		: adjusts the right margin. 
	#						Tends to be between [5,15].
	#						Default value is 7.
	#  top_margin		: adjusts the top margin of PLOT 
	#						(not including title). 
	#						Useful if labels are off the upper end of page. 	
	#						Likely between [1,5]. Default value is 1.5.
	#
	#
	#  label_spacing 		: Spacing between labels 
	#  lab_adjust			: shift label right or left vis-a-vis the line
	#  text_size				: font size
	#        
	#output:
	#
	#  object that needs "grid.draw()" to plot
	#
	################################################################
	
	data <- data.frame(f=f, y=y, labels=labels)
	pdata <- data %>% mutate(y_minus_f=y-f) %>% arrange(f) %>% mutate(forecastOrder = row_number())
	#still need to label worstN
	pdata <- pdata %>% group_by(y) %>% arrange(desc(abs(y_minus_f))) %>% mutate(label_worst=ifelse(row_number()<=worstN, as.character(labels), " "))
	#need to create var for absolute errors
	pdata<-pdata%>%mutate(abserr=abs(y_minus_f))
	#create indicator for worst values
	pdata <- pdata %>% group_by(y) %>% arrange(desc(abs(y_minus_f))) %>% mutate(isworstn=ifelse(row_number()<=worstN, 1, 0))
	#for coloring
	pdata <- pdata %>% mutate(coloring=
		ifelse(y==1 & isworstn==1, '1w',
		ifelse(y==0 & isworstn==1, '0w',
		ifelse(y==1 & isworstn==0, '1',
		'0'))))
	#arrange data for plotting
	pdata<-pdata%>%arrange(forecastOrder)
	N=nrow(pdata)
	labbuffer=(nchar(N)-3)*.3
	boolcolors<-as.character(c(
		'1w'='#cddff4', #very light blue
		'0w'='#0862ca', #bold blue
		'1'='#fecfdc', #very light red
		'0'='#fd1205')) #bold red
	boolscale<-scale_color_manual(name='coloring',values=boolcolors)
	###################
	#initialize plots.
	#	Object "o2" contains the full plot we care about,
	#		minus the lines & labels. 
	#	Object "margx" is the marginal on the x axis of f|y=0 & f|y=1
	###################
	o1 <- ggplot(pdata, aes(x=f,y=forecastOrder,group=y, color=as.factor(coloring)))+boolscale
	o2 <- o1 + geom_point(aes(alpha=(isworstn)))  +geom_rug(side="r")+xlim(c(0,1))+ylim(c(0,N))+theme_bw()+theme(panel.grid.major=element_line(colour='grey'),panel.grid.minor=element_line(colour='grey'),panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.minor.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank(),legend.position='none',plot.margin=unit(c(top_margin,right_margin,-.2,1),"lines")) +labs(y='Observation (ordered by f)')+boolscale
	margx<-ggplot(pdata,aes(f,fill=factor(y)))+geom_density(alpha=.4)+scale_fill_manual(values=c('blue','red'))+xlim(c(0,1))+labs(x='Forecast Value')+theme_bw()+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position="none",plot.margin=unit(c(0,right_margin,0.2,3.35+labbuffer),"lines"))
	
	###################
	#Lines and Labels
	###################	
	z<-o2
	count0=0
	count1=0
	for (i in 1:length(pdata$label_worst)) {
		
		################################
		#Prepare to position labels
		################################	
		text_spacing<-label_spacing
			
		labeltext<-pdata$label_worst[i]
		if(labeltext == ' '){
			next
		}
		obsy=pdata$y[i]
		if(obsy==0){
			count0<-count0+text_spacing
		}
		if(obsy==1){
			count1<-count1+text_spacing
		}
		if(count1==text_spacing){
			y1init=pdata$forecastOrder[i]
		}
		if(count0==text_spacing){
			y0init=pdata$forecastOrder[i]
		}
		
		fpos<-pdata$f[i]
		##############################
		#Set the parameters for labels
		##############################
		ycolor<-ifelse(obsy==0,'blue','red')
		ypos_text<-ifelse(obsy==0,
			(y0init+(count0-text_spacing)),
			(y1init+(count1-text_spacing))
			)
		ifelse(pdata$forecastOrder[i]>ypos_text,LineSlope<-c(1,0),LineSlope<-c(0,1))
		labjust_left=1.1
		labjust_right=labjust_left+lab_adjust
		
		###############################
		#Create the labels on plot
		###############################
		current<-
			z+
			annotation_custom(
			grob=textGrob(label=labeltext,
				gp=gpar(fontsize=text_size,col=ycolor)),
			ymin=ypos_text,
			ymax=ypos_text,
			xmin=labjust_left,
			xmax=labjust_right
			)+
			annotation_custom(
			grob=linesGrob(
				x=c(1,labjust_left),
				y=LineSlope,
				gp=gpar(col=ycolor)
				),
				ymin=
					ifelse(
					pdata$forecastOrder[i]<=ypos_text,
					pdata$forecastOrder[i],
					ypos_text),
				ymax=
					ifelse(
					pdata$forecastOrder[i]>ypos_text,
					pdata$forecastOrder[i],
					ypos_text)
			)+
			annotation_custom(
			grob=linesGrob(
				x=c(fpos+.05,.95),
				y=0,
				gp=gpar(col=ifelse(obsy==0,'#f0f5fb','#fef0f4'))
				),
				ymin=pdata$forecastOrder[i],
				ymax=pdata$forecastOrder[i]
			)
		z<-current
		}
		
	#Turn off clipping so we can render the plot
	gt <- ggplot_gtable(ggplot_build(z))
	gt$layout$clip[gt$layout$name == "panel"] <- "off"
	o3<-arrangeGrob(gt,margx,ncol=1,nrow=2,heights=c(4+size_adjust,1-size_adjust),top=textGrob(title,gp=gpar(fontsize=15,font=2),just='top'))
	return(o3)
}



BicepPlot <- function(f1, f2, y, labels, bestN=10, label_spacing=3,right_lab_adjust=0.02,bottom_lab_adjust=0.03,right_margin=5,bottom_margin=5,top_margin=1,transp_adjust=1, m1title="Model 1", m2title="Model 2", hlines=TRUE,vlines=TRUE,rare=FALSE) {
	################################################################
	#Function to create Bi-Separation plot for comparing two models
	#
	#input:
	#
	#	f1 = Vector of forecasts from model 1
	#	f2 = Vector of forecasts from model 2
	#	y = Vector of actual outcomes
	#	labels = Vector of labels per observation
	#	bestN = max num. of "most improved" obs you want labeled per model
	#
	#
	#	label_spacing = distance between labels
	#	right_lab_adjust = how far from the end of line do you want label?
	#						(usually very small; default is 0.02)
	#	bottom_lab_adjust = how far from end of line do you want label?
	#						(usually very small; default is 0.03)
	#	right_margin = how much whitespace needed on right side of plot?
	#					(for if labels don't fit; default is 5)
	#	bottom_margin = how much whitespace needed on bottom of plot?
	#					(for if labels don't fit; default is 5)
	#	top_margin = how much whitespace needed on top of plot?
	#					(for if labels don't fit; default is 1)
	#
	#	transp_adjust = float from [0,10] to adjust opacity of non-labeled
	#					points. Accounts for density of case universe.
	#					(Default is 1. Generally on the extremes (1 vs 10)).
	#						(0 will render them extremely light)
	#
	#	hlines = T/F whether you want faded lines horizontally pointing
	#				from M2 separation plot to most improved observations.
	#				(Default is TRUE)
	#	vlines = T/F whether you want faded lines vertically pointing
	#				from M1 separation plot to most improved observations.
	#				(Default is TRUE)
	#
	#	rare = T/F whether you have an excessive number of zeros.
	#				Adjusts coloring for visibility.
	#				(Default is FALSE)
	#
	#output:
	#	
	#	object that needs "grid.draw()" to plot
	# 
	################################################################
	
	data <- data.frame(f1=f1, f2=f2, y=y, labels=labels)
	
	pdata <- data %>% arrange(f1) %>% mutate(forecastOrder1 = row_number())
	pdata <- pdata %>% arrange(f2) %>% mutate(forecastOrder2 = row_number()) 
	pdata <- pdata %>% mutate(fdiff=forecastOrder2-forecastOrder1) %>% arrange(fdiff)

	
	###########################
	#Label the worst N for m1 and m2
	###########################
	
	#########
	#model 1, labels worstN given y=0, and given y=1
	#########
	
	#label best, given y==1
	pdata <- pdata %>% arrange(fdiff) %>% arrange(-y) %>% mutate(isbestn_m1_y1=ifelse(row_number()<=bestN & fdiff<0,1,0))
	
	#label best, given y==0
	pdata <- pdata %>% arrange(-fdiff) %>% arrange(y) %>% mutate(isbestn_m1_y0=ifelse(row_number()<=bestN & fdiff>0,1,0))
	
	#create "label_best_m1" based on those
	pdata <- pdata %>% mutate(label_best_m1= ifelse(isbestn_m1_y0==1 | isbestn_m1_y1==1, as.character(labels)," "))

	#########
	#model 2, same as above
	#########
	
	#label and color best, given y==1
	pdata <- pdata %>% arrange(-fdiff) %>% arrange(-y) %>% mutate(isbestn_m2_y1=ifelse(row_number()<=bestN & fdiff>0,1,0)) 
	
	#label best, given y==0
	pdata <- pdata %>% arrange(fdiff) %>% arrange(y) %>% mutate(isbestn_m2_y0=ifelse(row_number()<=bestN & fdiff<0,1,0))
	
	#create "label_best_m2" based on those
	pdata <- pdata %>% mutate(label_best_m2= ifelse(isbestn_m2_y0==1 | isbestn_m2_y1==1, as.character(labels)," "))
	
	N=nrow(pdata)
	############
	#Now we need to set up colors for plot
	############
	
	####
	#label "best n" for both models
	####
	pdata$transp[pdata$isbestn_m2_y0==1 | pdata$isbestn_m1_y0==1]<-1
	pdata$transp[pdata$isbestn_m2_y0==0 & pdata$isbestn_m1_y0==0 & pdata$y==0]<-0+(.1*transp_adjust)
	pdata$transp[pdata$isbestn_m2_y1==1 | pdata$isbestn_m1_y1==1]<-1
	pdata$transp[pdata$isbestn_m2_y1==0 & pdata$isbestn_m1_y1==0 & pdata$y==1]<-0+(.1*transp_adjust)
	
	####
	#1. if either m2 or m1 is "best", and y=0
	#2. if neither m2 nor m1 is "best", and y=0
	#3. if either m2 or m1 is "best" and y=1
	#4. if neither m2 nor m1 is "best" and y=1
	####
	
	pdata$coloring[pdata$isbestn_m2_y0==1 | pdata$isbestn_m1_y0==1]<-'0b'
	pdata$coloring[pdata$isbestn_m2_y0==0 & pdata$isbestn_m1_y0==0 & pdata$y==0]<-'0'
	pdata$coloring[pdata$isbestn_m2_y1==1 | pdata$isbestn_m1_y1==1]<-'1b'
	pdata$coloring[pdata$isbestn_m2_y1==0 & pdata$isbestn_m1_y1==0 & pdata$y==1]<-'1'
	
	
	if(transp_adjust==0){
		y0_litecol<-'#f5f8fc'
		y1_litecol<-ifelse(rare==TRUE,'#fecdca','fef5fb')
	} else {
		y0_litecol<-'#cddff4'
		y1_litecol<-ifelse(rare==TRUE,'#fd5950','#fecfdc')
	}
	
	boolcolors<-as.character(c(
		'0'= y0_litecol, #very light blue
		'0b'='#0862ca', #bold blue
		'1'= y1_litecol, #very light red
		'1b'='#fd1205')) #bold red
	boolscale<-scale_color_manual(name='coloring',values=boolcolors)
	
	##############
	#Arrange by model 2 for lines/labels
	##############
	pdata<- pdata %>% arrange(forecastOrder2)
	
	
	###################
	#initialize plots.
	#	Object "o2" contains the full plot we care about,
	#		minus the lines & labels. 
	###################
	o1 <- ggplot(pdata, aes(x=forecastOrder1,y=forecastOrder2,color=as.factor(coloring),group=y))+boolscale
	mart=F
	o2 <- o1+geom_point(aes(alpha=((transp))))+geom_rug(side="br")+geom_abline(intercept=0,slope=1)+xlim(c(0,nrow(pdata)))+ylim(c(0,nrow(pdata)))+theme_bw()+theme(plot.title=element_text(size=rel(1)),legend.position='none',plot.margin=unit(c(top_margin,right_margin,bottom_margin,1),'lines'),axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.text.x=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank())+labs(title=m1title,y=m2title)+boolscale

	###################
	#1.		Model 2 bestN
	###################	
	z<-o2
	count=0
	for (i in 1:length(pdata$label_best_m2)) {
		
		if(pdata$label_best_m2[i]==" ") {
			next
		}
		###############################
		#for the lines
		###############################
		
		obsy=pdata$y[i]
		label_spacing=label_spacing
		
		#set the first erroneous obs to a flat line
		#	since the next code adds "label_spacing",
		#	I subtract it out here so it'll end up
		#	a flat line.
		#
		#Provides a baseline using previous text
		#	for this current label's position
		
		if(count==0) {
			yinit<-pdata$forecastOrder2[i]-label_spacing
		} else {
			yinit<-ypos_text
		}						
		
		#####	
		#calculate 2nd y-point for every line
		#	note that the first one will be
		#	completely horizontal, 
		#####
		
			ypos_text<-yinit+label_spacing
			count=count+1
		
		#####
		#Determine whether you'll have a negative slope
		#	pointing from FO2 down to label, or a 
		#	positive slope pointing from label up to
		#	the FO2. 
		#####
					
		if(pdata$forecastOrder2[i]>ypos_text) {
			LineSlope<-c(1,0)
		} else {
			LineSlope<-c(0,1)
		}
		
		#Coloring for y
		ycolor<-ifelse(obsy==0,'blue','red')	
		###############################
		#Create the labels on plot
		###############################
		labeltext<-pdata$label_best_m2[i]
		labjust_left<-1.1
		labjust_right<-right_lab_adjust
		
		current<-
			z+
			annotation_custom(
			grob=textGrob(label=labeltext,
				x=labjust_left+labjust_right,
				gp=gpar(col=ycolor,fontsize=7)
				),
			ymin=ypos_text,
			ymax=ypos_text,
			)+
			annotation_custom(
			grob=linesGrob(
				x=c(1,labjust_left),
				y=LineSlope,
				gp=gpar(col=ycolor),
				),
			
			#need to constrain min & max y to 
			#	either observed or label position,
			#	depending on direction of slope
			ymin=ifelse(y==0,ypos_text,pdata$forecastOrder2[i]),
			ymax=ifelse(y==0,pdata$forecastOrder2[i],ypos_text),
			)
			
			###########################
			#Decide whether to add horizontal pointer lines
			###########################
			hcol<-ifelse(obsy==0,'#cddff4','#fecfdc')
			if(hlines==TRUE){
				current_fin<-current+
					annotation_custom(
					grob=linesGrob(
						x=c(0,1),
						y=LineSlope,
						gp=gpar(col=ifelse(mart==F,hcol,ycolor)),
						),
					ymin=pdata$forecastOrder2[i],
					ymax=pdata$forecastOrder2[i],
					xmin=ifelse(mart==F,pdata$forecastOrder1[i],0)
					)
			} else {
				current_fin<-current
			}

		z<-current_fin
	}
	
	pdata <- pdata %>% arrange(forecastOrder1)	
	#########################
	#2. 	Model 1 bestN
	#########################
	
	z2<-z
	count=0
	
	for (i in 1:length(pdata$label_best_m1)) {
		
		##################################
		#If it's not a worstN, pass it
		##################################
		
		if(pdata$label_best_m1[i]==" ") {
			next
		}
		
		###############################
		#for the labels
		###############################
		
		obsy=pdata$y[i]
		
		#
		#Set the baseline: first label should be
		#	directly under the FOpoint.
		#
		
		if(count==0) {
			xinit<-pdata$forecastOrder1[i]-label_spacing
		} else { #store the previous position so we can tack on space
			xinit<-hpos_text
		}
		
						
		
		#####	
		#calculate 2nd y-point for every line
		#	First line will be vertical, 
		#	each will have labels a certain distance
		#	from the previous label.
		#####
		label_spacing=label_spacing
		
		hpos_text<-xinit + label_spacing
		count=count+1
				
		#Coloring for y
		ycolor<-ifelse(obsy==0,'blue','red')	
		
		#####
		#Determine whether you'll have a negatively
		#	sloped line pointing to the label,
		#	or a positively sloped line, depending
		#	on where the label must go (based on
		#	its positioning)
		#####
		
		if(pdata$forecastOrder1[i] > hpos_text){
			LineSlope<-c(1,0)
		} else {
			LineSlope<-c(0,1)
		}
		
		
		###############################
		#Create the labels on plot
		###############################
		labeltext<-pdata$label_best_m1[i]
		label_adjust_top=bottom_lab_adjust
		
		current<-
			z2+
			annotation_custom(
			grob=textGrob(
				label=labeltext,
				y=-0.1-label_adjust_top,
				rot=-90,
				gp=gpar(col=ycolor,fontsize=7)
				),
			xmin=hpos_text,
			xmax=hpos_text,
			)+
			annotation_custom(
			grob=linesGrob(
				y=c(0,-.1),
				x=LineSlope,
				gp=gpar(col=ycolor),
				),

			xmin=ifelse(
				pdata$forecastOrder1[i] > hpos_text,
				hpos_text,
				pdata$forecastOrder1[i]
				),
			xmax=ifelse(
				pdata$forecastOrder1[i] > hpos_text,
				pdata$forecastOrder1[i],
				hpos_text
				),
			)
			
			########################
			#Decide whether to add vertical pointer lines
			########################
			vcol<-ifelse(obsy==0,'#cddff4','#fecfdc')
			if(vlines==TRUE){
				current_fin<-current+
					annotation_custom(
					grob=linesGrob(
						y=c(0,1),
						x=LineSlope,
						gp=gpar(col=ifelse(mart==F,vcol,ycolor)),
						),
					xmin=pdata$forecastOrder1[i],
					xmax= pdata$forecastOrder1[i],
					ymax=ifelse(mart==F,pdata$forecastOrder2[i],N)
					)
			} else {
				current_fin<-current
			}
		z2<-current_fin
		}

		
		
	#Turn off clipping so we can render the plot
	gt <- ggplot_gtable(ggplot_build(z2))
	gt$layout$clip[gt$layout$name == "panel"] <- "off"
	o3<-arrangeGrob(gt)
	return(o3)

}





########################################################
#End: functions
#
#begin: testing
########################################################


data1 <- RandomData2(n=500,noise_sd=1,err_m=2.5)
data2 <- RandomData(n=500,noise_sd=1)
test <- MegaPlot9(f=data2$f, y=data2$y, labels=data2$labels, worstN=10,size_adjust=0.2,right_margin=7,top_margin=1.5,label_spacing=10,lab_adjust=.2,text_size=7)

grid.draw(test) #WARNING: plot doesn't show up w/o "grid.draw()" function

test<-BicepPlot(f1=data1$f1,
	f2=data1$f2,
	y=data1$y,
	labels=data1$labels,
	bestN=10, 
	label_spacing=20,
	right_lab_adjust=.02,
	bottom_lab_adjust=0.03,
	transp_adjust=10,
	hlines=T,
	vlines=T
	)

grid.draw(test)

#############
#for manual use
#############

f1=data1$f1
f2=data1$f2
y=data1$y
labels=data1$labels
bestN=10
label_spacing=3
right_lab_adjust=.02
bottom_lab_adjust=0.03

#############
#FL
#############
test<-BicepPlot(f1=fl.three$pred2,
	f2=fl.three$pred,
	y=fl.three$onset,
	labels=fl.three$country,
	bestN=10,
	label_spacing=125,
	right_lab_adjust=.1,
	bottom_lab_adjust=0.1,
	right_margin=8,
	bottom_margin=7,
	top_margin=2,
	m1title='Model 1: gdp/pop only',
	m2title='Model 2: full FL',
	hlines=T,
	vlines=T)

grid.draw(test)