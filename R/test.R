library(lattice)
#library(latticeExtra)
body.count.data<- read.csv("http://files.figshare.com/1332945/film_death_counts.csv")
body.count.data<-within(
	body.count.data, {
		Deaths_Per_Minute <-Body_Count / Length_Minutes
		ord<-order(Deaths_Per_Minute, decreasing=TRUE)
		}
	)
body.count.data<-body.count.data[body.count.data$ord,]
body.count.data<-body.count.data[1:25,]
body.count.data<-within(body.count.data, {
	Full_Title<-paste0(Film, " (",Year,")")
	ord<-order(Deaths_Per_Minute, decreasing=TRUE)
	Full_Title<-ordered(Full_Title,levels=rev(unique(Full_Title[ord])))
	})
graph<-barchart(Full_Title~Deaths_Per_Minute,data=body.count.data)
print(graph)
