# 
# Generates pdf plots from a TSV table with the following command:
# > source("generatePlots.R")
#

raw_data = read.table("sweepDatapoints_n20good.tsv", header=T)

sat_data <- c()
unsat_data <- c()

for( i in seq(4,280,4)){

	CUT = raw_data$m == i
#	print(summary(raw_data[CUT,]))
	
	slice = raw_data[CUT,]	
	SAT = slice$isSat == 1
	UNSAT = slice$isSat == 0
	sat_slice = slice[SAT,]
	unsat_slice = slice[UNSAT,]	
	
#	print(paste(i," ", nrow(sat_slice), " ", nrow(unsat_slice), sep=""))

	sat_data = c(sat_data, nrow(sat_slice))
	unsat_data = c(unsat_data, nrow(unsat_slice))	
}


opt <- options("scipen" = 20)
getOption("scipen")


plotNames = list( c("executionTime", "Execution Time", "Execution Time in Seconds"), 
			   	  c("memory",        "Witness Memory", "Witness Memory Usage in Bytes"), 
			   	  c("mixCount",      "Mix",            "Number of Mixes"), 
			   	  c("extractCount",  "Extract",        "Number of Extracts"),
			   	  c("appendCount",   "Append",         "Number of Appends"),
			   	  c("purifyCount",   "Purify",         "Number of Purifications"),
			   	  c("splitCount",    "Split",          "Number of Splits"))



for (i in plotNames){

	filename = paste(i[1],".pdf", sep="")
	pdf( filename, width=10, height=8)								    #set pdf filename and size

	plot( raw_data$m/raw_data$n,										# select x-axis data
		  raw_data[,c(i[1])],											# select y-axis data
		  main=i[2],													# set plot title
		  xlab="Clause-Variable Ratio",
		  ylab=i[3],
		  		  
#		  ylim= c(1, 200),
#		  xlim= c(0, 10),
		  
#		  log = "y",
		  
		  cex=c(1, 2.5)[as.numeric(raw_data$isSat)+1],				   #set unsat/sat datapoint size
		  pch=c(21, 22, 24)[as.numeric(raw_data$Algorithm)+1],		   #set algorithm marker shape
		  col=rgb(0,0,0,0.5),                                          #set algorithm marker color
		  bg=c(rgb(1,0,0,0.1), rgb(0,1,0,0.1), rgb(0,0,1,0.1))[as.numeric(raw_data$Algorithm)+1])	


#x <- seq(60, 128, by=4)/20.0
#y <- sat_data[15:32]
x <- seq(4, 280, by=4)/20.0
y <- sat_data[1:length(x)]

par(new=TRUE)
plot(x/20,y, axes=FALSE, xlab="",ylab="")
lines(x/20, y, col='black', lwd=4)

axis(4)
mtext("Percent Satisfiable", side=4, line=-2, adj=0.5, cex=1.25)

#	barplot(sat_data)
#	barplot(unsat_data)

	dev.off()															#complete pdf	

	
  }
