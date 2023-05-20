#Q1 Print your name at the top of the script
#load these libraries: FSA, FSAdata, magrittr,dplyr, tidyr plyr, tidyverse

print("Devi Somalinga Bhuvanesh") #To print the name

install.packages("FSA")      #to install the package
library(FSA)                 #to import the package in the library
install.packages("FSAdata")
library(FSAdata)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("plyr")
library(plyr)
install.packages("tidyverse")
library(tidyverse)


#Q2 Import inchBio.csv and name the table bio 
bio <- read.csv2("/Users/devi/Documents/Devi/MPS Analytics/Introduction to Analytics/Module 3/inchBio.csv", sep=",") #to read the database in R
bio   #to view the database

#Q3 Display head, tail and structure of bio
head(bio)  #to check default top few rows and columns of bio database
tail(bio)  #to check default last few rows and columns of bio database
str(bio)   #to check structure of bio

#Q4 Create an object, <counts>, that counts and lists all the species records
counts <- bio[3]  #to retrieve the list of species name with the row numbers
counts #to view the count and list of all species records

#Q5 Display just the 8 levels (names) of the species
unique(bio[3])  #to view the unique names

#Q6 Create a <tmp> object that displays the different species and the number of record in each species in the dataset.
tmp <- table(bio[3])   #to create a table of different species and its frequency
tmp                    #to view the different species and its frequency which is horizontally placed
tmp <- as.data.frame(tmp) #convert tmp to data frame
tmp                    #to view the different species and its frequency in a table/list format
         
#Q7 Create a subset, <tmp2>, of just the species variable and display the first five records
#Option 1 - To view the first five value under each Species
tmp2 <- ddply(bio, .(species), function(x) head(x, n=5) [3])  #Using ddply function, displayed the first five records of each species in the dataset
tmp2   #to view the output of tmp2

#Option 2 - To view the subset of bio and the first five records
tmp2 <- subset(bio[3])  #to extract the subset values under bio species column
tmp2                  #to view the subset of bio
head(tmp2,n=5)       #to view the first five records

#Q8 Create a table, <w>, of the species variable. Display the class of w
w <- table(bio[3])   #table created for displaying species variable and its frequency
w                    #to view the species and its frequency
class(w)             #to view the type of class of w

#Q9 Convert <w> to a data frame named <t> and display the results
t <- as.data.frame(w)    # to convert w from table format to a dataframe
class(t)                 # to view the type of class of t
t                        # to view the output of t in table formatt
  
#Q10 Extract and display the frequency values from the <t> data frame
t[2]                   #extract the second column of t which has the frequency values

#Q11 Create a table named <cSpec> from the bio species attribute (variable) 
#confirm that it displays the number of species in the dataset <bio>
cSpec <- table(bio[3])     #to create a table cSpec of bio species with frequency
cSpec                      #to view the output of cSpec
class(cSpec)               #to view the class of cSpec
totalcSpec <- sum(cSpec)   #to add all the values in cSpec
totalcSpec                 #to view total values in cSpec

totaldataset <- nrow(bio)  #to check number of values in dataset bio 
totaldataset               #to view total values in dataset bio

totalcSpec == totaldataset  #to verify whether the total values in cSpec and in dataset are same

#Q12  Create a table named <cSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class.
cSpec                    #to view the cSpec data stored in R
nrow(cSpec)              #to check number of rows in cSpec
cSpecPct <- round(prop.table(cSpec)*100, 2) #to calculate percentage of freuqency of each species
cSpecPct                 #to view the output of cSpecPct which has the percentages of frequency of each species
class(cSpecPct)          #to check the class of cSpecPct

# TO ASK: When I give a margin as 1 (since it is a row vector) or margin="species"
# all values are set to 100%. Why is that the case? Without a margin value, what
# dimension is it using to calculate the marginal sum?

#Q13 Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame
u <- as.data.frame(cSpecPct) #convert cSpecPct from table to dataframe and store in u
u                            # to view the output of u
class(u)                     #to view the class of u

#Q14 Create a barplot of <cSpec> with the following: 
# Title: Fish Count
# Y axis is labeled “COUNTS”
# Color the bars Light Green
# Rotate Y axis to be horizontal
# Set the X axis font magnification to 60% of nominal

cSpec <- as.data.frame(cSpec)     #convert cSpec from table to data frame format
cSpec                             # to view cSpec
ggplot(data=cSpec) +              #Using ggplot, ggtitle, geom_bar, to provide the features
ggtitle("Fish Counts") +
geom_bar(stat = "identity", aes(x=species, y=Freq),fill="lightgreen") +
labs(y="COUNTS")+
theme(axis.text.x = element_text(size = rel(0.60))) + #to set x axis to 60% of nominal
coord_flip()                   #to rotate Y axis to the horizontal

#Q15 Create a barplot of <cSpecPct>, with the following specifications:
# Y axis limits of 0 to 40
# Y axis label color of Light Blue
# Title of “Fish Relative Frequency”
cSpecPct <- as.data.frame(cSpecPct)     #to convert cSpecPct to data frame format to make the bar chart
cSpecPct           #to view the output of cSpecPct
ggplot(data=cSpecPct) +                 #Using ggplot function to make the barchart
  ggtitle("Fish Relative Frequency") +  
  geom_bar(stat = "identity",aes(x=species, y=Freq)) +
  coord_cartesian(ylim = c(0,40))+
  theme(axis.title.y = element_text(colour="lightblue")) 
 
#Q16 Rearrange the <u> cSpecPct data frame in descending order of relative frequency. 
#Save the rearranged data frame as the object <d>
u                   #to view cSpecPct data stored in u
class(u)              #to know the type of class 
d <- u[order(-u$Freq), ]     #to order the frequency in descending order
d                            #to view the values stored in d

#Q17 Rename the <d> columns Var 1 to Species, and Freq to RelFreq
colnames(d) <- c("Species", "RelFreq")  #to change the column names 
d                                       #to view the output of d with names changed
class(d)                                #to view the class of d

#Q18 Add new variables to <d> and call them cumfreq, counts, and cumcounts

#to calculate Cumulative Frequency from d database
d                                   #to view the values stored in d
cumfreq <- cumsum(d$RelFreq)        #to calculate cumulative frequency
cumfreq                             #to view cumulative frequency

#to calculate counts from bio database
cSpec                                 #to view the values stored in cSpec
class(cSpec)                          #to view class of cSpec
counts <- cSpec[order(-cSpec$Freq), ] #descending order of cSpec
counts                                #value of counts

#to calculate cumulative counts 
cumcounts <- cumsum(counts$Freq)     #to calculate cumulative counts
cumcounts                            #to view cumulative counts

#to add new columns with its values in the table
d <- cbind(d,cumfreq,counts$Freq,cumcounts)        #to bind values of each column
colnames(d) <- c("Species", "RelFreq", "CumFreq", "Counts", "Cumcounts") #Column names
d  #to view output of d


#Q19 Create a parameter variable <def_par> to store parameter variables
def_par <- list(cex.axis = 0.7, mar = c(10, 5, 2, 4), yaxs = "i", las = 2 ) #listed to set parameter variables of font axis, margin of plot, set y limits within internal axis,set label of axis 
par(def_par)  #to store listed parameter variables
def_par      #to view the default parameter

#Q20 Create a barplot, <pc>, with the following specifications:
# d$counts of width 1, spacing of .15
# no boarder
# Axes: F
# Yaxis limit 0,3.05*max
# d$counts na.rm is true
# y label is Cummulative Counts
# scale x axis to 70%
# names.arg: d$Species
# Title of the barplot is “Species Pareto"
# las: 2
dev.new(width=7, height=7)           #to open the plot in new window and set the size
par(mar=c(10, 5, 2, 4))              #default margin in parameter
pc <- barplot(d$Counts, width = 1, space = 0.15, border = NA, axes = F,
             ylim = c(0, 3.05 * max(d$Counts, na.rm = TRUE)), 
             ylab = "Cummulative Counts" , cex.names = 0.70, 
             names.arg = d$Species,
             main = "Species Pareto", las=2)     #create barplot as per instruction

#Q21. Add a cumulative counts line to the <pc> plot with the following:
# Spec line type is b
# Scale plotting text at 70%
# Data values are solid circles with color cyan4
lines(pc, d$Cumcounts, type = "b", cex = 0.7, pch = 20, col="cyan4") #draw line with values as per instruction

#Q22 Place a grey box around the pareto plot
box(col="grey62")     #draw box for plot

#Q23 Add a left side axis with the following specifications
#Horizontal values at tick marks at cumcounts on side 2
#Tickmark color of grey62
#Color of axis is grey62
#Axis scaled to 80% of normal
axis(side = 2, at = c(0, d$Cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)   #add features to axis as mentioned

#Q24 Add axis details on right side of box with the specifications:
# Spec: Side 4
# Tickmarks at cumcounts with labels from 0 to cumfreq with %,
# Axis color of cyan5 and label color of cyan4 
# Axis font scaled to 80% of nominal
axis(side = 4, at=c(0,d$Cumcounts), labels=paste(c(0,d$CumFreq),"%"),las = 1, col.axis = "cyan4", col = "cyan4", cex.axis=0.8) #add features to axis as mentioned

#Q25 Display the finished Species Pareto Plot (without the star watermarks).Have your last name on the plot
mtext("Bhuvanesh", las=1,
      at=6,
      adj = 0,
      side=3, line=-3, col="navyblue")   #to add text inside pareto plot 


#Report 
#Descriptive Analysis
summary(bio)
str(bio)
d

#1. Stacked Bar Chart - Species vs Scale type
install.packages("ggplot2")
library(ggplot2)
bio <- read.csv2("/Users/devi/Documents/Devi/MPS Analytics/Introduction to Analytics/Module 3/inchBio.csv", sep=",") #to read the database in R
bio
par(ask=TRUE)
bio$species
ggplot(data = bio, aes(x = species, fill = scale))+
geom_bar() +
  ggtitle("Presence of Scale in Fish") +
  labs(x="Species",y="Counts") +
  theme(plot.title=element_text(face="bold.italic",
                                size="14", color="brown"),
        axis.title=element_text(face="bold.italic",
                                size=10, color="brown"),
        axis.text=element_text(face="bold", size=9,
                               color="darkblue"),
        panel.background=element_rect(fill="white",
                                      color="darkblue"),
        panel.grid.minor.x=element_blank(),
        legend.position="right") +
  coord_flip()

#2:Length vs Scale 
bio$scale <- as.factor(bio$scale)
bio$scale
plot(bio$scale,bio$tl,
     main="Fish Length Vs Fish Scale", 
     col.main="brown",
     ylab="Total Length", xlab="Scale", col=c("gray", "lightblue"))  

#3 Scale vs Length of each species
library(lattice)     #to split and plot the data based on each species
dotplot(bio$scale~bio$tl | bio$species, 
        main="Type of Species = Fish length Vs Fish Scale", 
        xlab="Total Length", ylab="Scale")
