custdata <-read.table("/Users/trenthaun/Documents/GitHub/R-for-Data-Science-Training-Repo/Custdata/custdata.tsv",
                      header = T, 
                      sep = '\t')

summary(custdata)

#Visualize for better understanding of distribution:

libarary(ggplot2)

#histogram example

ggplot(custdata) +
  geom_histogram(aes(x=age),
                 binwidth=5,
                 fill="gray")

#density example, need to load scales for labels

library(scales)

ggplot(custdata) +
  geom_density(aes(x=income)) +
  scale_x_continuous(labels = dollar)

#Data is heavy to one side, non-negative, and wide ranged, try logarithmic distro:

ggplot(custdata) +
  geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,100000), labels = dollar) +
  annotation_logticks(sides="bt")

#create a bar chart

ggplot(custdata) + geom_bar(aes(x=marital.stat), fill='gray')

#bar graph of large number of categories

ggplot(custdata) +
  geom_bar(aes(x=state.of.res), fill='gray') +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))

#sorting would be helpful!  Must be done at the factor level, not in ggplot2

statesums <- table(custdata$state.of.res)
statef <- as.data.frame(statesums)
colnames(statef) <- c("state.of.res", "count")
summary(statef)
statef <- transform(statef,state.of.res=reorder(state.of.res, count))
summary(statef)
ggplot(statef) + geom_bar(aes(x=state.of.res,y=count), 
                          stat="identity",
                          fill='gray') +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))

# Line Plots

x <- runif(100)
y <- x^200 + 0.2*x

ggplot(data.frame(x=x,y=y)) + aes(x=x,y=y) + geom_line()

#smoothing curves and scatter plots

custdata2 <- subset(custdata, custdata$age > 0 & custdata$age < 100 & custdata$income > 0)

cor(custdata2$age,custdata2$income)

ggplot(custdata2) + aes(x=age,y=income) + geom_point() + ylim(0,200000)

ggplot(custdata2) + aes(x=age,y=income) + geom_point() + geom_smooth(method="lm") + ylim(0,200000)

ggplot(custdata2) + aes(x=age,y=income) + geom_point() + geom_smooth() + ylim(0,200000)

#plot boolean scatterplot and use jitter to reduce overplotting

ggplot(custdata2) + aes(x=age, y=as.numeric(health.ins)) + geom_point(position=position_jitter(w=0.05,h=0.05)) + geom_smooth()

#when data sets get too large, may be easier to use hexbin (aggregating) plot, install hexbin package first

ggplot(custdata2, aes(x=age, y=income)) + geom_hex(binwidth=c(5,10000)) + geom_smooth(color="white", se=F) + ylim(0,100000)

#for two categorical variables, use bar charts:

# Title: Specifying different styles of bar chart 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins))   # Note: 1 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="dodge")      	# Note: 2 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="fill")        	# Note: 3

# Note 1: 
#   Stacked bar chart, the 
#   default 

# Note 2: 
#   Side-by-side bar chart 

# Note 3: 
#   Filled bar chart 

#Add a "Rug" to get a sense of density for fill charts:

ggplot(custdata,aes(x=marital.stat)) + geom_bar(aes(fill=health.ins), position="fill") + 
                                 geom_point(aes(y=-0.05), size=0.75, alpha=0.3, position=position_jitter(h=0.01))

# Title: Plotting a bar chart with and without facets 

ggplot(custdata2) +                                            # Note: 1 
  geom_bar(aes(x=housing.type, fill=marital.stat ),
           position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   	# Note: 2 

ggplot(custdata2) +                                          	# Note: 3 
  geom_bar(aes(x=marital.stat), position="dodge",
           fill="darkgray") +
  facet_wrap(~housing.type, scales="free_y") +               	# Note: 4 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   	# Note: 5

# Note 1: 
#   Side-by-side bar chart. 

# Note 2: 
#   coord_flip commandTilt the x-axis labels so they 
#   don’t overlap. You can also use coord_flip() to rotate the graph, as we 
#   saw previously. Some prefer coord_flip() because the theme() layer is 
#   complicated to use. 

# Note 3: 
#   The faceted bar chart. 

# Note 4: 
#   Facet the graph by housing.type. The scales="free_y" argument specifies that each facet has 
#   an independently scaled y-axis (the default is that all facets have 
#   the same scales on both axes). The argument free_x would free the 
#   x-axis scaling, and the argument free frees both axes. 

# Note 5: 
#   As of this writing, 
#   facet_wrap is incompatible with coord_flip, so we have to tilt the 
#   x-axis labels. 



