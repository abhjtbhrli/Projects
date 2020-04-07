# Here's a simple code to create a results matrix from one season of a football league
# We use data from Indian Super League 2019-20 (ISL 19-20)
# The data used is very basic
# The dataset in use contains all the results from the 90 matches of the league stage of ISL 19-20

# Install the readxl package to access and load the dataset from your PC
install.packages ("readxl")
library (readxl)
isl <- read_excel("C:/Users/AB/Documents/isl-results.xlsx")
# The dataset in my PC is saved in the location "C:/Users/AB/Documents/isl-results.xlsx", which goes as the argument inside the read_excel() function
# The dataset is saved as isl in the workspace

# Let's have a look at the structure of the dataset, its headers, etc.
head(isl)
# A tibble: 6 x 7
  day   date     home            result away             hgoal agoal
  <chr> <chr>    <chr>           <chr>  <chr>            <dbl> <dbl>
1 Sun   20/10/19 Kerala Blasters 2-1    ATK                  2     1
2 Mon   21/10/19 Bengaluru       0 - 0  NorthEast United     0     0
3 Tue   22/10/19 Jamshedpur      2-1    Odisha               2     1
4 Wed   23/10/19 Goa             3-0    Chennaiyin           3     0
5 Thu   24/10/19 Kerala Blasters 0 - 1  Mumbai City          0     1
6 Fri   25/10/19 ATK             5 - 0  Hyderabad            5     0

# Now that we know how the dataset looks like, let's get to plotting the results in matrix format

# We use the ggplot2 package
install.packages("ggplot2")
library(ggplot2)

ggplot(isl,aes(x=away,y=home,fill=hgoal-agoal))+
geom_tile()+
geom_label(aes(label=paste(hgoal,agoal,sep = "-")),fill="white")+
scale_fill_gradient2(low = "red",high = "green",midpoint = 0,guide = FALSE)+
scale_x_discrete(limits = levels(isl$home), position = "top") + 
scale_y_discrete(limits = rev(levels(isl$away)))+
theme_classic()+
theme(text = element_text("serif"))+
labs(title="ISL 19-20 LEAGUE STAGE RESULTS",caption="darker green = more emphatic home win, darker red = more emphatic away win")+
xlab("AWAY")+
ylab("HOME")
