# ------------------------------------------------
# Sourcing file that contains Homework 09 Functions
# 30 Mar 2020
# ELE
# ------------------------------------------------
#

# ------------------------------------------------
# FUNCTION GetData
# description: putting the global variables into a data frame
# inputs: global variables
# outputs: data frame of global varibales
##################################################
GetData <- function(var_a=12,
                    var_b=c("Control","Treatment_Pre","Treatment_Post"),
                    ex_control=rnorm(n=6,mean=5.2,sd=2),
                    ex_pre=rnorm(n=3,mean=4.9,sd=2),
                    ex_post=rnorm(n=3,mean=1.0,sd=2)) {
  
  ID <- 1:sum(var_a)
  Erosion_Rate <- c(ex_control, ex_pre, ex_post)
  Treatment <- rep(var_b, var_a)
  
  df <- data.frame(ID,Erosion_Rate,Treatment)

  return(df)
  
} # end of function GetData
# ------------------------------------------------
#my_df <- GetData()
#print(my_df)
# ------------------------------------------------
# FUNCTION CalculateStuff
# description: creating a anova model
# inputs: global variables
# outputs: Anova Model
##################################################
CalculateStuff <- function(df = my_df) {
  ANOmodel <- aov(df[,2]~df[,3],data=df)

return(ANOmodel)
  
} # end of function CalculateStuff
# ------------------------------------------------
#model <- CalculateStuff()
#print(model)
#print(anova)
# ------------------------------------------------
# FUNCTION GraphResults
# description: creates an anova graph of the dataset
# inputs: erosion rate data frame
# outputs: Graph
##################################################
GraphResults <- function(df = my_df) {
  ANOPlot <- ggplot(data=df, aes(x=df[,3], y=df[,2],
                    fill=df[,3]))+geom_boxplot()

print(ANOPlot)
  message("Message: ANOVA graph created")
} # end of function GraphResults
# ------------------------------------------------
#GraphResults()
# ------------------------------------------------
# FUNCTION Q_plot_graph
# description: description
# inputs: input_desciption
# outputs: output_description
##################################################
Q_plot_graph<- function(df=my_df) {
  p1 <- qplot(data=df,
              x=df[,3],
              y=df[,2],
              geom=c("smooth","point"))
  print(p1)
  message("Message: Regression graph created")

return(p1)
} # end of function Q_plot_graph
# ------------------------------------------------
#Q_plot_graph()
