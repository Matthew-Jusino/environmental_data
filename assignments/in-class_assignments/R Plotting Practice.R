library(MASS)
data(Animals)
head(Animals)

plot(x = Animals$body, y = Animals$brain)
plot(x = Animals$brain, y = Animals$body)

data_center_x = mean(Animals$body)
data_center_y = mean(Animals$brain)
c(data_center_x, data_center_y)

plot(x = Animals$body, y = Animals$brain, main = "Matthew's Purple Body/Brain Plot", col.main = "purple",
     xlab = "Body Mass",
     ylab = "Brain Mass")
points(x = data_center_x, y = data_center_y, col = "purple")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    0.3), 
  add = TRUE, col = "purple")


line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

line_point_slope()
