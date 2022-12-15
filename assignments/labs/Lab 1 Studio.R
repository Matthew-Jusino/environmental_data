my_list_1 <- list(5.2, "five point two", c(0:5))
my_list_1 <- list(two = 5.2,
                  one = "five point two",
                  three = c(0:5))
my_list_1

my_list_1[[1]]
my_list_1[[as.numeric("1")]]
my_list_1[["1"]]
my_list_1[["one"]]
my_list_1$one
my_list_1$"one"
my_list_1$1
my_list_1$"1"

