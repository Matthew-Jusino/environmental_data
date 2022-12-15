n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
vec_1[vec_2]
vec_2 = vec_1 == 3
vec_2

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
length(vec_1)
sum(vec_1 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

for (i in 1:10)
{
  print(
    paste0(
      "This is loop iteration: ",
      print(i)))
}

for (i in 1:n)
{
  n = 100
  print(i)
}

n = 17
vec_1 = sample(10, n, replace = TRUE)
for (i in 1:n)
{
  print(
    paste0(
      "The element of vec_1 at index ", i, " is ", vec_1[i]))
}


create_and_print_vec = function(n, min = 200, max = 2000)
{
  vec_1 = sample(x = min:max, size = n, replace = TRUE)
  for (i in 1:n)
  {
    print(
      paste0(
        "The element at index ", i, " is ", vec_1[i]))
  }
}
create_and_print_vec(10)