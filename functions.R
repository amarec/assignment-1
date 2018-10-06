# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  # Set a default value to return
  result <- NULL
  x <- d[[var]] # Remember, as we showed in class: there are two ways
  # to access a column, and this is one; you should try
  # and figure out why the other way won't work here,
  # but that's not part of the homework
  if (!is.null(x)) { # This tests to see whether the column exists in
    # d; again, you should try and find a way to test
    # this out for yourself to verify that it's true,
    # but that's not part of the homework
    if (is.numeric(x)) {
      count = 0
      for (i in x) {
        count <- count + i
      }
      result <- count
    }
    # You will need to do a bit of research in order to figure out how
    # to test whether a vector contains numbers.
  }
}

# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
#
my_sum <- function(x) {
  # Assign a default value to the result
  result <- NULL
  # Check if x contains numeric values
  if (is.numeric(x)) {
    # Initialise our count to 0
    count = 0
    # Go through every value in x
    for (i in x) {
      # Add every value in x to the total count, one at a time
      count <- count + i
    }
    # We calculated the sum, so store it in the result to be returned
    result <- count
  }
}

# Divide the sum of the values in a vector by a number.
#
# ARGUMENTS:
# x: a vector
# k: a number
#
# RETURN VALUE:
# if the vector's values and k are numbers, returns the sum of
# all values, divided by k; otherwise, returns NULL
#
sum_divided_by <- function(x, k) {
  # Assign a default value to the result
  result <- NULL
  # Check if x contains numeric values
  if (is.numeric(x)) {
    # Check if k is a number
    if (is.numeric(k)) {
      # Get the sum of x's values using the my_sum function
      sum <- my_sum(x)
      # Compute the division and store the result
      result <- sum / k
    }
  }
}