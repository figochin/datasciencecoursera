source('cachematrix.R')

# Create a random matrix with dimension: 1000 by 1000
rand_m <- matrix(runif(1000000, 1, 10), 1000, 1000)
test_m <- makeCacheMatrix(rand_m)

# First instance with normal inverse solving method
time_now1 <- Sys.time()
inverse_m1 <- cacheSolve(test_m)
time_diff1 <- Sys.time() - time_now1
print(paste0("Time taken for 1st trial: ", time_diff1))

# Second instance with cache data retrieval
time_now2 <- Sys.time()
inverse_m2 <- cacheSolve(test_m)
time_diff2 <- Sys.time() - time_now2
print(paste0("Time taken for 2nd trial: ", time_diff2))


# > source('cachematrix.R')
# > 
#     > # Create a random matrix with dimension: 1000 by 1000
#     > rand_m <- matrix(runif(1000000, 1, 10), 1000, 1000)
# > test_m <- makeCacheMatrix(rand_m)
# > 
#     > # First instance with normal inverse solving method
#     > time_now1 <- Sys.time()
# > inverse_m1 <- cacheSolve(test_m)
# > time_diff1 <- Sys.time() - time_now1
# > print(paste0("Time taken for 1st trial: ", time_diff1))
# [1] "Time taken for 1st trial: 3.74635410308838"
# > 
#     > # Second instance with cache data retrieval
#     > time_now2 <- Sys.time()
# > inverse_m2 <- cacheSolve(test_m)
# getting cached data
# > time_diff2 <- Sys.time() - time_now2
# > print(paste0("Time taken for 2nd trial: ", time_diff2))
# [1] "Time taken for 2nd trial: 0.0164299011230469"
