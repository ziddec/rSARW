# Self-Avoiding Random Walk
# Begins at (0, 0)
# Moves uniformly through possible directions (N, S, W, E) with step size = 1
# When there are no possible directions, goes back by a random number of steps

#### Auxiliary functions ####

# Checks if path matrix has a certain point
check_path <- function(mat, vet) {
    resp <- apply(mat, 1, function(x, want) isTRUE(all.equal(x, want)), vet)
    if (any(resp))
        ret <- which(resp) else 
            ret <- 0
        ret
}

# Moves the last point by a direction (N, S, E, W)
move <- function(pos, orient) {
    new_pos <- NA
    if (orient == 'N') new_pos <- pos + c(0, 1) else
        if (orient == 'S') new_pos <- pos + c(0, -1) else
            if (orient == 'E') new_pos <- pos + c(1, 0) else
                if (orient == 'W') new_pos <- pos + c(-1, 0)
                new_pos
}

#### Self Avoiding Random Walk ####

sarw <- function(iter = 100, plot = FALSE) {
    k <- 0
    path <- matrix(0, nrow = 1, ncol = 2) # starting point = c(0, 0)
    i <- 1
    
    while (i < iter) {
        k <- k + 1
        
        # calculates positions after all possible movements
        possible_pos <- rbind(move(path[i, ], 'N'),
                              move(path[i, ], 'S'),
                              move(path[i, ], 'E'),
                              move(path[i, ], 'W'))
        
        # checks which orientations are available
        avail_or <- apply(possible_pos, 1,
                          function(possible_pos) check_path(path, possible_pos))
        avail <- ifelse(avail_or > 0, 0, 1)
        names(avail) <- c('N', 'S', 'E', 'W')
        
        # if no orientation is available, go back
        if (all(avail == numeric(4))) {
            goback <- ifelse(i > 5, rpois(1, 5) + rbinom(1, i, 0.05), 0) + 1
            path <- path[1:(i - goback), ]
            i <- i - goback - 1
            } else {
                # if one or more orientations are available, sample one
                # uniformly and move to it
                chosen_or <- sample(x = names(avail), size = 1, prob = avail)
                path <- rbind(path, move(path[i, ], chosen_or))
            }
        
        i <- i + 1
    }
    
    if (plot) {
        xrange <- range(path[ , 1])
        yrange <- range(path[ , 2])
        plot(NA, xlim = c(xrange[1] - 10, xrange[2] + 10),
             ylim = c(yrange[1] - 10, yrange[2] + 10),
             xlab = NA, ylab = NA)
        points(0, 0, col = 'darkgreen', pch = 16, cex = 0.5)
        lines(path, col = 'darkgreen')
        points(path[nrow(path), 1], path[nrow(path), 2], col = 'red',
               pch = 4, cex = 0.5)
    }
    
    list(path = path, total_iterations = k)
}

#### Test ####

cobr <- sarw(iter = 200, plot = TRUE)
