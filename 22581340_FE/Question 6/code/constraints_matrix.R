# Define the new order of asset classes
new_order <- c("currency", "bond", "bond", "bond", "bond", "bond", "bond", "commodity", "currency","equity","equity","equity","equity")
constraints_matrix <- matrix(0, nrow = length(new_order), ncol = length(new_order))

# Loop through each element in the matrix
for (i in seq_along(new_order)) {
    for (j in seq_along(new_order)) {
        if (i == j) {
            constraints_matrix[i, j] <- -1  # Set diagonal elements to -1
        } else if (new_order[i] == new_order[j]) {
            constraints_matrix[i, j] <- 1   # Set elements with the same asset class to 1
        }
    }
}