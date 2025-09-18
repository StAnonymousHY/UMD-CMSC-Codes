import numpy as np

# Define your matrix A
A = np.array([
    [1, 1, 1],
    [1, 2, 4],
    [1, 3, 9],
    [1, 4, 16],
    [1, 5, 25],
])

AAT = np.dot(A, A.T)
ATA = np.dot(A.T,A)

# Compute the eigenvalues and eigenvectors
eigenvalues1, eigenvectors1 = np.linalg.eig(AAT)
eigenvalues2, eigenvectors2 = np.linalg.eig(ATA)

# Display the results
print("Eigenvalues AAT:")
print(eigenvalues1)

print("\nEigenvectors AAT:")
print(eigenvectors1)

print("Eigenvalues ATA:")
print(eigenvalues2)

print("\nEigenvectors ATA:")
print(eigenvectors2)


print(np.linalg.svd(A))