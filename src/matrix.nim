import vector

import math

type
  TMatrix*[T; R, C: static[int]] = array[0..R-1, array[0..C-1, T]] ## Row major matrix type. 


proc identity(mat: type TMatrix): TMatrix {.noSideEffect.} =
  ## Returns the identity matrix given the specified params
  ## This proc is unnecessarily inefficient for now because of
  ## the compiler
  const r = range[0..TMatrix.D-1]
  const T = TMatrix.T
  for i in r:
    for j in r:
      if i == j:
        result[i][j] = 1.T
      else:
        result[i][j] = 0.T

# It dun diddely work!
#template identity(m: typedesc[TMatrix]): expr =
#  when m.R != m.C:
#    {.fatal: "Identity is only defined for square matrices".}
#  identity[m.T, m.R]()

proc `==`*(a, b: TMatrix): bool {.noSideEffect.} =
  ## Checks if two matrices are equal to each other, that is, they have
  ## identical elements
  for i in low(a)..high(a):
    for j in low(a[i])..high(a[i]):
      if a[j][i] != b[j][i]:
        return false

  return true

proc `~=`*[T, R, C](a, b: TMatrix[T, R, C], tolerance: float = 1.0e-5): bool {.noSideEffect.} =
  ## Approximately equal
  ## Takes in to account that floating points are not usually the same
  result = true
  for i in 0..R-1:
    for j in 0..C-1:
      if abs(a[i][j] - b[i][j]) >= tolerance:
        return false

proc diagonal*[T, D](m: TMatrix[T, D, D]): TVector[T, D] {.noSideEffect.} =
  ## Returns the diagonal as a TVector
  ## Currently only works on square matrices
  for i in 0..D-1:
    result[i] = m[i][i]

proc trace*[T, D](m: TMatrix[T, D, D]): T {.noSideEffect.} =
  ## Returns the trace of the matrx (the sum of the diagonal)
  ## Currently only works on square matrices
  result = 0
  for i in 0..D-1:
    result += m[i][i]

proc transposed*(m: TMatrix): TMatrix {.noSideEffect.} = 
  ## Returns a transpoed version of the passed in matrix
  for i in low(m)..high(m):
    for j in low(m[i])..high(m[i]):
      result[j][i] = m[i][j]

template `+`*(m: TMatrix): expr =
  m

proc `+`*(a, b: TMatrix): TMatrix {.noSideEffect.} =
  for i in low(a)..high(b):
    for j in low(a[i])..high(a[i]):
      result[i][j] = a[i][j] + b[i][j]

proc `-`*(m: TMatrix): TMatrix {.noSideEffect.} =
  for i in low(m)..high(m):
    for j in low(m[i])..high(m[i]):
      result[i][j] = -m[i][j]

proc `-`*(a, b: TMatrix): TMatrix {.noSideEffect.} =
  for i in low(a)..high(a):
    for j in low(a[i])..high(a[i]):
      result[i][j] = a[i][j] - b[i][j]

proc `*`*(a: auto, b: TMatrix): TMatrix {.noSideEffect.} =
  ## Multiplies the matrix with the scalar and returns the result
  for i in low(b)..high(b):
    for j in low(b[i])..high(b[i]):
      result[i][j] = a * b[i][j]

proc `/`*[T, R, C](m: TMatrix[T, R, C], s: T): TMatrix[T, R, C] {.noSideEffect.} =
  ## Divides the matrix by the given scalar and returns the result
  for i in 0..R-1:
    for j in 0..C-1:
      result[i][j] = T(m[i][j]/s)

template `*`*(a: TMatrix, b: auto): TMatrix =
  ## Alias that reverses the parameter order so that mul(a, b) and mul(b, a)
  ## yields the same result when a is a scalar and b is a matrix
  b * a

template row*[T; R, C: static[int]](m: TMatrix[T, R, C], rowidx: range[0..R-1]): TVector[T, R] =
  m[rowidx]

proc col*[T; R, C: static[int]](m: TMatrix[T, R, C], colidx: range[0..C-1]): TVector[T, C] {.noSideEffect.} =
  for i in low(m)..high(m):
    result[i] = m[i][colidx]

proc `*`*[T; R, N, C: static[int]](a: TMatrix[T, R, N], b: TMatrix[T, N, C]): TMatrix[T, R, C] {.noSideEffect.} =
  ## Multiplies two matrices together and returns the result. The column count
  ## of the first matrix must be the same as the row count of the second
  ## matrix. The result matrix has the same row count as the first matrix
  ## and the same column count as the second.:
  for i in low(a)..high(a):
    for j in low(a[i])..high(a[i]):
      result[i][j] = a.row(i) *. b.col(j)

proc `*`*[T; R, C](a: TVector[T, C], b: TMatrix[T, R, C]): TVector[T, C] {.noSideEffect.} =
  ## Multiplies a vector with a matrix, the vector is treated as a row vector
  for i in low(a)..high(a):
    result[i] = a *. b.col(i)

proc `*`*[T; R, C](b: TMatrix[T, R, C], a: TVector[T, R]): TVector[T, R] {.noSideEffect.} = 
  ## Multiplies a vector with a matrix, the vector is treated as a column vector
  for i in low(a)..high(a):
    result[i] = a *. b.row(i)

when false:
  ## TODO: Add submatrix proc that takes arrays of which rows and cols that should be deleted
  proc sub*[T; R, C: range](m: TMatrix[T, R, C], therow, thecol: TInteger): TMatrix[T, R.low..R.high-2, C.low..C.high-2] {.noSideEffect.} =
    ## Returns the submatrix of the given matrix
    ## m.sub(2, 3) deletes row 2 and column 3 and returns the result.
    for i in R.low..R.high:
      if i == therow:
        continue
      for j in C.low..C.high:
        if j == thecol:
          continue
        let ro = if i < therow: i else: i - 1
        let co = if j < thecol: j else: j - 1
        result[ro][co] = m[i][j]

## Hard-coded implementations of the submatrix proc
proc sub*[T](m: TMatrix[T, 2, 2], therow, thecol: TInteger): T {.noSideEffect.} =
  result = m[if therow == 1: 0 else: 1][if thecol == 1: 0 else: 1]

# TODO: DRY this up
proc sub*[T](m: TMatrix[T, 3, 3], therow, thecol: TInteger): TMatrix[T, 2, 2] {.noSideEffect.} =
  for i in 0..2:
    if i == therow:
      continue
    for j in 0..2: 
      if j == thecol:
        continue
      let ro = if i < therow: i else: i - 1
      let co = if j < thecol: j else: j - 1
      result[ro][co] = m[i][j]

proc sub*[T](m: TMatrix[T, 4, 4], therow, thecol: TInteger): TMatrix[T, 3, 3] {.noSideEffect.} =
  for i in 0..3:
    if i == therow:
      continue
    for j in 0..3:
      if j == thecol:
        continue
      let ro = if i < therow: i else: i - 1
      let co = if j < thecol: j else: j - 1
      result[ro][co] = m[i][j]

when false:
  proc det*[T, N](m: TMatrix[T, N, N]): T {.noSideEffect.} =
    ## Returns determinant of specified NxN matrix
    const i = 0 # Arbitrarily select a row to use, 0 is proper since it's always there
    result = 1  # start with identity number

    for j in m[i].low..m[i].high:
      result *= m[i][j] * m.cofactor(i, j)

proc det*[T](m: TMatrix[T, 2, 2]): T {.noSideEffect.} =
  ## Returns the determinant of the specified 2x2 matrix
  result = m[0][0] * m[1][1] - m[0][1] * m[1][0]

proc det*[T](m: TMatrix[T, 3, 3]): T {.noSideEffect.} =
  ## Returns the determinant of the specified 3x3 matrix
  result =
            m[0][0] * m[1][1] * m[2][2] +
            m[0][1] * m[1][2] * m[2][0] +
            m[0][2] * m[1][0] * m[2][1] -
            m[0][2] * m[1][1] * m[2][0] -
            m[0][1] * m[1][0] * m[2][2] -
            m[0][0] * m[1][2] * m[2][1]

proc minor*[T, R, C](m: TMatrix[T, R, C], therow, thecol: TInteger): T {.noSideEffect.} =
  ## Returns the minor of matrix m.
  result = det(m.sub(therow, thecol))

template `{}`*(m: TMatrix, n: range[0..99]): expr =
  ## Alternative to (and more math looking) minor proc that currently has the
  ## limitation that it only works as expected on matrices where the dimensions
  ## do not exceed 99.
  m.minor(n div 10 mod 10, n mod 10)

# Only a helper for cofactor
# Should probably not be used by anyone
proc `^`(a, b: TInteger): TInteger {.noSideEffect.} =
  result = 1
  for i in 1..b:
    result *= a

proc cofactor*[T, R, C](m: TMatrix[T, R, C], therow, thecol: TInteger): T =
  ## Returns the cofactor of matrix m
  if (-1)^(therow+thecol) < 0:
    result = -m.minor(therow, thecol)
  else:
    result = m.minor(therow, thecol)

proc adj*[T, R, C](m: TMatrix[T, R, C]): TMatrix[T, R, C] {.noSideEffect.} =
  for i in 0..R-1:
    for j in 0..C-1:
      result[i][j] = m.cofactor(j, i)

proc inversed*(m: TMatrix): TMatrix {.noSideEffect.} =
  result = adj(m)/det(m)

proc orthogonalized*[T, R, C](m: TMatrix[T, R, C]): TMatrix[T, R, C] {.noSideEffect.} =
  ## Uses Gram-Schmidt orthogonalization on matrix m and returns the
  ## orthogonalized matrix
  var iterations = R.low
  for i in R.low..R.high:
    result[i] = m[i]
    for j in R.low..iterations:
      var prev = result.row(j)
      result[i] = result.row(i) - ((m.row(i) *. prev)/(prev *. prev))*prev
    inc(iterations)

proc isOrthogonal*[T, R, C](m: TMatrix[T, R, C], tolerance: float = 1.0e-5): bool =
  when T is TReal:
    result = m.transposed.`~=`(m.inversed, tolerance)
  else:
    result = m.transposed == m.inversed

proc `$`*[T, R, C](m: TMatrix[T, R, C]): string {.noSideEffect.} =
  result = ""
  for i in 0..R-1:
    result &= $m.row(i) & "\n"
