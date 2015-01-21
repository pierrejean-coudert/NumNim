from math import pow, sqrt
from macros import error

type
  TVector*[T; D: static[int]] = array[0..D-1, T] ## T = Type, I = Indices - will possibly later be changed to Dimension (number)


proc `==`*(a, b: TVector): bool {.noSideEffect.} =
  for i in low(a)..high(a):
    if a[i] != b[i]:
      return false

  return true

## TODO: change tolerance to TReal once regression in compiler has been fixed
proc `~=`*(a, b: TVector, tolerance: float = 1.0e-5): bool {.noSideEffect.} =
  ## Approximately equal
  ## Takes in to account that floating points are not usually the same
  for i in low(a)..high(a):
    if abs(a[i] - b[i]) >= tolerance:
      return false

  return true

template `+`*(a: TVector): expr =
  a

proc `+`*(a, b: TVector): TVector {.noSideEffect.} =
  for i in low(a)..high(a):
    result[i] = a[i] + b[i]

proc `-`*(a: TVector): TVector {.noSideEffect.} =
  for i in low(a)..high(a):
    result[i] = -a[i]

proc `-`*(a, b: TVector): TVector {.noSideEffect.} =
  for i in low(a)..high(a):
    result[i] = a[i] - b[i]

proc mag*(a: TVector): float =
  for i in low(a)..high(a):
    when TVector.T is int:
      result += pow(toFloat(a[i]), 2.0)
    elif TVector.T is float:
      result += pow(a[i], 2.0)
    else:
      {.fatal: "Cannot pow that datatype".}
  result = sqrt(result)

proc `*.`*(a, b: TVector): TVector.T {.noSideEffect.} =
  ## Vector dot product
  for i in low(a)..high(a):
    result += a[i] * b[i]

proc `*+`*(a, b: TVector): TVector {.noSideEffect.} =
  ## Vector cross product
  when len(a) != 3:
    {.fatal: "Cross product only works with 3 dimensional vectors".}
  result =
    [a[1] * b[2] - b[1] * a[2],
     a[2] * b[0] - b[2] * a[0],
     a[0] * b[1] - b[0] * a[1]]

proc dist*(a, b: TVector): float {.noSideEffect.} =
  result = (a - b).mag

proc `*`*[T](a: T; b: TVector): TVector {.noSideEffect.} =
  for i in low(b)..high(b):
    result[i] = a * b[i]

template `*`*[T](a: TVector; b: T): expr =
  b * a

# Kind of sucks for ints currently, perhaps convert an int vector to float in this case?
proc normalized*(a: TVector): TVector =
  let m = mag(a)
  for i in low(a)..high(a):
    result[i] = a[i] / m


proc `$`*(a: TVector): string {.noSideEffect.} =
  result = ""
  result &= "["
  var h = high(a)
  for i in low(a)..h:
    result &= $a[i]
    if i != h:
      result &= ", "
  result &= "]"

