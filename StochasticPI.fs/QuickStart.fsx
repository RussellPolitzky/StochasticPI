#r "Fmat.Numerics.dll"
open Fmat.Numerics
open Fmat.Numerics.Conversion
open Fmat.Numerics.MatrixFunctions
open Fmat.Numerics.BasicStat
open Fmat.Numerics.LinearAlgebra
open System

//See documentation in FmatDoc.chm for more details

//Construct a matrix
//Use Matrix32 for single precision, BoolMatrix for bool matrix, IntMatrix for int32 based and StringMatrix for string based matrix.
let m0 = Matrix.Empty // 0x0
let m1 = Matrix(2.0) // from scalar
let m2 = Matrix([2;3;4], 2.0) // fill matrix 2x3x4 with 2.0
let m3 = Matrix([2;3;4], [1.0..24.0]) // fill matrix 2x3x4 with numbers 1..24 in column major order, data in seq<'T>
let m4 = Matrix([1.0..4.0]) // create matrix 1x4 (row vector), accepts seq<'T>
let m5 = Matrix(Array2D.create 2 3 2.0) // from 2D .NET array
let m6 = Matrix(Array3D.create 2 3 4 2.0) // from 3D .NET array
let m7 = Matrix(Array4D.create 2 3 4 5 2.0) // from 4D .NET array
let m8 = Matrix([ [1.0;2.0]
                  [3.0;4.0] ]) //from list of rows, accepts seq<seq<'T>>
let m9 = Matrix([2;3], fun i -> float(i)) // from generating function using linear index in column major order
let m10 = Matrix([2;3], fun i j -> float(i) + float(j)) // from generating function of row,col
let m11 = Matrix([2;3;4], fun i j k -> float(i+j+k)) // as above in 3 dims
let m12 = Matrix([2;3;4;5], fun i j k l -> float(i+j+k+l)) // as above in 4 dims
let m13 = zeros [2;3;4] // all zeros
let m14 = ones [2;3;4] // all ones
let m15 = I (2,3) // identity matrix, ones on diagonal, zero otherwise
let b1 = BoolMatrix([2;3], true) // also IntMatrix, Matrix32 and StringMatrix

//Conversion and casting, use (!!) generic explicit cast operator defined in Fmat.Numerics.Conversion
//Type annotation might be needed
let m16 : Matrix = !!2.0
let m17 : Matrix = !![1.0..24.0]
let m18 : Matrix = !!(Array2D.create 2 3 2.0)
let m19 : Matrix = !!(Array3D.create 2 3 4 2.0)
let m20 : Matrix = !!(Array4D.create 2 3 4 5 2.0)
let m21 : Matrix = !![ [1.0;2.0]
                       [3.0;4.0] ]
let a1 : float = !!m16 // etc

//Matrix info and formatting
let size = m2.Size // [|2;3;4|]
let n = m2.Length // 24
let isScalar = m1.IsScalar // true
let isVector = m4.IsVector // true
let colMajorSeq = m3.ToColMajorSeq() // seq<'T> elements in column major order
Matrix.DisplayFormat <- "G3" // apply "G3" format to each element when calling ToString()
Matrix.DisplayDigits <- 3 // as above
Matrix.MaxDisplaySize <- [|3;4;5|] // show up to 3 rows, 4 column, 5 pages when calling ToString()

//Indexing and slicing
let a2 = m2.[1] // second element in column major order
let a3 = m2.[0, 1, 2] // element in first row, second column an third page 
let m22 = m2.[0..1, 0..1, 1..2] // slice of first 2 rows, first 2 columns and 2nd and 3rd page
let m23 = m5.[ [0;1], [1;1] ] // specify seq of rows, cols etc to choose, can be unordered and with duplicates
let m24 = m5.[m5 .> 0.0] // boolean indexing
let m25 = m5.[fun x -> x > 0.0] // bool indexing with predictor function

//Manipulation
let m26 = repmat(m5, [2;3]) // replicate matrix 2 times vertically and 3 times horizontally
let m27 = transpose(m5) // also m5.T
let m28 = m5.Diag() // extract diagonal, optional offset
let m29 = concat([m5;m5], 1) // concatenate along given dimension
let m30 = diag(m4, 0) // create diagonal matrix from vector, optional offset
m2.ApplyFun(fun x -> x + 2.0) // apply function in place
let m31 = applyFun(m2, fun x -> x + 2.0) // return new matrix, also for function of 2 and 3 arguments

//Comparison
let b2 = m2 .< m3 // compare elementwise, also with scalar or scalar matrix
let chk1 = m2 == m3 // compare by value, might be different instances, opposite : !=
let allcond = m2 &< 0.01 // true if all elements < 0.01
let mx = maxXY(m2, m3) // elementwise maximum, also between matrix and scalar

//Arithmetic
let m32 = m2 .* m3 // elementwise, matrix matrix or matrix scalar
let m33 = m8 * m8 // matrix multiplication

//boolean
let b3 = b1 .&& false // elementwise && || and not available

//vector functions
let m34 = sqrt(m2) // all standard functions work elementwise, also special functions: erf, erfc, erfinv, erfcinv, normcdf and norminv

//random numbers
let r1 = rand [2;3;4] // uniform in [0,1]
let r2 = normalRnd(0.0, 1.0, [2;3;4]) // normal distribution, also lognormal, multivariate normal, Poisson, Bernoulli, binomial

//linear algebra
let (l,u,p) = lu(m8) // lu decomposition, also qr, chol and svd
let B = rand [2;1]
let x = luSolve(m8, B) // solve linear equation using lu decomp, also for chol, qr and svd

//basic stats
let m = mean(m2, 1) // calculate mean along dim 1: by rows, also var, skewness, kurtosis, min, max, sum, cumsum, prod, cumprod, corr and cov
