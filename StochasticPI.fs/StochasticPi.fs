module StochasticPI.fs

open System
open SequenceExtensions

open Fmat.Numerics
open Fmat.Numerics.MatrixFunctions
open Fmat.Numerics.BasicStat

 
//------------------------------------------------------------------------------------
// Find PI using a stochastic method.                                                          
//------------------------------------------------------------------------------------
type point             = { x:float; y:float }
type pointsAccumulator = { mutable insideCircle:int64; mutable total:int64 }


//------------------------------------------------------------------------------------
// Generates a sequence of numbers which should approach PI, if the 
// underlying random number generator were any good (it's not).
//------------------------------------------------------------------------------------
let piEstimationSequence = 

    let randompoint_t (rndgen:unit -> double) = { x = rndgen(); y = rndgen() }
    let randInstance = new Random()
    let getRandompoint = fun unit -> { x = randInstance.NextDouble(); y = randInstance.NextDouble() }
    let points = Seq.initInfinite (fun _ -> getRandompoint() )

    // is the point in the unit circle
    let inline inUnitCircle point = (point.x ** 2.0 + point.y ** 2.0) <= 1.0

    // Factored out from below.
    let inline answer accumulator =
        accumulator.total <- accumulator.total + 1L
        4.0*(float accumulator.insideCircle)/(float accumulator.total) 

    points 
    |> Seq.mapWithAccumulator 
        (fun point accumulator -> 
             if inUnitCircle point then 
               accumulator.insideCircle <- accumulator.insideCircle + 1L 
               answer accumulator
             else
               //accumulator.outsideCircle <- accumulator.outsideCircle + 1L
               answer accumulator)
        { insideCircle = 0L; total = 0L } // Accumulator 


//------------------------------------------------------------------------------------
// This a simple imperative version of the raw algo.  It does not
// use anything special and is really a simple and naive implementation.
//
// This is a lot like what we might have written in C or Fortran.
//------------------------------------------------------------------------------------
let piImperative n = 

    let randInstance     = new Random()
    let mutable incircle = 0L
    let mutable total    = 0L
    let mutable x        = 0.0 
    let mutable y        = 0.0 

    for i = 0 to n do
        x <- randInstance.NextDouble()
        y <- randInstance.NextDouble()
        if ((x**2.0 + y**2.0) <= 1.0) then incircle <- incircle + 1L
        total <- total + 1L

    4.0*(float incircle)/(float total)
    

//------------------------------------------------------------------------------------
// See: http://fssnip.net/h2
// 
// This uses the Fmat library.  The code and syntax are very Matlab like
// but this approach is not inline and uses a lot of memory.
//------------------------------------------------------------------------------------
let fmatCalcPi n =
   let x = rand [1;n]
   let y = rand [1;n]
   let d = x .* x + y .* y
   let circ = new Matrix(d .< 1.0)
   let m = sum(circ,1)
   float(m)/(float)n*4.0
