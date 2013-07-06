module SequenceExtensions

module Seq =
    let mapWithAccumulator fn acc sequence  = seq { for item in sequence do yield fn item acc }  