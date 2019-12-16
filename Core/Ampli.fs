namespace Core

open Core.Computer

module Ampli =

    let rec getAmplitude input nb lists =
        match lists with
        | [] -> Some nb
        | current::rest -> match compute [current;nb] input with None -> None | Some((_, output)) -> getAmplitude input output rest

    let findOptimal input =
        let values = seq { for A in 0..4 do for B in 0..4 do for C in 0..4 do for D in 0..4 do for E in 0..4 -> [A;B;C;D;E] } |> Seq.where (fun list -> list |> List.distinct |> List.length = 5)
        values |> Seq.map (fun x -> getAmplitude input 0 x) |> Seq.choose id |> Seq.max
