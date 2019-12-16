namespace Core


open Core.ComputerPart2

module AmpliPart2 =

    let transferEnergy current amps news =
        let updateCurrent = buildAmp current.inputs (current.output |> List.skip 1) current.interpreter.instructions current.interpreter.position
        (amps, updateCurrent::news, current.output |> List.head)
        
    let reRootAmps current news =
        let updateCurrent = buildAmp current.inputs (current.output |> List.skip 1) current.interpreter.instructions current.interpreter.position
        (updateCurrent::news |> List.rev, [], current.output |> List.head)

    let rec getAmplitude (amps, news, intensity) =
        match amps with
        | [] -> None
        | current::[] -> match amplify current intensity with None -> None | Some(Done(amp)) -> Some(amp.output |> List.last) | Some(Running(amp)) -> getAmplitude (reRootAmps amp news)
        | current::rest -> match amplify current intensity with None -> None | Some(Done(amp)) | Some(Running(amp)) -> getAmplitude (transferEnergy amp rest news)

    let findOptimal input =
        let values = seq { for A in 5..9 do for B in 5..9 do for C in 5..9 do for D in 5..9 do for E in 5..9 -> [A;B;C;D;E] } |> Seq.where (fun list -> list |> List.distinct |> List.length = 5)
        let combinaisons = values |> Seq.map (List.map (fun x -> initAmp [x] input))
        combinaisons |> Seq.map (fun input -> getAmplitude (input, [], 0)) |> Seq.choose id |> Seq.max

    let findSpeOptimal input values =
        let amps = values |> List.map (fun x -> initAmp [x] input)
        match getAmplitude (amps, [], 0) with
        | None -> -42
        | Some(nb) -> nb