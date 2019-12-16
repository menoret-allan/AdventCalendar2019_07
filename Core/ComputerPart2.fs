namespace Core

module ComputerPart2 =
    type Instructions = int []
    type Position = int
    type Interpreter = {instructions:Instructions;position:Position}
    type Inputs = int list
    type Output = int list
    type Amplifier =
    | Running of Amp
    | Done of Amp
    and Amp = {interpreter:Interpreter;inputs:Inputs;output:Output}

    type Mode =
        | Position
        | Immediate
    
    type Instruction =
        | Add of (int * int * int)
        | Mult  of (int * int * int)
        | EmptyInput
        | Input of (int * int * int list)
        | Output of int
        | Jump of int
        | NoAction of int
        | LessThan of (int * int)
        | Equal of (int * int)
        | End

    let translate (instructions: string) =
        instructions.Split [|','|]
        |> Seq.map(int)
        |> Seq.toArray
    
    let translateBack result =
        match result with
        | None -> None
        | Some(instructions, outputs) -> Some ((instructions |> Seq.map(string) |> String.concat ","), (outputs |> Seq.last))

    let buildAmp (inputs: int list) (outputs: int list) (instructions: Instructions) (position: int) =
        {output=outputs;inputs=inputs;interpreter={instructions=instructions;position=position}}

    let initAmp inputs instructions =
        {output=[];inputs=inputs;interpreter={instructions=instructions |> translate;position=0}}

    let getValue (instructions: int []) pos =
        instructions.[pos]

    let setValue (instructions: int []) value pos =
        instructions.[pos] <- value
        instructions

    let getModeX list place =
        if list |> List.length < place then Position
        else if list.[place - 1] = 0 then Position
        else Immediate

    let getMode2 getVal pos list place =
        match getModeX list place with
            | Position -> getVal (pos + place)
            | Immediate -> pos + place

    let jumpIf cond value position =
        match (value <> 0) = cond with
        | true -> Jump(position)
        | false -> NoAction(3)

    let lessThan left right position =
        if left < right then LessThan(1, position)
        else LessThan(0, position)

    let equalOp left right position =
        if left = right then Equal(1, position)
        else Equal(0, position)

    let findInstruction inputValue instructions pos =
        let getVal = getValue instructions
        let getPosition = getMode2 getVal pos
        let intList = instructions.[pos] |> string |> Seq.rev |> Seq.map string |> Seq.map int |> Seq.toList
        match intList with
        | 9::9::_ -> Some(End)
        | 1::0::rest | 1::rest -> Some(Add(getPosition rest 1 |> getVal, getPosition rest 2 |> getVal, getVal (pos + 3)))
        | 2::0::rest | 2::rest -> Some(Mult(getPosition rest 1 |> getVal, getPosition rest 2 |> getVal, getVal (pos + 3)))
        | 3::0::rest | 3::rest ->  if inputValue |> List.isEmpty then  Some(EmptyInput) else Some(Input(inputValue |> List.head, getPosition rest 1, inputValue |> List.skip 1))
        | 4::0::rest | 4::rest -> Some(Output(getPosition rest 1 |> getVal))
        | 5::0::rest | 5::rest -> Some(jumpIf true (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal))
        | 6::0::rest | 6::rest -> Some(jumpIf false (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal))
        | 7::0::rest | 7::rest -> Some(lessThan (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal) (getVal (pos + 3)))
        | 8::0::rest | 8::rest -> Some(equalOp (getPosition rest 1 |> getVal) (getPosition rest 2 |> getVal) (getVal (pos + 3)))
        | _ -> None

    let update instructions pos value =
        Array.set instructions pos value
        instructions

    let interprete (amplifier:Amp) =
        let rec loop inputs (instructions: Instructions) (output: int list) (pos: int) =
            let insertInInstruction = setValue instructions
            match findInstruction inputs instructions pos with
            | Some End -> Some (Done(buildAmp inputs output instructions pos))
            | Some(EmptyInput) -> Some(Running(buildAmp [] output instructions pos))
            | Some(Output(x)) -> loop inputs instructions (List.append output (List.singleton x)) (pos+2)
            | Some(Input(value, position, newInputs)) -> loop newInputs (insertInInstruction value position) output (pos+2)
            | Some(Add(left, right, position)) -> loop inputs (insertInInstruction (left + right) position) output (pos+4)
            | Some(Mult(left, right, position)) -> loop inputs (insertInInstruction (left * right) position) output (pos+4)
            | Some(Jump(position)) -> loop inputs instructions output position
            | Some(NoAction(increment)) -> loop inputs instructions output (pos+increment)
            | Some(LessThan(value, position)) -> loop inputs (insertInInstruction value position) output (pos+4)
            | Some(Equal(value, position)) -> loop inputs (insertInInstruction value position) output (pos+4)
            | None -> None
        loop amplifier.inputs amplifier.interpreter.instructions amplifier.output amplifier.interpreter.position

    let amplify (amplifier:Amp) (intensity:int) =
        buildAmp (List.append amplifier.inputs [intensity]) amplifier.output amplifier.interpreter.instructions amplifier.interpreter.position |> interprete

