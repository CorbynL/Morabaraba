﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System


(*
//--------------------Program Info--------------------

    Random Notes about Game:
        24 possible Possitions
        20 possible rows of 3 cows
        8 Possitions with the possiblity of 4 cows next to them, the rest can only have 3

*)

module GameSession =      
 
    Console.ForegroundColor <- ConsoleColor.Green 

    type State = {

         playerTurn : int
    }    

    type Cow = {

        Position : int 
        isFlyingCow : bool 
        Id : int
    }

    let emptyCow = {Position = -1; isFlyingCow = false; Id = -1}

    type Player = {

        Id : int            // Player Id. Player 1 will be (int 1)
        isWinner : bool     // Player has won      
    }
 
    let getChar (cow : Cow)=
        match cow.Id with
        | 0 -> 'r'
        | 1 -> 'b'
        | _ -> ' '

    let translatePos (posInput : string)  =
        match posInput.ToLower() with
        | "a1" -> 0 | "a4" -> 1 | "a7" -> 2
        | "b2" -> 3 | "b4" -> 4 | "b6" -> 5
        | "c3" -> 6 | "c4" -> 7 | "c5" -> 8
        | "d1" -> 9 | "d2" -> 10| "d3" -> 11| "d5" -> 12| "d6" -> 13| "d7" -> 14
        | "e3" -> 15| "e4" -> 16| "e5" -> 17
        | "f2" -> 18| "f4" -> 19| "f6" -> 20
        | "g1" -> 21| "g4" -> 22| "g7" -> 23
        | _ -> -1        

    let drawBoard (list : Cow List)  =                                                                           // print the board
         printfn "             1   2   3   4   5   6   7"
         printfn ""
         printfn "        A   (%c)---------(%c)---------(%c)" (getChar list.[0]) (getChar list.[1]) (getChar list.[2])
         printfn "             | \         |         / |"
         printfn "        B    |  (%c)-----(%c)-----(%c)  |" (getChar list.[3]) (getChar list.[4]) (getChar list.[5])
         printfn "             |   | \     |     / |   |"
         printfn "        C    |   |  (%c)-(%c)-(%c)  |   |" (getChar list.[6]) (getChar list.[7]) (getChar list.[8])
         printfn "             |   |   |       |   |   |"
         printfn "        D   (%c)-(%c)-(%c)     (%c)-(%c)-(%c)" (getChar list.[9]) (getChar list.[10]) (getChar list.[11]) (getChar list.[12]) (getChar list.[13]) (getChar list.[14])
         printfn "             |   |   |       |   |   |"
         printfn "        E    |   |  (%c)-(%c)-(%c)  |   |" (getChar list.[15]) (getChar list.[16]) (getChar list.[17])
         printfn "             |   | /     |     \ |   |"
         printfn "        F    |  (%c)-----(%c)-----(%c)  |" (getChar list.[18]) (getChar list.[19]) (getChar list.[20])
         printfn "             | /         |         \ |"
         printfn "        G   (%c)---------(%c)---------(%c)" (getChar list.[21]) (getChar list.[22]) (getChar list.[23])
    
    type Mill = {

        millPos : int List
        wasFormed : bool

    }

    let allMills = [
        { millPos = 0::1::2::[]; wasFormed = false}; // A1, A4, A7
        { millPos = 3::4::5::[]; wasFormed = false}; // B2, B4, B6
        { millPos = 6::7::8::[]; wasFormed = false}; // C3, C4, C5
        { millPos = 9::10::11::[]; wasFormed = false}; // D1, D2, D3
        { millPos = 12::13::14::[]; wasFormed = false}; // D5, D6, D7
        { millPos = 15::16::17::[]; wasFormed = false}; // E3, E4, E5
        { millPos = 18::19::20::[]; wasFormed = false}; // F2, F4, F6
        { millPos = 21::22::23::[]; wasFormed = false}; // G1, G4, G7
        { millPos = 0::9::21::[]; wasFormed = false}; // A1, D1, G1
        { millPos = 3::10::18::[]; wasFormed = false}; // B2, D2, F2
        { millPos = 6::11::15::[]; wasFormed = false}; // C3, D3, E3
        { millPos = 1::4::7::[]; wasFormed = false}; // A4, B4, C4
        { millPos = 16::19::22::[]; wasFormed = false}; // E4, F4, G4
        { millPos = 8::12::17::[]; wasFormed = false}; // C5, D5, E5
        { millPos = 5::13::20::[]; wasFormed = false}; // B6, D6, F6
        { millPos = 2::14::23::[]; wasFormed = false}; // A7, D7, G7
        { millPos = 0::3::6::[]; wasFormed = false}; // A1, B2, C3
        { millPos = 15::18::21::[]; wasFormed = false}; // E3, F2, G1
        { millPos = 2::5::8::[]; wasFormed = false}; // C5, B6, A7
        { millPos = 17::20::23::[]; wasFormed = false}; // E5, F6, G7
    ]

    let getCowID pos (cows : Cow List) = 
        (List.find (fun (x : Cow) -> pos = x.Id) cows).Id

    let getCowAtPos (pos) (cows : Cow List) =
        List.find (fun (x : Cow) -> pos = x.Position) cows

    let findMill (cows : Cow List) (mills : Mill list) (playerID : int) =
        let rec check (i : int) millList =
            match i = cows.Length with 
            | true -> millList
            | _ ->  
                match (getCowID mills.[i].millPos.[0] cows, getCowID mills.[i].millPos.[1] cows, getCowID mills.[i].millPos.[2] cows )  = (playerID,playerID,playerID) with
                | true -> check (i + 1) (mills.[i]::millList)
                | _ -> check (i+1) millList
        check 0 []

    let canKill (pos : int) (mills : Mill List) (player : int) =
        let rec check i =   
            match i = mills.Length with
            | true -> false
            | _ -> 
                match  List.exists ((=) pos) mills.[i].millPos with
                | false -> check (i + 1)
                | _ -> true
        match mills.Length = 0 with
        | true -> false
        | _ -> check 0
        

    let Start = 
        printfn "Place your cows: Player one will place first\n"               
        
            
    let rec getPos ()=                  // Richard: Check to see if a valid input has been recieved           
            let pos = (Console.ReadLine () |> translatePos)
            match pos = -1 with
            | false -> pos
            | _ -> 
                printfn "Incorrect possition, please enter a new one:"
                getPos ()

    let updateCOWList (oldList: Cow List) (possition: int) (newCow: Cow) =
        let rec updateList (newList:Cow List) a =
            match a < 0 with
            | true -> newList
            | _ ->
                match a = possition with
                | true -> updateList (newCow::newList) (a-1)
                |_-> 
                    updateList (oldList.[a]::newList) (a-1)
        updateList [] 23
    
    let canKillCow (mills : Mill List) (cow : Cow) : bool =
        let rec findCow i =
            match i < mills.Length with
            | true -> 
                match List.exists ((=) cow.Position) mills.[i].millPos with
                | true -> false
                | _ -> findCow (i + 1)
            | _ -> false
        match mills.Length = 0 with
        | true -> false
        | _ -> findCow 0
                    
    let killCow (pos : int) (cows : Cow List) =        
        updateCOWList cows pos emptyCow

    let emptyList () =
        List.init 24 (fun x -> {Position = -1; isFlyingCow = false; Id = -1 })

    let phaseOne cowList =
        let rec getCows i (list : Cow List) =
            match i = 24 with
                    | true -> list
                    | _ ->    
                        Console.Clear()
                        drawBoard list
                        printfn "\n\nPlayer %d: Enter a cow position" (i%2 + 1)
                        let pos = getPos()
                        let newCow = {Position = pos; isFlyingCow = false; Id = i % 2 }
                        let newCowList = updateCOWList list pos newCow
                        let millList = findMill list allMills (i % 2) 
                        
                        // if mill, player input, if valid input, KILL THE COW
                        
                        let a = getCowAtPos 0 newCowList
                        getCows (i + 1) newCowList
        getCows 0 cowList
    let y = emptyList ()
    phaseOne (emptyList ())

    
[<EntryPoint>]

let main argv = 
    let x = GameSession.Start
    printfn "%A" argv
    0 // return an integer exit code
