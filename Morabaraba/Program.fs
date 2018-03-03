// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.IO


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
        cowID : int
    }

    type Board = {
                
        A1 : Cow
        A4 : Cow
        A7 : Cow
        B2 : Cow
        B4 : Cow
        B6 : Cow
        C3 : Cow
        C4 : Cow
        C5 : Cow
        D1 : Cow
        D2 : Cow
        D3 : Cow
        D5 : Cow
        D6 : Cow
        D7 : Cow
        E3 : Cow
        E4 : Cow
        E5 : Cow
        F2 : Cow
        F4 : Cow
        F6 : Cow
        G1 : Cow
        G4 : Cow
        G7 : Cow
    }

    let initiateBoard =
        let emptyCow = {Position = -1; isFlyingCow = false; Id = -1; cowID = -1}
        {A1 = emptyCow; A4 = emptyCow; A7 = emptyCow;
         B2 = emptyCow; B4 = emptyCow; B6 = emptyCow;
         C3 = emptyCow; C4 = emptyCow; C5 = emptyCow;
         D1 = emptyCow; D2 = emptyCow; D3 = emptyCow; D5 = emptyCow; D6 = emptyCow; D7 = emptyCow; 
         E3 = emptyCow; E4 = emptyCow; E5 = emptyCow;
         F2 = emptyCow; F4 = emptyCow; F6 = emptyCow;
         G1 = emptyCow; G4 = emptyCow; G7 = emptyCow 
         }

    type Player = {

        Id : int            // Player Id. Player 1 will be (int 1)
        isWinner : bool     // Player has won      
    }
 
    let getChar (cow : Cow)=
        match cow.Id with
        | 0 -> 'r'
        | 1 -> 'b'
        | _ -> ' '

    let updateBoardPosition (posInput : int) (boardState : Board) (newCow : Cow) =
        match posInput with
        | 0 ->  {boardState with A1 = newCow } | 1 ->  {boardState with A4 = newCow } | 2 ->  {boardState with A7 = newCow }
        | 3 ->  {boardState with B2 = newCow } | 4 ->  {boardState with B4 = newCow } | 5 ->  {boardState with B6 = newCow }
        | 6 ->  {boardState with C3 = newCow } | 7 ->  {boardState with C4 = newCow } | 8 ->  {boardState with C5 = newCow }
        | 9 ->  {boardState with D1 = newCow } | 10 -> {boardState with D2 = newCow } | 11 -> {boardState with D3 = newCow }
        | 12 -> {boardState with D5 = newCow } | 13 -> {boardState with D6 = newCow } | 14 -> {boardState with D7 = newCow }
        | 15 -> {boardState with E3 = newCow } | 16 -> {boardState with E4 = newCow } | 17 -> {boardState with E5 = newCow }
        | 18 -> {boardState with F2 = newCow } | 19 -> {boardState with F4 = newCow } | 20 -> {boardState with F6 = newCow }
        | 21 -> {boardState with G1 = newCow } | 22 -> {boardState with G4 = newCow } | 23 -> {boardState with G7 = newCow }
        | _ -> failwith "Bruh"

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

    let drawBoard (board : Board)  =                                                                           // print the board
         printfn "\n\n             1   2   3   4   5   6   7"
         printfn ""
         printfn "        A   (%c)---------(%c)---------(%c)" (getChar board.A1) (getChar board.A4) (getChar board.A7)
         printfn "             | \         |         / |"
         printfn "        B    |  (%c)-----(%c)-----(%c)  |" (getChar board.B2) (getChar board.B4) (getChar board.B6)
         printfn "             |   | \     |     / |   |"
         printfn "        C    |   |  (%c)-(%c)-(%c)  |   |" (getChar board.C3) (getChar board.C4) (getChar board.C5)
         printfn "             |   |   |       |   |   |"
         printfn "        D   (%c)-(%c)-(%c)     (%c)-(%c)-(%c)" (getChar board.D1) (getChar board.D2) (getChar board.D3) (getChar board.D5) (getChar board.D6) (getChar board.D7)
         printfn "             |   |   |       |   |   |"
         printfn "        E    |   |  (%c)-(%c)-(%c)  |   |" (getChar board.E3) (getChar board.E4) (getChar board.E5)
         printfn "             |   | /     |     \ |   |"
         printfn "        F    |  (%c)-----(%c)-----(%c)  |" (getChar board.F2) (getChar board.F4) (getChar board.F6)
         printfn "             | /         |         \ |"
         printfn "        G   (%c)---------(%c)---------(%c)" (getChar board.G1) (getChar board.G4) (getChar board.G7)

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

    let getCowID pos (boardState : Board) = 
        match pos with
        | 0 ->  boardState.A1.Id | 1 ->  boardState.A4.Id | 2 ->  boardState.A7.Id
        | 3 ->  boardState.B2.Id | 4 ->  boardState.B4.Id | 5 ->  boardState.B6.Id
        | 6 ->  boardState.C3.Id | 7 ->  boardState.C4.Id | 8 ->  boardState.C5.Id
        | 9 -> boardState.D1.Id  | 10 -> boardState.D2.Id | 11 -> boardState.D3.Id
        | 12 -> boardState.D5.Id | 13 -> boardState.D6.Id | 14 -> boardState.D7.Id
        | 15 -> boardState.E3.Id | 16 -> boardState.E4.Id | 17 -> boardState.E5.Id
        | 18 -> boardState.F2.Id | 19 -> boardState.F4.Id| 20 -> boardState.F6.Id
        | 21 -> boardState.G1.Id | 22 -> boardState.G4.Id | 23 -> boardState.G7.Id
        | _ -> failwith "Bruh"

    let getCowPos (pos) (boardState : Board) =
        match pos with
        | 0 ->  boardState.A1.Position | 1 ->  boardState.A4.Position | 2 ->  boardState.A7.Position
        | 3 ->  boardState.B2.Position | 4 ->  boardState.B4.Position | 5 ->  boardState.B6.Position
        | 6 ->  boardState.C3.Position | 7 ->  boardState.C4.Position | 8 ->  boardState.C5.Position
        | 9 -> boardState.D1.Position | 10 -> boardState.D2.Position | 11 -> boardState.D3.Position
        | 12 -> boardState.D5.Position | 13 -> boardState.D6.Position | 14 -> boardState.D7.Position
        | 15 -> boardState.E3.Position | 16 -> boardState.E4.Position | 17 -> boardState.E5.Position
        | 18 -> boardState.F2.Position | 19 -> boardState.F4.Position| 20 -> boardState.F6.Position
        | 21 -> boardState.G1.Position | 22 -> boardState.G4.Position | 23 -> boardState.G7.Position
        | _ -> failwith "Bruh"

    let findMill (boardState : Board) (mills : Mill list) (playerID : int) =
        let rec check (i : int) millList =
            match i = 20 with //fuck you
            | true -> millList
            | _ ->  
                match (getCowID mills.[i].millPos.[0] boardState, getCowID mills.[i].millPos.[1] boardState, getCowID mills.[i].millPos.[2] boardState)  = (playerID,playerID,playerID) with
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
    //let killCow (pos : int) (mills : Mill List) =
        

    let Start = 
        printfn "Place your cows: Player one will place first\n"               
        
            
        let rec getPos (boardState : Board)=                  // Richard: Check to see if a valid input has been recieved           
            let pos = (Console.ReadLine () |> translatePos)
            match pos = -1 with
            | true -> 
                printfn "Incorrect possition, please enter a new one:"
                getPos boardState
            | _ -> 
                match (getCowPos pos boardState) = -1 with
                | false -> 
                     printfn "Already a cow at that position"
                     getPos boardState
                | _ -> pos
                

        let phaseOne (boardState : Board) =
            drawBoard boardState
            let rec getCows i (board : Board) =
                match i = 24 with
                | true -> boardState
                | _ ->    
                    Console.Clear()
                    drawBoard board
                    printfn "\n\nPlayer %d: Enter a cow position" (i%2 + 1)
                    let newPos = getPos board
                    let newCow = {Position = newPos; isFlyingCow = false; Id = i % 2; cowID = i } 
                    let newBoard = updateBoardPosition newPos board newCow
                    let currentMills = findMill newBoard allMills (i % 2)
                    let b = canKill 0 currentMills (i % 2)
                    getCows (i + 1)  (updateBoardPosition newPos board newCow)
            getCows 0 boardState
        
        phaseOne (initiateBoard)
        printf "\nPhase one completed"
        Console.ReadKey ()


[<EntryPoint>]

let main argv = 
    let x = GameSession.Start
    printfn "%A" argv
    0 // return an integer exit code
