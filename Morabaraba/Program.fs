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
        let emptyCow = {Position = -1; isFlyingCow = false; Id = -1}
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


    let Start = 
        printfn "Place your cows: Player one will place first\n"       
        
        
        let rec getPos ()=                  // Richard: Check to see if a valid input has been recieved           
            let pos = (Console.ReadLine () |> translatePos)
            match pos = -1 with
            | false -> pos
            | _ -> 
                printfn "Incorrect possition, please enter a new one:"
                getPos ()
        
        let cowCompare (x : Cow) (y : Cow) =
            match x.Position = y.Position with
            | true -> 0
            | _ -> 
                match x.Position > y.Position with
                | true -> 1
                |_ -> -1

        let phaseOne (boardState : Board) =
            drawBoard boardState
            let rec getCows i (board : Board) =
                match i = 24 with
                | true -> boardState
                | _ ->    
                    Console.Clear()
                    drawBoard board
                    printfn "\n\nPlayer %d: Enter a cow position" (i%2 + 1)
                    let newPos = getPos()
                    let newCow = {Position = newPos; isFlyingCow = false; Id = i % 2 } 
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
