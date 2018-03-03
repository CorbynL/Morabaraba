// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System

module GameSession =      //Corbyn: basic idea of a game skeleton
 
    Console.ForegroundColor <- ConsoleColor.Green 

    type State = {

         playerTurn : int
    }
    
    type Board = {
                
        board : string list
    }

    type Cow = {

        Position : int  // store position has letter and number, ie: (A,3)
        isFlyingCow : bool 
        Id : int
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

    let translatePos (posInput : string) =
        match posInput.ToLower() with
        | "a1" -> 0 | "a4" -> 1 | "a7" -> 2
        | "b2" -> 3 | "b4" -> 4 | "b6" -> 5
        | "c3" -> 6 | "c4" -> 7 | "c5" -> 8
        | "d1" -> 9 | "d2" -> 10| "d3" -> 11
        | "d5" -> 12| "d6" -> 13| "d7" -> 14
        | "e3" -> 15| "e4" -> 16| "e5" -> 17
        | "f2" -> 18| "f4" -> 19| "f6" -> 20
        | "g1" -> 21| "g4" -> 22| "g7" -> 23
        | _ -> -1
        

    let drawBoard (list : Cow List)  =                                                                           // print the board
         printfn "\n\n             1   2   3   4   5   6   7"
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


    let Start = 
        printfn "Place your cows: Player one will place first\n"
        
        let emptyList () =
            let rec COW i (list: Cow list) =
                match i < 24 with
                | false -> list
                | _ -> 
                    let newCow = {Position = i; isFlyingCow = true; Id = -1}
                    COW (i + 1) (List.append [newCow] list)
                
            COW 0 []
        
        
        let rec getPos ()=                  // Richard: Check to see if a valid input has been recieved           
            let pos = (Console.ReadLine () |> translatePos)
            match pos = -1 with
            | false -> pos
            | _ -> 
                printfn "Incorrect possition, please enter a new one:"
                getPos ()


        let phaseOne cowList =
            drawBoard cowList
            let rec getCows i list =
                match i = 24 with
                | true -> list
                | _ ->    
                    Console.Clear()
                    drawBoard list
                    printfn "\n\nPlayer %d: Enter a cow position" (i%2 + 1)
                    let pos =getPos()
                    let newCow = {Position = pos; isFlyingCow = false; Id = i % 2 }
                    let newCowList =
                        let rec updateList (newList:Cow List) a =
                            match a < 0 with
                            | true -> newList
                            | _ ->
                                match a = pos with
                                | true -> updateList (newCow::newList) (a-1)
                                |_-> 
                                    updateList (list.[a]::newList) (a-1)
                        updateList [] 23         
                    
                    //let a = List.toArray(list)
                    //Array.set (a) newCow.Position newCow
                    //let newlist = Array.toList (a) 
                    getCows (i + 1)  newCowList
            getCows 0 cowList
        
        let y = emptyList ()
        phaseOne (emptyList ())


[<EntryPoint>]

let main argv = 
    let x = GameSession.Start
    printfn "%A" argv
    0 // return an integer exit code
