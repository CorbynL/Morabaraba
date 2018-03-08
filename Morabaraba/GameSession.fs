namespace Morabaraba
open System

module GameSession =
    

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

    let drawBoard (list : Cow List)  =  // print the board           
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
        isFormed : bool
        isNew : bool
    }

    //All possible mill variations
    let allMills = [
        { millPos =    0::1::2::[]; isFormed = false; isNew = false}; // A1, A4, A7
        { millPos =    3::4::5::[]; isFormed = false; isNew = false}; // B2, B4, B6
        { millPos =    6::7::8::[]; isFormed = false; isNew = false}; // C3, C4, C5
        { millPos =  9::10::11::[]; isFormed = false; isNew = false}; // D1, D2, D3
        { millPos = 12::13::14::[]; isFormed = false; isNew = false}; // D5, D6, D7
        { millPos = 15::16::17::[]; isFormed = false; isNew = false}; // E3, E4, E5
        { millPos = 18::19::20::[]; isFormed = false; isNew = false}; // F2, F4, F6
        { millPos = 21::22::23::[]; isFormed = false; isNew = false}; // G1, G4, G7
        { millPos =   0::9::21::[]; isFormed = false; isNew = false}; // A1, D1, G1
        { millPos =  3::10::18::[]; isFormed = false; isNew = false}; // B2, D2, F2
        { millPos =  6::11::15::[]; isFormed = false; isNew = false}; // C3, D3, E3
        { millPos =    1::4::7::[]; isFormed = false; isNew = false}; // A4, B4, C4
        { millPos = 16::19::22::[]; isFormed = false; isNew = false}; // E4, F4, G4
        { millPos =  8::12::17::[]; isFormed = false; isNew = false}; // C5, D5, E5
        { millPos =  5::13::20::[]; isFormed = false; isNew = false}; // B6, D6, F6
        { millPos =  2::14::23::[]; isFormed = false; isNew = false}; // A7, D7, G7
        { millPos =    0::3::6::[]; isFormed = false; isNew = false}; // A1, B2, C3
        { millPos = 15::18::21::[]; isFormed = false; isNew = false}; // E3, F2, G1
        { millPos =    2::5::8::[]; isFormed = false; isNew = false}; // C5, B6, A7
        { millPos = 17::20::23::[]; isFormed = false; isNew = false}; // E5, F6, G7
    ]

    //get ID of cow at given position
    let getCowID pos (cows : Cow List) = 
        cows.[pos].Id

    //Get cow at given position
    let getCowAtPos (pos) (cows : Cow List) =
        List.find (fun (x : Cow) -> pos = x.Position) cows
 
    // Replace a mill at given position with newMill
    let updateMillList (oldList: Mill List) (position: int) (newMill: Mill) : Mill List =
            let rec updateList (newList:Mill List) a =
                match a < 0 with
                | true -> newList
                | _ ->
                    match a = position with
                    | true -> updateList (newMill::newList) (a-1)
                    |_-> 
                        updateList (oldList.[a]::newList) (a-1)
            updateList [] (oldList.Length - 1)  

    let getCurrentMillPos (millList : Mill List) (mill: Mill) =
        let rec find i =
            match i < millList.Length with
            | true ->
                match mill.millPos = millList.[i].millPos with
                | true -> i
                | _ -> find (i + 1)
            | _ -> failwith "does not exist"
        find 0

    // If mill no longer exists, remove it from the list
    let removeBrokenMill (millList : Mill List) (pos : int) =
        List.filter ((fun (x : Mill) y -> not (x.millPos = y.millPos)) allMills.[pos]) millList

    // Does this player own this mill?
    let isOwned (playerID : int) (idx : int) (cows : Cow List) =
        (getCowID allMills.[idx].millPos.[0] cows, getCowID allMills.[idx].millPos.[1] cows, getCowID allMills.[idx].millPos.[2] cows ) = (playerID,playerID,playerID)

    //Get all current mills a player currently owns
    let updateMills (cows : Cow List)  (playerID : int) (currMill : Mill List) : Mill List =
        let rec check (i : int) newMillList =
           match i < allMills.Length with
           | false -> newMillList
           | _ ->  
                match isOwned playerID i cows with        // Does this particular mill exist on the board (for playerID)?
                | true -> 
                    match List.exists ((fun (x : Mill) (y : Mill) -> x.millPos = y.millPos) allMills.[i]) newMillList with      // Is the mill currently in our list?    
                    | false -> check (i + 1) ({millPos = allMills.[i].millPos; isFormed = true; isNew = true} :: newMillList)
                    // Can optimise
                    | _ -> check (i + 1) (updateMillList newMillList (getCurrentMillPos newMillList allMills.[i])  {allMills.[i] with isNew = false; isFormed = true}) // Mill is in current Mills, therfore is no longer new
                | _ -> 
                    match (List.tryFind ((fun (x : Mill) y -> x.millPos = y.millPos && isOwned playerID i cows) allMills.[i]) newMillList) with        // Check if a mill has been broken
                    | None -> check (i + 1) newMillList                                                                     // Mill didn't exist, carry on
                    // Should never execute in phase one
                    | _ -> check (i + 1) (removeBrokenMill newMillList i)   // Mill was broken, remove it from list
        check 0 currMill  
    
    let getPlayerMills (currentMills : Mill List) (playerID : int) (cows : Cow List)  =
        List.filter (fun (x : Mill) ->  (((getCowID x.millPos.[0] cows), (getCowID x.millPos.[1] cows), (getCowID x.millPos.[2] cows)) = (playerID,playerID,playerID))) currentMills
        
    let getOpponent playerID =
        match playerID with
        | 0 -> 1
        | 1 -> 0
        | _ -> failwith "Invalid player"

    // Check if chosen cow is in a mill
    // TODO: there's a bug here that throws an exception if you try kill a cow at an index where there is no cow
    let canKill (pos : int) (mills : Mill List) (player : int) (cows : Cow List) =
        let playerMills = getPlayerMills mills player cows                    // Get Player mills
        let enemyMills = getPlayerMills mills (getOpponent player) cows           // Get enemy mills
        match (getCowAtPos pos cows).Id = player with                         // Check if cow to kill is players own cow    
        | true -> false
        | _ ->
            let rec check i =   
                match i <  enemyMills.Length with                            // If opponent has no mills, can kill any of their cows
                | false -> true
                | _ -> 
                    match  List.exists ((=) pos) enemyMills.[i].millPos with // Check if cow to kill is in a mill
                    | false -> check (i + 1)
                    | _ -> false                                              // Cow is in mill. Cannot kill it
            match playerMills.Length = 0 with                                 // Start checking if cow is in mill
            | true -> false
            | _ -> check 0
        
    // Check to see if a valid input has been recieved
    let rec getPos ()=                             
            let pos = (Console.ReadLine () |> translatePos)
            match pos = -1 with
            | false -> pos
            | _ -> 
                printfn "Incorrect possition, please enter a new one:"
                getPos ()
    
    //Replace cow at a given position with a given cow
    let updateCOWList (oldList: Cow List) (possition: int) (newCow: Cow) =
            let rec updateList (newList:Cow List) a =
                match a < 0 with
                | true -> newList
                | _ ->
                    match a = possition with
                    | true -> updateList (newCow::newList) (a-1)
                    |_-> 
                        updateList (oldList.[a]::newList) (a-1)
            updateList [] (oldList.Length - 1)

    
    //Kill chosen cow and replace dead cow with empty cow
    let killCow (pos : int) (cows : Cow List) =        
        updateCOWList cows pos emptyCow
    
    // Checks if given mill was created on the players current turn
    let isNewMill (mills : Mill List) =
        let newMill (mill : Mill) = mill.isNew = true
        List.exists newMill mills
    
    let getPlayerColour (playerID : int) =
        match playerID with
        | 0 -> 'b'
        | 1 -> 'r'
        | _ -> failwith "Invalid player"


    // Check for mill, let player kill cow if mill exists. Verification Needed
    let checkMill (cows : Cow List) (mills : Mill list) (playerID : int) =
        let playerMills = getPlayerMills mills playerID cows
        match playerMills.Length with
        | 0 -> cows
        | _ -> 
            match isNewMill playerMills with
            | false -> cows
            | _ ->
                printfn "Chose cow to fill"
                let rec tryKill () =
                    let cowToKill = getPos ()
                    match canKill (cowToKill) mills playerID  cows with
                    | true -> killCow cowToKill cows                   
                    | _ ->
                        Console.Clear ()
                        drawBoard cows
                        printfn "Cannot kill that one"
                        tryKill ()
                tryKill () 
            
    //Initialise cow list with empty cows                
    let emptyList () =
        List.init 24 (fun x -> {Position = -1; isFlyingCow = false; Id = -1 })
    
    //Start game loop
    let public Start =                       
        let phaseOne cowList =
            let rec getCows i (list : Cow List) (currentMill : Mill List) =
                match i = 24 with
                | true -> list
                | _ ->
                    //
                    // TO DO: Clean up method
                    //
                    Console.Clear()
                    printfn "\nPlace your cows: Player one will place first\n"
                    drawBoard list                                                      //When a mill is formed, it only draws the cow that formed the mill, after you kill a cow
                    printfn "\n\nPlayer %d: Enter a cow position" (i%2 + 1)
                    let pos = getPos()
                    let newCow = {Position = pos; isFlyingCow = false; Id = i % 2 }
                    let newCowList = updateCOWList list pos newCow          //List before checking for mills and possibly killing cow
                    let currMills = updateMills newCowList (i % 2) currentMill
                    let newCowList2 =  checkMill newCowList currMills (i%2)  //List after ^
                    getCows (i + 1) newCowList2 currMills 
            getCows 0 cowList []    
        phaseOne (emptyList ())

