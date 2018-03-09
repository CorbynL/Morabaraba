// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System

(*
//--------------------Program Info--------------------

    Random Notes about Game:
        24 possible Possitions
        20 possible rows of 3 cows
        8 Possitions with the possiblity of 4 cows next to them, the rest can only have 3

*)

    
   
  
 
Console.ForegroundColor <- ConsoleColor.Green 

type Cow = {

    Position : int 
    isFlyingCow : bool 
    Id : int
    cowNumber : int
}   
let emptyCow = {Position = -1; isFlyingCow = false; Id = -1 ; cowNumber = -1}

// Positions a cow can move from at a position
let isValidMove (cow : Cow) (position : int) =
    match cow.Position,position with
    | (0,1) | (0,3) | (0,9)
    | (1,0) | (1,2) | (1,4) 
    | (2,1) | (2,5) | (2,14)
    | (3,0) | (3,4) | (3,6) | (3,10) 
    | (4,1) | (4,3) | (4,5) | (4,7) 
    | (5,2) | (5,4) | (5,8) | (5,13)
    | (6,3) | (6,7) | (6,11) 
    | (7,4) | (7,6) | (7,8) 
    | (8,5) | (8,7) | (8,12) 
    | (9,0) | (9,10)| (9,21) 
    | (10,3) | (10,9) | (10,11) | (10,18) 
    | (11,6) | (11,10) | (11,15) 
    | (12,8) | (12,13) | (12,17) 
    | (13,5) | (13,12) | (13,14) | (13,20) 
    | (14,2) | (14,13) | (14,23) 
    | (15,11) | (15,16) | (15,18) 
    | (16,15) | (16,17) | (16,19) 
    | (17,12) | (17,16) | (17,20) 
    | (18,10) | (18,15) | (18,19) | (18,21) 
    | (19,16) | (19,18) | (19,20) | (19,22) 
    | (20,13) | (20,17) | (20,19) | (20,23) 
    | (21,9) | (21,18) | (21,22) 
    | (22,19) | (22,21) | (22,23)
    | (23,14) | (23,20) | (23,22) -> position
    | _ -> -1 
        

//Initialise cow list with empty cows                
let emptyList () =
    let list = List.init 24 (fun x -> emptyCow)
    List.mapi (fun i (x : Cow) -> {x with Position = i}) list 

let getCowID pos (cows : Cow List) = 
    cows.[pos].Id

let getCowAtPos (pos) (cows : Cow List) =
    List.find (fun (x : Cow) -> pos = x.Position) cows

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
    updateCOWList cows pos {emptyCow with Position=pos}

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
    isNew : bool
    previousForms :  ((int List) * int) List
    owner : int
}

    //All possible mill variations
let allMills = [
    { millPos =    3::4::5::[]; isNew = false; previousForms = [[],0]; owner = -1} // B2, B4, B6
    { millPos =    0::1::2::[]; isNew = false; previousForms = [[],0]; owner = -1} // A1, A4, A7
    { millPos =    6::7::8::[]; isNew = false; previousForms = [[],0]; owner = -1} // C3, C4, C5
    { millPos =  9::10::11::[]; isNew = false; previousForms = [[],0]; owner = -1} // D1, D2, D3
    { millPos = 12::13::14::[]; isNew = false; previousForms = [[],0]; owner = -1} // D5, D6, D7
    { millPos = 15::16::17::[]; isNew = false; previousForms = [[],0]; owner = -1} // E3, E4, E5
    { millPos = 18::19::20::[]; isNew = false; previousForms = [[],0]; owner = -1} // F2, F4, F6
    { millPos = 21::22::23::[]; isNew = false; previousForms = [[],0]; owner = -1} // G1, G4, G7
    { millPos =   0::9::21::[]; isNew = false; previousForms = [[],0]; owner = -1} // A1, D1, G1
    { millPos =  3::10::18::[]; isNew = false; previousForms = [[],0]; owner = -1} // B2, D2, F2
    { millPos =  6::11::15::[]; isNew = false; previousForms = [[],0]; owner = -1} // C3, D3, E3
    { millPos =    1::4::7::[]; isNew = false; previousForms = [[],0]; owner = -1} // A4, B4, C4
    { millPos = 16::19::22::[]; isNew = false; previousForms = [[],0]; owner = -1} // E4, F4, G4
    { millPos =  8::12::17::[]; isNew = false; previousForms = [[],0]; owner = -1} // C5, D5, E5
    { millPos =  5::13::20::[]; isNew = false; previousForms = [[],0]; owner = -1} // B6, D6, F6
    { millPos =  2::14::23::[]; isNew = false; previousForms = [[],0]; owner = -1} // A7, D7, G7
    { millPos =    0::3::6::[]; isNew = false; previousForms = [[],0]; owner = -1} // A1, B2, C3
    { millPos = 15::18::21::[]; isNew = false; previousForms = [[],0]; owner = -1} // E3, F2, G1
    { millPos =    2::5::8::[]; isNew = false; previousForms = [[],0]; owner = -1} // C5, B6, A7
    { millPos = 17::20::23::[]; isNew = false; previousForms = [[],0]; owner = -1} // E5, F6, G7
]

let removeBrokenMill (millList : Mill List) (mill : Mill) =
    List.filter ((fun (x : Mill) y -> not (x.millPos = y.millPos)) mill) millList

// Does this player own this mill?
let isOwned (playerID : int) (idx : int) (cows : Cow List) =
    (getCowID allMills.[idx].millPos.[0] cows, getCowID allMills.[idx].millPos.[1] cows, getCowID allMills.[idx].millPos.[2] cows ) = (playerID,playerID,playerID)

let getPlayerMills (currentMills : Mill List) (playerID : int) (cows : Cow List)  =
    List.filter (fun (x : Mill) ->  (((getCowID x.millPos.[0] cows), (getCowID x.millPos.[1] cows), (getCowID x.millPos.[2] cows)) = (playerID,playerID,playerID))) currentMills

//get ID of cow at given position

//Get cow at given position

 
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

let getMillPos (millList : Mill List) (mill: Mill) =
    let rec find i =
        match i < millList.Length with
        | true ->
            match mill.millPos = millList.[i].millPos with
            | true -> i
            | _ -> find (i + 1)
        | _ -> failwith "does not exist"
    find 0

// If mill no longer exists, remove it from the list

let rec getMillForms (prevMills : (int List * int) List) =
    List.map (fun (nums,counter) -> nums) prevMills

// This should be a choose. Luckly it's should, not have to. sigh....
let rec updateCounters  (list : ((int List)*int) List) =
   let list = List.map (fun (nums,counter) -> nums,counter-1) list
   List.filter (fun (nums,counter) -> 
                    match counter with
                    | 0 -> false
                    | _ -> true) list



// Dear, Yusuf. RUN WHILE YOU STILL CAN!
//Update mill list
let updateMills (cows : Cow List)  (playerID : int) (currMill : Mill List) : Mill List =
    let rec check (i : int) newMillList =
        match i < allMills.Length with
        | false -> newMillList
        | _ ->
            // Defining data here for function readability
            let cow1,cow2,cow3 = (getCowAtPos allMills.[i].millPos.[0] cows),(getCowAtPos allMills.[i].millPos.[1] cows),(getCowAtPos allMills.[i].millPos.[2] cows)
            let cowNumberList = [cow1.cowNumber;cow2.cowNumber;cow3.cowNumber]
            // Does this particular mill exist on the board (for playerID)?
            match isOwned playerID i cows with
            // Yes. The mill currently exists on the board
            | true ->
                // Is the mill currently in our list? 
                match List.tryFind ((fun (x : Mill) (y : Mill) -> x.millPos = y.millPos) allMills.[i]) newMillList with
                // No. Add new mill to list
                | None ->                       
                        check (i + 1) ({millPos = allMills.[i].millPos; isNew = true; previousForms = [cowNumberList,2]; owner = playerID} :: newMillList)
                // Yes. Is it unique?
                | Some {millPos = a; isNew = b; previousForms = prevMill} ->
                         match prevMill with
                         | list -> 
                            match List.tryFind ((fun x y -> x = y) cowNumberList) (getMillForms prevMill ) with
                            // It is not unique, mill has been formed twice in the past two terms with different cows
                            | None -> check (i + 1) (updateMillList newMillList (getMillPos newMillList allMills.[i]) {millPos = a; isNew = false ;previousForms = (cowNumberList,2)::prevMill; owner = playerID})
                            // The same mill has been formed with the same cows within two turns. Reset counter to 2
                            | _ -> check (i + 1) (updateMillList newMillList (getMillPos newMillList allMills.[i]) {millPos = a; isNew = false ;previousForms = [cowNumberList,2]; owner = playerID})
                         | _ -> failwith "Dear Satan. Why hath thou forsaken thine loyal follower? :("
            // No. This mill does not exist on the board
            | _   ->
                // Has a mill has been broken?
                match (List.tryFind ((fun (x : Mill) y -> (x.millPos = y.millPos) && (y.owner = playerID)) allMills.[i]) newMillList) with
                // Mill didn't exist, carry on
                | None -> check (i + 1) newMillList 
                // Mill did exist, update counters
                | Some {millPos = a; isNew = b; previousForms = prevMill} -> // Are all the counters zero?
                                                                             let updatedMill =  {millPos = a; isNew = false ;previousForms = (updateCounters prevMill); owner = playerID}
                                                                             match updatedMill.previousForms  with
                                                                             // Yes, remove mill from list. Mill can be used to kill cows again
                                                                             | [] -> check (i + 1) (removeBrokenMill newMillList updatedMill)
                                                                             // No. Atleast one form of the mill had been formed within the last two turns
                                                                             | _ -> check (i + 1) (updateMillList newMillList (getMillPos newMillList updatedMill) updatedMill)
    check 0 currMill

        
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
            
let rec getMove (cowList : Cow List) (playerID : int) =
    printfn "Which cow do you want to move?"
    match getPos () with
    | -1 -> 
        printfn "Invalid position chosen."
        getMove cowList playerID
    | position ->
        let cow = getCowAtPos position cowList
        match playerID = cow.Id with
        | true -> position
        | _ -> 
            printfn "The position you chose is either empty or not your cow."
            getMove cowList playerID

    
let rec checkMove (position : int) (cowList : Cow List) =            
    printfn "Where do you want to move this cow?"
    match getPos () with
    | -1 ->
        drawBoard cowList
        printfn "Invalid position chosen."
        checkMove position cowList
    | newPosition ->
        let cow = getCowAtPos  position cowList 
        match isValidMove cow newPosition with
        | -1 -> 
            drawBoard cowList
            printfn "Invalid move choice for given cow."
            checkMove position cowList
        | _ -> 
            match (getCowAtPos newPosition cowList).Id with
            | -1 -> position, newPosition
            | _ -> checkMove position cowList
            
let rec phase2 (cowList : Cow List) (playerID : int) (mills : Mill List)=
    match true with
    | true ->
        Console.Clear ()
        drawBoard cowList
        let cowToMove = getMove cowList playerID
        let cowToMove, placeToMove = checkMove cowToMove cowList // WOW!!! tuple pattern!!!!!!!
        let cowMoved = updateCOWList cowList placeToMove {getCowAtPos cowToMove cowList with Position = placeToMove } // Move our cow to this position
        let newCowList = updateCOWList cowMoved cowToMove {emptyCow with Position = cowToMove}   // Replace cow at position with empty cow
        let newMills = updateMills newCowList playerID mills
        let newNewCowList = checkMill newCowList newMills playerID
        phase2 newNewCowList (playerID % 2) newMills
    | _ -> failwith "asdasdfdsfg"



     //Phase 2: Update the cowlist
    //Remove cow at old position
    //Add cow at new position 
    
//Start game loop
let Start () =                       
    let phaseOne cowList =
        let rec getCows i (list : Cow List) (currentMill : Mill List) =
            match i = 24 with
            | true -> list, currentMill
            | _ ->
                //
                // TO DO: Clean up method
                //
                Console.Clear()
                printfn "\nPlace your cows: Player one will place first\n"
                drawBoard list                                                      //When a mill is formed, it only draws the cow that formed the mill, after you kill a cow
                printfn "\n\nPlayer %d: Enter a cow position" (i%2 + 1)
                let pos = getPos()
                let newCow = {Position = pos; isFlyingCow = false; Id = i % 2; cowNumber = i }
                let newCowList = updateCOWList list pos newCow          //List before checking for mills and possibly killing cow
                let currMills = updateMills newCowList (i % 2) currentMill
                let newCowList2 =  checkMill newCowList currMills (i%2)  //List after ^
                getCows (i + 1) newCowList2 currMills 
        getCows 0 cowList []    
    let cows,mills = phaseOne (emptyList ())
    phase2 cows 0 mills
   
[<EntryPoint>]


let main argv = 
    let x = Start ()
    printfn "%A" argv
    0 // return an integer exit code
