open System  

//******************************************************************************************************************************
//
// -------------------------------------------------------- Data Structures ----------------------------------------------------
//
//******************************************************************************************************************************

//NOTE: Since one of our project members keeps referring to Discriminated Unions as Unicorns, they will be called as such (He is special).

// Unicorn - Player
type Player = //For us to distinguish between players in our code
| ONE    
| TWO
| EMPTY

// Record - Cow:
type Cow = { // This is our basic cow-data unit used in our game.   

    Position : int     // Current position of cow on the board as an index
    isFlyingCow : bool // For third phase movement
    Id : Player           // To differentiate between player's cows
    cowNumber : int    // To differentiate between cows in general
}

// Record - Mill
type Mill = { // Creates the structure to allow for mills and its rules.

    millPos : int List                        // A list of each position the cows hold in the mill
    isNew : bool                              // A check to see if this mill was just formed
    previousForms :  ((int List) * int) List  // A list to keep track of all cows who formed this mill
    Id : Player                               // To check who this mill belongs too
}

//******************************************************************************************************************************
//
// -------------------------------------------------------- User Interaction ---------------------------------------------------
//
//******************************************************************************************************************************

// Set the Console Size to fit all the rules etc.
Console.SetWindowSize(Console.WindowWidth, 50)

// Offset the text to the center of the console
let printCenterLine =
    fun (line: string) -> 
        Console.SetCursorPosition((Console.WindowWidth - line.Length) / 2, Console.CursorTop)
        Console.WriteLine (line)

// Prompted when games starts displaying rules and controls
let startUpPrompt () =   
    Console.ForegroundColor <- ConsoleColor.Green 

    printCenterLine "   *                                                        "
    printCenterLine " (  `                       )                      )        "
    printCenterLine " )\))(        (       )  ( /(     )  (       )  ( /(     )  "
    printCenterLine "((_)()\   (   )(   ( /(  )\()) ( /(  )(   ( /(  )\()) ( /(  "
    printCenterLine "(_()((_)  )\ (()\  )(_))((_)\  )(_))(()\  )(_))((_)\  )(_)) "
    printCenterLine "|  \/  | ((_) ((_)((_)_ | |(_)((_)_  ((_)((_)_ | |(_)((_)_  "
    printCenterLine "| |\/| |/ _ \| '_|/ _` || '_ \/ _` || '_|/ _` || '_ \/ _` | "
    printCenterLine "|_|  |_|\___/|_|  \__,_||_.__/\__,_||_|  \__,_||_.__/\__,_| "
    printfn "\n\n\n"
    printCenterLine " ------------------- Let the Games Begin! ------------------- "
    printfn "\n\n"
    printCenterLine " ---- [ Rules ] ---- "    
    printfn ""
    printCenterLine "1. Morabaraba consists of a board with pieces which we refer to as 'cows'." 
    printCenterLine "Each player starts with 12 cows and the game consists of 3 phases:"
    printCenterLine  "Placing, Moving and Flying.\n"
    printCenterLine "i. Placing - Place one of your 12 cows you start with at a board-position."
    printCenterLine "ii. Moving - Once you have placed your 12 cows, move your cow to any empty"
    printCenterLine "position neighbouring your cow."
    printCenterLine "iii. Flying - When you have 3 cows left, you can then move your cow to any"
    printCenterLine "empty position on the board you feel like.\n"
    printfn ""
    printCenterLine "2. You can eliminate your opponent's cows using mills. A mill is when you get 3"
    printCenterLine "of your cows in a row (including straight diagonals). When formed, you will be asked"
    printCenterLine "which of your opponents cows you would like to kill. Enter their board-position and"
    printCenterLine "*poof* free Beef-Wellington for dinner. There are some rules to mills, such as:\n"
    printCenterLine "i. You can only form the same mill with the same cows after 2 of your turns has passed."
    printCenterLine "ii. If you form a mill, you cannot kill cows that are in a mill already i.e. they are safe."
    printCenterLine "iii. If you form a mill and all of your opponent's cows are in mills, then you may kill any of cow.\n"
    printfn ""
    printCenterLine "3. WIN - When your opponent has only 2 cows left, then you win!\n"
    printfn ""
    printCenterLine "4. If no vaild moves are available to any player, the game ends in a DRAW.\n"
    printfn ""
    printCenterLine" --- [ Press Enter to Begin ] --- "

    Console.ReadKey()

// Get and check to see if a valid input has been recieved
let rec getPos ()=                             
        Console.SetCursorPosition((Console.WindowWidth) / 2, Console.CursorTop)
        let pos = 
            match (Console.ReadLine ()).ToLower() with
            | "a1" -> 0 | "a4" -> 1 | "a7" -> 2
            | "b2" -> 3 | "b4" -> 4 | "b6" -> 5
            | "c3" -> 6 | "c4" -> 7 | "c5" -> 8
            | "d1" -> 9 | "d2" -> 10| "d3" -> 11| "d5" -> 12| "d6" -> 13| "d7" -> 14
            | "e3" -> 15| "e4" -> 16| "e5" -> 17
            | "f2" -> 18| "f4" -> 19| "f6" -> 20
            | "g1" -> 21| "g4" -> 22| "g7" -> 23
            | _ -> -1  
        match pos with
        | -1 -> 
            printfn ""
            printCenterLine "Invalid position, please enter a new one:"
            getPos ()
        | _ -> pos            
    
// Get and check to see if the position is blank and valid
let rec getBlankPos (boardState: Cow List)=                             
        let pos = getPos()
        match boardState.[pos].Id with
        | EMPTY -> pos
        | _-> 
            printCenterLine "-----  [ Please choose a position where there are no cows ]  -----"
            getBlankPos (boardState)

// Takes a cow and returns the char representation for the board
let getChar (cow : Cow)=
    match cow.Id with
    | ONE -> 'r'
    | TWO -> 'b'
    | _ -> ' '

// Takes any ID and converts it to its numerical representation
let showWinner (playerID : Player) =
    match playerID with
    | ONE -> 
        printfn ""
        printCenterLine "Player 2 wins!"
    | TWO -> 
        printfn ""
        printCenterLine "Player 1 wins!"
    | _ -> failwith "There is no 3, because the rule is 3 is a crowd."

// Takes a list of cows and prints them into a predefined board format at their respective positions
let drawBoard (list : Cow List)  =  // print the board           
        Console.Clear()
        printfn ""
        printCenterLine " __  __                           _                               _              "
        printCenterLine "|  \/  |   ___      _ _   __ _   | |__    __ _      _ _   __ _   | |__    __ _   "
        printCenterLine "| |\/| |  / _ \    | '_| / _` |  | '_ \  / _` |    | '_| / _` |  | '_ \  / _` |  "
        printCenterLine "|_|__|_|  \___/   _|_|_  \__,_|  |_.__/  \__,_|   _|_|_  \__,_|  |_.__/  \__,_|  "
        printCenterLine "_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"|_|\"\"\"\"\"| "
        printCenterLine "\"`-0-0-'\"`-0-0-'\"`-0-0-'\"`-0-0-'\"`-0-0-'\"`-0-0-'\"`-0-0-'\"`-0-0-'\"`-0-0-'\"`-0-0-' "
        printfn ""
    
        printCenterLine "  1   2   3   4   5   6   7"
        printCenterLine ""
        printCenterLine (String.Format(" A   ({0})---------({1})---------({2})    ", (getChar list.[0]), (getChar list.[1]), (getChar list.[2])))
        printCenterLine "     | \         |         / |    "
        printCenterLine (String.Format(" B    |  ({0})-----({1})-----({2})  |    ", (getChar list.[3]), (getChar list.[4]), (getChar list.[5])))
        printCenterLine "     |   | \     |     / |   |    "
        printCenterLine (String.Format(" C    |   |  ({0})-({1})-({2})  |   |    ", (getChar list.[6]), (getChar list.[7]), (getChar list.[8])))
        printCenterLine "     |   |   |       |   |   |    "
        printCenterLine (String.Format(" D   ({0})-({1})-({2})     ({3})-({4})-({5})    ", (getChar list.[9]), (getChar list.[10]), (getChar list.[11]), (getChar list.[12]), (getChar list.[13]), (getChar list.[14])))
        printCenterLine "     |   |   |       |   |   |    "
        printCenterLine (String.Format(" E    |   |  ({0})-({1})-({2})  |   |    ", (getChar list.[15]), (getChar list.[16]), (getChar list.[17])))
        printCenterLine "     |   | /     |     \ |   |    "
        printCenterLine (String.Format(" F    |  ({0})-----({1})-----({2})  |    ", (getChar list.[18]), (getChar list.[19]), (getChar list.[20])))
        printCenterLine "     | /         |         \ |    "
        printCenterLine (String.Format(" G   ({0})---------({1})---------({2})    ", (getChar list.[21]), (getChar list.[22]), (getChar list.[23])))

// A base value to use if we want to create an empty cow
let emptyCow = {Position = -1; isFlyingCow = false; Id = EMPTY ; cowNumber = -1}

// A base value to use if we want to create an empty mill
let emptyMill = { millPos = []; isNew = false; previousForms = [[],0]; Id = EMPTY}

//******************************************************************************************************************************
//
// -------------------------------------------------------- State Checking -----------------------------------------------------
//
//******************************************************************************************************************************    

// Takes a cow position and returns all of the neighbours to that position
let possibleMoves cowPos = 
    match cowPos with
    | 0  -> [ 1; 3; 9]     
    | 1  -> [ 0; 2; 4]
    | 2  -> [ 1; 5; 14]
    | 3  -> [ 0; 4; 6; 10]
    | 4  -> [ 1; 3; 5; 7]
    | 5  -> [ 2; 4; 8; 13]
    | 6  -> [ 3; 7; 11]
    | 7  -> [ 4; 6; 8]
    | 8  -> [ 5; 7; 12]
    | 9  -> [ 0; 10; 21]
    | 10 -> [ 3; 9; 11; 18]
    | 11 -> [ 6; 10; 15]
    | 12 -> [ 8; 13; 17]
    | 13 -> [ 5; 12; 14; 20]
    | 14 -> [ 2; 13; 23]
    | 15 -> [ 11; 16; 18]
    | 16 -> [ 15; 17; 19]
    | 17 -> [ 12; 16; 20]
    | 18 -> [ 10; 15; 19; 21]
    | 19 -> [ 16; 18; 20; 22]
    | 20 -> [ 13; 17; 19; 23]
    | 21 -> [ 9; 18; 22]
    | 22 -> [ 19; 21; 23]
    | 23 -> [ 14; 20; 22]
    | _ -> failwith "No no, jail is that way."

 // Takes a cow and a new position and checks to see if the possible move is valid or not (are they neighbouring positions?)
let isValidMove (cow : Cow) (position : int) =
    match List.tryFind (fun t -> t = position) (possibleMoves (cow.Position)) with
    | None -> -1
    | Some (pos) -> pos
    
// All possible mill variations
let allMills = [  
    { emptyMill with millPos =    0::1::2::[]} // A1, A4, A7
    { emptyMill with millPos =    3::4::5::[]} // B2, B4, B6    
    { emptyMill with millPos =    6::7::8::[]} // C3, C4, C5
    { emptyMill with millPos =  9::10::11::[]} // D1, D2, D3
    { emptyMill with millPos = 12::13::14::[]} // D5, D6, D7
    { emptyMill with millPos = 15::16::17::[]} // E3, E4, E5
    { emptyMill with millPos = 18::19::20::[]} // F2, F4, F6
    { emptyMill with millPos = 21::22::23::[]} // G1, G4, G7
    { emptyMill with millPos =   0::9::21::[]} // A1, D1, G1
    { emptyMill with millPos =  3::10::18::[]} // B2, D2, F2
    { emptyMill with millPos =  6::11::15::[]} // C3, D3, E3
    { emptyMill with millPos =    1::4::7::[]} // A4, B4, C4
    { emptyMill with millPos = 16::19::22::[]} // E4, F4, G4
    { emptyMill with millPos =  8::12::17::[]} // C5, D5, E5
    { emptyMill with millPos =  5::13::20::[]} // B6, D6, F6
    { emptyMill with millPos =  2::14::23::[]} // A7, D7, G7
    { emptyMill with millPos =    0::3::6::[]} // A1, B2, C3
    { emptyMill with millPos = 15::18::21::[]} // E3, F2, G1
    { emptyMill with millPos =    2::5::8::[]} // C5, B6, A7
    { emptyMill with millPos = 17::20::23::[]} // E5, F6, G7
]

//******************************************************************************************************************************
//
// -------------------------------------------------------- Function Things ----------------------------------------------------
//
//****************************************************************************************************************************** 

// Initialise cow list with empty cows                
let emptyList () =
    let list = List.init 24 (fun x -> emptyCow)
    List.mapi (fun i (x : Cow) -> {x with Position = i}) (list) 

// Gets the ID of a cow at a position on the board
let getCowID pos (cows : Cow List) =                            
    cows.[pos].Id

// Replace cow at a given position with a given cow
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
        
// Kill chosen cow and replace dead cow with empty cow
let killCow (pos : int) (cows : Cow List) =        
    updateCOWList cows pos {emptyCow with Position=pos}

// Checks to see if the board is in a Draw state 
let isDraw (cowList : Cow List) playerID =

    let rec getPlayerCowPos i cowPos =
        match i < cowList.Length with
        | false -> cowPos 
        | _ ->
            match cowList.[i].Id = playerID with
            | false -> getPlayerCowPos (i + 1) cowPos 
            | _ -> getPlayerCowPos (i + 1) (cowList.[i].Position::cowPos)

    let rec allMoves i (cowNum : int List) (moves : int List) =
        match i < cowNum.Length with
        | false -> moves |> Seq.distinct |> List.ofSeq                
        | _ -> allMoves (i + 1) cowNum moves@(possibleMoves cowNum.[i])
    
    let possibleMoves = allMoves 0 (getPlayerCowPos 0 []) []

    let rec Draw i  =
        match i < possibleMoves.Length with
        | false -> true
        | _ -> 
            match (cowList.[possibleMoves.[i]]).Id with
            | EMPTY -> false
            | _ -> Draw (i + 1)
    
    Draw 0

// Removes a mill from a list of mills when a mill is broken
let removeBrokenMill (millList : Mill List) (mill : Mill) =
    List.filter ((fun (x : Mill) y -> not (x.millPos = y.millPos)) mill) millList

// Does this player own this mill?
let isOwned (playerID : Player) (idx : int) (cows : Cow List) =
    (getCowID allMills.[idx].millPos.[0] cows, getCowID allMills.[idx].millPos.[1] cows, getCowID allMills.[idx].millPos.[2] cows ) = (playerID,playerID,playerID)

// Returns a list of mills owned by a particular player
let getPlayerMills (currentMills : Mill List) (playerID : Player) (cows : Cow List)  =
    List.filter (fun (x : Mill) ->  (((getCowID x.millPos.[0] cows), (getCowID x.millPos.[1] cows), (getCowID x.millPos.[2] cows)) = (playerID,playerID,playerID))) (currentMills)
 
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

// Gets the index of a given mill 
let getMillPos (millList : Mill List) (mill: Mill) =
    let rec find i =
        match i < millList.Length with
        | true ->
            match mill.millPos = millList.[i].millPos with
            | true -> i
            | _ -> find (i + 1)
        | _ -> failwith "Does not exist"
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
let updateMills (cows : Cow List)  (playerID : Player) (currMill : Mill List) : Mill List =
    let rec check (i : int) newMillList =
        match i < allMills.Length with
        | false -> newMillList
        | _ ->
            // Defining data here for function readability
            let cow1,cow2,cow3 = (cows.[allMills.[i].millPos.[0]]),(cows.[allMills.[i].millPos.[1]]),(cows.[allMills.[i].millPos.[2]])
            let cowNumberList = [cow1.cowNumber;cow2.cowNumber;cow3.cowNumber]
            // Does this particular mill exist on the board (for playerID)?
            match isOwned (playerID : Player) i cows with
            // Yes. The mill currently exists on the board
            | true ->
                // Is the mill currently in our list? 
                match List.tryFind ((fun (x : Mill) (y : Mill) -> x.millPos = y.millPos) allMills.[i]) newMillList with
                // No. Add new mill to list
                | None ->                       
                        check (i + 1) ({millPos = allMills.[i].millPos; isNew = true; previousForms = [cowNumberList,2]; Id = playerID} :: newMillList)
                | Some {millPos = a; isNew = b; previousForms = prevMill} ->
                // Yes. Is it unique?
                    match List.tryFind ((fun x y -> x = y) cowNumberList) (getMillForms prevMill ) with
                    // It is not unique, mill has been formed twice in the past two terms with different cows
                    | None -> check (i + 1) (updateMillList newMillList (getMillPos newMillList allMills.[i]) {millPos = a; isNew = true ;previousForms = (cowNumberList,2)::prevMill; Id = playerID})
                    // The same mill has been formed with the same cows within two turns. Reset counter to 2
                    | _ -> check (i + 1) (updateMillList newMillList (getMillPos newMillList allMills.[i]) {millPos = a; isNew = false ;previousForms = [cowNumberList,2]; Id = playerID})
            // No. This mill does not exist on the board
            | _   ->
                // Has a mill has been broken?
                match (List.tryFind ((fun (x : Mill) y -> (x.millPos = y.millPos) && (y.Id = playerID)) allMills.[i]) newMillList) with
                // Mill didn't exist, carry on
                | None -> check (i + 1) newMillList 
                // Mill did exist, update counters
                | Some {millPos = a; isNew = b; previousForms = prevMill} -> // Are all the counters zero?
                    let updatedMill =  {millPos = a; isNew = false ;previousForms = (updateCounters prevMill); Id = playerID}
                    match updatedMill.previousForms  with
                    // Yes, remove mill from list. Mill can be used to kill cows again
                    | [] -> check (i + 1) (removeBrokenMill newMillList updatedMill)
                    // No. Atleast one form of the mill had been formed within the last two turns
                    | _ -> check (i + 1) (updateMillList newMillList (getMillPos newMillList updatedMill) updatedMill)
    check 0 currMill

// It returns the playerID of the opponent for the given playerID     
let getOpponent (playerID : Player) =
    match playerID with
    | ONE -> TWO
    | TWO -> ONE
    | _ -> failwith "Invalid player"
    
// Gets the number of cows that belong to a specific player in our cow list
let getPlayerCowLength (cowList : Cow List) (playerID : Player) =
    List.fold (fun initial (cow : Cow) -> 
        match cow.Id = playerID with
        | true -> initial + 1
        | _ -> initial) 0 cowList

// Check if all cows a player owns are in mills
let allInMill (cows : Cow List) (mills : Mill List) (playerID : Player) = 
    let rec check (i : int) (cowList : int List) =
        match i < allMills.Length with
        | false -> cowList
        | _ ->
            match isOwned (playerID : Player) i cows with
            | true -> check (i + 1) (allMills.[i].millPos@cowList)
            | _ -> check (i + 1) cowList
    
    // Im a little proud of this one :^)
    // If number of cows = number of distinct cows in mills, then all cows are in mills
    let a = getPlayerCowLength cows playerID
    let b = (check 0 []) |> Seq.distinct |> List.ofSeq
    match a = b.Length with
    | res -> res    

// Check if chosen cow is in a mill
let canKill (pos : int) (mills : Mill List) (playerID : Player) (cows : Cow List) =
    let playerMills = getPlayerMills mills playerID cows                          // Get Player mills
    let enemyMills = getPlayerMills mills (getOpponent playerID) cows             // Get enemy mills
    match cows.[pos].Id = playerID || cows.[pos].Id = EMPTY with                  // Check if cow to kill is players own cow    
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
        | _ ->
            match allInMill cows mills (getOpponent playerID) with
            | false -> check 0
            | _ -> true  

// Check to see if any new mills have been formed
let isNewMill (mills : Mill List) =
    let newMill (mill : Mill) = mill.isNew = true
    List.exists newMill mills
    
// Check for mill, let player kill cow if mill exists. Verification Needed
let checkMill (cows : Cow List) (mills : Mill list) (playerID : Player) =
    let playerMills = getPlayerMills mills playerID cows
    match playerMills.Length with
    | 0 -> cows
    | _ -> 
        match isNewMill playerMills with
        | false -> cows
        | _ ->
            drawBoard cows
            printfn ""
            printCenterLine "-----  [ Choose a cow to kill ]  -----"
            let rec tryKill () =
                let cowToKill = getPos ()
                match canKill (cowToKill) mills playerID cows with
                | true -> killCow cowToKill cows                   
                | _ ->
                    drawBoard cows
                    printfn ""
                    printCenterLine "You cannot kill that cow."
                    printCenterLine "-----  [ Please enter a new position ] -----"
                    tryKill ()
            tryKill () 
 
// Ask the player to choose a cow and to see if they is a valid choice, else return the position chosen
let rec getMove (cowList : Cow List) (playerID : Player) =
    printCenterLine "-----  [ Which cow do you want to move? ]  -----"
    match getPos () with
    | -1 -> 
        printfn ""
        printCenterLine "Invalid position chosen."
        getMove cowList playerID
    | position ->
        let cow = cowList.[position] 
        match playerID = cow.Id with
        | true -> position
        | _ -> 
            printfn ""
            printCenterLine "The position you chose is either empty or not your cow."
            getMove cowList playerID

// With cow to move, ask where to move it and see if it is valid, else return the new position to move too
let rec checkMove (position : int) (cowList : Cow List) (playerID : Player) =
    let numberOfCows = getPlayerCowLength cowList playerID
    printCenterLine "-----  [ Where do you want to move this cow? ]  -----"
    match getPos () with
    | -1 ->
        drawBoard cowList
        printfn ""
        printCenterLine "Invalid position chosen."
        checkMove position cowList playerID
    | newPosition ->
        let cow = cowList.[position] 
        let isValid = 
            match numberOfCows < 4 with
            | true -> newPosition
            | _ -> isValidMove cow newPosition
        match isValid with
        | -1 -> 
            drawBoard cowList
            printfn ""
            printCenterLine "Invalid move choice for given cow."
            checkMove position cowList playerID
        | _ -> 
            match cowList.[newPosition].Id with
            | EMPTY -> position, newPosition
            | _ -> checkMove position cowList playerID

//******************************************************************************************************************************
//
// -------------------------------------------------------- Game States -----------------------------------------------------
//
//****************************************************************************************************************************** 

// Alters the console colour depending on whose turn it is
let changeBoardColour (playerID : Player) = 
    match playerID with
    | ONE -> Console.ForegroundColor <- ConsoleColor.Red           
    | TWO -> Console.ForegroundColor <- ConsoleColor.Cyan
    | _ -> failwith "Just like half-life, there is no number 3"

// The placing of cows phase
let phaseOne cowList =
    let rec getCows i (list : Cow List) (currentMill : Mill List) (playerID : Player) =
        match i with
        | 24 -> list, currentMill // All cows have been placed
        | _ ->
            // New Turn
            changeBoardColour playerID // Change board colour
            drawBoard list // Draw board from last known arrangement of cows
            printfn ""
            printCenterLine (String.Format("-----  [ Player {0}: Enter a cow position, You have {1} cows left to place ]  -----", (i % 2 + 1), floor (float(25-i)/2.0))) // Print amount of cows left to place
            let pos = getBlankPos list
            let BoardUpdate = updateCOWList list pos {Position = pos; isFlyingCow = false; Id = playerID; cowNumber = i }  // List before checking for mills and possibly killing cow
            let currMills = updateMills BoardUpdate playerID currentMill // Gets a list of all current mills in this given cow list state
            let newCowList =  checkMill BoardUpdate currMills playerID // Checks for any new mills and if any cows are killed, returns the updated cow list
            getCows (i + 1) newCowList currMills (getOpponent playerID) // Go to next turn
    getCows 0 cowList [] ONE
 
// The moving of cows phase
let rec phase2 (cowList : Cow List) (playerID : Player) (mills : Mill List) =
    match isDraw cowList playerID with // Check for draw condition
    | true -> 
              drawBoard cowList
              printfn ""
              printCenterLine "DRAW!"
              Console.ReadKey ()
    | _ ->
        match (getPlayerCowLength cowList playerID) with // Check for win condition
        | 2 -> 
            showWinner (playerID)
            Console.ReadKey ()
        | _ -> // Else go carry on with current players turn        
            changeBoardColour playerID // Change board colour
            drawBoard cowList // Draw board from last known arrangement of cows
            printfn ""
            let cowToMove = getMove cowList playerID // Get the next move
            let cowToMove, placeToMove = checkMove cowToMove cowList playerID // Check this move -- WOW!!! tuple pattern!!!!!!!
            let cowMoved = updateCOWList cowList placeToMove {cowList.[cowToMove] with Position = placeToMove } // Move our cow to this position
            let newCowList = updateCOWList cowMoved cowToMove {emptyCow with Position = cowToMove}   // Replace cow at old-position with empty cow
            let newMills = updateMills newCowList playerID mills // Update mill list with new cow formations
            let newNewCowList = checkMill newCowList newMills playerID // Update cow positions if any cow was killed from the mills
            phase2 newNewCowList (getOpponent playerID) newMills // Go to next turn

//Start game loop
let Start () =      
    startUpPrompt () // Starting text     
    let cows,mills = phaseOne (emptyList ())
    phase2 cows ONE mills
// FINISH HIM!

[<EntryPoint>]

let main argv = 
    let x = Start () //Let the games begin
    printfn "%A" argv
    0 // return an integer exit code
