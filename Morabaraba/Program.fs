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
//VVVVVVVVVVVVV//

module GameSession =      //Corbyn: basic idea of a game skeleton
    open System.Security.Policy
 
    type Board = {
                
        board : string list
    }
    //let DrawBoard =
    //let UpdateBoard = 

    type Cow = {

        Position : char*int  // store position has letter and number, ie: (A,3)
        isFlyingCow : bool 
        isAlive : bool        // Richard: True if Alive
    }

    type CowList = {
        
        hasThree : bool     // To now when cows can fly
        hasTwo : bool       // Player has lost
        Cows : Cow List          // Richard: Was this meant to be CowList or Cow List? I changed it to Cow List
    }

    type Player = {

        Id : int            // Player Id. Player 1 will be (int 1)
        IsPlaying : bool    // Is it this players turn?
        isWinner : bool     // Player has won
        Cows : CowList       
    }
    
    //let nextTurn =
    //let gameOver =




//--------------------Initialize board--------------------

    let theBoard = {
        board = [
                "             1   2   3   4   5   6   7 " //Line 0
                "        A   ( )---------( )---------( )"   //Line 1
                "             | \         |         / |"    //Line 2
                "        B    |  ( )-----( )-----( )  |"    //Line 3
                "             |   | \     |     / |   |"    //Line 4
                "        C    |   |  ( )-( )-( )  |   |"    //Line 5
                "             |   |   |       |   |   |"    //Line 6
                "        D   ( )-( )-( )     ( )-( )-( )"   //Line 7
                "             |   |   |       |   |   |"    //Line 8
                "        E    |   |  ( )-( )-( )  |   |"    //Line 9
                "             |   | /     |     \ |   |"    //Line 10
                "        F    |  ( )-----( )-----( )  |"    //Line 11
                "             | /         |         \ |"    //Line 12
                "        G   ( )---------( )---------( )"]  //Line 13
    }
    
//--------------------Initialize Cows--------------------

    let Cow = {
        
        Position = ('Z',-1)
        isFlyingCow = false
        isAlive = true
    }

    let listOfCows = {
        
        hasThree = false
        hasTwo = false
        Cows = [Cow;Cow;Cow;Cow;Cow;Cow;Cow;Cow;Cow;Cow;Cow;Cow]    // 12 Cows 
    }

//--------------------Initialize Players-------------------- 

    let player1 = {
        
        Id = 1
        IsPlaying = true
        isWinner = false
        Cows = listOfCows 
    }

    let player2 = {
        
        Id = 2
        IsPlaying = true
        isWinner = false
        Cows = listOfCows 
    }

    
//--------------------Console Properties--------------------

Console.ForegroundColor <- ConsoleColor.Green 


//--------------------Let The Games Begin--------------------
open GameSession

//Test
//Console.WriteLine player2.Id


printfn "%A" theBoard.board     // print the board










Console.ReadKey()       //Just to keep the console open


//[<EntryPoint>]
//let main argv = 
  //  printfn "%A" argv
    //0 // return an integer exit code














//---------------------------RANDOM BITS OF CODE REMOVED------------------------------

(*
Console.ForegroundColor <- ConsoleColor.Green 

Console.WriteLine "             1   2   3   4   5   6   7"
Console.WriteLine ""
Console.WriteLine "        A   ( )---------( )---------( )"
Console.WriteLine "             | \         |         / |"
Console.WriteLine "        B    |  ( )-----( )-----( )  |"
Console.WriteLine "             |   | \     |     / |   |"
Console.WriteLine "        C    |   |  ( )-( )-( )  |   |"
Console.WriteLine "             |   |   |       |   |   |"
Console.WriteLine "        D   ( )-( )-( )     ( )-( )-( )"
Console.WriteLine "             |   |   |       |   |   |"
Console.WriteLine "        E    |   |  ( )-( )-( )  |   |"
Console.WriteLine "             |   | /     |     \ |   |"
Console.WriteLine "        F    |  ( )-----( )-----( )  |"
Console.WriteLine "             | /         |         \ |"
Console.WriteLine "        G   ( )---------( )---------( )"




module Players = 
    
    type Character= {
        Name : string
        CurrentPos : int List
        RemainingCows : int         //Just messing around with an idea...
    }








*)