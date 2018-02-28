// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System



(*Random Notes about Program:
    24 possible Possitios
    20 possible rows of 3 cows
    8 Possitions with the possiblity of 4 cows next to them, the rest can only have 3


*)


module Players = 
    
    type Character= {
        Name : string
        CurrentPos : int List
        RemainingCows : int         //Just messing around with an idea...
    }
    


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



let bored = [
    "             1   2   3   4   5   6   7 \n" 
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





Console.ReadKey()       //Just to keep the console open


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
