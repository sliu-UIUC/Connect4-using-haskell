# Connect4-using-haskell
This is a connect 4 game I created for my functional course final project using Haskell. It's capable of playing connect 4 1 on1 with a real person or play with the build-in AI

The gameboard is a 6x6 matrix and you get to insert your piece at any column. The game is simple. Pieces will drop on top of each other and who connects 4 of their own piece, either horizontally, vertically or diagonally, wins the game. A wed-app connect 4 is available here for example: https://www.mathsisfun.com/games/connect4.html. 

To compile the project, run:
```haskell
ghc Main.hs
```

Optional flags include: 

Flag        | Long version     | Description   
----------- | ---------------- | --------------------------------------------------------------------------------------:
  -h        | --help           | Print out a help message and quit the program
  -w        | --winner         | Print out who will win this game, using an exhaustive search (no cut-off depth)
  -d \<num\>| --depth=\<num\>  | Use <num> as a cutoff depth, instead of your default
  -m \<num\>| --move=\<num\>|  | Make <move> and print out the resulting board, in the input format, to stdout
  -v        | --verbose        | Output both the move and a description of how good it is: win, lose, tie, or a rating
  -i        | --interactive    | Start a new game and play against the computer
  

For instance, to play 1 on 1 interactive game after successful compilation, run: 

```haskell
Main -i
```
