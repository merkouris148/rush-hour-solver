# rush-hour-solver
A [rush hour](https://en.wikipedia.org/wiki/Rush_Hour_(puzzle)) solver in Haskell. In collaboration with @sanantoniochili

The programme **rush_hour.hs** is an implementation in Haskell of a solver for the classic puzzle game rush hour. There are two main solvers implemented by the functions ```solve``` and ```solve_astar```. The function ```solve``` implements a simple Bridth First Search, while ```solve_astar``` uses a heuristic described [here](https://github.com/atheed/UnblockMe-Solver/).

The input of the solve functions is an initial state of the rush hour grid. Although a grid state is encoded internally differently, there are provided the functions ```readState``` and ```writeState``` to convert a simple string in a valid grid state. An example of such a valid _state-string_ is given bellow.

```
"...a\n==.a\n....\n....\n"  
```
which describes the following state:

```
...a  
==.a  
....  
....  
```

The blocking cars are represented with the same character (e.g. 'a'), the [_red car_](https://en.wikipedia.org/wiki/Rush_Hour_(puzzle)#Objective), is represented by the character '=' and the empty places are represented by '.'.
Calling the function ```solve (readState "...a\n==.a\n....\n....\n")``` in GHCi prompt, will return a list of moves in order to the _red car_, be unblocked. For a more _user friendly_ list of subsequent string-states to be printed, you may use the function ```printSolution```, see the examples bellow.

```
> state = readState "...a\n==.a\n....\n....\n"
> printSolution state (solve state)

...a
==.a
....
....
....

==..
...a
...a
....

..==
...a
...a

> state = readState "..abbb\n..a.c.\n..==c.\n....d.\n....d.\n....d.\n"
> printSolution state (solve_astar state)

..abbb
..a.c.
..==c.
....d.
....d.
....d.

..abbb
..a.c.
==..c.
....d.
....d.
....d.

...bbb
....c.
==..c.
....d.
..a.d.
..a.d.

.bbb..
....c.
==..c.
....d.
..a.d.
..a.d.

.bbbc.
....c.
==....
....d.
..a.d.
..a.d.

.bbbc.
....c.
....==
....d.
..a.d.
..a.d.
```

## Notes
The file **rush_hour.pdf** contains a detailed description of the assignment's requirements (in Greek), while in **README.pdf** we describe in detail the implementation.

### Issues
If a sting-state is given to ```solve``` (or ```solve_astar```) the function will _not_ terminate.
