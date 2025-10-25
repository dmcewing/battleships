# battleships
Found some code I wrote when I was 17.  Thought I'd resurrect it.

## Background

In year 11 or 12 (age 16 to 17) I was in a option class that looked at interfacing between two computers using a bi-directional parallel port cable.  At the time (early 90s) the computers were 486 vintage (probably SX25s) the language used was QBasic (or the larger Quick Basic (QB45)).  After working out how to pass a string message from one machine to another the next task was to build a game of battleships. 

While playing with a bunch of my Retro hardware I found a floppy containing the code I had written back then.

Finding that there is a [QB64](https://qb64.com) community that has kept Quick Basic alive I thought I'd give it a go.

As some of the file (battle.bas.txt) was corrupted there was a little bit of work to do to fix up the destroyed parts of the code.  It appears to have had a join with the message sending program.  After getting that recovered and restored, I took the next step of replacing the parallel port messaging with some TCP messaging.  To assist with outbound network calls I created an additional Battle Host to proxy and manage the connection between the game instances and bingo...

Battleships was alive again.

Now the extensions begin, because... why not.

## Base files

* [battle.bas.txt](./battle.bas.txt) - The source code file before recovery attempts.
* [battle_ip.bas](./battle_ip.bas) - The base TCP protocol based program
* [battlehost.bas](./battlehost.bas) - The host/server for the TCP based program.
* [battlebot.bas](./battlebot.bas) - A Robotic host for playing against the computer

