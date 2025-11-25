# fp-2025

## Lesson notes

Can be viewed [here](https://vipo.github.io/fp-2025/)

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

#BNF
```
<command> ::= "AddCar" <car>
            | "FilterByNumber" <carNumber>
            | "AddMechanic" <carNumber> <mechanicName> <mechanicAge>
            | "Sequence" <command> <command>
            | "CalculateAverageLapTime" <car>
            | "Dump" <dumpable>

<car> ::= "Car" <carNumber> <driver> <mechanics> <lapTimes>

<driver> ::= "Driver" <driverName> <driverNationality> <driverAge>

<mechanics> ::= "[" <mechanicList> "]"

<mechanicList> ::= <mechanic> | <mechanic> "," <mechanicList>

<mechanic> ::= "Mechanic" <mechanicName> <mechanicAge>

<lapTimes> ::= "[" <intList> "]"

<intList> ::= <integer> 
            | <integer> "," <intList>

<carNumber> ::= <string>
<driverName> ::= <string>
<driverNationality> ::= <string>
<mechanicName> ::= <string>
<mechanicAge> ::= <integer>
<driverAge> ::= <integer>

<dumpable> ::= "Examples"

<string> ::= "[a-zA-Z0-9_]+"
<integer> ::= "[0-9]+"
```
