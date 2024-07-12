# Data Bias

This project takes input given in .txt files and randomizes it in a specific way to output potential Minecraft building challenges. This program would be great to use for synchronous building challenges in the game, as it would require coming up with prompts that increase in difficulty as the event continues.

## Description

While attending university, I used a lot of my knowledge to create projects that would help me accomplish things I was doing in my free time. One of my hobbies is playing video games. For a while, I was the head of a small gaming community that played an assortment of games together. At one point, we added a few people to the team and started hosting events, with different prizes. One of the events that we held was a Minecraft build competition. However, it wasn't easy to prove that members who were friends with admins would not get access to the prompts before the event. So, I created a program that we used to make the prompts progressively more difficult in a way that we could manage by editing the files that provided different aspects of the prompts. This way, the secrecy of the prompts could remain, as not even those running the event would know what the prompts were until they were picked that day. 

## Getting Started

### Dependencies
The program imports two classes:
* java.io.File;
* java.util.Scanner;
These imports allow the program to read the .txt files.

### Installing

* Use Git Hub to download the program files.
* Import into your favorite Java IDE.
* If you change the .txt files to include more or fewer lines, adjust the beginning of the MinecraftBuild.java file to include the new number of lines in the corresponding files.
Example:
If you change the adjectives.txt file to include 75 lines, each with a different adjective on each line, line 7 of MinecraftBuild.java should instead read as follows:
```
String[] adjectives = new String[75]; #75 lines in the adjectives.txt file
```

### Executing program

* Download the program and import it into your favorite Java-enabled IDE.
* Press run.
* The program will terminate with output to the console with random challenges.

## Help
If you wish to change the file used or the number of items in a file, you must edit the source code at the beginning of the MinecraftBuild.java file. Note that the number of items in a file is the same as the number of lines in the file, which is also the same as the number in the arrays created at the beginning of the MinecraftBuild.java file.

## Authors
McKayla McAllister
