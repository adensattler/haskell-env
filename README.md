# ProgrammingLanguages



# Getting Started

Open the haskell-dev-env-main folder. You should get the following pop-ups

1. Install a docker plug-in (say yes)
2. Reopen the repository as a Container, you must say yes

Note that every time you open VS Code and this folder it will ask that question--you have to say yes for things to work.

It will take a while to open the first time as it downloads and builds the project. 


## Creating Stack Projects

Stack will create a Haskell project for you based on templates. To this, it creates a number of files for configurations, including:

- stack.yaml - this configures stack and determines what version of haskell is installed

- package.yaml - this configures your project to describe how to build the project, which libraries are included, and which compiler options are used

In a terminal or shell window, the following:

```bash
stack new ProjectName stack-templates/comp3351
```

will create a folder called ProjectName in the current working folder using the template created for this course. This will also populate ProjectName with configuration files and subfolders to hold your code and tests.

The ProjectName will contain all the Haskell files for the project. Source code files should be added to the src folder and all test files should be added to the test folder.

You can access your new project by cd-ing into it:

```
cd NewProject
```

Now you can type ```stack build``` and ```stack test``` to make sure it all works.

## Using Stack and GHCi

In a terminal window or shell window the following:

``` bash
stack ghci 
```

This will start the interactive Read-Eval-Print Loop (REPL) for Haskell. This allows Haskell expressions to be entered and evaluated. The result of evaluating the expression will be shown as soon as evaluation is finished (i.e. GHCi will "run" the program consisting of the expression and show the result).

The REPL will be executing in the current working folder. If you change the working folder to one which contains a stack project, then stack ghci will automatically load all Haskell source files from the src folder of the project.

## Useful GHCI Commands
To quit from ghci:

``` :quit ```
at the prompt

The following will tell ghci to show the type of the current result ("it"):

```:set +t```
and

``` :unset +t ```
will turn off the reporting of the result type.

 ## Testing
 I am using the Hspec library for testing.

 Testing files are located within the test folder of each project.

 To run the testing code for a project simply enter that project's directory and enter the command:

 ```bash
 stack test
 ```

 This will run all associated tests!

## Projects
### DailyHaskell
- Contains basic haskell functions that progress in difficulty

### Examples
- Houses miscellaneous programs I am experimenting with

### MiniRacketProject
- Uses haskell to implement a miniture version of the programming language racket with lambda functions, recursion, and more!