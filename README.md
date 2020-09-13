# Haskell-Chess

This projects consists of a chess programm that was made in haskell. It supports singleplayer against an ai, and multiplayer either locally or on network.

## Dependencies

The project requires [gtk3](https://www.gtk.org/docs/installations/).

The project is built with [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Building the Project 

Run the following command from the project's root directory to compile the program, run all checks and create executables:

```
stack build
```

If the command succeeds, the executables can be executed with

```
stack exec Chess-exe
stack exec Shell-exe
stack exec Server-exe
```
