1. Install stack if you don't have it installed:
```sh
ghcup install stack
```
3. Put the file `testing.hs` in same directory as `HW2.hs`.
3. Install the HUnit package
```sh
stack install HUnit
```
4. Compile the testing file
```sh
stack ghc -- testing.hs
```
5. Run the testing file
```sh
./testing
```
