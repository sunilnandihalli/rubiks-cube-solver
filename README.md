rubiks-cloact-webapp
====================
A web-app which can solve any rubiks cube. It needs a lot of UI improvements to make it usable. An exercise to learn clojurescript, scenejs and reagent.

What it does?

Just click on the shuffle button and it randomly shuffles the rubiks cube. It does not remember the moves made. It then
uses a set of predefined rules to solve the puzzle and animates it using scenejs. You can see the application  in action here
http://sunilnandihalli.github.io

To download and run the application you will need to have leiningen installed. Then one can simply clone the repository and
run 

lein dev

then visit 

http://localhost:3000/
