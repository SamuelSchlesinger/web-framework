# Minimalist Haskell Application Framework

This is a minimalist framework for writing a Haskell application which uses a
stack that I find pallatable. For the database layer, I use
[squeal-postgresql](https://hackage.haskell.org/package/squeal-postgresql), as
I like that it doesn't allow me to construct ill-formed queries and statements,
and to rigorously encode the schema that I expect of my database. It is also
very performant, and allows me to use my database in a flexible and performant
way if I require that. For the HTTP API and endpoint definitions I use 
[servant](https://www.servant.dev/), as I really enjoy defining the API of my
application in a clear and flexible way. Both squeal and servant are very similar
in spirit, allowing me to flexibly and precisely define what I want my application's
behavior to be, in a way that I can leverage the Haskell compiler to help me
implement and validate it. For the server itself I use
[warp](http://hackage.haskell.org/package/warp), which I simply use because it
is blazing fast compared to anything else I know of written in Haskell.

The application currently contains very little, just a basic migration system
and an application which implements an empty API. This is meant to be used as a
template to get going when I have to implement an HTTP application which needs some
database tables.
