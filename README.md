# scotty-api
A very simple sample project for playing around with the Haskell [Scotty](https://hackage.haskell.org/package/scotty) web framework.


If you are interested in the code in its most simplest api form (i.e. just past 'hello world', before further, shall we generously say, experiments were performed on the code), try it at this commit:

https://github.com/hombredequeso/scotty-api/tree/b29c8e2342ab96692e49727af2e099288f64d84d

Why Scotty? Because of this:
["A Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp."](https://hackage.haskell.org/package/scotty)
which puts it squarely in the set of simple web frameworks I like. Notable, it gives it a genetic relationship to the [Nancy](http://nancyfx.org/) .net framwork, which I have used in both C# and F# (although to be honest, parts of it look a lot more like the more functional [Suave](https://suave.io/)) framework.

## Getting Started

### Prerequisites

The primary prerequisite is the [Haskell Platform](https://www.haskell.org/platform/).
Ensure that is is at least 8.2.2.

### Running
From the project root directory:
```
> stack build
> stack ghci
*Main>  main
```

### Usage
```
> curl localhost:3000/api/eventStream/abcd
{"header":{"messageStream":"abcd"},"messages":[{"mType":"type1","description":"some message"},{"mType":"type2","description":"another message"}]}
>
> curl localhost:3000/api/eventStream/thisStreamNameIsWayTooLongAndWillResultInAnError
>
```

## Resources
Helpful resources along the way.

This play project was started having reached 'chapter 19' of the [HaskellBook](http://haskellbook.com/). By this stage one should well and truly know enough to be dangerous.
* [Scotty Package](https://hackage.haskell.org/package/scotty)
* [Scotty Wiki](https://github.com/scotty-web/scotty/wiki)
* [Scotty Tutorials & Examples](https://github.com/scotty-web/scotty/wiki/Scotty-Tutorials-&-Examples)

Of the tutorials, these were initially the most helpful in getting (just) past 'hello world'
* [24 Days of Hackage: scotty](https://ocharles.org.uk/blog/posts/2013-12-05-24-days-of-hackage-scotty.html)
* [PRACTICAL HASKELL - BUILDING A JSON API](http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html)

## Initial Project Creation
This project was created as follows:

##### Create Project Using Stack
```
> stack new scotty-api simple
> cd scotty-api
> stack setup
```

##### Dependency Management
Add dependency on scotty to cabel file.
See: scotty-api.cabal > executable > build depends

##### Write Website Code

See Main.hs

#### Build and Run
```
> stack build
> stack ghci
*Main>  main
```
