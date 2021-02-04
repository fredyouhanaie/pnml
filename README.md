# `pnml`

`pnml` is an Erlang module for processing Petri Net Markup Language
(PNML) files.

## PNML

PNML is an XML based mark up language that acts as an interchange
format for Petri net definitions.

To learn more about PNML see [http://www.pnml.org/](http://www.pnml.org/)

Also, these following two papers provide good introduction to the
language:

- L.M. Hillah and E. Kindler and F. Kordon and L. Petrucci and
  N. Trèves,
  A primer on the Petri Net Markup Language and ISO/IEC 15909-2
  [PDF](http://www.pnml.org/papers/pnnl76.pdf)

- Michael Weber and Ekkart Kindler,
  The Petri Net Markup Language, 2003
  [PDF](http://www.pnml.org/papers/PNML_LNCS.pdf)

## Petri Net

To quote from the description on Wikipedia:

> _"A Petri net, also known as a place/transition (PT) net, is one of
> several mathematical modeling languages for the description of
> distributed systems. It is a class of discrete event dynamic
> system."_

To learn more about Petri Nets follow the links below:

- [https://en.wikipedia.org/wiki/Petri_net](https://en.wikipedia.org/wiki/Petri_net)
- [https://www.informatik.uni-hamburg.de/TGI/PetriNets/index.php](https://www.informatik.uni-hamburg.de/TGI/PetriNets/index.php)
- [http://www.scholarpedia.org/article/Petri_net](http://www.scholarpedia.org/article/Petri_net)

## Build

`rebar3` is the main build tool used for this project. See the
[rebar3 web site](https://www.rebar3.org/) for details.

    $ rebar3 compile
    $ rebar3 eunit
    $ rebar3 dializer
