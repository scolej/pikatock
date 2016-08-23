PIKATOK
=======

_Tiny time tracking_

What is this?
-------------

Keep a plain text log file like so:

```
2016-08-22
0900-0930 warm up : waving the arms
0930-1030 warm up : waving the legs
1230-1700 rest of the day : not doing the work
1700-1730 rest of the day : maybe do a little bit of work
```

And then get nice a nice summed up tree of times as output:

```
6.5
    5.0 rest of the day
        4.5 not doing the work
        0.5 maybe do a little bit of work
    1.5 warm up
        1.0 waving the legs
        0.5 waving the arms
```

### Skip the repeated bits

The previous log can also be written:

```
2016-08-22
0900-0930 warm up : waving the arms
    -1030         : waving the legs
1230-1700 rest of the day : not doing the work
    -1730                 : maybe do a little bit of work
```

and the missing elements will be filled from the previous line.

Building
--------

`cabal build` should do it.

Usage
-----

Use me with an input file like this:

`pikatok input.time`