Code for computing sustainability and other
measures from a directed graph whose edges
are labeled by real numbers.

Look at the file `Test.elm` for examples,
then try this:

```
$ elm repl
> import Test
> Test.run ex2
{ alpha = 0.3848187874515115
  , edgeCount = 13
  , efficiency = 621.709
  , nodeCount = 8
  , resilience = 993.88
  , sustainability = 0.9564
  , totalFlow = 456.2 }
```

The test data is in CSV format:

```
ex2 =
    """
Lucca, Pablo, 30
Lucca, Karla, 90.4
Pablo, Ranulfo, 22
Karla, Luz, 40
Karla, Maria, 55
Karla, Ranulfo, 31.4
Jim, Karla, 30
Jim, Ranulfo, 20
Pablo, Karla, 34
Ranulfo, Lucca, 20
Luz, Maria, 22
George, Maria, 31.4
Lucca, Jim, 30
"""
```
